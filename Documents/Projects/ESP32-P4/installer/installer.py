"""Clock installer for the Waveshare ESP32-P4-WIFI6-Touch-LCD-3.5.

Takes a U.S. street address, a Wi-Fi network name and its password, then flashes the
clock program and its settings to the board over USB.

  - The address is turned into coordinates with the U.S. Census geocoder, and the time
    zone is looked up from Open-Meteo. Only the coordinates, "CITY, ST ZIP" and the time
    zone are stored on the clock, never the street address.
  - The Wi-Fi name and password are stored in the board's flash in plain text.

Run with no arguments for the window, or see --help for a scripted run.
"""
import argparse
import json
import os
import queue
import re
import ssl
import sys
import tempfile
import threading
import urllib.parse
import urllib.request

import certifi
import esptool
import serial.tools.list_ports

HERE = getattr(sys, "_MEIPASS", os.path.dirname(os.path.abspath(__file__)))
FIRMWARE_DIR = os.path.join(HERE, "firmware")

# Flash layout; must match p4clock/partitions.csv and build/flash_args.
APP_FILES = [(0x2000, "bootloader.bin"), (0x8000, "partition-table.bin"), (0x10000, "p4clock.bin")]
CONFIG_OFFSET = 0x187000
WAVESHARE_USB_VID = 0x1A86          # the board's USB-serial chip (CH343)

# IANA zone name (from Open-Meteo) -> POSIX TZ string used by the clock.
_EASTERN = "EST5EDT,M3.2.0,M11.1.0"
_CENTRAL = "CST6CDT,M3.2.0,M11.1.0"
_MOUNTAIN = "MST7MDT,M3.2.0,M11.1.0"
_PACIFIC = "PST8PDT,M3.2.0,M11.1.0"
_ALASKA = "AKST9AKDT,M3.2.0,M11.1.0"
TIME_ZONES = {
    "America/New_York": _EASTERN, "America/Detroit": _EASTERN,
    "America/Kentucky/Louisville": _EASTERN, "America/Kentucky/Monticello": _EASTERN,
    "America/Indiana/Indianapolis": _EASTERN, "America/Indiana/Vincennes": _EASTERN,
    "America/Indiana/Winamac": _EASTERN, "America/Indiana/Marengo": _EASTERN,
    "America/Indiana/Petersburg": _EASTERN, "America/Indiana/Vevay": _EASTERN,
    "America/Chicago": _CENTRAL, "America/Indiana/Tell_City": _CENTRAL,
    "America/Indiana/Knox": _CENTRAL, "America/Menominee": _CENTRAL,
    "America/North_Dakota/Center": _CENTRAL, "America/North_Dakota/New_Salem": _CENTRAL,
    "America/North_Dakota/Beulah": _CENTRAL,
    "America/Denver": _MOUNTAIN, "America/Boise": _MOUNTAIN,
    "America/Phoenix": "MST7",
    "America/Los_Angeles": _PACIFIC,
    "America/Anchorage": _ALASKA, "America/Juneau": _ALASKA, "America/Sitka": _ALASKA,
    "America/Metlakatla": _ALASKA, "America/Yakutat": _ALASKA, "America/Nome": _ALASKA,
    "America/Adak": "HST10HDT,M3.2.0,M11.1.0",
    "Pacific/Honolulu": "HST10",
    "America/Puerto_Rico": "AST4",
    "Pacific/Guam": "ChST-10",
    "Pacific/Pago_Pago": "SST11",
}


class InstallError(Exception):
    """A problem the user can act on; the message is shown as is."""


_ssl_context = ssl.create_default_context(cafile=certifi.where())


def _get_json(url):
    req = urllib.request.Request(url, headers={"User-Agent": "p4clock-installer"})
    try:
        with urllib.request.urlopen(req, timeout=20, context=_ssl_context) as resp:
            return json.load(resp)
    except OSError as e:
        raise InstallError(f"Could not reach {urllib.parse.urlparse(url).netloc}: {e}. "
                           "Check this computer's internet connection.") from e


def geocode(address):
    """U.S. Census geocoder: returns dict(matched, lat, lon, city, state, zip)."""
    if not address.strip():
        raise InstallError("Enter a street address.")
    url = ("https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?" +
           urllib.parse.urlencode({"address": address, "benchmark": "Public_AR_Current", "format": "json"}))
    matches = _get_json(url).get("result", {}).get("addressMatches", [])
    if not matches:
        raise InstallError("The Census geocoder could not find that address. Use the form "
                           "'123 Main St, City, ST 12345' (US addresses only).")
    m = matches[0]
    comp = m["addressComponents"]
    return {"matched": m["matchedAddress"], "lat": m["coordinates"]["y"], "lon": m["coordinates"]["x"],
            "city": comp.get("city", ""), "state": comp.get("state", ""), "zip": comp.get("zip", "")}


def time_zone_for(lat, lon):
    """Returns (IANA name, POSIX TZ string) for a coordinate."""
    data = _get_json("https://api.open-meteo.com/v1/forecast?latitude=%.4f&longitude=%.4f"
                     "&current=temperature_2m&timezone=auto" % (lat, lon))
    iana = data.get("timezone", "")
    if iana not in TIME_ZONES:
        raise InstallError(f"The time zone for that location ({iana or 'unknown'}) is not supported.")
    return iana, TIME_ZONES[iana]


def place_label(g):
    """'CITY, ST ZIP' in plain printable ASCII (the clock's font has no other glyphs)."""
    label = f"{g['city']}, {g['state']} {g['zip']}".upper()
    label = re.sub(r"[^ -~]", "", label).strip()
    return label[:39]


def make_config(ssid, password, g, posix_tz):
    """The JSON block the clock reads from its 'config' partition (NUL-terminated)."""
    cfg = {"ssid": ssid, "pass": password, "label": place_label(g),
           "lat": "%.4f" % g["lat"], "lon": "%.4f" % g["lon"], "tz": posix_tz}
    return json.dumps(cfg, separators=(",", ":")).encode("ascii") + b"\0"


def check_wifi(ssid, password):
    if not ssid or len(ssid.encode("utf-8")) > 32:
        raise InstallError("The network name must be 1 to 32 characters.")
    if not 8 <= len(password) <= 63:
        raise InstallError("The Wi-Fi password must be 8 to 63 characters (WPA2/WPA3 network). "
                           "Open networks are not supported. The board uses 2.4 GHz Wi-Fi only.")


def find_ports():
    """[(device, description, looks_like_the_board)], the board's chip first."""
    ports = [(p.device, p.description, p.vid == WAVESHARE_USB_VID) for p in serial.tools.list_ports.comports()]
    return sorted(ports, key=lambda p: (not p[2], p[0]))


class _LogWriter:
    def __init__(self, log):
        self.log = log

    def write(self, text):
        if text:
            self.log(text)
        return len(text or "")

    def flush(self):
        pass


def run_esptool(args, log):
    old = sys.stdout, sys.stderr
    sys.stdout = sys.stderr = _LogWriter(log)
    try:
        esptool.main(args)
    except SystemExit as e:
        if e.code not in (0, None):
            raise InstallError("Flashing failed (see the log). Check the USB cable and that no other "
                               "program is using the port.") from e
    except Exception as e:  # esptool raises its own error types for port and chip problems
        raise InstallError(f"Flashing failed: {e}") from e
    finally:
        sys.stdout, sys.stderr = old


def install(address, ssid, password, port, settings_only=False, log=print, confirm=None):
    """Looks up the address, then flashes the clock and its settings. Raises InstallError."""
    check_wifi(ssid, password)
    if not port:
        raise InstallError("No board found. Plug it into the USB-C port that makes it appear as a "
                           "COM port (the other USB-C port does not), then click Refresh.")
    log("Looking up the address...\n")
    g = geocode(address)
    iana, posix_tz = time_zone_for(g["lat"], g["lon"])
    summary = (f"Address found: {g['matched']}\nClock will show: {place_label(g)}\n"
               f"Time zone: {iana}\nWi-Fi network: {ssid}")
    log(summary + "\n")
    if confirm and not confirm(summary):
        raise InstallError("Cancelled.")

    with tempfile.TemporaryDirectory() as tmp:
        cfg_path = os.path.join(tmp, "config.bin")
        with open(cfg_path, "wb") as f:
            f.write(make_config(ssid, password, g, posix_tz))
        files = [(CONFIG_OFFSET, cfg_path)]
        if not settings_only:
            for offset, name in APP_FILES:
                path = os.path.join(FIRMWARE_DIR, name)
                if not os.path.isfile(path):
                    raise InstallError(f"Missing firmware file: {name}")
                files.append((offset, path))
        args = ["--chip", "esp32p4", "--port", port, "--baud", "460800", "--before", "default_reset",
                "--after", "hard_reset", "write_flash", "--flash_mode", "dio", "--flash_freq", "80m",
                "--flash_size", "16MB"]
        for offset, path in sorted(files):
            args += [hex(offset), path]
        log(f"\nWriting to the board on {port}...\n")
        run_esptool(args, log)
    log("\nDone. The clock restarts by itself and shows the time in about 15 seconds.\n")


# ---------------------------------------------------------------------------
# Window
# ---------------------------------------------------------------------------
def run_gui():
    import tkinter as tk
    from tkinter import messagebox, ttk

    root = tk.Tk()
    root.title("Waveshare P4 Clock Installer")
    root.minsize(560, 560)
    frame = ttk.Frame(root, padding=12)
    frame.pack(fill="both", expand=True)
    frame.columnconfigure(1, weight=1)

    address, ssid, password, port = tk.StringVar(), tk.StringVar(), tk.StringVar(), tk.StringVar()
    settings_only, show_pw = tk.BooleanVar(), tk.BooleanVar()

    ttk.Label(frame, text="U.S. street address").grid(row=0, column=0, sticky="w", pady=4)
    ttk.Entry(frame, textvariable=address).grid(row=0, column=1, columnspan=2, sticky="ew")
    ttk.Label(frame, text="e.g. 123 Main St, Springfield, IL 62701. Used once to find your coordinates;\n"
                          "the street address is not stored on the clock.",
              foreground="#555").grid(row=1, column=1, columnspan=2, sticky="w")
    ttk.Label(frame, text="Wi-Fi network name").grid(row=2, column=0, sticky="w", pady=(10, 4))
    ttk.Entry(frame, textvariable=ssid).grid(row=2, column=1, columnspan=2, sticky="ew", pady=(10, 4))
    ttk.Label(frame, text="Wi-Fi password").grid(row=3, column=0, sticky="w", pady=4)
    pw_entry = ttk.Entry(frame, textvariable=password, show="*")
    pw_entry.grid(row=3, column=1, sticky="ew")
    ttk.Checkbutton(frame, text="Show", variable=show_pw,
                    command=lambda: pw_entry.config(show="" if show_pw.get() else "*")).grid(row=3, column=2, padx=6)
    ttk.Label(frame, text="The board uses 2.4 GHz Wi-Fi only. The password is saved on the clock.",
              foreground="#555").grid(row=4, column=1, columnspan=2, sticky="w")
    ttk.Label(frame, text="Board (USB port)").grid(row=5, column=0, sticky="w", pady=(10, 4))
    port_box = ttk.Combobox(frame, textvariable=port, state="readonly")
    port_box.grid(row=5, column=1, sticky="ew", pady=(10, 4))

    port_devices = []

    def refresh_ports():
        ports = find_ports()
        port_devices[:] = [p[0] for p in ports]
        port_box["values"] = [f"{d}  {desc}" + ("   <- Waveshare board" if ours else "") for d, desc, ours in ports]
        if ports:
            port_box.current(0)
        else:
            port.set("")
    ttk.Button(frame, text="Refresh", command=refresh_ports).grid(row=5, column=2, padx=6)
    refresh_ports()

    ttk.Checkbutton(frame, text="Only change the settings (keep the clock program already on the board)",
                    variable=settings_only).grid(row=6, column=0, columnspan=3, sticky="w", pady=(10, 4))
    install_btn = ttk.Button(frame, text="Install")
    install_btn.grid(row=7, column=0, columnspan=3, sticky="ew", pady=8, ipady=4)
    bar = ttk.Progressbar(frame, mode="indeterminate")
    bar.grid(row=8, column=0, columnspan=3, sticky="ew")
    log_box = tk.Text(frame, height=14, wrap="word", state="disabled", font=("Consolas", 9))
    log_box.grid(row=9, column=0, columnspan=3, sticky="nsew", pady=(8, 0))
    frame.rowconfigure(9, weight=1)

    events = queue.Queue()

    def pump():
        try:
            while True:
                kind, payload = events.get_nowait()
                if kind == "log":
                    log_box.config(state="normal")
                    for part in re.split(r"(\r)", payload):
                        if part == "\r":                       # esptool progress: rewrite the last line
                            log_box.delete("end-1c linestart", "end-1c")
                        else:
                            log_box.insert("end", part)
                    log_box.see("end")
                    log_box.config(state="disabled")
                elif kind == "confirm":
                    text, answer = payload
                    answer["ok"] = messagebox.askyesno("Is this right?", text + "\n\nInstall now?")
                    answer["event"].set()
                elif kind == "done":
                    bar.stop()
                    install_btn.config(state="normal")
                    if payload is None:
                        messagebox.showinfo("Finished", "The clock is installed. It restarts by itself "
                                                        "and shows the time in about 15 seconds.")
                    elif str(payload) != "Cancelled.":
                        messagebox.showerror("Could not install", str(payload))
        except queue.Empty:
            pass
        root.after(100, pump)

    def confirm(text):
        answer = {"event": threading.Event()}
        events.put(("confirm", (text, answer)))
        answer["event"].wait()
        return answer["ok"]

    def work(args):
        try:
            install(*args, log=lambda t: events.put(("log", t)), confirm=confirm)
            events.put(("done", None))
        except InstallError as e:
            events.put(("log", f"\n{e}\n"))
            events.put(("done", e))

    def start():
        idx = port_box.current()
        install_btn.config(state="disabled")
        bar.start(12)
        threading.Thread(target=work, daemon=True, args=(
            (address.get(), ssid.get(), password.get(), port_devices[idx] if idx >= 0 else "",
             settings_only.get()),)).start()
    install_btn.config(command=start)
    pump()
    root.mainloop()


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--address", help="scripted run: U.S. street address")
    ap.add_argument("--ssid", help="scripted run: Wi-Fi network name")
    ap.add_argument("--password-env", metavar="NAME", help="scripted run: environment variable holding the password")
    ap.add_argument("--port", help="scripted run: serial port, e.g. COM12 (default: the Waveshare board)")
    ap.add_argument("--settings-only", action="store_true", help="only rewrite the settings, not the program")
    args = ap.parse_args()
    if not args.address:
        run_gui()
        return
    port = args.port or next((p[0] for p in find_ports() if p[2]), "")
    console = sys.stdout            # run_esptool swaps sys.stdout for the logger, so keep the real one

    def log(text):
        if console:
            console.write(text)
            console.flush()
    try:
        install(args.address, args.ssid or "", os.environ.get(args.password_env or "", ""), port,
                args.settings_only, log=log)
    except InstallError as e:
        sys.exit(f"\n{e}")


if __name__ == "__main__":
    main()
