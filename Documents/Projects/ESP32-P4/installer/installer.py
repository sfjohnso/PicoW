"""Clock installer for the Waveshare ESP32-P4-WIFI6-Touch-LCD-3.5.

Takes a location, a Wi-Fi network name and its password, then flashes the clock program
and its settings to the board over USB. The location is either

  - a U.S. street address, turned into coordinates by the U.S. Census geocoder, or
  - a latitude and longitude anywhere in the world, e.g. "51.5074, -0.1278"
    (north and east positive; south and west negative, or add N/S/E/W).

Or skip all of that ("set up from my phone"): the clock then starts in setup mode, shows QR codes
on its screen, and the Wi-Fi network, password and location are entered on a phone.

The time zone comes from Open-Meteo and its daylight-saving rule from the IANA time zone
database (the tzdata package). The clock stores only the coordinates, a short place name
and the time zone, never a street address. The Wi-Fi name and password are stored in the
board's flash in plain text.

Run with no arguments for the window, or see --help for a scripted run.
"""
import argparse
import importlib.resources
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
                           "'123 Main St, City, ST 12345' (U.S. addresses only). Anywhere else, enter "
                           "a latitude and longitude instead, e.g. 51.5074, -0.1278.")
    m = matches[0]
    comp = m["addressComponents"]
    label = f"{comp.get('city', '')}, {comp.get('state', '')} {comp.get('zip', '')}".upper()
    return {"described": f"Address found: {m['matchedAddress']}", "lat": m["coordinates"]["y"],
            "lon": m["coordinates"]["x"], "label": _ascii_label(label)}


def time_zone_for(lat, lon):
    """Returns (IANA name, POSIX TZ string) for a coordinate anywhere in the world."""
    data = _get_json("https://api.open-meteo.com/v1/forecast?latitude=%.4f&longitude=%.4f"
                     "&current=temperature_2m&timezone=auto" % (lat, lon))
    iana = data.get("timezone", "")
    if not iana:
        raise InstallError("Could not determine the time zone for that location.")
    return iana, posix_tz(iana)


def posix_tz(iana):
    """The POSIX TZ rule for an IANA zone, read from the footer of its tzdata file."""
    parts = iana.split("/")
    package = "tzdata.zoneinfo" + "".join("." + p for p in parts[:-1])
    try:
        data = importlib.resources.files(package).joinpath(parts[-1]).read_bytes()
    except (ImportError, OSError) as e:
        raise InstallError(f"The time zone '{iana}' is not in the built-in time zone database.") from e
    rule = data.rstrip(b"\n").rsplit(b"\n", 1)[-1].decode("ascii", "replace") if data[:4] == b"TZif" else ""
    if not rule or len(rule) > 63:
        raise InstallError(f"Could not read the daylight-saving rule for '{iana}'.")
    # The clock's C library cannot parse a negative rule time (only Greenland has one,
    # "M3.5.0/-1"); "/0" shifts that one changeover by an hour on a single night.
    return re.sub(r"/-\d+(?::\d+)*", "/0", rule)


_NUM = r"[-+]?\d+(?:\.\d+)?"
_COORDS = re.compile(rf"^\s*({_NUM})\s*\u00b0?\s*([NSns])?\s*[,;/ ]\s*({_NUM})\s*\u00b0?\s*([EWew])?\s*$")


def parse_coordinates(text):
    """(lat, lon) if the text is 'latitude, longitude' in decimal degrees, else None.

    Latitude comes first. South and west are negative, or use an N/S/E/W letter.
    """
    m = _COORDS.match(text)
    if not m:
        return None
    lat, lon = float(m.group(1)), float(m.group(3))
    if m.group(2):
        lat = -abs(lat) if m.group(2).upper() == "S" else abs(lat)
    if m.group(4):
        lon = -abs(lon) if m.group(4).upper() == "W" else abs(lon)
    if not -90 <= lat <= 90:
        raise InstallError("The latitude must be between -90 and 90 (north is positive). "
                           "Enter the latitude first, then the longitude.")
    if not -180 <= lon <= 180:
        raise InstallError("The longitude must be between -180 and 180 (east is positive).")
    return lat, lon


def _ascii_label(text):
    """Plain printable ASCII, at most 39 characters (the clock's font has no other glyphs)."""
    return re.sub(r"[^ -~]", "", text).strip()[:39]


def locate(text):
    """Turns what the user typed (address or coordinates) into a dict: lat, lon, label, described."""
    if not text.strip():
        raise InstallError("Enter a U.S. street address, or a latitude and longitude.")
    coords = parse_coordinates(text)
    if coords is None:
        return geocode(text)
    lat, lon = coords
    label = "%.2f %s, %.2f %s" % (abs(lat), "N" if lat >= 0 else "S", abs(lon), "E" if lon >= 0 else "W")
    return {"described": "Coordinates: %.4f, %.4f" % (lat, lon), "lat": lat, "lon": lon, "label": label}


def make_config(ssid, password, g, posix_tz):
    """The JSON block the clock reads from its 'config' partition (NUL-terminated)."""
    cfg = {"ssid": ssid, "pass": password, "label": g["label"],
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


PHONE_SETUP_CONFIG = b"{}\0"        # empty settings: the clock starts in setup mode


def install(location, ssid, password, port, settings_only=False, phone_setup=False, log=print, confirm=None):
    """Flashes the clock and its settings. Raises InstallError.

    phone_setup: write no settings; the clock opens its setup page (QR codes) on first start.
    """
    if not port:
        raise InstallError("No board found. Plug the cable into the board's UART-C USB-C port (the "
                           "other USB-C port will not program), then click Refresh.")
    if phone_setup:
        config = PHONE_SETUP_CONFIG
        summary = ("The clock will start in setup mode: it shows two QR codes, and you enter the Wi-Fi network, "
                   "its password and the location on your phone.")
    else:
        check_wifi(ssid, password)
        log("Looking up the location...\n")
        g = locate(location)
        iana, posix_tz = time_zone_for(g["lat"], g["lon"])
        config = make_config(ssid, password, g, posix_tz)
        summary = (f"{g['described']}\nClock will show: {g['label']}\n"
                   f"Time zone: {iana}\nWi-Fi network: {ssid}")
    log(summary + "\n")
    if confirm and not confirm(summary):
        raise InstallError("Cancelled.")

    with tempfile.TemporaryDirectory() as tmp:
        cfg_path = os.path.join(tmp, "config.bin")
        with open(cfg_path, "wb") as f:
            f.write(config)
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
    log("\nDone. " + ("The clock restarts in setup mode: scan the QR code on its screen.\n" if phone_setup else
                         "The clock restarts by itself and shows the time in about 15 seconds.\n"))


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
    settings_only, show_pw, phone_setup = tk.BooleanVar(), tk.BooleanVar(), tk.BooleanVar()

    ttk.Label(frame, text="Location").grid(row=0, column=0, sticky="w", pady=4)
    addr_entry = ttk.Entry(frame, textvariable=address)
    addr_entry.grid(row=0, column=1, columnspan=2, sticky="ew")
    ttk.Label(frame, text="A U.S. street address (123 Main St, Springfield, IL 62701) or a latitude and "
                          "longitude anywhere in the world (51.5074, -0.1278; south and west are negative). "
                          "Only the coordinates, a short place name and the time zone are stored on the clock.",
              foreground="#555", wraplength=440, justify="left").grid(row=1, column=1, columnspan=2, sticky="w")
    ttk.Label(frame, text="Wi-Fi network name").grid(row=2, column=0, sticky="w", pady=(10, 4))
    ssid_entry = ttk.Entry(frame, textvariable=ssid)
    ssid_entry.grid(row=2, column=1, columnspan=2, sticky="ew", pady=(10, 4))
    ttk.Label(frame, text="Wi-Fi password").grid(row=3, column=0, sticky="w", pady=4)
    pw_entry = ttk.Entry(frame, textvariable=password, show="*")
    pw_entry.grid(row=3, column=1, sticky="ew")
    ttk.Checkbutton(frame, text="Show", variable=show_pw,
                    command=lambda: pw_entry.config(show="" if show_pw.get() else "*")).grid(row=3, column=2, padx=6)
    ttk.Label(frame, text="The board uses 2.4 GHz Wi-Fi only. The password is saved on the clock.",
              foreground="#555").grid(row=4, column=1, columnspan=2, sticky="w")
    ttk.Label(frame, text="Board (UART-C port)").grid(row=5, column=0, sticky="w", pady=(10, 4))
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

    def toggle_phone_setup():
        state = "disabled" if phone_setup.get() else "normal"
        for entry in (addr_entry, ssid_entry, pw_entry):
            entry.config(state=state)
    options = ttk.Frame(frame)
    options.grid(row=6, column=0, columnspan=3, sticky="w", pady=(10, 4))
    ttk.Checkbutton(options, text="I'll enter the Wi-Fi and location on my phone (the clock shows QR codes)",
                    variable=phone_setup, command=toggle_phone_setup).pack(anchor="w")
    ttk.Checkbutton(options, text="Only change the settings (keep the clock program already on the board)",
                    variable=settings_only).pack(anchor="w", pady=(4, 0))
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
                        messagebox.showinfo("Finished", "The clock is installed and restarting. With phone setup, "
                                                        "scan the QR code on its screen to finish; otherwise it "
                                                        "shows the time in about 15 seconds.")
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
             settings_only.get(), phone_setup.get()),)).start()
    install_btn.config(command=start)
    pump()
    root.mainloop()


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--location", "--address", dest="location",
                    help="scripted run: U.S. street address, or \"latitude, longitude\"")
    ap.add_argument("--ssid", help="scripted run: Wi-Fi network name")
    ap.add_argument("--password-env", metavar="NAME", help="scripted run: environment variable holding the password")
    ap.add_argument("--port", help="scripted run: serial port, e.g. COM12 (default: the Waveshare board)")
    ap.add_argument("--settings-only", action="store_true", help="only rewrite the settings, not the program")
    ap.add_argument("--phone-setup", action="store_true",
                    help="scripted run: write no Wi-Fi or location; the clock starts in phone setup mode")
    args = ap.parse_args()
    if not args.location and not args.phone_setup:
        run_gui()
        return
    port = args.port or next((p[0] for p in find_ports() if p[2]), "")
    console = sys.stdout            # run_esptool swaps sys.stdout for the logger, so keep the real one

    def log(text):
        if console:
            console.write(text)
            console.flush()
    try:
        install(args.location or "", args.ssid or "", os.environ.get(args.password_env or "", ""), port,
                args.settings_only, args.phone_setup, log=log)
    except InstallError as e:
        sys.exit(f"\n{e}")


if __name__ == "__main__":
    main()
