"""Installer for the Waveshare ESP32-P4-WIFI6-Touch-LCD-3.5: launcher, clock, apps and SD card.

A five-step window:

  1. Connect the board over USB and flash the launcher and the clock, plus the clock's settings.
  2. Put the board's microSD card in this PC; the installer adds the apps (Quake Demo, Photos, Music, Life, Sound Meter) and
     creates the id1, Music and Photos folders. The Quake .pak files come from the user's own copy of Quake.
  3. The user adds music and photos.
  4. Eject the card.
  5. Put the card in the board; the installer restarts the board and checks that everything works.

The clock's settings are a location and a Wi-Fi network, or "set up from my phone". The location is any of

  - a U.S. ZIP code, e.g. "62701" (looked up first, as a U.S. ZIP),
  - a U.S. street address, turned into coordinates by the U.S. Census geocoder,
  - a city or postal code anywhere in the world (Open-Meteo's place search), or
  - a latitude and longitude anywhere in the world, e.g. "51.5074, -0.1278"
    (north and east positive; south and west negative, or add N/S/E/W).

The time zone comes from Open-Meteo and its daylight-saving rule from the IANA time zone
database (the tzdata package). The clock stores only the coordinates, a short place name
and the time zone, never a street address. The Wi-Fi name and password are stored in the
board's flash in plain text.

Run with no arguments for the window, or see --help for scripted runs of each step.
"""
import argparse
import ctypes
import importlib.resources
import json
import os
import queue
import re
import shutil
import ssl
import struct
import sys
import tempfile
import threading
import time
import unicodedata
import urllib.parse
import urllib.request

import certifi
import esptool
import serial
import serial.tools.list_ports

HERE = getattr(sys, "_MEIPASS", os.path.dirname(os.path.abspath(__file__)))
FIRMWARE_DIR = os.path.join(HERE, "firmware")
APPS_DIR = os.path.join(HERE, "apps")           # the apps that go on the SD card, named as they appear in the menu

# Flash layout; must match launcher/partitions.csv (the clock lives in ota_0, the launcher in the factory slot).
FLASH_FILES = [(0x2000, "bootloader.bin"), (0x8000, "partition-table.bin"), (0x10000, "launcher.bin"),
               (0x188000, "ota_data_initial.bin"), (0x190000, "p4clock.bin")]
CONFIG_OFFSET = 0x187000
WAVESHARE_USB_VID = 0x1A86          # the board's USB-serial chip (CH343)
CARD_FOLDERS = ("apps", "id1", "Music", "Photos")
CARD_FILESYSTEMS = ("FAT", "FAT32", "EXFAT")    # what the board can read


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


def census_lookup(address):
    """U.S. Census geocoder for a street address: dict(described, lat, lon, label), or None if it has no match."""
    url = ("https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?" +
           urllib.parse.urlencode({"address": address, "benchmark": "Public_AR_Current", "format": "json"}))
    matches = _get_json(url).get("result", {}).get("addressMatches", [])
    if not matches:
        return None
    m = matches[0]
    comp = m["addressComponents"]
    label = f"{comp.get('city', '')}, {comp.get('state', '')} {comp.get('zip', '')}".upper()
    return {"described": f"Address found: {m['matchedAddress']}", "lat": m["coordinates"]["y"],
            "lon": m["coordinates"]["x"], "label": _ascii_label(label)}


def place_search(name, country=None):
    """Open-Meteo place search (cities, towns and postal codes): dict(described, lat, lon, label), or None."""
    query = {"name": name, "count": 1, "language": "en", "format": "json"}
    if country:
        query["countryCode"] = country
    results = _get_json("https://geocoding-api.open-meteo.com/v1/search?" + urllib.parse.urlencode(query)).get("results")
    if not results:
        return None
    r = results[0]
    if not isinstance(r.get("latitude"), (int, float)) or not isinstance(r.get("longitude"), (int, float)):
        return None
    region = r.get("admin1") or r.get("country_code") or ""
    label = _ascii_label(f"{r.get('name', '')}, {region}".upper())
    where = ", ".join(x for x in (r.get("name"), r.get("admin1"), r.get("country")) if x)
    return {"described": f"{'ZIP code' if country == 'US' and name[:1].isdigit() else 'Place'} found: {where}",
            "lat": r["latitude"], "lon": r["longitude"], "label": label}


_ZIP = re.compile(r"^\s*(\d{5})(?:-\d{4})?\s*$")
_TRAILING_ZIP = re.compile(r"(?<!\d)(\d{5})(?:-\d{4})?\s*$")


def zip5(text):
    """The five digits of a bare U.S. ZIP code ('91320' or '91320-1234'), else None."""
    m = _ZIP.match(text)
    return m.group(1) if m else None


def trailing_zip(text):
    """The ZIP code at the end of an address such as '123 Main St, Springfield, IL 62701', else None."""
    m = _TRAILING_ZIP.search(text)
    return m.group(1) if m else None


def geocode(text):
    """Turns a street address, city, ZIP code or a mix like 'Springfield, 62701' into coordinates.

    A bare U.S. ZIP code is looked up first, as a U.S. ZIP (the same digits are also postal codes elsewhere: 75001 is
    in Texas as well as in Paris). Then, in order: the Census geocoder for a street address; a ZIP code at the end of
    the text (an address Census does not know still has its ZIP); Open-Meteo's search for a city or postal code
    anywhere; and each comma-separated piece on its own, ZIP codes first.
    """
    if not text.strip():
        raise InstallError("Enter a U.S. street address, a city, a ZIP code, or a latitude and longitude.")
    z = zip5(text)
    if z:
        found = place_search(z, "US")
        if found:
            return found
    found = census_lookup(text)
    if found:
        return found
    z = trailing_zip(text)
    if z:
        found = place_search(z, "US")
        if found:
            return found
    found = place_search(text)
    if found:
        return found
    pieces = [p.strip() for p in text.split(",") if p.strip()]
    for piece in sorted(pieces, key=lambda p: zip5(p) is None):        # ZIP pieces first
        z = zip5(piece)
        found = place_search(z, "US") if z else place_search(piece)
        if found:
            return found
    raise InstallError("Could not find that place. Use a U.S. street address ('123 Main St, City, ST 12345'), a "
                       "city, a ZIP code, or a latitude and longitude such as 51.5074, -0.1278.")


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
    """Plain printable ASCII, at most 39 characters (the clock's font has no other glyphs); accents are dropped."""
    text = unicodedata.normalize("NFKD", text).encode("ascii", "ignore").decode("ascii")
    return re.sub(r"[^ -~]", "", text).strip()[:39]


def locate(text):
    """Turns what the user typed (coordinates, ZIP code, address or city) into a dict: lat, lon, label, described."""
    if not text.strip():
        raise InstallError("Enter a U.S. ZIP code, a street address, a city, or a latitude and longitude.")
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


# ---------------------------------------------------------------------------
# Step 1: the board
# ---------------------------------------------------------------------------
PHONE_SETUP_CONFIG = b"{}\0"        # empty settings: the clock starts in setup mode
MODES = ("all", "programs", "settings")     # what step 1 writes: everything / the programs only / the settings only


def install(location, ssid, password, port, mode="all", phone_setup=False, log=print, confirm=None):
    """Flashes the launcher and clock and/or the clock's settings. Raises InstallError.

    mode: "all" (programs and settings), "programs" (keep the settings already on the board) or "settings".
    phone_setup: write no settings; the clock opens its setup page (QR codes) on first start.
    """
    if mode not in MODES:
        raise ValueError(mode)
    if not port:
        raise InstallError("No board found. Plug the cable into the board's UART-C USB-C port (the "
                           "other USB-C port will not program), then click Refresh.")
    files = []
    summary = ""
    if mode != "settings":
        for offset, name in FLASH_FILES:
            path = os.path.join(FIRMWARE_DIR, name)
            if not os.path.isfile(path):
                raise InstallError(f"Missing firmware file: {name}")
            files.append((offset, path))
        summary = "The launcher and the clock will be written to the board.\n"
    config = None
    if mode != "programs":
        if phone_setup:
            config = PHONE_SETUP_CONFIG
            summary += ("The clock will start in setup mode: it shows two QR codes, and you enter the Wi-Fi network, "
                        "its password and the location on your phone.")
        else:
            check_wifi(ssid, password)
            log("Looking up the location...\n")
            g = locate(location)
            iana, tz_rule = time_zone_for(g["lat"], g["lon"])
            config = make_config(ssid, password, g, tz_rule)
            summary += (f"{g['described']}\nClock will show: {g['label']}\n"
                        f"Time zone: {iana}\nWi-Fi network: {ssid}")
    else:
        summary += "The Wi-Fi and location already on the board are kept."
    log(summary + "\n")
    if confirm and not confirm(summary):
        raise InstallError("Cancelled.")

    with tempfile.TemporaryDirectory() as tmp:
        if config is not None:
            cfg_path = os.path.join(tmp, "config.bin")
            with open(cfg_path, "wb") as f:
                f.write(config)
            files.append((CONFIG_OFFSET, cfg_path))
        args = ["--chip", "esp32p4", "--port", port, "--baud", "460800", "--before", "default_reset",
                "--after", "hard_reset", "write_flash", "--flash_mode", "dio", "--flash_freq", "80m",
                "--flash_size", "16MB"]
        for offset, path in sorted(files):
            args += [hex(offset), path]
        log(f"\nWriting to the board on {port}...\n")
        run_esptool(args, log)
    log("\nDone. The board restarts into the launcher, which starts the clock after 3 seconds.\n")


# ---------------------------------------------------------------------------
# Step 2: the SD card
# ---------------------------------------------------------------------------
def list_drives():
    """Removable drives with media: [(root, label, filesystem, size_bytes)]. Windows only."""
    k = ctypes.windll.kernel32
    out = []
    mask = k.GetLogicalDrives()
    for i in range(26):
        if not mask & (1 << i):
            continue
        root = f"{chr(65 + i)}:\\"
        if k.GetDriveTypeW(root) != 2:                  # DRIVE_REMOVABLE
            continue
        label, fs = ctypes.create_unicode_buffer(261), ctypes.create_unicode_buffer(261)
        if not k.GetVolumeInformationW(root, label, 261, None, None, None, fs, 261):
            continue                                    # no card in this reader
        total = ctypes.c_ulonglong()
        k.GetDiskFreeSpaceExW(root, None, ctypes.byref(total), None)
        out.append((root, label.value, fs.value, total.value))
    return out


def describe_drive(drive):
    root, label, fs, size = drive
    return f"{root[:2]}  {label or '(no label)'}  {fs}  {size / 1e9:.1f} GB"


def find_quake_data():
    """The folder holding PAK0.PAK in a Steam or GOG install of Quake, or ''."""
    bases = []
    for env in ("ProgramFiles(x86)", "ProgramFiles"):
        if os.environ.get(env):
            bases.append(os.path.join(os.environ[env], "Steam"))
    for steam in list(bases):                           # other Steam library folders
        vdf = os.path.join(steam, "steamapps", "libraryfolders.vdf")
        try:
            with open(vdf, encoding="utf-8", errors="replace") as f:
                bases += [p.replace("\\\\", "\\") for p in re.findall(r'"path"\s+"([^"]+)"', f.read())]
        except OSError:
            pass
    candidates = [os.path.join(b, "steamapps", "common", "Quake", "id1") for b in bases]
    candidates += [r"C:\GOG Games\Quake\id1", r"C:\Program Files (x86)\GOG Galaxy\Games\Quake\id1"]
    return next((c for c in candidates if os.path.isfile(os.path.join(c, "PAK0.PAK"))), "")


def bundled_apps():
    """File names of the apps bundled with the installer."""
    if not os.path.isdir(APPS_DIR):
        raise InstallError("The apps are missing from this installer (apps folder).")
    apps = sorted(f for f in os.listdir(APPS_DIR) if f.lower().endswith(".bin"))
    if not apps:
        raise InstallError("The apps are missing from this installer (apps folder).")
    return apps


def _copy(src, dst, log):
    size = os.path.getsize(src)
    log(f"  {os.path.basename(dst)}  ({size / 1e6:.1f} MB)\n")
    done, shown = 0, 0
    with open(src, "rb") as fi, open(dst, "wb") as fo:
        while True:
            block = fi.read(1 << 20)
            if not block:
                break
            fo.write(block)
            done += len(block)
            if size > 8 << 20 and done * 10 // size > shown:
                shown = done * 10 // size
                log(f"\r    {shown * 10}%")
        fo.flush()
        os.fsync(fo.fileno())
    if size > 8 << 20:
        log("\r    done\n")


def prepare_card(root, pak_dir="", log=print):
    """Adds the apps, the folders and the Quake data to the card at root (a drive such as 'E:' or any folder).

    Never formats anything and only overwrites this installer's own app files. Returns a list of warnings.
    """
    root = root + "\\" if re.fullmatch(r"[A-Za-z]:", root) else root
    if not os.path.isdir(root):
        raise InstallError(f"Cannot find {root}. Insert the card and refresh the list.")
    apps = bundled_apps()
    paks = []
    if pak_dir:
        paks = sorted(f for f in os.listdir(pak_dir) if f.lower().endswith(".pak")) if os.path.isdir(pak_dir) else []
        if not paks:
            raise InstallError(f"No .pak files in {pak_dir}. Pick the folder that holds PAK0.PAK (for Steam: "
                               "...\\steamapps\\common\\Quake\\id1), or leave it empty to skip Quake's data.")
    need = sum(os.path.getsize(os.path.join(APPS_DIR, a)) for a in apps)
    need += sum(os.path.getsize(os.path.join(pak_dir, p)) for p in paks)
    if shutil.disk_usage(root).free < need + (50 << 20):
        raise InstallError("There is not enough free space on the card for the apps and the Quake data.")

    log("Creating folders...\n")
    for folder in CARD_FOLDERS:
        os.makedirs(os.path.join(root, folder), exist_ok=True)
        log(f"  {folder}\n")
    log("Adding the apps to the apps folder...\n")
    for app in apps:
        _copy(os.path.join(APPS_DIR, app), os.path.join(root, "apps", app), log)
    warnings = []
    if paks:
        log("Adding the Quake data to the id1 folder...\n")
        for p in paks:
            dst = os.path.join(root, "id1", p)
            if os.path.isfile(dst) and os.path.getsize(dst) == os.path.getsize(os.path.join(pak_dir, p)):
                log(f"  {p} is already there\n")
            else:
                _copy(os.path.join(pak_dir, p), dst, log)
        if not any(p.lower() == "pak0.pak" for p in paks):
            warnings.append("PAK0.PAK was not among the Quake files: Quake Demo needs it.")
        elif not any(p.lower() == "pak1.pak" for p in paks):
            warnings.append("Only PAK0.PAK was added (the shareware episode); Quake Demo will play that.")
    elif not any(f.lower() == "pak0.pak" for f in os.listdir(os.path.join(root, "id1"))):
        warnings.append("No Quake data was added: Quake Demo will not start until PAK0.PAK is in the id1 folder.")
    log("The card is ready.\n")
    return warnings


def check_card_filesystem(drive):
    """Raises InstallError if the board could not read this card."""
    root, label, fs, size = drive
    if fs.upper() not in CARD_FILESYSTEMS:
        raise InstallError(f"The card is formatted as {fs}. The board reads FAT32 or exFAT only. Reformat the card "
                           "(this installer never does that), then run this step again.")


# ---------------------------------------------------------------------------
# Step 4: eject
# ---------------------------------------------------------------------------
def eject_drive(root, log=print):
    """Flushes, locks, dismounts and ejects the volume (what 'Safely Remove Hardware' does)."""
    letter = root[0].upper()
    k = ctypes.windll.kernel32
    k.CreateFileW.restype = ctypes.c_void_p
    k.CreateFileW.argtypes = [ctypes.c_wchar_p, ctypes.c_uint32, ctypes.c_uint32, ctypes.c_void_p,
                              ctypes.c_uint32, ctypes.c_uint32, ctypes.c_void_p]
    k.CloseHandle.argtypes = [ctypes.c_void_p]
    k.FlushFileBuffers.argtypes = [ctypes.c_void_p]
    k.DeviceIoControl.argtypes = [ctypes.c_void_p, ctypes.c_uint32, ctypes.c_void_p, ctypes.c_uint32,
                                  ctypes.c_void_p, ctypes.c_uint32, ctypes.c_void_p, ctypes.c_void_p]
    h = k.CreateFileW(f"\\\\.\\{letter}:", 0x80000000 | 0x40000000, 3, None, 3, 0, None)   # read+write, share all, open existing
    if h in (None, ctypes.c_void_p(-1).value):
        raise InstallError("Windows would not let the installer eject the card. Use the 'Safely Remove Hardware' "
                           "icon in the taskbar instead, then tick the box below.")
    returned = ctypes.c_uint32()

    def ioctl(code, data=None):
        buf = ctypes.create_string_buffer(data, len(data)) if data else None
        return bool(k.DeviceIoControl(h, code, buf, len(data) if data else 0, None, 0, ctypes.byref(returned), None))
    try:
        k.FlushFileBuffers(h)
        if not ioctl(0x00090018):                       # FSCTL_LOCK_VOLUME
            raise InstallError("The card is still in use. Close any windows that show it (File Explorer, a music "
                               "or photo program) and try again.")
        ioctl(0x00090020)                               # FSCTL_DISMOUNT_VOLUME
        ioctl(0x002D0804, struct.pack("<B", 0))         # IOCTL_STORAGE_MEDIA_REMOVAL: allow removal
        if not ioctl(0x002D4808):                       # IOCTL_STORAGE_EJECT_MEDIA
            raise InstallError("The card reader would not eject it. Use the 'Safely Remove Hardware' icon in the "
                               "taskbar instead, then tick the box below.")
    finally:
        k.CloseHandle(h)
    log(f"{letter}: is ejected. You can take the card out now.\n")


# ---------------------------------------------------------------------------
# Step 5: test and verify
# ---------------------------------------------------------------------------
def _capture(port, seconds, until=None, log=None):
    """Resets the board through the serial chip's RTS line and returns its console output."""
    try:
        ser = serial.Serial(port, 115200, timeout=0.2)
    except (OSError, serial.SerialException) as e:
        raise InstallError(f"Cannot open {port}: {e}. Is another program using it?") from e
    text = ""
    try:
        ser.dtr = False
        ser.rts = True
        time.sleep(0.1)
        ser.rts = False
        end = time.time() + seconds
        while time.time() < end:
            data = ser.read(4096)
            if data:
                text += data.decode("utf-8", "replace")
                if until and until(text):
                    time.sleep(0.5)
                    text += ser.read(4096).decode("utf-8", "replace")
                    break
    finally:
        ser.close()
    return text


def verify_board(port, expected_apps, phone_setup=False, log=print):
    """Restarts the board twice and reads its log. Returns (all_ok, [(ok, text)])."""
    if not port:
        raise InstallError("No board found. Connect the board to this PC with the UART-C cable, then click Refresh.")
    checks = []

    def check(ok, text):
        checks.append((ok, text))
        log(("  [ok]    " if ok else "  [FAIL]  ") + text + "\n")

    log("Restarting the board and watching it start (about 40 seconds)...\n")
    clock_up = (lambda t: "p4clock start" in t and "settings" in t and ("got IP" in t or "setup" in t.lower())) \
        if not phone_setup else (lambda t: "p4clock start" in t)
    a = _capture(port, 45, until=clock_up)
    check("Loaded app from partition at offset 0x10000" in a and "Project name:     launcher" in a,
          "The board starts the launcher")
    if "SD mount failed" in a:
        check(False, "The SD card was not detected: re-seat the card in the board's microSD slot and run the test again")
    else:
        m = re.search(r"launcher: (\d+) app\(s\) on the SD card, (\d+) skipped", a)
        if m:
            n = int(m.group(1))
            check(n >= expected_apps, f"The launcher found {n} app(s) on the SD card"
                  + ("" if n >= expected_apps else f" (expected {expected_apps})"))
            if int(m.group(2)):
                check(False, f"{m.group(2)} file(s) in the card's apps folder are not apps for this board")
        else:
            check(False, "The launcher found no apps: is there an apps folder on the card, and is the card FAT32 or exFAT?")
    check("booting Clock" in a, "The 3-second countdown starts the clock")
    check("Loaded app from partition at offset 0x190000" in a and "Project name:     p4clock" in a,
          "The clock runs from its own slot")
    m = re.search(r"settings from the config partition: network '([^']*)', place '([^']*)'", a)
    if phone_setup:
        check("p4clock start" in a, "The clock is running and waiting for phone setup (scan the QR code on its screen)")
    else:
        check(bool(m), "The clock found its settings" + (f" (network '{m.group(1)}', place '{m.group(2)}')" if m else ""))
        m = re.search(r"got IP (\S+)", a)
        check(bool(m), "The clock joined Wi-Fi" + (f" and got {m.group(1)}" if m else
                                                    " (it did not within 45 s: check the Wi-Fi name and password, "
                                                    "and that the network is 2.4 GHz)"))
    log("Pressing Reset to check that the board returns to the menu...\n")
    b = _capture(port, 8, until=lambda t: "app_init: Project name:" in t)
    check("Loaded app from partition at offset 0x10000" in b and "Project name:     launcher" in b,
          "Reset returns to the launcher menu")
    return all(ok for ok, _ in checks), checks


# ---------------------------------------------------------------------------
# Window
# ---------------------------------------------------------------------------
STEPS = [
    ("Install the launcher and the clock",
     "Connect the board to this PC with a USB-C cable in the UART-C port (the board's other USB-C port will not "
     "program it). If no port appears, install the \"CH343\" USB serial driver from WCH (wch-ic.com)."),
    ("Add the apps to the SD card",
     "Put the board's microSD card in this PC (a card reader or the laptop's slot). This adds the apps and creates "
     "the apps, id1, Music and Photos folders. Nothing on the card is deleted or formatted."),
    ("Add your music and photos",
     "Copy your music and photos onto the card now. Music: one folder per album in the Music folder, each holding "
     "that album's .mp3 files (track numbers at the start of the file names put them in order). Photos: .jpg, "
     ".jpeg and .png files in the Photos folder."),
    ("Eject the SD card",
     "Eject the card so nothing is lost, then take it out of this PC."),
    ("Put the card in the board, then test",
     "Push the card into the board's microSD slot. Keep the USB cable connected: the installer restarts the board "
     "and checks that the launcher, the SD card, the apps and the clock all work."),
]


def run_gui():
    import tkinter as tk
    from tkinter import filedialog, messagebox, ttk

    root = tk.Tk()
    root.title("Waveshare P4 Installer")
    root.minsize(640, 720)
    outer = ttk.Frame(root, padding=12)
    outer.pack(fill="both", expand=True)

    title = ttk.Label(outer, font=("Segoe UI", 13, "bold"))
    title.pack(anchor="w")
    blurb = ttk.Label(outer, wraplength=600, justify="left", foreground="#444")
    blurb.pack(anchor="w", pady=(2, 8))
    body = ttk.Frame(outer)
    body.pack(fill="x")
    bar = ttk.Progressbar(outer, mode="indeterminate")
    bar.pack(fill="x", pady=(8, 0))
    log_box = tk.Text(outer, height=12, wrap="word", state="disabled", font=("Consolas", 9))
    log_box.pack(fill="both", expand=True, pady=(6, 0))
    nav = ttk.Frame(outer)
    nav.pack(fill="x", pady=(8, 0))
    back_btn = ttk.Button(nav, text="< Back")
    back_btn.pack(side="left")
    next_btn = ttk.Button(nav, text="Next >")
    next_btn.pack(side="right")

    frames = [ttk.Frame(body) for _ in STEPS]
    done = [False] * len(STEPS)
    state = {"step": 0, "busy": False, "mode": "all", "phone": False, "drive": ""}

    # ---- step 1: board
    f = frames[0]
    f.columnconfigure(1, weight=1)
    address, ssid, password, port = tk.StringVar(), tk.StringVar(), tk.StringVar(), tk.StringVar()
    mode, show_pw, phone_setup = tk.StringVar(value="all"), tk.BooleanVar(), tk.BooleanVar()
    ttk.Label(f, text="Location").grid(row=0, column=0, sticky="w", pady=4)
    addr_entry = ttk.Entry(f, textvariable=address)
    addr_entry.grid(row=0, column=1, columnspan=2, sticky="ew")
    ttk.Label(f, text="A U.S. ZIP code (62701), a U.S. street address (123 Main St, Springfield, IL 62701), a city, "
                      "or a latitude and longitude anywhere in the world (51.5074, -0.1278; south and west are "
                      "negative). Only the coordinates, a short place name and the time zone are stored on the clock.",
              foreground="#555", wraplength=440, justify="left").grid(row=1, column=1, columnspan=2, sticky="w")
    ttk.Label(f, text="Wi-Fi network name").grid(row=2, column=0, sticky="w", pady=(8, 4))
    ssid_entry = ttk.Entry(f, textvariable=ssid)
    ssid_entry.grid(row=2, column=1, columnspan=2, sticky="ew", pady=(8, 4))
    ttk.Label(f, text="Wi-Fi password").grid(row=3, column=0, sticky="w", pady=4)
    pw_entry = ttk.Entry(f, textvariable=password, show="*")
    pw_entry.grid(row=3, column=1, sticky="ew")
    ttk.Checkbutton(f, text="Show", variable=show_pw,
                    command=lambda: pw_entry.config(show="" if show_pw.get() else "*")).grid(row=3, column=2, padx=6)
    ttk.Label(f, text="The board uses 2.4 GHz Wi-Fi only. The password is saved on the clock.",
              foreground="#555").grid(row=4, column=1, columnspan=2, sticky="w")
    ttk.Label(f, text="Board (UART-C port)").grid(row=5, column=0, sticky="w", pady=(8, 4))
    port_box = ttk.Combobox(f, textvariable=port, state="readonly")
    port_box.grid(row=5, column=1, sticky="ew", pady=(8, 4))
    port_devices = []

    def refresh_ports():
        ports = find_ports()
        port_devices[:] = [p[0] for p in ports]
        port_box["values"] = [f"{d}  {desc}" + ("   <- Waveshare board" if ours else "") for d, desc, ours in ports]
        if ports:
            port_box.current(0)
        else:
            port.set("")
    ttk.Button(f, text="Refresh", command=refresh_ports).grid(row=5, column=2, padx=6)
    refresh_ports()

    def update_fields():
        off = mode.get() == "programs" or phone_setup.get()
        for entry in (addr_entry, ssid_entry, pw_entry):
            entry.config(state="disabled" if off else "normal")
        phone_chk.config(state="disabled" if mode.get() == "programs" else "normal")
    opts = ttk.Frame(f)
    opts.grid(row=6, column=0, columnspan=3, sticky="w", pady=(8, 2))
    phone_chk = ttk.Checkbutton(opts, text="I'll enter the Wi-Fi and location on my phone (the clock shows QR codes)",
                                variable=phone_setup, command=update_fields)
    phone_chk.pack(anchor="w")
    for value, text in (("all", "First install: write the programs and the settings"),
                        ("programs", "Update the programs only (keep the Wi-Fi and location on the board)"),
                        ("settings", "Only change the Wi-Fi and location")):
        ttk.Radiobutton(opts, text=text, variable=mode, value=value, command=update_fields).pack(anchor="w")
    install_btn = ttk.Button(f, text="Install on the board")
    install_btn.grid(row=7, column=0, columnspan=3, sticky="ew", pady=8, ipady=3)

    # ---- step 2: card
    f = frames[1]
    f.columnconfigure(1, weight=1)
    drive_var, pak_var = tk.StringVar(), tk.StringVar()
    ttk.Label(f, text="SD card").grid(row=0, column=0, sticky="w", pady=4)
    drive_box = ttk.Combobox(f, textvariable=drive_var, state="readonly")
    drive_box.grid(row=0, column=1, sticky="ew")
    drives = []

    def refresh_drives():
        drives[:] = list_drives()
        drive_box["values"] = [describe_drive(d) for d in drives]
        if drives:
            drive_box.current(0)
        else:
            drive_var.set("")
    ttk.Button(f, text="Refresh", command=refresh_drives).grid(row=0, column=2, padx=6)
    ttk.Label(f, text="Quake data (.pak)").grid(row=1, column=0, sticky="w", pady=(10, 4))
    ttk.Entry(f, textvariable=pak_var).grid(row=1, column=1, sticky="ew", pady=(10, 4))
    ttk.Button(f, text="Browse...", command=lambda: pak_var.set(
        filedialog.askdirectory(title="Folder holding PAK0.PAK (and PAK1.PAK)") or pak_var.get())).grid(row=1, column=2, padx=6)
    ttk.Label(f, text="Quake Demo needs your own copy of Quake's data: the folder that holds PAK0.PAK and PAK1.PAK "
                      "(Steam: ...\\steamapps\\common\\Quake\\id1). The installer cannot supply it. Leave this empty to "
                      "skip it; the other apps do not need it.", foreground="#555", wraplength=520,
              justify="left").grid(row=2, column=1, columnspan=2, sticky="w")
    card_btn = ttk.Button(f, text="Add the files to the card")
    card_btn.grid(row=3, column=0, columnspan=3, sticky="ew", pady=10, ipady=3)
    found = find_quake_data()
    if found:
        pak_var.set(found)
    refresh_drives()

    # ---- step 3: music and photos
    f = frames[2]
    music_btn = ttk.Button(f, text="Open the Music folder")
    music_btn.pack(fill="x", pady=(6, 4), ipady=3)
    photos_btn = ttk.Button(f, text="Open the Photos folder")
    photos_btn.pack(fill="x", pady=4, ipady=3)
    ttk.Label(f, text="Click Next when you are finished. You can also add more later by putting the card back in "
                      "this PC.", foreground="#555", wraplength=560, justify="left").pack(anchor="w", pady=4)

    # ---- step 4: eject
    f = frames[3]
    eject_btn = ttk.Button(f, text="Eject the card")
    eject_btn.pack(fill="x", pady=(6, 4), ipady=3)
    ejected_by_hand = tk.BooleanVar()
    ttk.Checkbutton(f, text="I removed the card myself with 'Safely Remove Hardware'", variable=ejected_by_hand,
                    command=lambda: (done.__setitem__(3, ejected_by_hand.get() or done[3]), update_nav())).pack(anchor="w")

    # ---- step 5: test
    f = frames[4]
    verify_btn = ttk.Button(f, text="Restart the board and test")
    verify_btn.pack(fill="x", pady=(6, 4), ipady=3)
    ttk.Label(f, text="After it passes, try the apps yourself: restart the board and touch the screen within 3 "
                      "seconds to keep the menu, then tap Photos, Music, Life, Sound Meter or Quake Demo and Launch. The Reset button "
                      "always brings you back to the menu.", foreground="#555", wraplength=560,
              justify="left").pack(anchor="w", pady=4)

    # ---- plumbing
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
                    answer["ok"] = messagebox.askyesno("Is this right?", text)
                    answer["event"].set()
                elif kind == "done":
                    step, error, extra = payload
                    state["busy"] = False
                    bar.stop()
                    if error is None:
                        done[step] = True
                        if extra:
                            messagebox.showinfo(extra[0], extra[1])
                    elif str(error) != "Cancelled.":
                        messagebox.showerror("Could not finish this step", str(error))
                    update_nav()
        except queue.Empty:
            pass
        root.after(100, pump)

    def log(text):
        events.put(("log", text))

    def confirm(text):
        answer = {"event": threading.Event()}
        events.put(("confirm", (text, answer)))
        answer["event"].wait()
        return answer["ok"]

    def run_step(step, fn):
        """Runs fn() in a thread; it returns None or (title, message) to show when it succeeds."""
        state["busy"] = True
        bar.start(12)
        update_nav()

        def work():
            try:
                extra = fn()
                events.put(("done", (step, None, extra)))
            except InstallError as e:
                log(f"\n{e}\n")
                events.put(("done", (step, e, None)))
            except Exception as e:                      # unexpected: show it instead of dying silently
                log(f"\nUnexpected error: {e!r}\n")
                events.put(("done", (step, InstallError(f"Unexpected error: {e!r}"), None)))
        threading.Thread(target=work, daemon=True).start()

    def update_nav():
        s = state["step"]
        busy = state["busy"]
        title.config(text=f"Step {s + 1} of {len(STEPS)}: {STEPS[s][0]}")
        blurb.config(text=STEPS[s][1])
        back_btn.config(state="normal" if s > 0 and not busy else "disabled")
        last = s == len(STEPS) - 1
        next_btn.config(text="Finish" if last else "Next >",
                        state="normal" if (done[s] or s == 2) and not busy else "disabled")
        for b in (install_btn, card_btn, music_btn, photos_btn, eject_btn, verify_btn):
            b.config(state="disabled" if busy else "normal")

    def show(step):
        state["step"] = step
        for i, fr in enumerate(frames):
            if i == step:
                fr.pack(fill="x")
            else:
                fr.pack_forget()
        update_nav()

    def go_next():
        if state["step"] == len(STEPS) - 1:
            root.destroy()
        else:
            show(state["step"] + 1)
    next_btn.config(command=go_next)
    back_btn.config(command=lambda: show(state["step"] - 1))

    # ---- step actions
    def do_install():
        idx = port_box.current()
        args = (address.get(), ssid.get(), password.get(), port_devices[idx] if idx >= 0 else "",
                mode.get(), phone_setup.get())
        state["mode"], state["phone"] = mode.get(), phone_setup.get()
        run_step(0, lambda: install(*args, log=log, confirm=lambda t: confirm(t + "\n\nWrite this to the board now?")) or
                 ("Board is ready", "The launcher and the clock are installed. The board restarts and shows the clock."))
    install_btn.config(command=do_install)

    def chosen_drive():
        idx = drive_box.current()
        if idx < 0:
            raise InstallError("No SD card found. Put the card in this PC, then click Refresh.")
        return drives[idx]

    def do_card():
        idx = drive_box.current()
        if idx < 0:
            messagebox.showerror("No card", "No SD card found. Put the card in this PC, then click Refresh.")
            return
        drive, pak = drives[idx], pak_var.get().strip()
        if not messagebox.askyesno("Add files to this card?", f"Add the apps and folders to:\n\n{describe_drive(drive)}\n\n"
                                   "Nothing on the card is deleted or formatted."):
            return
        state["drive"] = drive[0]

        def work():
            check_card_filesystem(drive)
            warnings = prepare_card(drive[0], pak, log=log)
            return ("Card is ready", "The apps and folders are on the card." + ("".join("\n\n" + w for w in warnings)))
        run_step(1, work)
    card_btn.config(command=do_card)

    def open_folder(name):
        drive = state["drive"] or (drives[drive_box.current()][0] if drive_box.current() >= 0 else "")
        path = os.path.join(drive, name)
        if drive and os.path.isdir(path):
            os.startfile(path)
        else:
            messagebox.showerror("Card not found", "Put the card back in this PC (and finish step 2) first.")
    music_btn.config(command=lambda: open_folder("Music"))
    photos_btn.config(command=lambda: open_folder("Photos"))

    def do_eject():
        if not state["drive"]:
            messagebox.showerror("No card", "Finish step 2 first.")
            return
        run_step(3, lambda: eject_drive(state["drive"], log=log) or
                 ("Card ejected", "You can take the card out of this PC and push it into the board."))
    eject_btn.config(command=do_eject)

    def do_verify():
        idx = port_box.current()
        board = port_devices[idx] if idx >= 0 else (port_devices[0] if port_devices else "")

        def work():
            ok, checks = verify_board(board, len(bundled_apps()), state["phone"], log=log)
            if ok:
                return ("Installation verified", "Everything works. Press Finish.")
            raise InstallError("Some checks failed (see the log). Fix the problem and click the button again.")
        run_step(4, work)
    verify_btn.config(command=do_verify)

    show(0)
    pump()
    if os.environ.get("P4_GUI_SELFTEST"):               # build and walk through every step, then quit (for testing)
        for i in range(len(STEPS)):
            root.after(100 + 100 * i, lambda i=i: show(i))
        root.after(1000, root.destroy)
    root.mainloop()


# ---------------------------------------------------------------------------
# Command line
# ---------------------------------------------------------------------------
def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--location", "--address", dest="location",
                    help="step 1, scripted: U.S. ZIP code, street address, city, or \"latitude, longitude\"")
    ap.add_argument("--ssid", help="step 1, scripted: Wi-Fi network name")
    ap.add_argument("--password-env", metavar="NAME", help="step 1, scripted: environment variable holding the password")
    ap.add_argument("--port", help="serial port, e.g. COM12 (default: the Waveshare board)")
    ap.add_argument("--settings-only", action="store_true", help="step 1: only rewrite the settings, not the programs")
    ap.add_argument("--keep-settings", action="store_true",
                    help="step 1, scripted: rewrite the programs only; keep the Wi-Fi and location on the board")
    ap.add_argument("--phone-setup", action="store_true",
                    help="step 1, scripted: write no Wi-Fi or location; the clock starts in phone setup mode")
    ap.add_argument("--prepare-card", metavar="DRIVE", help="step 2, scripted: add the apps and folders to this drive or folder")
    ap.add_argument("--pak-dir", help="with --prepare-card: folder holding PAK0.PAK (and PAK1.PAK)")
    ap.add_argument("--eject", metavar="DRIVE", help="step 4, scripted: eject this drive")
    ap.add_argument("--verify", action="store_true", help="step 5, scripted: restart the board and check it")
    args = ap.parse_args()

    console = sys.stdout            # run_esptool swaps sys.stdout for the logger, so keep the real one

    def log(text):
        if console:
            console.write(text)
            console.flush()
    port = args.port or next((p[0] for p in find_ports() if p[2]), "")
    try:
        if args.prepare_card:
            for w in prepare_card(args.prepare_card, args.pak_dir or "", log=log):
                log(f"Note: {w}\n")
        elif args.eject:
            eject_drive(args.eject, log=log)
        elif args.verify:
            ok, _ = verify_board(port, len(bundled_apps()), args.phone_setup, log=log)
            sys.exit(0 if ok else 1)
        elif args.location or args.phone_setup or args.keep_settings or args.settings_only:
            mode = "programs" if args.keep_settings else "settings" if args.settings_only else "all"
            install(args.location or "", args.ssid or "", os.environ.get(args.password_env or "", ""), port,
                    mode, args.phone_setup, log=log)
        else:
            run_gui()
    except InstallError as e:
        sys.exit(f"\n{e}")


if __name__ == "__main__":
    main()
