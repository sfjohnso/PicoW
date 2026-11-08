# P4 Clock Installer

A Windows program that installs the clock and weather display on a Waveshare
ESP32-P4-WIFI6-Touch-LCD-3.5. You give it a U.S. street address, a Wi-Fi network name
and its password; it does the rest.

## Using it (for the person receiving the .exe)

1. Plug the board into the PC with a USB-C cable, using the port that makes the board
   show up as a COM port (the board's other USB-C port does not). If no port appears,
   install the "CH343" USB serial driver from WCH (wch-ic.com).
2. Run `P4ClockInstaller.exe`. Windows may warn about an unknown publisher because the
   program is not code-signed: choose "More info", then "Run anyway".
3. Enter the address (for example `123 Main St, Springfield, IL 62701`), the Wi-Fi name and
   password, check the board in the list, and click **Install**.
4. Check the summary it shows (matched address, place name, time zone) and confirm.
   Writing takes about 30 seconds; the clock restarts by itself and shows the time in about 15.

To change the Wi-Fi or the address later, run it again and tick "Only change the settings".

Good to know:
- The board uses **2.4 GHz** Wi-Fi only, with a WPA2/WPA3 password (8 to 63 characters).
- The address is sent once to the U.S. Census geocoder (and the coordinates to Open-Meteo,
  to find the time zone). The clock stores only the coordinates, "CITY, ST ZIP" and the time
  zone, never the street address. The Wi-Fi password is stored in the board's flash in plain text.
- The clock starts by itself every time the board is reset or powered up.

## How it works

`p4clock` reads its settings from a 4 KB `config` flash partition (a JSON block at 0x187000);
the compile-time values in `p4clock/main/user_config.h` are only the fallback. The installer
writes the program (bootloader, partition table, app) and that settings block with `esptool`.

## Building the installer (maintainer)

Needs ESP-IDF 5.5.6 at `C:\Espressif`, the `p4clock` project with its `font_data.c`
(generated from Segoe UI on a Windows PC), and a Python 3 with tkinter:

    python -m venv .venv
    .venv\Scripts\python -m pip install -r requirements.txt
    powershell -ExecutionPolicy Bypass -File package.ps1

`package.ps1` builds the firmware in its own folder with **placeholder** settings
(`installer_defaults.h`), copies the three binaries to `firmware\`, runs
`check_no_secrets.py` (it fails if your real Wi-Fi name or password from `user_config.h`
is inside the binaries) and then makes `dist\P4ClockInstaller.exe` with PyInstaller.

The window can also be skipped for testing:

    set P4_PASS=your-wifi-password
    python installer.py --address "123 Main St, City, ST 12345" --ssid MyNetwork --password-env P4_PASS [--settings-only] [--port COM12]

## Limits

- U.S. addresses only (Census geocoder); time zones for the U.S. and its territories are mapped in `installer.py`.
- The `.exe` and `firmware\` contain the Segoe UI font data from the clock's generated
  `font_data.c`. Keep them private; do not post them publicly.
