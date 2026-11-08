# P4 Clock Installer

A Windows program that installs the clock and weather display on a Waveshare
ESP32-P4-WIFI6-Touch-LCD-3.5. Either give it the Wi-Fi network, its password and a location
right here, or let it install the clock and finish setup on a phone.

## Using it (for the person receiving the .exe)

1. Plug the board into the PC with a USB-C cable, using the **UART-C** port (the
   board's other USB-C port will not program it). If no port appears,
   install the "CH343" USB serial driver from WCH (wch-ic.com).
2. Run `P4ClockInstaller.exe`. Windows may warn about an unknown publisher because the
   program is not code-signed: choose "More info", then "Run anyway".
3. Choose how to set up Wi-Fi and location, then click **Install** (about 30 seconds):
   - **On a phone (easiest):** tick *I'll enter the Wi-Fi and location on my phone*. Nothing else
     to fill in. See "Setup from a phone" below.
   - **Here on the PC:** enter the Wi-Fi name and password and a location, which can be
     - a U.S. street address, for example `123 Main St, Springfield, IL 62701`, or
     - a latitude and longitude, **latitude first**, for example `51.5074, -0.1278`.
       South and west are negative (`-33.8688, 151.2093`), or add letters
       (`33.8688 S, 151.2093 E`). Any place on Earth works.
     Check the summary it shows (place, name on the clock, time zone) and confirm.

To change the Wi-Fi or location later, either run the installer again and tick "Only change the
settings", or hold a finger on the clock's screen for 5 seconds to open the phone setup.

## Setup from a phone

The clock starts in setup mode (the first time, or after you hold the screen for 5 seconds, or if
it cannot get onto Wi-Fi for 3 minutes). Its screen shows two QR codes and the words to match:

1. Scan the **left** code with the phone's camera and tap to join the network
   `P4Clock-XXXX`. (Or join it by hand with the name and password printed on the screen.)
2. The setup page should open by itself. If it does not, scan the **right** code, or open
   `192.168.4.1` in a browser.
3. Pick the home Wi-Fi network from the list, type its password, and enter the location: a U.S.
   street address, a city, a ZIP code (U.S. ZIP codes are tried first), or `latitude, longitude`.
   Tap **Save**.
4. The clock tests the password. It says *Connected!* and restarts into the clock. If it cannot join,
   it says so and keeps the setup network open, so you can fix the password and try again.
   If it cannot find the place, it reopens setup with a message.

The phone drops off the clock's network when the clock restarts, so its browser may show an
error page at the end. That is expected.

The setup network has its own random password (shown on the screen), so only someone who can see
the clock can join it. The page is plain HTTP on that private network.

## Good to know

- The board uses **2.4 GHz** Wi-Fi only, with a WPA2/WPA3 password (8 to 63 characters).
- A street address is sent once to the U.S. Census geocoder, and a city or ZIP to Open-Meteo's place
  search. The clock stores only the coordinates, a short place name and the time zone, never a street
  address. For a coordinate entry the clock shows the coordinates (for example `51.51 N, 0.13 W`) as
  the place name. The Wi-Fi password is stored in the board's flash in plain text.
- **The clock shows temperatures in degrees Fahrenheit and wind in mph everywhere.** There is
  no metric option yet.
- The clock starts by itself every time the board is reset or powered up.
- The installer itself accepts a U.S. street address or coordinates. The clock's own setup page
  also accepts a city or ZIP code.

## How it works

`p4clock` reads its settings from a 4 KB `config` flash partition (a JSON block at 0x187000);
the compile-time values in `p4clock/main/user_config.h` are only the fallback. The installer
writes the program (bootloader, partition table, app) and that settings block with `esptool`.
Phone setup writes an empty block (`{}`), which is what puts the clock into setup mode; the clock
writes the real block itself when the phone form is saved.

The time zone's daylight-saving rule is read from the IANA time zone database (the `tzdata`
Python package, bundled in the `.exe`; the clock carries the same table, `main/tz_table.h`, for
setup from a phone): each zone's file ends with its rule in the POSIX format the clock uses.
All 598 zones in tzdata 2026f were run through the board's C library and compared with Python's
own time zone engine at four dates in 2027; 596 agree. Two differences remain: both names of
Cuba's zone, where the clock follows the rule that tzdata publishes but Python's transition table
differs in January 2027. Greenland's rule contains a negative time that the board's C library
cannot parse, so it is rewritten (a one-hour shift of the March changeover on a single night).

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
PyInstaller cannot overwrite an `.exe` that is running; close it first.

The window can also be skipped for testing:

    set P4_PASS=your-wifi-password
    python installer.py --location "123 Main St, City, ST 12345" --ssid MyNetwork --password-env P4_PASS [--settings-only] [--port COM12]
    python installer.py --location "51.5074, -0.1278" --ssid MyNetwork --password-env P4_PASS
    python installer.py --phone-setup [--settings-only]

(`--address` is accepted as another name for `--location`.)

## Limits

- The installer's street addresses work for the U.S. only (Census geocoder). Elsewhere, enter
  coordinates, or use phone setup, which also accepts a city or ZIP code.
- Fahrenheit and mph only (see above).
- Tested with an iPhone (the setup page opened by itself). Android phones should behave the same
  way but have not been tried.
- The `.exe` and `firmware\` contain the Segoe UI font data from the clock's generated
  `font_data.c`. Keep them private; do not post them publicly.
