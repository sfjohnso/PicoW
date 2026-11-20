# P4 Installer

A Windows program that sets up a Waveshare ESP32-P4-WIFI6-Touch-LCD-3.5 from scratch: the **launcher** (a touch
menu that starts apps from the SD card), the **clock and weather display**, and an SD card with the apps
(**Quake Demo, Photos, Music, Life**). It walks through five steps and finishes by testing the board.

## Using it (for the person receiving the .exe)

Run `P4ClockInstaller.exe`. Windows may warn about an unknown publisher because the program is not code-signed:
choose "More info", then "Run anyway". The window has five steps; the board stays connected by USB the whole time.

1. **Install the launcher and the clock.** Plug the board into the PC with a USB-C cable, using the **UART-C** port
   (the board's other USB-C port will not program it). If no port appears, install the "CH343" USB serial driver
   from WCH (wch-ic.com). Choose how to set up Wi-Fi and location, then click **Install on the board**
   (about 30 seconds):
   - **On a phone (easiest):** tick *I'll enter the Wi-Fi and location on my phone*. Nothing else to fill in.
     See "Setup from a phone" below.
   - **Here on the PC:** enter the Wi-Fi name and password and a location, which can be
     - a U.S. ZIP code, for example `62701` (looked up first, as a U.S. ZIP),
     - a U.S. street address, for example `123 Main St, Springfield, IL 62701`,
     - a city or postal code anywhere, for example `Springfield, Illinois` or `Paris`, or
     - a latitude and longitude, **latitude first**, for example `51.5074, -0.1278`.
       South and west are negative (`-33.8688, 151.2093`), or add letters
       (`33.8688 S, 151.2093 E`). Any place on Earth works.
     Check the summary it shows (place, name on the clock, time zone) and confirm.
   - The other two choices under the form are for later: *Update the programs only* keeps the Wi-Fi and location
     already on the board, and *Only change the Wi-Fi and location* leaves the programs alone.
2. **Add the apps to the SD card.** Put the board's microSD card in the PC. Choose it in the list; the installer
   shows its name, format and size and asks before touching it. It creates the `apps`, `id1`, `Music` and `Photos`
   folders and copies the three apps into `apps`. It never formats the card or deletes anything, and it refuses a card
   the board cannot read (the board reads FAT32 and exFAT, not NTFS).
   - **Quake data:** *Quake Demo* needs your own copy of Quake's data files, `PAK0.PAK` (and `PAK1.PAK` for the
     full game). The installer cannot include them. It looks for a Steam copy and fills in the folder
     (`...\steamapps\common\Quake\id1`); otherwise use **Browse**. Leave it empty to skip Quake: the other apps do not
     need it.
3. **Add your music and photos.** The buttons open the card's folders. *Music:* one folder per album, holding that
   album's `.mp3` files (numbers at the start of the file names put the tracks in order). *Photos:* `.jpg`, `.jpeg`
   and `.png` files. Progressive JPEGs and interlaced PNGs are not supported.
4. **Eject the card.** The installer ejects it like "Safely Remove Hardware". If Windows says the card is in use,
   close any File Explorer window that shows it. Then take the card out.
5. **Put the card in the board and test.** Push the card into the board's microSD slot and click the button. The
   installer restarts the board and reads its startup messages, then reports a checklist: the launcher starts, it finds
   the card and the apps, the countdown starts the clock, the clock finds its settings and joins Wi-Fi, and the Reset
   button brings back the menu. With phone setup the Wi-Fi check is replaced by "scan the QR code on its screen".

To change the Wi-Fi or location later, run the installer again and choose *Only change the Wi-Fi and location*
(step 1), or hold a finger on the clock's screen for 5 seconds to open the phone setup.

## Using the board

After every start or reset the board shows the **launcher menu**: the built-in Clock and each app from the card.
If nothing is touched for 3 seconds it starts the clock. Touch the screen to keep the menu; drag to scroll, tap an
app, tap **LAUNCH**. The **Reset** button always returns to the menu. See `launcher\README.md` for how it works.

## The alarm

The clock has one daily alarm. A bell in the top corner of the right-hand column is solid when the alarm is on and an
outline when it is off, and the bottom line says when it will ring.

- **Set it:** hold the **BOOT** button for about 1.5 seconds. Use the arrows for the hour, minute and AM/PM, then tap
  **DONE**. Changing the time turns the alarm on; **ALARM ON/OFF** turns it off. The time is kept in the board's
  config area, so it survives power loss.
- **When it rings:** a soft chime that starts quietly and gets louder (full volume after about 45 seconds).
  Tap the screen to snooze for 10 minutes. Double-press **BOOT** to stop it for the day. Nobody answering stops it
  after 5 minutes.
- **Phone setup** can also be opened by holding BOOT for 8 seconds.
- The alarm only works while the clock is running. If the board is reset or another app is started, it is silent
  until the clock runs again (after a reset the launcher starts the clock after 3 seconds).
- *Only change the Wi-Fi and location* rewrites the board's config area and clears the alarm; set it again afterwards.

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
- **Every app switch restarts the board**, so the clock reconnects to Wi-Fi each time it comes back (a few seconds).
- A street address is sent once to the U.S. Census geocoder, and a ZIP code or city to Open-Meteo's place
  search. The clock stores only the coordinates, a short place name and the time zone, never a street
  address. For a coordinate entry the clock shows the coordinates (for example `51.51 N, 0.13 W`) as
  the place name. The Wi-Fi password is stored in the board's flash in plain text.
- **The clock shows temperatures in degrees Fahrenheit and wind in mph everywhere.** There is
  no metric option yet.
- **Night mode uses the board's camera as a light sensor:** in a dark room the colors turn muted red and
  the backlight dims. Stand the clock so the camera on its back sees the room, not a wall right behind it
  (about 6 inches / 15 cm away worked). No picture is taken. Without a working camera it uses 22:00 to 07:00.
- The installer looks a place up in this order: a bare U.S. ZIP code first (as a U.S. ZIP, so `75001` is Addison,
  Texas, not Paris), then a street address (U.S. Census), then a ZIP code at the end of the text (an address Census does not
  know still has its ZIP), then a city or postal code anywhere (Open-Meteo), then each comma-separated piece on its
  own with ZIP codes first. The clock's own phone setup page uses the same order, except that it has no "ZIP at the
  end of the text" step and tries comma-separated pieces left to right.

## How it works

Step 1 writes five files and the settings with `esptool`:

| offset   | file                  | |
|----------|-----------------------|---|
| 0x2000   | `bootloader.bin`      | the launcher's bootloader (app rollback enabled) |
| 0x8000   | `partition-table.bin` | launcher layout: factory (launcher), `config`, `otadata`, `ota_0` (clock), `ota_1` (scratch for SD apps) |
| 0x10000  | `launcher.bin`        | the launcher |
| 0x188000 | `ota_data_initial.bin`| clears the boot selection |
| 0x190000 | `p4clock.bin`         | the clock |
| 0x187000 | (generated)           | the clock's settings |

`p4clock` reads its settings from the 4 KB `config` flash partition (a JSON block); the compile-time values in
`p4clock/main/user_config.h` are only the fallback. Phone setup writes an empty block (`{}`), which is what puts the
clock into setup mode; the clock writes the real block itself when the phone form is saved.

Step 2 copies the apps bundled in the installer (`apps\`) to the card's `apps` folder, under the names the menu shows.
Step 4 uses the same Windows calls as "Safely Remove Hardware" (flush, lock, dismount, eject). Step 5 resets the board
through the USB-serial chip's RTS line, reads the console at 115200 baud and looks for the launcher's and the
clock's startup messages; it resets the board a second time to confirm that Reset returns to the launcher.

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
(generated from Segoe UI on a Windows PC), the `launcher`, `photos`, `music`, `life` and `quake` projects (each configured
once with `idf.py set-target esp32p4`), and a Python 3 with tkinter:

    python -m venv .venv
    .venv\Scripts\python -m pip install -r requirements.txt
    powershell -ExecutionPolicy Bypass -File package.ps1

`package.ps1` builds the clock with **placeholder** settings (`installer_defaults.h`) and the other four projects,
copies the launcher's bootloader, partition table, `launcher.bin`, `ota_data_initial.bin` and the clock into
`firmware\` and the four apps into `apps\` (as `Quake Demo.bin`, `Photos.bin`, `Music.bin`, `Life.bin`), runs
`check_no_secrets.py` on both folders (it fails if your real Wi-Fi name or password from `user_config.h` is inside
the binaries) and then makes `dist\P4ClockInstaller.exe` with PyInstaller. PyInstaller cannot overwrite an `.exe`
that is running; close it first.

Each step can also be run without the window (the installer finds the board by its USB chip unless `--port` is given):

    set P4_PASS=your-wifi-password
    python installer.py --location "123 Main St, City, ST 12345" --ssid MyNetwork --password-env P4_PASS [--port COM12]
    python installer.py --location "51.5074, -0.1278" --ssid MyNetwork --password-env P4_PASS
    python installer.py --phone-setup
    python installer.py --keep-settings            (programs only)    python installer.py --settings-only ...
    python installer.py --prepare-card E: --pak-dir "C:\...\Steam\steamapps\common\Quake\id1"
    python installer.py --eject E:
    python installer.py --verify [--phone-setup]

(`--address` is accepted as another name for `--location`.) `P4_GUI_SELFTEST=1 python installer.py` builds the window,
steps through the five screens and exits, as a smoke test.

## Limits

- Street addresses work for the U.S. only (Census geocoder). Elsewhere, enter a city, a postal code or
  coordinates.
- Fahrenheit and mph only (see above).
- It never formats the SD card; a card that is not FAT32 or exFAT must be reformatted by the user first.
- Quake's data files are not included and must come from the user's own copy of Quake.
- Tested with an iPhone (the setup page opened by itself). Android phones should behave the same
  way but have not been tried.
- The `.exe` and `firmware\` contain the Segoe UI font data from the clock's generated
  `font_data.c`. Keep them private; do not post them publicly.
