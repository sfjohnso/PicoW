# P4 launcher and apps

A boot menu for the Waveshare ESP32-P4 3.5" board. It lists the built-in clock plus every `/apps/*.bin` on the SD
card; tap an app, tap Launch. Pressing Reset (or any crash or power cycle) comes back to the menu, which counts
down 3 s and then starts the clock unless the screen is touched.

## How it works

Flash layout (`partitions.csv`; the first four rows keep the standalone clock's offsets):

| partition | offset   | size     | holds |
|-----------|----------|----------|-------|
| nvs       | 0x9000   | 0x6000   | |
| phy_init  | 0xf000   | 0x1000   | |
| factory   | 0x10000  | 0x177000 | this launcher |
| config    | 0x187000 | 0x1000   | JSON settings (Wi-Fi, location), shared by every app |
| otadata   | 0x188000 | 0x2000   | |
| ota_0     | 0x190000 | 2 MB     | the built-in clock, flashed once |
| ota_1     | 0x390000 | 8 MB     | scratch slot: the launcher copies the chosen SD app here |

Choosing an app copies `/apps/<name>.bin` into `ota_1` (skipped when the slot already holds that exact image, compared
by ELF hash and the image's trailing SHA-256), selects it with `esp_ota_set_boot_partition()` and reboots.

**One-shot boot.** The launcher is built with `CONFIG_BOOTLOADER_APP_ROLLBACK_ENABLE`. No app ever calls
`esp_ota_mark_app_valid_cancel_rollback()`, so the bootloader marks each launch "pending verify" and, on the next reset
of any kind, treats it as aborted and falls back to the factory image (the launcher). Apps need no code to return to the
menu, and must not mark themselves valid.

An SD app is any ESP32-P4 app image (ESP-IDF 5.5.x, chip rev v3.x) that fits in `ota_1`. The menu label is the file name.

## First-time flash (from a project's build folder)

```
python -m esptool --chip esp32p4 -p COM12 write_flash --flash_mode dio --flash_size 16MB --flash_freq 80m ^
  0x2000 build/bootloader/bootloader.bin 0x8000 build/partition_table/partition-table.bin ^
  0x10000 build/launcher.bin 0x188000 build/ota_data_initial.bin 0x190000 <p4clock.bin>
```

Do not write `config` (0x187000): it holds the clock's settings. Later launcher-only updates: write just `0x10000`.

## Apps in this tree

* `../p4clock`   built-in clock (runs from `ota_0`; unchanged by the launcher)
* `../quake`     Quake port, `TOUCH_CONTROLS 0` = attract-mode demo only (own git repo)
* `../photos`    slideshow of `/Photos` (JPEG by the P4 hardware decoder, PNG by libpng)
* `../music`     album player for `/Music` (minimp3, ES8311 codec)
* `../life`      Conway's Game of Life: five start modes, cells coloured by born / surviving / just died (no SD files needed)

Each builds like any IDF project (`idf.py set-target esp32p4 && idf.py build`); copy its `build/<name>.bin` to
`/apps/` on the card. Quake also needs `/id1/PAK0.PAK` (+ `PAK1.PAK`).

## exFAT: the local `components/fatfs`

ESP-IDF's FAT library is built without exFAT, and the 30 GB card may be exFAT. The launcher, photos, music and quake
projects each carry a copy of `$IDF_PATH/components/fatfs` as `components/fatfs` (git-ignored, 2.6 MB each) with two
edits to `src/ffconf.h`:

```c
#ifdef CONFIG_FATFS_USE_LABEL        // was: #define FF_USE_LABEL CONFIG_FATFS_USE_LABEL
#define FF_USE_LABEL 1
#else
#define FF_USE_LABEL 0
#endif
...
#define FF_FS_EXFAT 1                // was 0
```

Run `idf.py reconfigure` after adding it, or the override is not picked up. Settings used: `CONFIG_FATFS_LFN_STACK=y`,
`CONFIG_FATFS_MAX_LFN=255` (music also `CONFIG_FATFS_API_ENCODING_UTF_8=y` for non-ASCII file names).

## Testing an app without going through the menu

With the card in the board, write the app straight into `ota_1` and select it; the setting is then persistent, so
restore the menu afterwards by erasing otadata:

```
python -m esptool --chip esp32p4 -p COM12 write_flash 0x390000 build/<app>.bin
python %IDF_PATH%/components/app_update/otatool.py --port COM12 --partition-table-file ../launcher/build/partition_table/partition-table.bin --partition-table-offset 0x8000 switch_ota_partition --name ota_1
...
python %IDF_PATH%/components/app_update/otatool.py ... erase_otadata
```

Remember to copy the same build to `/apps/` on the card, or the launcher will copy the older card version over it.

## Going back to the standalone clock

The pre-launcher single-app firmware is tagged `p4clock-standalone-v1`; `installer/` still builds an installer that
rewrites the partition table to that layout.
