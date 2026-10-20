# Full-screen animation player for the SwissMicros DM42

A native ARM program (not an HP/Free42 program) built with the SwissMicros DMCP
SDK. `ANIMPLAY.pgm` plays `.DMA` animations from the DM42's FAT drive; the line
copy is hand-written Thumb-2 assembly (`src/anim.s`), the rest is C. A Python
converter (`tools/gif2dma.py`) turns animated GIFs (for example from Jasc
Animation Shop) into `.DMA` files.

## The .DMA format (little-endian)

    header, 16 bytes:  'DMA1', u16 frames, u16 width (400), u16 height (240), u8 flags, 5 reserved
    each frame:        u8 delay (units of 10 ms), u8 n, then n records of [u8 line number][50 bytes]

Only the lines that differ from the previous frame are stored; the first frame
stores all 240, so a clip loops cleanly. Line data is exactly what the DM42 LCD
buffer holds: 50 bytes, running right to left (display pixel x is bit `i & 7` of
byte `i >> 3` with `i = 399 - x`), where a 0 bit is black. The converter flips x
before packing; a naive left-to-right packing shows the picture mirrored.

## Using it

Put `.DMA` files in `/ANIMS` on the DM42 drive (8.3 names). The picker lists the
names in `/ANIMS/INDEX.TXT` (one per line) plus a few well-known names. UP and
DOWN choose (wrapping at the ends), ENTER plays, EXIT quits. While playing: F1
pause, EXIT stop, F6 screenshot to `/SCREENS`. Afterwards the player shows the
frame rate and a per-frame breakdown (read, copy, wait for the LCD, idle) and
appends them to `/ANIMTEST.TXT`; the cursor stays on the clip just played.

## Converting a GIF

    python tools/gif2dma.py in.gif OUT.DMA [--dither bayer|fs|fs-stable|threshold]
           [--fit fit|fill|stretch] [--pad white|black] [--gamma G] [--verify] [--sheet sheet.png]

* `--fit fill` scales to cover 400x240 and crops (no squashing); `fit` letterboxes.
* `--gamma` below 1 brightens (dark clips need about 0.45-0.6).
* `bayer` (ordered) keeps still regions identical from frame to frame, so they are
  not re-sent. `fs` (Floyd-Steinberg) makes moving objects drag a "shadow" and
  inflates files; `fs-stable` dithers the median background once and re-dithers
  only what moves.
* `--verify` decodes the result and checks every frame; `--sheet` writes a contact
  sheet of 12 frames for previewing without a calculator.
* `tools/make_demo_gifs.py` makes the synthetic test clips (bounce, cube, ballstat).

## Measured

USB power (80 MHz): 25 fps sustained with up to 240 changed lines per frame; the
display transfer overlaps the frame wait (`lcd_refresh_dma`). Battery (24 MHz):
about 0.29 ms per line, so a 240-line frame takes about 69 ms (12.8 fps) and
flickers; 110-line frames still hold 25 fps. A full raw frame is 12,000 bytes; the
drive is about 6 MB.

## Building (Windows, Git Bash)

Needs `arm-none-eabi-gcc` 10.3-2021.10 and `mingw32-make`. The SDK scripts need a
`crc32` command; `tools/crc32` is a Python stand-in. Do not let Git convert the
line endings of `bin/*` to CRLF.

    export PATH="$PWD/tools:/c/VSARM/armcc/bin:/c/VSARM/mingw/mingw32/bin:$PATH"
    mingw32-make            # -> build/ANIMPLAY.pgm

Install: copy `ANIMPLAY.pgm` to the root of the DM42 drive (USB disk mode), eject,
reach the DMCP menu (EXIT from a native program, or SHIFT+SETUP > System > Reset
to DMCP menu), Load Program, Run Program. Copying a `.pgm` does not replace the
program already in flash; load it again after every copy.

## Things learned

* A native program must handle the 10-minute auto-off itself: idle screens check
  `is_auto_off()` and return to the DMCP menu (`wait_key()`/`quit_req`).
* FatFs allows one open file at a time; there is no directory listing.
* `samples/` and `screens/` (test clips, contact sheets, screenshots) are not
  tracked; regenerate the clips with the tools above.
* `dmcp/`, `bin/`, `Makefile`, `stm32_program.ld` come from the SwissMicros
  DMCP_SDK (BSD 3-Clause, see `LICENSE`).
