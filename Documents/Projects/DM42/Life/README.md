# Conway's Game of Life for the SwissMicros DM42

A native ARM program (not an HP/Free42 program) for the DM42, built with the
SwissMicros DMCP SDK. The generation step is hand-written Thumb-2 assembly
(`src/life.s`); the rest is C. Runs on a 200x120 torus drawn as 2x2-pixel
cells on the 400x240 Sharp LCD.

## Using it

Title menu: **1** Soup (random), **2** Garden (one methuselah per quadrant),
**3** Gun battle (two Gosper guns), **4** Hybrid (thin soup plus organisms),
**5** Kaleidoscope (random quadrant mirrored four ways). **ENTER** repeats the
last mode. **EXIT** during a run returns to the menu; **EXIT** on the menu
quits. A run also ends by itself when the field is still or period 2, or when
its population count settles into a cycle of up to 32.

## Building (Windows, Git Bash)

Needs `arm-none-eabi-gcc` 10.3-2021.10 and `mingw32-make`. The SDK scripts
need a `crc32` command; `tools/crc32` is a Python stand-in. Do not let Git
convert line endings of `bin/*` to CRLF.

    export PATH="$PWD/tools:/c/VSARM/armcc/bin:/c/VSARM/mingw/mingw32/bin:$PATH"
    mingw32-make            # -> build/LIFE.pgm

## Installing on a DM42

1. Setup > USB disk mode; copy `LIFE.pgm` to the **root** of the DM42 drive; eject.
2. Reach the DMCP menu: SHIFT+SETUP > System > Reset to DMCP menu
   (or hold F1 and press RESET).
3. DMCP menu > Load Program > LIFE.pgm, then Run Program.

Notes learned the hard way:

* DMCP's Load Program lists only `.pgm` files in the FAT **root**. The
  SETUP > File > Load Program inside Free42 reads `.raw` files from `/PROGRAMS`
  and is a different thing.
* The program needs DMCP firmware matching the SDK (interface version 17 =
  DMCP 3.31 here). `DMCP_flash_X.Y.bin` is *OS only*: flashing it removes the
  Free42 program, which is restored by loading `DM42-3.26.pgm` the same way.
  Loading `LIFE.pgm` likewise replaces Free42 in flash until it is reloaded.
* The CPU runs at 80 MHz on USB power and 24 MHz on battery (per the manual),
  so everything is about 3x slower unplugged.

## Layout

* `src/life.s` - `life_step` (8 cells per operation, saturating bit-plane
  counter) and `life_fix_edges`. Grid layout is described in its header.
* `src/main.c` - menu, starting fields, stability detection, display.
* `src/patterns.c`, `patterns.h` - RLE organisms and the stamper.
* `src/life_grid.h` - grid geometry shared by the above.
* `dmcp/`, `bin/`, `Makefile`, `stm32_program.ld` - from the SwissMicros
  DMCP_SDK (BSD 3-Clause, see `LICENSE`).
* `archive/` - earlier versions (cell-at-a-time kernel, a version with a plain-C
  reference step and self-test, a version with timing statistics).
* `tools/` - checks that run on the PC: `simlife.py` proves the assembly's
  algorithm against brute-force Life; `patterns_check.py` verifies each
  organism's known behaviour; `host_test.c` + `host_compare.py` check the C
  stamper against Python for every pattern and orientation.

## Measured (USB power)

Life step 3.1 ms per generation (the cell-at-a-time version was about 30 ms).
Display transfer is about 0.19 ms per redrawn Life row and is the limit:
roughly 41 generations/s for a busy soup, 74/s for a gun battle, because only
rows that changed are redrawn.
