# FAT drive read-speed benchmark for the SwissMicros DM42

A native ARM program built with the SwissMicros DMCP SDK. `DISKTEST.pgm` times
reads from the DM42's internal FAT drive with `sys_current_ms`, to show whether
the drive limits a streaming program such as an animation player.

It needs a 1 MiB file named `TESTDATA.BIN` in the root of the drive (any content;
`build/TESTDATA.BIN` is not tracked). Tests: sequential reads at 512 B, 4 KB,
12,000 B (one raw 400x240 frame) and 32 KB; a 2-byte header plus 6,000 B (the
shape of a delta frame); random seek plus 12,000 B; open and close. ENTER runs
again, F6 saves a screenshot to `/SCREENS`, EXIT quits. Results are appended to
`/DISKTEST.TXT` on the drive and shown on screen with the power source and clock
(USB 80 MHz or battery 24 MHz).

## Results (2026-10-15, two runs per power source; `results/disktest_2026-10-15.txt`)

| Test | USB, 80 MHz | Battery, 24 MHz |
|---|---|---|
| sequential 4 KB | 16,000 KB/s | 2,534-2,560 KB/s |
| sequential 12,000 B (one frame) | 0.71-0.76 ms | 4.7 ms |
| sequential 32 KB | 17,066-18,285 KB/s | 2,585 KB/s |
| random seek + 12,000 B | 1.05-1.10 ms | 6.05-6.40 ms |
| open + close | 0.25 ms | 1.1 ms |

The drive is not the bottleneck for full-screen animation: pushing a full frame
to the LCD costs about 23 ms on USB power and about 69 ms on battery.

## Building (Windows, Git Bash)

Needs `arm-none-eabi-gcc` 10.3-2021.10 and `mingw32-make`. The SDK scripts need a
`crc32` command; `tools/crc32` is a Python stand-in. Do not let Git convert the
line endings of `bin/*` to CRLF.

    export PATH="$PWD/tools:/c/VSARM/armcc/bin:/c/VSARM/mingw/mingw32/bin:$PATH"
    mingw32-make            # -> build/DISKTEST.pgm

Install: copy `DISKTEST.pgm` and `TESTDATA.BIN` to the root of the DM42 drive (USB
disk mode), eject, reach the DMCP menu, Load Program, Run Program.

`dmcp/`, `bin/`, `Makefile`, `stm32_program.ld` come from the SwissMicros
DMCP_SDK (BSD 3-Clause, see `LICENSE`).
