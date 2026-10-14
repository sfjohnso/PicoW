# The Ackermann function on the SwissMicros DM42

A native ARM program (not an HP/Free42 program) built with the SwissMicros
DMCP SDK. The brute-force engine is hand-written Thumb-2 assembly
(`src/ack.s`); the closed forms, the memoized evaluator and the user interface
are C.

A(m,n) is computed three ways, and you can compare them on the same problem:

| Mode | How | A(4,1) = 65,533 |
|---|---|---|
| Brute force (asm) | one call of the textbook recursion per step | 2,862,984,010 steps, about 14 minutes on USB power |
| Memoized chain (C) | remember how far each m level has climbed | 98,315 steps, milliseconds |
| Closed form | formulas for m <= 3, known values for m = 4, 5 | instant |

The memoized mode still reports the exact brute-force step count (carried along
with each remembered value), and the program checks all three against each
other at start-up.

## Using it

Setup screen: number keys set the active value, UP/DOWN switch between m and n,
BSP deletes, F1 cycles the mode, F2 shows a table of small values, ENTER runs,
EXIT quits. The screen predicts the value, the brute-force step count, an
estimated time (from a speed measured at start-up) and the stack depth.

Running screen (brute force): steps, depth and peak depth, a progress bar, the
stack drawn to scale as runs of equal values, and a depth-over-time graph. F1
pauses, EXIT stops.

Results too big for 32 bits (A(4,2) = 2^65536 - 3 and beyond) are not computed;
the closed form is shown instead.

**F6** on any screen saves a screenshot (BMP) into `/SCREENS` on the DM42 drive.

## Why the brute-force engine is small

The engine has no recursion: it keeps the pending outer calls as a stack of m
values. For A(4,1) that stack is 65,531 entries deep but never more than 4 runs
of equal values, so it is stored run-length encoded (a few bytes instead of
64 KB). The engine runs in slices (`ack_run(state, budget)`), which lets the C
side redraw, handle keys and pause between slices.

## Building (Windows, Git Bash)

Needs `arm-none-eabi-gcc` 10.3-2021.10 and `mingw32-make`. The SDK scripts need
a `crc32` command; `tools/crc32` is a Python stand-in. Do not let Git convert
the line endings of `bin/*` to CRLF.

    export PATH="$PWD/tools:/c/VSARM/armcc/bin:/c/VSARM/mingw/mingw32/bin:$PATH"
    mingw32-make            # -> build/ACKERMANN.pgm

## Installing on a DM42

Copy `ACKERMANN.pgm` to the **root** of the DM42 drive (USB disk mode), eject,
reach the DMCP menu (EXIT from another native program, SHIFT+SETUP > System >
Reset to DMCP menu from Free42, or hold F1 and press RESET), then Load Program
and Run Program. Loading replaces whatever program is in flash; reload
`DM42-3.26.pgm` the same way to get Free42 back.

## Checks that run on the PC

`tools/ack_host_test.c` contains a C twin of the assembly engine (same
registers, same run-length stack, same slicing) and checks it, the closed
forms, the exact step counts, the peak stack depth and the memoized evaluator
against plain recursion: 39 small cases at several slice sizes plus the big ones
(A(3,10..13), A(4,0), A(4,1), A(5,0)). Run it with `full` to include the
2.9-billion-step A(4,1) engine run.

    gcc -std=gnu99 -D__USE_MINGW_ANSI_STDIO=1 -O2 tools/ack_host_test.c src/ack_math.c -o ack_host_test
    ./ack_host_test            # about a second;  ./ack_host_test full  for A(4,1)

The assembly itself is checked on the calculator by the start-up self-test.

## Things learned

* A native program has to handle the system's inactivity auto-off itself. The
  timer is 10 minutes; a long run calls `reset_auto_off()` each slice, and idle
  screens return to the DMCP menu when it expires.
* The small font (t20) is proportional, so right-aligning text by counting
  characters clips it. `bar_line()` measures with `lcd_textWidth()` and pads with
  spaces by pixel width.
* Redrawing between slices costs time: 120 ms slices lost about 20%, 300 ms
  slices about 9%.
* Measured engine speed is about 3.9M steps/s on USB power (the CPU is 80 MHz
  on USB, 24 MHz on battery).
