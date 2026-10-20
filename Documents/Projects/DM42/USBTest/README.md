# USB serial experiment for the SwissMicros DM42

A native ARM program built with the SwissMicros DMCP SDK, plus a PC-side monitor,
written to find out whether a native program can use the USB port as a serial
link (for example to an ESP32-S3).

`USBTEST.pgm`: F1 `usb_acm_on()` (the port becomes a virtual serial port), F2 send
one line, F3 toggle a numbered line every second, F4 `usb_turn_off()`, F5 burst of
100 numbered 32-character lines, F6 quiet receive test (12 s of hands off while
the PC sends all 256 byte values; the screen counts key events), EXIT turns USB
off and leaves. `tools/serial_monitor.py LOGFILE [seconds] [drive-letter]`
(pyserial) waits for the new COM port, logs identity and everything received with
time stamps, sends numbered test lines, and notes whether the drive letter
disappears.

## Result (2026-10-13)

* The SDK exports only `usb_acm_on`, `usb_turn_off`, `usb_is_on`, `acm_puts`,
  `usb_powered` and `switch_usb_powered_freq`: no receive function.
* The PC sees USB ID 0483:5740 (an ST virtual COM port); Windows loads its generic
  "USB Serial Device" driver by itself.
* Calculator-to-PC output arrives intact and in order. PC-to-calculator: nothing;
  all 256 byte values sent, zero key events seen.
* USB disk (the E: drive) and serial are mutually exclusive.

So through the documented API the link is one-way (calculator to host).

## Building (Windows, Git Bash)

Needs `arm-none-eabi-gcc` 10.3-2021.10 and `mingw32-make`. The SDK scripts need a
`crc32` command; `tools/crc32` is a Python stand-in. Do not let Git convert the
line endings of `bin/*` to CRLF.

    export PATH="$PWD/tools:/c/VSARM/armcc/bin:/c/VSARM/mingw/mingw32/bin:$PATH"
    mingw32-make            # -> build/USBTEST.pgm

Install: copy `USBTEST.pgm` to the root of the DM42 drive (USB disk mode), eject,
reach the DMCP menu, Load Program, Run Program.

`dmcp/`, `bin/`, `Makefile`, `stm32_program.ld` come from the SwissMicros
DMCP_SDK (BSD 3-Clause, see `LICENSE`).
