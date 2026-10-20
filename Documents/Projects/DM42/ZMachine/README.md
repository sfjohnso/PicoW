# An Infocom story player (Z-machine, version 3) for the SwissMicros DM42

A native ARM program (not an HP/Free42 program) built with the SwissMicros DMCP
SDK. It is a Z-machine interpreter written from scratch in portable C
(`src/zmachine.c`, `src/zmachine.h`) plus a DM42 front end (`src/main.c`). It
runs version-3 story files: Zork I-III, Starcross, Planetfall and the other
Infocom games of that generation. It has been played on a real DM42 with
Zork I release 119 (serial 880429).

## Story files

No story file is included. Microsoft/Activision released the Zork I-III source
under the MIT license in November 2025; the compiled `zork1.z3` is in
`COMPILED/` of github.com/historicalsource/zork1 (86,838 bytes). Put story files
in `/STORIES` on the DM42 drive. The player probes a list of well-known names
and an optional `/STORIES/INDEX.TXT` (one file name per line), because the SDK
cannot list a directory.

`stories/` and `screens/` in this folder are not tracked by Git.

## Using it

* **Typing**: the letters printed at the lower right of each key work directly,
  with no SHIFT or ALPHA mode. A-F, G-L, M N O, P-S, T-W, X Y Z follow the
  faceplate rows; `-` is the minus key, `:` the 0 key, `.` the dot key, `?` R/S,
  space the + key. SHIFT+digit types the digit, SHIFT+`.` a comma, SHIFT+R or S
  an apostrophe, SHIFT+minus a double quote. The backspace key deletes.
* **F1-F6**: completions drawn from the story's own dictionary.
* **UP/DOWN**: scroll back through the text.
* **ENTER** sends the line. **EXIT** quits; the game is autosaved first and
  resumes at the same prompt next time.
* Save and restore use the game's own SAVE and RESTORE commands:
  `/SAVES/<STORY>.S1` to `.S9`. The autosave is `/SAVES/<STORY>.AUT`, also written
  when the calculator reaches its inactivity timeout.
* **SHIFT+F6** saves a screenshot to `/SCREENS`.

## Building (Windows, Git Bash)

Needs `arm-none-eabi-gcc` 10.3-2021.10 and `mingw32-make`. The SDK scripts need
a `crc32` command; `tools/crc32` is a Python stand-in. Do not let Git convert
the line endings of `bin/*` to CRLF (`.gitattributes` forces LF).

    export PATH="$PWD/tools:/c/VSARM/armcc/bin:/c/VSARM/mingw/mingw32/bin:$PATH"
    mingw32-make            # -> build/ZMACHINE.pgm   (about 20 KB)

The build uses `-fno-tree-loop-distribute-patterns`: without it gcc turns a
zeroing loop into a call to `memset`, which does not exist on the DM42.

## Installing on a DM42

1. Setup > USB disk mode; copy `ZMACHINE.pgm` to the **root** of the DM42 drive,
   and a story file into `/STORIES`. Eject.
2. Reach the DMCP menu: EXIT from another native program, SHIFT+SETUP > System >
   Reset to DMCP menu from Free42, or hold F1 and press RESET.
3. Load Program > `ZMACHINE.pgm`, then Run Program. Loading replaces Free42 in
   flash; reload `DM42-3.26.pgm` the same way to get it back.

## Design

* `zmachine.c/.h`: the interpreter never touches hardware. The host supplies I/O
  through a table of callbacks (`zm_io_t`: story bytes, character output, status
  line, line input, save/restore streams) and all memory through `zm_mem_t`.
* Only the writable "dynamic" part of the story (11,282 bytes for Zork I) is held
  in RAM, allocated with `malloc` (the program's static RAM region is only 8 KB).
  The rest is read on demand in 512-byte pages through a small cache.
* `zm_save_state(at_input)` can save at an input prompt so that loading resumes
  exactly there. Save format tag: "ZMS4".
* `src/main.c`: story picker, word-wrapped text window (font t24, nine rows),
  reverse-video status bar, scrollback, key mapping, dictionary completions.
* `tools/zm_host.c`: a PC console harness for the same core (`-autosave`,
  `-load`), used for the tests below.

## Tested

On the PC with Zork I: scripted play, save/restore, restart, `$verify` ("The disk
is correct"), troll combat, a 12,000-random-command fuzz run with no interpreter
errors, and resume-at-prompt (a run split by save and load matches an
uninterrupted run). Save files written on the calculator load correctly in the
PC harness. Not done: the CZECH conformance suite (the downloaded package has
only a version-5 build), and version-5 story support.

## Things learned

* FatFs on the DM42 allows **one open file at a time**. The story file is closed
  around every save, restore and `file_exists` call and reopened afterwards
  (`story_close()`/`story_reopen()`); otherwise saves fail with "Could not
  create save file".
* The disk is read-only to programs until `sys_disk_write_enable(1)`.
* The fonts are proportional: right-aligned text is padded by measured pixel
  width (`lcd_textWidth`), not character count.
* A native program has to handle the 10-minute inactivity auto-off itself:
  idle screens check `is_auto_off()` and return to the DMCP menu.
* `dmcp/`, `bin/`, `Makefile`, `stm32_program.ld` come from the SwissMicros
  DMCP_SDK (BSD 3-Clause, see `LICENSE`).
