"""Front matter and chapters 1-6 of the DM42 programming guide."""
import os
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.shared import Pt

FIG = os.path.join(os.path.dirname(__file__), "fig")
def f(n): return os.path.join(FIG, n)

GOTCHAS = [
    ("G1", "Two different \"Load Program\" menus", "DMCP's loader lists only `.pgm` files in the FAT root; Free42's File menu reads `.raw` from /PROGRAMS."),
    ("G2", "Firmware and flash side effects", "An OS-only flash erases Free42; loading any program replaces Free42; the SDK needs matching firmware."),
    ("G3", "Windows build quirks", "CRLF line endings break the SDK scripts; no `crc32` tool; shell and quoting traps."),
    ("G4", "No C library", "gcc turns simple loops into `memset`/`memcpy` calls that do not exist."),
    ("G5", "Mirrored LCD line buffer", "Display x runs right to left in the buffer; naive bit packing mirrors the picture."),
    ("G6", "Proportional fonts", "Right-aligned text computed from character counts is clipped or misplaced."),
    ("G7", "`sys_delay` is unreliable", "A 1.5 s delay took 10-15 s; timing must come from `sys_current_ms`."),
    ("G8", "Auto-off hang", "The 10-minute inactivity timer hangs a program that waits for a key or computes silently."),
    ("G9", "Screenshot key", "Native programs must handle F6/`KEY_SCREENSHOT` themselves; files are BMP."),
    ("G10", "FatFs limits", "One open file at a time, no directory listing, writes need enabling, 8.3 names."),
    ("G11", "8 KB program RAM", "Only 8 KB of .data/.bss; large buffers must be allocated at run time."),
    ("G12", "Clock speed and LCD cost", "80 MHz on USB, 24 MHz on battery; pushing pixels to the LCD dominates graphics programs."),
    ("G13", "USB disk and serial are exclusive and one-way", "Serial is calculator-to-PC only through the SDK."),
    ("G14", "Dither artifacts in motion", "Error-diffusion dithering makes moving objects drag a \"shadow\"."),
    ("G15", "A copied program is not a loaded program", "Copying a `.pgm` to the drive does not change the program in flash."),
    ("G16", "Keyboard codes are not ASCII", "Letters on the faceplate must be mapped by hand; F-keys and SHIFT need care."),
]
G = {g[0]: g for g in GOTCHAS}

def used(guide, ids, note=None):
    items = [f"**{i}** {G[i][1]} \u2014 see Chapter 11." for i in ids]
    if note: items.append(note)
    guide.box("Gotchas that applied in this chapter", items)


def front(g):
    d = g.d
    for _ in range(2): d.add_paragraph()
    p = g.p("Programming the SwissMicros DM42", align=WD_ALIGN_PARAGRAPH.CENTER, size=30, color=None)
    for r in p.runs: r.bold = True
    g.p("Native ARM applications with the DMCP SDK", align=WD_ALIGN_PARAGRAPH.CENTER, size=16)
    g.p("A field guide built from five working programs: Conway's Life, the Ackermann function, an Infocom Z-machine player, "
        "a disk-speed benchmark, and a full-screen animation player", align=WD_ALIGN_PARAGRAPH.CENTER, size=12, italic=True)
    d.add_paragraph()
    g.fig(f("photo_keyboard.png"), "The DM42 running the Life program (a random \"soup\") on its 400x240 monochrome display, with the faceplate whose "
          "legends the Z-machine player reads for keyboard input.", width=2.2)
    g.p("Draft 1 \u2014 October 15, 2026", align=WD_ALIGN_PARAGRAPH.CENTER, size=11)
    g.p("Prepared for Steve Johnson with Claude. All programs, measurements and screenshots come from sessions of October 7\u201315, 2026, on Steve's DM42 "
        "(DMCP 3.31).", align=WD_ALIGN_PARAGRAPH.CENTER, size=10, italic=True)
    d.add_page_break()
    cp = g.p("**Contents**", size=20, color=__import__("docx_lib").NAVY)
    g.toc()
    g.h2("How to read this guide")
    g.p("Chapters 1\u20133 describe the machine, the tool chain and the references. Chapters 4\u201310 each document one program: what it had to do, how it was built, "
        "and which pitfalls (\"gotchas\", numbered G1\u2013G16) it ran into. Chapter 11 explains each gotcha in detail with the fix. If you only want to avoid "
        "wasting a day, read the one-page gotcha table in Chapter 2 and then Chapter 11.")
    g.p("Where the guide gives a measurement it says whether the calculator was on USB power (80 MHz) or battery (24 MHz). Everything attributed to the "
        "calculator was observed on the real device; everything attributed to a PC was run on the PC. Items that were not tested are labelled as such.")


def ch1(g):
    g.h1("1  The platform")
    g.h2("1.1  What the DM42 is")
    g.p("The SwissMicros DM42 is a calculator built around an STMicroelectronics STM32L476 microcontroller (an ARM Cortex-M4F core) driving a 400x240, "
        "one-bit-per-pixel Sharp memory LCD. Its operating system, DMCP (\"DM Calculator Platform\"), provides the display, keyboard, USB, a FAT file "
        "system on the internal flash, and a loader for programs. The calculator application most people use, Free42, is itself just a program loaded "
        "on top of DMCP.")
    g.p("SwissMicros publishes the DMCP interface as an SDK, so anyone can write a native program: no HP RPN, no emulator. A native program is a "
        "Thumb-2 binary with a `.pgm` extension that DMCP loads into flash and runs with full access to the SDK's functions. That is the programming "
        "model this guide covers.")
    g.table(["Item", "Value (as used here)"], [
        ["Processor", "STM32L476, Arm Cortex-M4F, armv7e-m Thumb-2; SDK builds with `-mcpu=cortex-m4 -mfloat-abi=hard -mfpu=fpv4-sp-d16`"],
        ["Clock", "80 MHz on USB power, 24 MHz on battery (from the DM42 manual; confirmed by every benchmark below)"],
        ["Display", "400 x 240 pixels, 1 bit per pixel, 50 bytes per line; 240 lines"],
        ["Program RAM", "only 8 KB for `.data`/`.bss`; anything bigger must come from `malloc`"],
        ["Storage", "FAT drive in internal flash, about 6 MB total; visible to the PC in USB disk mode"],
        ["Firmware used", "DMCP 3.31 (SDK interface version 17); Free42 `DM42-3.26.pgm`"],
        ["Programs", "`.pgm` files in the root of the FAT drive, loaded from the DMCP menu"],
    ], widths=[1.4, 5.1], caption="Platform summary.")
    g.h2("1.2  The faceplate and the display")
    g.p("The photograph below shows the whole front of Steve's DM42. The six unlabeled soft keys under the display are F1\u2013F6. Each key may carry "
        "three legends: white (the key's main function), yellow (the shifted function, selected with the yellow SHIFT key) and a small letter at the lower right "
        "(the ALPHA-mode letter). The Z-machine player in Chapter 7 reads typed letters directly from those lower-right letters, without needing SHIFT or "
        "ALPHA mode.")
    g.fig(f("photo_keyboard.png"), "The DM42 faceplate (photo by Steve). The display shows the Life program's \"soup\" mode. The small letters at the lower right of each key are the ones "
          "the Z-machine player maps to text input.", width=3.1)
    g.p("Photographs of the display are easy but imperfect: glare, perspective and the dark bezel make them hard to read. The DM42 can capture its own screen, "
        "which is how most figures in this guide were made (see G9). The two photographs below show the Ackermann program early in its development "
        "(they were taken with a phone, and the program's layout was later revised).")
    g.figs([f("photo_setup.png"), f("photo_table.png")], "Phone photographs of the Ackermann program during development: the setup screen (left) and the table of small values (right).", width=3.1)
    g.h2("1.3  How a native program starts and stops")
    g.p("In the DMCP menu choose **Load Program**, pick a `.pgm` file from the root of the drive, then **Run Program**. The loaded program replaces the program "
        "in flash (including Free42) until another is loaded. The SDK calls `program_main()`; when it returns, the DMCP menu reappears. EXIT from a native program "
        "is therefore normally implemented as returning from `program_main()`. To return to Free42, load `DM42-3.26.pgm` the same way.")


def ch2(g):
    g.h1("2  The development environment")
    g.h2("2.1  Tools")
    g.table(["Role", "Tool", "Notes"], [
        ["Compiler/assembler/linker", "GNU Arm Embedded Toolchain `arm-none-eabi-gcc` 10.3-2021.10", "Installed at `C:\\VSARM\\armcc\\bin`; exactly the version the SDK recommends"],
        ["Make", "`mingw32-make` from `C:\\VSARM\\mingw\\mingw32\\bin`", "Run from Git Bash"],
        ["SDK", "DMCP_SDK from `github.com/swissmicros/DMCP_SDK`", "Cloned to `Documents\\Projects\\DM42\\DMCP_SDK`; each project is a copy of the SDK template (dmcp/, bin/, Makefile, linker script)"],
        ["Helper", "`crc32` command (Python stand-in in each project's `tools/`)", "The SDK's post-build step calls `crc32`, which Windows lacks"],
        ["Host tests", "gcc (MinGW) and Python 3.10 with Pillow, NumPy, pyserial", "Used for PC-side tests, image conversion and the serial monitor"],
        ["Content tools", "Jasc Animation Shop 3", "Produces the multi-frame GIFs that `gif2dma.py` converts"],
        ["Version control", "Git (the home directory is the repo; whitelist `.gitignore`)", "Explicit paths only; no bulk adds"],
    ], widths=[1.5, 2.6, 2.4], caption="The tool chain.")
    g.h2("2.2  Project layout")
    g.p("Every project is a copy of the SDK's application template with these parts:")
    g.bullets([
        "`src/main.c` \u2014 the program (`program_main()` is the entry point), plus other C files and, for four of the programs, one assembly file (`life.s`, `ack.s`, `anim.s`).",
        "`dmcp/` \u2014 SDK headers and the static library (`dmcp.h` is the interface reference in header form).",
        "`bin/` \u2014 SDK build scripts (packaging, CRC); `stm32_program.ld` \u2014 linker script; `Makefile`.",
        "`tools/` \u2014 PC-side tests and converters; `build/` \u2014 outputs, including the `.pgm`; `screens/` \u2014 screenshots from the device.",
    ])
    g.p("The Makefile builds with `-O2`, hard-float Cortex-M4 flags, and (added by us) `-fno-tree-loop-distribute-patterns` (see G4). It links `libgcc`, which matters "
        "for 64-bit arithmetic: the Ackermann step counter exceeds 32 bits.")
    g.h2("2.3  The build and deploy loop")
    g.code('''export PATH="$PWD/tools:/c/VSARM/armcc/bin:/c/VSARM/mingw/mingw32/bin:$PATH"
mingw32-make                     # -> build/NAME.pgm
cp build/NAME.pgm /e/            # the DM42 drive, in USB disk mode''', "Build and copy (Git Bash). The drive letter was E: throughout; the volume label is DM42.")
    g.numbered([
        "Put the calculator in USB disk mode (SETUP, then the USB disk entry). The drive appears on the PC; copy the `.pgm` to its root.",
        "Eject the drive on the PC and leave disk mode on the calculator.",
        "Reach the DMCP menu: press EXIT from a native program, or use SHIFT+SETUP > System > Reset to DMCP menu from Free42, or hold F1 and press RESET.",
        "Load Program > pick the `.pgm`, then Run Program.",
        "To get Free42 back, load `DM42-3.26.pgm` the same way.",
        "To read results (screenshots, benchmark text files), put the calculator back into disk mode and mount the drive again.",
    ])
    g.p("In practice the human in the loop was Steve, who ran each program on the calculator and reported what he saw or let the program record it. "
        "This arrangement shaped the working method: programs write their own statistics to text files and take their own screenshots so that nothing depends on "
        "eyeballing a small LCD.")
    g.h2("2.4  The sixteen gotchas at a glance")
    g.p("Each program hit some of these. Chapters 4\u201310 note which ones; Chapter 11 gives the cause and the fix.")
    g.table(["#", "Gotcha", "In one sentence"], [[a, b, c] for a, b, c in GOTCHAS], widths=[0.45, 2.1, 3.95], size=9, caption="Gotcha index.")
    g.h2("2.5  Working method")
    g.bullets([
        "**Step by step.** Each program started from a small, verifiable first version (a Hello World, a random field, a table) and grew only after Steve confirmed the previous step on hardware.",
        "**Test on the PC first** wherever logic is portable (Ackermann engine twin, Z-machine core, the Life algorithm, the animation converter's decode/verify), then confirm on the device.",
        "**Measure, don't guess.** Programs report their own timing (generations per second, steps per second, KB/s, per-frame milliseconds), and the numbers drove design changes (Life's dirty-row redraw, Ackermann's slice size, the animation delta format).",
        "**Report what was observed.** Where a result was not captured (for example Zork screenshots), this guide says so.",
    ])


def ch3(g):
    g.h1("3  References and sources")
    g.p("This chapter lists what was used and where it came from. Items marked *consulted* were opened or surfaced by searches during the work and used for facts "
        "in this guide. Items marked *by title* are standard manuals cited from background knowledge; they were not downloaded for the project, and the instruction "
        "behaviour relied on was verified by assembling and running the code (on the PC for the logic, on the DM42 for the real thing).")
    g.h2("3.1  The processor and its assembly language")
    g.table(["Document", "Used for", "Status"], [
        ["Arm, *ARMv7-M Architecture Reference Manual* (DDI 0403)", "Authoritative Thumb-2 instruction semantics (LDR/STR unaligned access, UMULL, shifts, IT blocks)", "by title"],
        ["Arm, *Cortex-M4 Devices Generic User Guide* (DUI 0553) and *Technical Reference Manual* (DDI 0439)", "Instruction timing, unaligned access rules (LDR/STR allowed, LDM/STM not), the FPU option", "by title"],
        ["Arm, *Procedure Call Standard for the Arm Architecture* (AAPCS, IHI 0042)", "Which registers an assembly routine may clobber (r0\u2013r3, r12), which it must save (r4\u2013r11), and 8-byte stack alignment at calls", "by title"],
        ["GNU Binutils, *Using as*: ARM-dependent features, `.syntax unified`", "Directives and syntax of the assembly files (`.thumb`, `.thumb_func`, `.equ`, macros)", "by title"],
        ["STMicroelectronics RM0351 (STM32L4x6 reference manual) and the STM32L476 datasheet", "Peripheral and memory background (flash, LCD interface timing, clock options)", "by title"],
    ], widths=[2.7, 3.0, 0.8], caption="Processor references.")
    g.p("All three assembly programs use the **unified assembler syntax** (`.syntax unified`, `.cpu cortex-m4`, `.thumb`) and follow the AAPCS: arguments arrive in r0\u2013r3, "
        "the routine saves any of r4\u2013r11 it uses plus `lr`, and keeps the stack 8-byte aligned (`push {r4-r8, lr}` in `anim.s` pushes six registers for exactly that reason).")
    g.h2("3.2  SwissMicros documentation and downloads")
    g.table(["Resource", "URL", "Used for"], [
        ["DMCP SDK page and download", "tech.swissmicros.com/DMCP_SDK ; github.com/swissmicros/DMCP_SDK", "The SDK itself, template Makefile and linker script"],
        ["DMCP interface reference (HTML)", "technical.swissmicros.com/dmcp/doc/DMCP-ifc-html/", "Function documentation by group: lcd_graphics, lcd_text, kbd_basic, disk_fat, disk_fatfs, disk_other, sys_timing, sys_autooff, usb, pwr, printer"],
        ["SDK demo program (source and binary)", "tech.swissmicros.com/SDKdemo/ ; github.com/swissmicros/SDKdemo", "The first known-good `.pgm` (SDKdemo-1.0.pgm); the pattern for `program_main()`"],
        ["DMCP firmware downloads and history", "technical.swissmicros.com/dmcp/firmware/ (and /history.html)", "DMCP 3.31 flash image; release notes"],
        ["DM42 programs (Free42 for DM42)", "tech.swissmicros.com/DM42PGM", "`DM42-3.26.pgm`, the program that restores Free42"],
        ["DM42 user manual", "technical.swissmicros.com/dm42/doc/dm42_user_manual ; usermanual-dm42.swissmicros.com", "USB disk mode, system menu, CPU clock on USB vs battery, auto-off"],
    ], widths=[1.9, 2.6, 2.0], size=9, caption="SwissMicros sources (consulted).")
    g.p("The SDK headers are the real specification. `dmcp/dmcp.h` lists all 202 exported functions; several behaviours this guide depends on (one open file at a "
        "time, no directory enumeration, USB serial without a receive function) are visible there as absences.")
    g.h2("3.3  Forum posts and community projects")
    g.p("These surfaced in web searches while looking for prior art. The searches found **no existing Game of Life, Ackermann or Z-machine program for the DM42**; the "
        "forum material was useful for orientation rather than for code.")
    g.table(["Source", "What it offered"], [
        ["MoHPC (hpmuseum.org) forum, thread 18232, \"DM41X creating modules using platform hardware\"", "Confirms the DM41X runs on the same DMCP platform; pointers to the SDK and SDKdemo"],
        ["MoHPC forum posts 88169 and 88181", "SDK landscape discussion that appeared in the search for native-program examples"],
        ["MoHPC forum post 195261, \"WP34S for DM42 project question\"", "Example of a large native port built with the SDK"],
        ["DB48X (github.com/c3d/db48x; hpcalc.org/details/9562)", "A large native replacement firmware program for the DM42; evidence of what the SDK can host"],
        ["DESK42 by zooxo (github.com/zooxo/desk42)", "Calculator/spreadsheet/games program for the DM42; confirms native programs with full-screen UIs are practical"],
        ["DM42pcm by ljubo106 (github.com/ljubo106/DM42pcm)", "Another native program built with the SDK"],
    ], widths=[3.2, 3.3], size=9, caption="Community sources (surfaced by search; their contents were not reviewed in depth).")
    g.h2("3.4  Z-machine and interactive-fiction sources")
    g.table(["Source", "Used for"], [
        ["*The Z-Machine Standards Document* (inform-fiction.org/zmachine/standards/current/), including Appendix D, history of the Z-machine", "The instruction set, text encoding (ZSCII, abbreviations), object tree, dictionary format; the interpreter was written from this"],
        ["IFWiki, \"Z-machine\" (ifwiki.org/Z-machine)", "Version overview (v3 = Zork I\u2013III, Starcross, Planetfall)"],
        ["Frotz (reference interpreter; manual pages for frotz/sfrotz) and *Zork and the Z-machine*, Adafruit Learning System", "Background on interpreter design on small hardware"],
        ["Zorkduino (github.com/rossumur/Zorkduino); A2Z; Z-MIM; CPZ Machine", "Prior art for Z-machine interpreters on microcontrollers; none targets the DM42"],
        ["Microsoft/Activision MIT-licensed release of Zork I\u2013III, Nov 2025: github.com/historicalsource/zork1 (COMPILED/zork1.z3)", "The legal story file (86,838 bytes, release 119, serial 880429). News coverage: The Register, GamingOnLinux, Thurrott"],
        ["CZECH, the Z-machine conformance test (IF Archive, czech_0_8.zip)", "Planned but not completed: the zip contains only a version-5 build, and our interpreter is version 3"],
    ], widths=[3.7, 2.8], size=9, caption="Z-machine sources (consulted).")
    g.h2("3.5  Other tools")
    g.bullets([
        "Pillow and NumPy for image conversion and dithering; pyserial for the USB serial monitor; python-docx for this document.",
        "Jasc Animation Shop 3 (installed on the PC) for creating and editing GIF animations; Steve supplied `Coriolis.gif` from the Downloads folder.",
    ])


def ch4(g):
    g.h1("4  Chapter one program: Hello World and the loading puzzle")
    g.h2("4.1  Requirements")
    g.bullets([
        "Find out which of Steve's SwissMicros calculators accept side-loaded native programs (DM32, DM41X, DM42). The DM42 and DM32 run DMCP and load `.pgm` files; the DM41X is based on the same platform but was not tested.",
        "Build a minimal program with the vendor tool chain on Windows and run it on the DM42, with no HP programming language involved.",
        "Success means: the loader reports the program, and \"Hello World!\" appears on the Sharp display.",
    ])
    g.h2("4.2  How it was done")
    g.numbered([
        "Cloned the DMCP SDK (with `core.autocrlf=false`; see G3), installed the exact recommended Arm GCC 10.3-2021.10, and wrote a Python `crc32` stand-in for the post-build step.",
        "Built the SDK's template as `TESTPGM.pgm` (1,781 bytes): it clears the LCD buffer, writes a string with `lcd_putsAt`, calls `lcd_refresh`, and waits for a key.",
        "Backed up the calculator's drive (DM42_backup_2026-10-07) before touching anything.",
        "Trying to run it: Steve first searched Free42's own menus and `/PROGRAMS` (which hold `.raw` files). That is the wrong loader (G1).",
        "The calculator's firmware was DMCP 3.11 while the SDK expects interface 17, so it was updated to DMCP 3.31. Flashing the OS-only image **erased Free42** (G2); Free42 was restored by loading `DM42-3.26.pgm` through the DMCP menu.",
        "To separate \"my program is wrong\" from \"my loading procedure is wrong\", Steve downloaded SwissMicros' own `SDKdemo-1.0.pgm` and loaded it. Once the loader saw files in the root of the drive, `TESTPGM` loaded as well: \"Program loaded: TESTPGM Version 1.0\", then \"Hello World!\" on screen.",
    ])
    g.p("The lesson of this chapter is mostly procedural: the vendor documentation describes the SDK well but not the surrounding ritual, and several unrelated "
        "facts (two loaders, an OS-only firmware image, matching interface versions, CRLF line endings) looked like a single failure.")
    used(g, ["G1", "G2", "G3", "G15"])


def ch5(g):
    g.h1("5  Conway's Game of Life")
    g.h2("5.1  Requirements")
    g.p("Steve's brief was to run a game of Life on the Sharp display and to use the ARM instruction set directly rather than the HP programming paradigm. From the conversation:")
    g.bullets([
        "A 2x2-pixel cell (a 200 x 120 world), with the world wrapped top-to-bottom and left-to-right (a torus).",
        "Start from a pseudo-random field; press ENTER to start; EXIT stops. (This became a title menu, since EXIT on a running field returns there and EXIT on the menu quits.)",
        "Detect a stable (or period-2) field and return to the menu on its own, with a generations-per-second readout.",
        "Make it fast. Later: more interesting starting states, not just random noise, with multi-generation organisms.",
        "Final cosmetics: remove debug statistics, show \"Conway's Game of Life\" in reverse video.",
    ])
    g.h2("5.2  Design")
    g.h3("The grid and the kernel")
    g.p("The world is stored as bits, 8 cells per byte, in 122 stored rows of 27 bytes. Row 0 and row 121 are copies of the last and first Life rows; byte 0 and byte 26 of each row "
        "are copies of byte 25 and byte 1. With those wrap copies the inner loop has **no edge tests at all**: every byte is computed the same way, and `life_fix_edges` refreshes the copies afterwards.")
    g.p("For each byte of eight cells, the eight neighbour bit-planes (three above, three below, west, east) are added into a small saturating counter built from three bit-planes: "
        "`n0` (ones), `n1` (twos) and `n2` (four or more). One neighbour plane x is added with five logic operations. A cell is alive in the next generation iff it has two or three neighbours and is "
        "either already alive or has an odd count: `n1 & ~n2 & (n0 | self)`.")
    g.code('''/* x += plane; destroys x, uses t as scratch (excerpt from life.s) */
.macro  ADDP x, t
        and     \\t, r0, \\x        /* carry out of the ones plane */
        eor     r0, r0, \\x         /* ones plane */
        and     \\x, r1, \\t        /* carry out of the twos plane */
        eor     r1, r1, \\t         /* twos plane */
        orr     r2, r2, \\x         /* saturate: four or more */
.endm''', "The bit-parallel neighbour counter: eight cells per operation.")
    g.p("The first assembly version processed one cell at a time and was **no faster than compiled C**: Steve measured about 30 ms per generation (\"asm = C\"). Rewriting for eight cells per operation "
        "brought the step to **3.1 ms per generation**, against 28.7 ms for the plain C reference, about nine times faster. The algorithm was first proven on the PC "
        "(`simlife.py` compares it with brute-force Life; `patterns_check.py` and `host_test.c` check the organisms and the stamper).")
    g.h3("Starting states")
    g.p("Steve asked whether the screen could be seeded with interesting multi-generation organisms. A small run-length-encoded pattern library (`patterns.c`) provides them, stamped in all "
        "orientations. Five modes: **1 Soup** (random), **2 Garden** (one methuselah per quadrant), **3 Gun battle** (two Gosper glider guns), **4 Hybrid** (thin soup plus organisms) and "
        "**5 Kaleidoscope** (a random quadrant mirrored four ways); ENTER repeats the last mode.")
    g.h3("Ending a run")
    g.p("The program compares each generation with the one two back (catching still lifes and period-2 oscillators) and watches the population count for cycles up to length 32 "
        "(catching glider guns' steady state). When it detects either, it shows \"Stable after N generations\" and returns to the menu.")
    g.h3("Drawing")
    g.p("Life writes cells straight into the LCD line buffer. `lcd_line_addr(y)` returns a line's buffer and marks the line dirty; `lcd_refresh()` then sends only the dirty lines. "
        "At first every row was redrawn each generation (23.5 ms per frame, 36.5 generations/s). Redrawing only rows whose contents changed raised the rates below.")
    g.fig(f("chart_life.png"), "Life generations per second on USB power. Steve's measurements, one per mode, after the dirty-row refresh; the first bar is the original full-redraw version.", width=5.6)
    g.table(["Mode", "Generations/s (USB)", "Notes"], [
        ["Soup", "40.9", "Busy field: most rows change each generation"],
        ["Garden", "52.4", ""],
        ["Gun battle", "73.8", "Mostly empty field: few dirty rows"],
        ["Hybrid", "42.3", ""],
        ["Kaleidoscope", "44.1", ""],
        ["All modes, battery", "> 10", "About 70% slower unplugged, as the manual's 24 MHz clock predicts"],
    ], widths=[1.5, 1.6, 3.4], align_right_from=None, caption="Life performance (observed on the calculator).")
    g.p("Display transfer is about 0.19 ms per redrawn row and is the limit once the kernel costs only 3.1 ms.")
    g.h2("5.3  Problems met")
    g.bullets([
        "The first title screen ended with \"pixel bit order: LSB first (probe 00)\", the bit order the first version had assumed. A random soup looks identical when mirrored, so the **mirrored buffer (G5)** went unnoticed. Life still displays its world mirrored left-to-right; this is invisible for random soups and flips only the guns and organisms. A one-line fix is known (use byte 49-b and bit 7-p when rendering) but has not been applied.",
        "A stable field left the program \"hanging, waiting for a keypress\" for about 10 more seconds, until the inactivity timer fired (**G8**). The program now checks the timer on every idle wait.",
        "Steve noted once that EXIT returned to the title screen with a blank first line, so the statistics line had scrolled half off the bottom; this was not reported again in later versions.",
        "Timing loops: delays used by the menu had to be rebuilt on `sys_current_ms` after `sys_delay(1500)` took 10\u201315 seconds (**G7**).",
        "Battery: at 24 MHz the program still exceeds 10 generations/s, but it is about 70% slower (**G12**).",
    ])
    g.p("**Results.** Committed as `a0c5bcc` (\"DM42: native Conway's Game of Life in Thumb-2 assembly\") and later `a1a8412` (auto-off handling). Not pushed.")
    used(g, ["G5", "G7", "G8", "G12", "G15"])


def ch6(g):
    g.h1("6  The Ackermann function")
    g.h2("6.1  Requirements")
    g.bullets([
        "Implement the Ackermann function in assembly in the same spirit as Life; first propose a user interface and show an illustration.",
        "Steve declined a big-number routine but asked what **memoization** would do as a speed-up.",
        "Show what the program is doing while it runs: progress, stack depth, a graph of how the stack evolves.",
        "Be usable: parameter entry on the faceplate, a table of small values, pause and stop, and results with timing.",
        "Handle the monster case, A(4,1) = 65,533, which takes 2,862,984,010 recursive steps by brute force.",
    ])
    g.h2("6.2  Design")
    g.h3("A brute-force engine with no recursion")
    g.p("The textbook recursion would overflow any stack, so the engine keeps the pending outer calls as an explicit stack of m values. Each step is exactly one call of the recursive "
        "definition (so the step count equals the number of calls):")
    g.code('''m == 0   n += 1, then m = pop()           (done if the stack is empty)
n == 0   m -= 1, n = 1
else     push(m - 1), n -= 1''', "The three cases of one engine step (from the header of ack.s).")
    g.p("For A(4,1) the stack is 65,531 entries deep but never more than four runs of equal values, so it is **run-length encoded**: a few bytes instead of 64 KB (G11). The topmost run is cached in "
        "registers; the engine runs in slices (`ack_run(state, budget)`), so C code can redraw, handle keys and pause between slices. The engine sustains about 3.9\u20134.0 million steps per second on USB power.")
    g.h3("Three ways to the same answer")
    g.table(["Mode", "How", "A(4,1) = 65,533"], [
        ["Brute force (asm)", "one step per call of the textbook recursion", "2,862,984,010 steps; estimated 11 min 47 s at 4.0M steps/s"],
        ["Memoized chain (C)", "remember how far each m level has climbed (one high-water entry per level)", "98,315 steps; milliseconds"],
        ["Closed form", "formulas for m \u2264 3, known values for m = 4, 5", "instant"],
    ], widths=[1.5, 2.8, 2.2], caption="The three evaluation modes.")
    g.p("The memoized mode still reports the exact brute-force step count, carried along with each remembered value, so the two can be compared directly. The program checks all three against each other "
        "at start-up (the \"self-test OK, 4.0M steps/s\" line on the setup screen also measures the engine speed used for the time estimates).")
    g.h3("Verification on the PC")
    g.p("`tools/ack_host_test.c` contains a C twin of the assembly engine (same registers, same RLE stack, same slicing) and checks it, the closed forms, the exact step counts, the peak stack "
        "depth and the memoized evaluator against plain recursion: 39 small cases at several slice sizes plus the big ones (A(3,10..13), A(4,0), A(4,1), A(5,0)). The assembly itself is checked on the calculator by the start-up self-test.")
    g.h3("The user interface")
    g.p("Steve's first look at the calculator was frank: \"Everything works, but the display doesn't look anything like your initial proposed display format.\" The first working screen (left) was plain text; the "
        "final layout adds boxed entry fields, a progress bar and graphs. Steve photographed the early display and then sent screenshots once the program could take its own (G9). The final setup screen is on the right.")
    g.figs([f("photo_setup.png"), f("ack_setup.png")], "Setup screen: an early version photographed from the calculator (left) and the final layout from an on-device screenshot (right).", width=3.1)
    g.figs([f("ack_run_a.png"), f("ack_run_b.png")], "Running screens for A(3,10) at 16% and 28%: steps, current and peak stack depth, a progress bar, the stack drawn to scale as runs of equal values, and a depth-over-time graph. F1 pauses and EXIT stops.", width=3.1)
    g.figs([f("ack_run41_46.png"), f("ack_run41_91.png")], "A(4,1) brute force at 46% and 91%: 2.6 billion steps in, the stack is 56,847 deep (peak 63,658) and the depth-over-time graph fills in below the numbers.", width=3.1)
    g.figs([f("ack_brute_310.png"), f("ack_memo_310.png")], "The payoff of memoization: A(3,10) = 8,189 takes 44,698,325 steps and 14.5 s by brute force (left), but 12,294 steps and 28 ms memoized (right): 3,635 times fewer steps.", width=3.1)
    g.fig(f("ack_table.png"), "The F2 table of small values (n = 0..4), with the closed forms for m = 4 and m = 5 where the values are too large to print.", width=3.4)
    g.h2("6.3  Results")
    g.table(["Case", "Steps (brute force)", "Brute-force time", "Memoized"], [
        ["A(3,6) = 509", "172,233", "46 ms", "\u2014"],
        ["A(3,10) = 8,189", "44,698,325", "14.5 s", "12,294 steps, 28 ms (3,635x fewer)"],
        ["A(4,1) = 65,533", "2,862,984,010", "13 min 26 s (screenshot run); 15 min 17 s (first run, before the auto-off fix)", "98,315 steps"],
        ["Engine speed", "3.7\u20134.0 million steps/s", "USB power, 80 MHz", ""],
    ], widths=[1.3, 1.5, 2.2, 1.5], caption="Ackermann measurements from the calculator.", size=9)
    g.fig(f("ack_done41.png"), "The finished A(4,1) brute-force run: 2,862,984,010 steps in 13 min 26 s, agreeing with the closed form.", width=3.6)
    g.h2("6.4  Problems met")
    g.bullets([
        "**The calculator hung after the long run** (G8). When A(4,1) finished, the results screen waited for a key, and the 10-minute inactivity timer expired while the screen was waiting. The fix \u2014 call `reset_auto_off()` each slice during a run and return to the DMCP menu when the timer expires on an idle screen \u2014 was verified by Steve on a second A(4,1) run (\"returned to the main screen without a problem\").",
        "**Clipped right-hand text** (G6): the bottom bar's \"time ...\" and the result's \"closed form agrees\" were cut off, because right-alignment counted characters in a proportional font. `bar_line()` now pads with spaces by measured pixel width.",
        "**Redraw cost** (G12): updating the screen between slices slowed the engine: 120 ms slices lost about 20%, 300 ms slices about 9%.",
        "**Screenshots** (G9): F6 was added for this program; the files go to `/SCREENS` as BMP, converted to PNG on the PC for this guide.",
        "Results over 32 bits (A(4,2) = 2^65536 \u2212 3 and beyond) are not computed; the closed form is shown instead.",
    ])
    g.p("**Results.** Committed as `9195331` and `eea2181` (right-aligned bar text by pixel width). Not pushed.")
    used(g, ["G6", "G8", "G9", "G11", "G12", "G15"])
