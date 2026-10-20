"""Chapters 7-11 and appendices of the DM42 programming guide."""
import os
from content_a import f, used, GOTCHAS


def ch7(g):
    g.h1("7  An Infocom Z-machine player (Zork I)")
    g.h2("7.1  Requirements")
    g.p("Steve asked whether a Z-machine interpreter in the style of Frotz could run Infocom-style games on the DM42, and chose Zork I. He owns Starcross, Planetfall and others but has no story files "
        "(\"long since discarded those 5.25-inch diskettes\"), and asked to proceed **from scratch** rather than porting Frotz.")
    g.bullets([
        "A correct version-3 Z-machine interpreter (the format of Zork I\u2013III, Starcross, Planetfall) in portable C, with no library dependencies.",
        "A legal story file. Microsoft/Activision released Zork I\u2013III under the MIT license in November 2025; `zork1.z3` (86,838 bytes, release 119, serial 880429) was downloaded with Steve's explicit approval from the historicalsource repository.",
        "A DM42 front end: scrolling text with word wrap, a status bar, line input from the faceplate (no SHIFT needed for letters), save/restore, and a way to choose among story files.",
        "Run within the 8 KB program-RAM limit of the DM42 (the story is 87 KB, and Zork's writable memory is 11,282 bytes).",
        "Exit cleanly to the DMCP menu, so the USB disk can be re-enabled.",
    ])
    g.h2("7.2  Design")
    g.h3("A portable core with callbacks")
    g.p("`zmachine.c`/`zmachine.h` (about 770 lines of C, roughly 9.5 KB of Thumb-2 code) implement the Z-machine instruction set. The interpreter never touches hardware: the host supplies all I/O through a table of callbacks "
        "(`read` story bytes, `print` a character, draw the status line, `read_line`, and save/restore streams) and all memory through a small structure. The same code ran first in a PC test harness (`tools/zm_host.c`) and then on the calculator.")
    g.code('''typedef struct {
  int  (*read)(void *ctx, uint32_t off, uint8_t *buf, uint32_t len);   // story file bytes
  void (*print)(void *ctx, int zscii);
  void (*status)(void *ctx, const char *where, int a, int b, int is_time);
  int  (*read_line)(void *ctx, char *buf, int max);                    // -1 = quit
  int  (*save_begin)(void *ctx);  int (*save_write)(void *ctx, const uint8_t *d, uint32_t n);  void (*save_end)(void *ctx, int ok);
  int  (*restore_begin)(void *ctx); int (*restore_read)(void *ctx, uint8_t *d, uint32_t n);    void (*restore_end)(void *ctx);
  void (*message)(void *ctx, const char *msg);
} zm_io_t;''', "The host interface (abridged from zmachine.h).")
    g.h3("Memory")
    g.p("Only the **dynamic** (writable) part of the story lives in RAM: 11,282 bytes for Zork I, taken with `malloc` at start-up (G11). Everything else is read on demand a 512-byte page at a time from the story file "
        "through a small page cache, so the whole 87 KB file never needs to fit in memory. The player sizes its buffers from `sys_largest_free_mem()`.")
    g.h3("Save, restore and resume")
    g.p("A saved game stores the full interpreter state (format tag \"ZMS4\"). `zm_save_state(at_input)` can save *at an input prompt* so that loading it resumes exactly at the prompt; the PC test showed a split run is identical to an uninterrupted one. "
        "Slots S1\u2013S9 live in `/SAVES/<STORY>.Sn`; an **autosave** (`.AUT`) is written when you leave with EXIT and when the inactivity timer fires, and the player resumes from it.")
    g.h3("Keyboard")
    g.p("Typed letters come straight from the faceplate legends, with no SHIFT or ALPHA mode (G16). Steve's reaction: \"I thought I would have to keep the Shift key pressed to get the alphas; nope!\" "
        "The six function keys carry **word completions** drawn from the story's own 684-word dictionary, so most commands take a few keystrokes. See Appendix A for the full map.")
    g.h3("Screen")
    g.p("A text window of nine 25-pixel rows (font t24) with word wrap, a status bar in reverse video (room name, score and moves, or the time for time-based games), a scrollback ring buffer (UP/DOWN), and a story picker that probes known names and `/STORIES/INDEX.TXT` "
        "(the SDK cannot list a directory, G10).")
    g.h2("7.3  Verification")
    g.bullets([
        "On the PC with the real story file: scripted play (open the mailbox, and so on), save and restore, restart, `$verify` (\"The disk is correct\" \u2014 the story's own checksum), troll combat (random), and a 12,000-random-command fuzz run with zero interpreter errors.",
        "Resume-at-prompt: a run split by save/load matches an uninterrupted run exactly.",
        "Save files written on the calculator (`ZORK1.S1`, `ZORK1.AUT`) load correctly in the PC interpreter, so both sides agree on the format.",
        "Not done: the CZECH conformance suite (the downloaded package contains only a version-5 build).",
    ])
    g.p("**Figures.** No screenshots of the Zork player exist yet. The player has a screenshot key, but Steve's testing sessions with Zork did not capture any; add one or two (the story list and a game screen) in a later revision.")
    g.h2("7.4  Problems met")
    g.bullets([
        "**\"Could not create save file. Is the disk write-protected?\"** (G10). The disk is read-only to programs until `sys_disk_write_enable(1)` is called, *and* FatFs on the DM42 allows only one open file at a time. The story file was held open for page reads, so saving failed. The fix closes the story file around every save, restore and file-exists check, then reopens it (`story_close()`/`story_reopen()`).",
        "**Exit handling** (G8): in the first build EXIT did not leave the story-list screen (Steve: \"EXIT key doesn't!\"). Later builds return to the DMCP menu from every screen, offering to save first (ENTER saves, EXIT leaves), and watch the inactivity timer as described in G8.",
        "**gcc and `memset`** (G4): the zeroing helper compiled into a call to `memset`, which does not exist. Build flag `-fno-tree-loop-distribute-patterns` stops the transformation.",
        "**Proportional font alignment** (G6): see Ackermann; fixed in both programs.",
        "Stale program after copying (G15): the version on the drive had a leftover diagnostic line until Steve reloaded it.",
    ])
    g.p("**Status.** Working on the calculator (Steve played it). The project is intentionally **not yet committed**, by Steve's request; it needs a README and a `.gitignore` whitelist block (excluding `stories/` and `screens/`) before one commit. `ZMACHINE.pgm` is 20,477 bytes.")
    used(g, ["G4", "G6", "G8", "G9", "G10", "G11", "G15", "G16"])


def ch8(g):
    g.h1("8  USB serial: the ESP32 link experiment")
    g.h2("8.1  Requirements")
    g.p("Steve asked whether the SDK lets a program use the USB port as a serial port, so that the DM42 could talk to an ESP32-S3 board for Wi-Fi and Bluetooth. "
        "The requirement was a clear yes/no with evidence, in both directions.")
    g.h2("8.2  Approach")
    g.bullets([
        "**Read the SDK.** The exported USB functions are only `usb_acm_on()`, `usb_turn_off()`, `usb_is_on()`, `acm_puts(str)`, `usb_powered()` and `switch_usb_powered_freq()` (202 library functions in all). There is no receive function.",
        "**Build a test program** (`USBTest`): F1 switches the port to a virtual serial port, F2/F3/F5 send text, F4 turns USB off, F6 runs a \"quiet\" receive test that watches for key events.",
        "**Build a PC monitor** (`tools/serial_monitor.py`, pyserial): waits for a new COM port, logs identity and everything received with time stamps, sends numbered lines every 2 s, and tracks whether the drive letter disappears.",
        "Steve connected the DM42 and ran the test; the quiet test then sent all 256 byte values from the PC.",
    ])
    g.h2("8.3  Results")
    g.table(["Question", "Observed"], [
        ["Does the PC see a serial port?", "Yes: USB ID 0483:5740 (ST virtual COM port); Windows loaded its generic \"USB Serial Device\" driver by itself (COM8) within seconds"],
        ["Does calculator output arrive?", "Yes, intact and in order, one line per `acm_puts`; marker timing matched the program's 11 s"],
        ["Does PC input arrive?", "**No.** All 256 byte values and text were sent; the calculator saw zero key events; the PC saw no write errors"],
        ["Disk and serial together?", "No. The E: drive disappears while serial is active and returns when the program turns USB off"],
    ], widths=[2.0, 4.5], caption="USB serial experiment results (USBTest, October 13, 2026).")
    g.p("**Conclusion.** Through the documented API the link is one-way, calculator to host. For an ESP32-S3 partner, that means it would have to be a USB host (and supply 5 V); data could flow calculator to ESP32 only. "
        "A return path would need something else (the shared FAT disk works only while the calculator is in disk mode, or a firmware-level approach). The infrared LED, driven by the HP-82240 printer calls (`print_byte`), is another outward-only channel and a candidate for a future universal-remote project. Steve's response: \"That's fine. We'll play somewhere else.\"")
    g.p("A side finding useful for later work: the SDK's symbol data includes `key_to_alpha_table`, the operating system's own key-to-letter table, which the Zork player re-implements by hand (see Appendix A).")
    used(g, ["G13"], "Also kept for future reference: the experiment programs live in their own folder (`USBTest`) and are not committed.")


def ch9(g):
    g.h1("9  How fast can the calculator read its drive?")
    g.h2("9.1  Requirements")
    g.p("Steve asked what full-screen animation might be possible, and to \"explore how fast it can read from its drive.\" The design of an animation player depends on the answer: if the drive were slow, frames "
        "would have to be small or compressed; if it were fast, raw frames could stream. The requirement was therefore a repeatable measurement on both power sources.")
    g.h2("9.2  The test program")
    g.p("`DiskTest` creates or uses a 1 MiB file `/TESTDATA.BIN` and times several access patterns with `sys_current_ms` (G7): sequential reads at 512 B, 4 KB, 12,000 B (one full raw frame) and 32 KB; \"2-byte header plus 6,000 B\" (the shape of a delta frame); "
        "random seek plus 12,000 B; and open/close. It shows the results, appends them to `/DISKTEST.TXT`, and supports F6 screenshots. Steve ran it twice on USB power and twice on battery.")
    g.figs([f("disk_usb1.png"), f("disk_bat1.png")], "DiskTest screens: USB power at 80 MHz (left) and battery at 24 MHz (right).", width=3.1)
    g.fig(f("chart_disk.png"), "Read throughput (average of two runs each) by access pattern.", width=6.0)
    g.table(["Test", "USB 80 MHz (KB/s, \u00b5s/op)", "Battery 24 MHz (KB/s, \u00b5s/op)"], [
        ["sequential 512 B x512", "10,666\u201311,636; 42\u201346 \u00b5s", "2,115\u20132,133; 234\u2013236 \u00b5s"],
        ["sequential 4 KB x64", "16,000; 250 \u00b5s", "2,534\u20132,560; 1,562\u20131,578 \u00b5s"],
        ["sequential 12,000 B x21 (one frame)", "15,380\u201316,406; 714\u2013761 \u00b5s", "2,460\u20132,485; 4,714\u20134,761 \u00b5s"],
        ["sequential 32 KB x8", "17,066\u201318,285; 1.75\u20131.88 ms", "2,585; 12.4 ms"],
        ["2-byte header + 6,000 B x40", "13,791\u201314,653; 400\u2013425 \u00b5s", "2,344\u20132,368; 2.48\u20132.50 ms"],
        ["random seek + 12,000 B x20", "10,653\u201311,160; 1.05\u20131.10 ms", "1,831\u20131,936; 6.05\u20136.40 ms"],
        ["open + close x20", "250 \u00b5s", "1,100 \u00b5s"],
    ], widths=[2.4, 2.1, 2.0], size=9, caption="DiskTest results, two runs per power source (results/disktest_2026-10-15.txt).")
    g.h2("9.3  What it means")
    g.bullets([
        "USB power: 16\u201318 MB/s sequential; a 12,000-byte raw frame reads in under 1 ms. Battery: about 2.5 MB/s, under 5 ms per frame. Battery reads are about 6.5 times slower, more than the 3.3x clock ratio, so more than the CPU clock is involved.",
        "The drive is **not** the bottleneck for animation. A full frame costs about 23 ms (USB) or about 69\u201375 ms (battery) to push to the LCD (see Chapter 10). That is why delta-coded frames (send only the lines that changed) win: they cut LCD time, not disk time.",
        "Free space is the practical limit: about 3.9 MB with the test files removed, roughly 325 raw frames.",
    ])
    used(g, ["G7", "G9", "G10", "G12"])


def ch10(g):
    g.h1("10  Full-screen animation")
    g.h2("10.1  Requirements")
    g.bullets([
        "Play animations on the whole 400x240 display under assembly-language control, choosing the best image format. The calculator already holds many BMP images but no sequence of frames.",
        "Source material: multi-frame GIFs (Steve's tool is Jasc Animation Shop 3), converted on the PC.",
        "Works on USB power (80 MHz) and battery (24 MHz); tolerate the flicker and slowdown of battery, but never hang.",
        "Player conveniences Steve asked for: keep the cursor on the file just played; wrap around when scrolling the list; fix the idle hang.",
        "Final test clip: Steve's `Coriolis.gif` (400 x 225, 81 frames, 70 ms each, a hand pouring a can into a glass) \u2014 \"scale it to 800x480 and crop, don't squish\".",
    ])
    g.h2("10.2  Choosing a format")
    g.p("A raw 1-bit frame is 50 bytes x 240 lines = 12,000 bytes, but frames rarely change everywhere. The `.DMA` format stores each frame as a list of **changed lines only**, and the first frame holds all 240 so the clip loops cleanly. "
        "Because the LCD is updated line by line, whole lines are the natural unit of change (a line costs the same to send whether one pixel or 400 changed).")
    g.code('''header, 16 bytes:   'DMA1'  u16 frames  u16 400  u16 240  u8 flags  5 reserved
each frame:         u8 delay (units of 10 ms)  u8 n  then n records of
                    [ u8 line number ][ 50 bytes of line data ]
line data:          exactly what the LCD buffer holds: 50 bytes, x mirrored (see G5), 0 bit = black''', "The .DMA file format (little-endian).")
    g.h2("10.3  The converter")
    g.p("`tools/gif2dma.py` converts a GIF: it composites frames, converts to grey, resizes (`--fit fit|fill|stretch`), applies a gamma (`--gamma`, below 1 brightens), dithers to one bit, **flips x to match the LCD buffer** (`pack()`), diffs against the previous frame, "
        "and writes the file. `--verify` decodes the result and checks that every frame is reproduced exactly; `--sheet` writes a contact sheet of 12 frames so the result can be previewed without a calculator.")
    g.figs([f("anim_cube_sheet.png"), f("anim_bounce_sheet.png")], "Contact sheets from the converter for the two synthetic test clips: a rotating wireframe cube (line art compresses well) and a shaded bouncing ball over a gradient (worst case for one-bit dithering).", width=3.2)
    g.h2("10.4  The player")
    g.p("`ANIMPLAY.pgm` (7,421 bytes) lists the `.DMA` files in `/ANIMS` (the picker reads `/ANIMS/INDEX.TXT`, or probes known names), plays the selected clip with F1 pause, EXIT stop, F6 screenshot, then shows statistics and appends them to `/ANIMTEST.TXT`. "
        "The per-frame loop reads the next frame's lines, waits for the previous LCD transfer to finish, copies the lines into the LCD buffer with a short assembly routine, and starts the next transfer by DMA so it overlaps the following read and the wait for the next deadline.")
    g.code('''apply_frame(const uint8_t *rec, int nlines, uint8_t *(*line_addr)(int)):
        push    {r4-r8, lr}              /* six registers: stack stays 8-byte aligned */
next_line:
        ldrb    r0, [r4], #1             /* line number */
        blx     r6                       /* r0 = lcd_line_addr(line); marks the line dirty */
        mov     r3, r0
        mov     r2, #12
copy_words:
        ldr     r1, [r4], #4             /* twelve unaligned words ...    */
        str     r1, [r3], #4
        subs    r2, r2, #1
        bne     copy_words
        ldrh    r1, [r4], #2             /* ... plus the last two bytes   */
        strh    r1, [r3], #2
        subs    r5, r5, #1
        bne     next_line''', "The line-copy kernel (anim.s, abridged). The Cortex-M4 permits unaligned LDR/STR; LDM/STM would not be safe here.")
    g.code('''lcd_refresh_wait();                       // previous frame must be on the display
apply_frame(buf, (int)n, lcd_line_addr);  // copy only the changed lines
lcd_refresh_dma();                        // start the transfer; returns at once
deadline += delay_ms;                     // absolute-deadline pacing: no drift
while (sys_current_ms() - t_start < deadline) { /* poll keys: EXIT, F1 */ }''', "Frame pacing in play() (abridged).")
    g.h2("10.5  Results")
    g.fig(f("chart_anim_frame.png"), "Per-frame time for a clip that changes all 240 lines (ANIM1, first version). On battery the LCD transfer takes about 69 ms per frame, which caps the clip at 12.8 fps; on USB power the transfer is hidden behind the 40 ms frame period.", width=5.6)
    g.table(["Run", "Frames", "Rate", "Read / apply / wait for LCD / idle (ms per frame)", "Lines/frame", "Late frames"], [
        ["USB: bounce.dma", "292", "25.0 fps", "0.0 / 1.0 / 0.0 / 38.9", "225", "0"],
        ["USB: cube.dma", "193", "25.0 fps", "0.0 / 0.0 / 0.0 / 39.8", "110", "0"],
        ["USB: anim1.dma (early version, 240 lines)", "171", "25.0 fps", "0.0 / 1.0 / 0.0 / 38.9", "240", "0"],
        ["USB: bounce.dma (a worse run)", "265", "19.8 fps", "1.6 / 1.2 / 20.5 / 26.7", "225", "73"],
        ["Battery: cube.dma", "110", "25.0 fps", "1.9 / 0.9 / 2.0 / 34.9", "110", "2"],
        ["Battery: anim1.dma (240 lines)", "99", "12.8 fps", "5.4 / 1.9 / 68.9 / 0.3", "240", "85"],
        ["USB: anim2.dma (stable error diffusion)", "209", "25.0 fps", "0.0 / 0.5 / 0.0 / 39.4", "153", "0"],
    ], widths=[1.9, 0.6, 0.7, 2.0, 0.7, 0.6], size=8.5, caption="Player statistics from /ANIMTEST.TXT (selection; see Appendix C).")
    g.p("The 19.8 fps USB run for `bounce.dma` is an outlier from the first sessions (73 late frames, 20.5 ms per frame waiting for the LCD); later runs of the same file held 25 fps with zero late frames. The cause of the outlier was not established. Battery behaviour matches the model: LCD transfer of about 0.29 ms per line "
        "means about 69 ms for 240 lines; a clip changing about 110 lines per frame (the cube) still holds 25 fps.")
    g.h3("Steve's observations, in order")
    g.bullets([
        "\"Refresh flicker is noticeable on battery, imperceptible when plugged in\" \u2014 consistent with line-by-line transfer at 24 MHz.",
        "\"The cube screen is reversed left-to-right\" \u2014 the mirrored buffer (G5); `pack()` now flips x.",
        "\"The ANIM2 presentation looks better, but has a shadow cast to the right by the ball\" \u2014 an artifact of error-diffusion dithering (G14).",
        "\"The shadow is gone from ANIM2, but still present in ANIM3\" \u2014 correct: the program did not change; only the generation of the files did.",
        "\"The Coriolis clip is not appreciably slower on battery\" \u2014 predicted at about 10% slower (about 12.8 fps against the clip's 14.3 fps).",
    ])
    g.h3("Dithering in motion")
    g.p("Converting a grey frame to one bit needs dithering, and the dithering method decides whether static parts of the picture stay identical from frame to frame (so they are not re-sent) and whether moving objects leave artifacts. "
        "Three methods were compared on a ball moving over a still textured background:")
    g.table(["Method", "Pixels changing per frame", "Average frame size", "Behaviour"], [
        ["Ordered (Bayer 8x8)", "597", "3.3 KB", "Static regions identical frame to frame; regular texture"],
        ["Stable error diffusion (`fs-stable`)", "1,653", "3.6 KB", "Floyd\u2013Steinberg quality on the moving part over a once-dithered background"],
        ["Naive error diffusion (`fs`)", "14,683 (27% to the right of the ball)", "7.8 KB", "Rounding error cascades right and down: the visible \"shadow\""],
    ], widths=[1.8, 1.7, 1.1, 1.9], size=9, caption="Dithering comparison on the static-background ball test.")
    g.fig(f("anim_changes_all.png"), "Pixels that change between two consecutive frames (shown black on white). Ordered dithering changes a small cluster around the ball; naive error diffusion also re-patterns everything right of and below it; stable error diffusion stays close to the ball.", width=6.2)
    g.fig(f("chart_dither.png"), "Changed pixels per frame by dithering method.", width=5.2)
    g.h3("The Coriolis clip")
    g.p("Steve asked for 800x480; DM42 frames are 400x240 (the same 5:3 shape), so the clip (400x225) was scaled to fill 400x240 and cropped rather than squashed (`--fit fill`). The clip is dark and the whole picture moves, so a brightening gamma of 0.5 and ordered dithering were used "
        "(`--gamma 0.5 --dither bayer`; error diffusion does not help when about 32% of pixels differ from the median background). The result is `CORIOLIS.DMA`: 957,091 bytes, 81 frames, about 11.8 KB per frame (nearly raw), installed with an `INDEX.TXT` in `/ANIMS`. "
        "Copying the whole file system to the PC first (102 files, 5.80 MB, verified identical) made room: free space after the install was 0.36 MB.")
    g.fig(f("anim_coriolis_sheet.png"), "Twelve frames of CORIOLIS.DMA from the converter's contact sheet: the converted clip as the calculator displays it (gamma 0.5, ordered dither).", width=5.8)
    g.h2("10.6  Problems met")
    g.bullets([
        "Mirrored cube (**G5**); idle hang on the statistics and message screens (**G8**): fixed with `wait_key()`/`quit_req`, and Steve's own timeout report prompted the fix before the Coriolis work.",
        "Dither \"shadow\" (**G14**): `fs-stable` was invented for it; ordered dithering remains the default for full-motion material.",
        "F6 screenshots missing: the old program was still in flash after the file was copied (**G15**).",
        "Free space (**G10**): the drive has about 6 MB; a nearly-raw 81-frame clip is almost 1 MB.",
        "Battery cost (**G12**): flicker and a lower frame ceiling are physical, not bugs; the doc for users should say \"animate on USB power if you can\".",
    ])
    g.p("**Status.** Not yet committed (Anim, DiskTest, USBTest are untracked); the converter and player are ready for more clips.")
    used(g, ["G5", "G8", "G9", "G10", "G12", "G14", "G15"])


SOL = {
"G1": dict(
 symptom="The program is on the drive, but Load Program says \"Cannot open file\", or lists nothing, or the PC sees it and the calculator doesn't.",
 cause="There are two different menus with the same name. **DMCP's** Load Program (reached from the system menu) lists only `.pgm` files in the **root** of the FAT drive. **Free42's** SETUP > File > Load Program reads `.raw` HP-42S programs from `/PROGRAMS`. Subfolders (`/STEVE`, `/PROGRAMS`) are not searched by the DMCP loader.",
 fix=["Put the `.pgm` in the root of the drive.", "Reach the DMCP menu: SHIFT+SETUP > System > Reset to DMCP menu (from Free42), EXIT (from a native program), or F1+RESET.", "Test the procedure first with SwissMicros' own `SDKdemo-1.0.pgm`: if it loads, your loading process is right and any remaining problem is your program."],
 where="Chapter 4"),
"G2": dict(
 symptom="After updating firmware, Free42 vanished and the calculator showed only the DMCP menu; or the program refuses to run on older firmware.",
 cause="`DMCP_flash_X.Y.bin` is an **operating-system-only** image; flashing it removes the loaded Free42 program. Loading *any* `.pgm` replaces whatever program is in flash. The SDK builds against a numbered interface (version 17 for DMCP 3.31), so the firmware has to match.",
 fix=["Back up the drive before flashing; FAT contents survive an OS flash.", "Restore Free42 by loading `DM42-3.26.pgm` through the DMCP menu.", "Update the OS to the SDK's version (DMCP 3.31) before expecting a program to run."],
 where="Chapter 4"),
"G3": dict(
 symptom="SDK `bin/` scripts fail with odd errors; the post-build step cannot find `crc32`; quoting differences between shells corrupt files; `rm -rf` on the working directory fails.",
 cause="Git on Windows converts line endings to CRLF, which breaks the SDK's shell scripts; the SDK calls an external `crc32` command Windows lacks; PowerShell 5.1 and Git Bash quote differently.",
 fix=["Clone with `git clone -c core.autocrlf=false`, and commit a `.gitattributes` containing `* text eol=lf` in each project.", "Put a Python `crc32` stand-in in the project's `tools/` and on the PATH for the build.", "Build from Git Bash with the PATH shown in Chapter 2; write files with an editor tool rather than shell here-strings (PowerShell doubles apostrophes in single-quoted here-strings).", "Do not delete a directory while a shell's working directory is inside it."],
 where="Chapters 2, 4"),
"G4": dict(
 symptom="Link error `undefined reference to memset` (or `memcpy`) in code that never calls them.",
 cause="There is no C library. At `-O2` gcc recognises loops that zero or copy memory and replaces them with library calls.",
 fix=["Add `-fno-tree-loop-distribute-patterns` to `C_DEFS` (the project Makefiles do).", "Write explicit helpers for small jobs, e.g. `static void zero(void *p, uint32_t n) { uint8_t *b = p; while (n--) *b++ = 0; }`.", "`libgcc` *is* linked, so 64-bit arithmetic is available."],
 code="C_DEFS += -fno-tree-loop-distribute-patterns",
 where="Chapter 7"),
"G5": dict(
 symptom="A picture or game world appears mirrored left-to-right on the calculator (the cube animation was; Life's guns are).",
 cause="The SDK builds the LCD buffer with `LCD_INVERT_XAXIS`: the buffer runs **right to left**. Display pixel x is bit `(i & 7)` of byte `(i >> 3)`, with `i = 399 - x`; a 0 bit is black. Code that packs pixels left to right, LSB first, produces a mirror image. A random field hides the problem.",
 fix=["Flip x before packing (the converter does this in `pack()`), or index backwards when rendering directly.", "Remember that 0 means black.", "Life still has the naive layout; the known fix is byte index `49 - b` and bit `7 - p` in its render and table-build code."],
 code='''def pack(bits):
    """LCD layout: display pixel x is bit (i & 7) of byte (i >> 3), i = 399 - x; 1 = white, 0 = black."""
    bits = bits[:, ::-1]
    b = bits.reshape(H, LB, 8).astype(np.uint8)
    weights = (1 << np.arange(8, dtype=np.uint8))
    return (b * weights).sum(axis=2).astype(np.uint8)          # (H, 50)''',
 where="Chapters 5, 10"),
"G6": dict(
 symptom="Right-aligned text (\"time 13 min 26 s\", \"closed form agrees\") is cut off at the right edge, or bars don't line up.",
 cause="The SDK fonts `t20` and `t24` are **proportional**: counting characters doesn't give a width. `lcd_fontWidth` is misleading.",
 fix=["Measure with `lcd_textWidth(font, text)` and pad with spaces until the pixel total fits.", "Place text at explicit positions with `lcd_setXY` and set `newln = 0`; reverse video is `inv = 1`.", "For a reverse-video bar, pad to the full width so the whole row is black."],
 code='''static void bar_line(disp_stat_t *ds, int y, const char *left, const char *right) {
  /* ... copy left into b ... */
  int ws  = lcd_textWidth(ds, " ");
  int gap = LCD_X - lcd_textWidth(ds, b) - lcd_textWidth(ds, right) - 6;
  int sp  = gap / ws;                       /* spaces that fit in the gap */
  /* ... append sp spaces, then right ... */
  ds->inv = 1; ds->newln = 0;               /* reverse video, no newline  */
  lcd_setXY(ds, 0, y);  lcd_puts(ds, b);
}''',
 where="Chapters 6, 7, 9, 10"),
"G7": dict(
 symptom="`sys_delay(1500)` takes 10\u201315 seconds.",
 cause="`sys_delay` is not reliable for this use (observed on Steve's DM42).",
 fix=["Busy-wait on `sys_current_ms()` (polling keys inside the loop), or use `sys_sleep()` between polls where timing precision does not matter.", "Use `sys_current_ms()` for all measurements and deadlines."],
 code="uint32_t t0 = sys_current_ms();\nwhile (sys_current_ms() - t0 < 1500) { /* poll keys */ }",
 where="Chapters 5, 9"),
"G8": dict(
 symptom="The calculator \"hangs\" at the end of a long run, or on an idle screen, and only a reset recovers it. Seen in Ackermann (15-minute run), Life, Zork and the animation player.",
 cause="The OS has a **10-minute inactivity timer**. A native program that waits for a key without watching it, or computes for longer than the timer without resetting it, is left in a state the OS can't power off.",
 fix=["During long work, call `reset_auto_off()` every slice.", "On every idle or waiting screen, poll `is_auto_off()` and `ST(STAT_PGM_END)`; when either fires, **return from the program** so the DMCP menu can power off.", "Wrap all key waits in one `wait_key()` and set a `quit_req` flag so callers unwind."],
 code='''static int idle_event(void) { return is_auto_off() || ST(STAT_PGM_END); }

static int wait_key(void) {                     // returns KEY_EXIT (quit_req set) if the timer fires first
  for (;;) {
    int k = poll_key();
    if (k > 0) return k;
    if (idle_event()) { quit_req = 1; return KEY_EXIT; }
    sys_sleep();
  }
}''',
 where="Chapters 5, 6, 7, 10"),
"G9": dict(
 symptom="Pressing F6 (or the screenshot key) does nothing; screenshots are missing; the BMP cannot be viewed directly.",
 cause="Screenshots are not automatic in a native program: the program must call `create_screenshot(1)` when it sees `KEY_SCREENSHOT` or F6. The OS writes a BMP (with a 108-byte V4 header) into `/SCREENS`; some tools, including the image reader used while preparing this guide, could not open it directly.",
 fix=["Handle both keys in the key poll function.", "Convert the BMP to PNG on the PC (Python/Pillow) for documents.", "Reload the program after copying a new build (G15), or F6 will still not work."],
 code='''static int poll_key(void) {
  while (!key_empty()) {
    int k = key_pop();
    if (k <= 0) continue;
    if (k == KEY_SCREENSHOT || k == KEY_F6) { create_screenshot(1); continue; }
    return k;
  }
  return -1;
}''',
 where="Chapters 6, 7, 9, 10"),
"G10": dict(
 symptom="\"Could not create save file. Is the disk write-protected?\"; a second `f_open` fails; a file is not found by a directory scan.",
 cause="The FatFs layer permits **one open file at a time**; the SDK offers `f_open/f_close/f_read/f_write/f_lseek/f_rename/f_unlink`, `file_exists`, `file_size` and `check_create_dir`, but **no directory listing**; the disk is read-only to programs until `sys_disk_write_enable(1)`; names should be 8.3.",
 fix=["Close a long-lived read handle (the Zork story file) before any other open, including `file_exists`; reopen afterwards.", "Wrap writes in `prev = sys_is_disk_write_enable(); sys_disk_write_enable(1); ...; sys_disk_write_enable(prev);`.", "Find data files by probing known names or reading an `INDEX.TXT` you maintain.", "Use 8.3 names and upper-case folders (`/ANIMS`, `/SAVES`, `/STORIES`)."],
 code='''static void story_close(void) { if (story_is_open) { f_close(&story_fp); story_is_open = 0; } }
static int  story_reopen(void) {
  if (story_is_open) return 1;
  story_is_open = f_open(&story_fp, story_path, FA_READ) == FR_OK;
  return story_is_open;
}
/* every save, restore and file_exists: story_close(); ...; story_reopen(); */''',
 where="Chapters 7, 9, 10"),
"G11": dict(
 symptom="Link error \"region RAM overflowed\", or odd crashes when a large array is declared.",
 cause="The program's static data region (`.data` + `.bss`) is only **8 KB**. Big buffers don't fit there.",
 fix=["Allocate large buffers with `malloc` at start-up (the heap is much bigger), sized from `sys_largest_free_mem()` if flexible.", "Choose compact representations: run-length-encode the Ackermann stack; keep only Zork's 11,282 dynamic bytes in RAM and cache story pages; keep the Life grid bit-packed."],
 code="buf = malloc(240 * LINE_REC);                // Anim: one full frame's worth of line records\navail = (uint32_t)sys_largest_free_mem();     // Zork: size the page cache from what is left",
 where="Chapters 6, 7"),
"G12": dict(
 symptom="A program that is fast on USB power is 3\u201370% slower on battery, and animation flickers.",
 cause="The CPU runs at **80 MHz on USB power and 24 MHz on battery**. Disk reads slow about 6.5 times; LCD transfers take about 0.095 ms per line on USB and 0.29 ms per line on battery. In graphics programs the LCD transfer, not computation, is the limit once the kernel is fast.",
 fix=["Redraw only the lines that changed (`lcd_line_addr()` marks lines dirty; `lcd_refresh()` sends them).", "Overlap work with the transfer: `lcd_refresh_dma()` returns immediately; call `lcd_refresh_wait()` before touching the buffer.", "Pace by absolute deadlines, and if far behind, resynchronise instead of rushing.", "Budget for both power sources and report which one a figure refers to.", "Slice long computations and don't redraw too often (Ackermann: 120 ms slices lost about 20%; 300 ms slices lost about 9%)."],
 where="Chapters 5, 6, 9, 10"),
"G13": dict(
 symptom="The PC sees a serial port but can't talk to the calculator; the E: drive vanishes when serial starts.",
 cause="The SDK exports `usb_acm_on()`, `usb_turn_off()`, `usb_is_on()`, `acm_puts()`, `usb_powered()` and `switch_usb_powered_freq()` only: no receive. USB disk and serial are mutually exclusive.",
 fix=["Use serial only for calculator-to-PC logging. Don't plan a bidirectional link without a different mechanism.", "To exchange files, use disk mode, with the calculator out of the program.", "A reverse-engineering route (reading input from the USB peripheral directly) is possible but unexplored."],
 where="Chapter 8"),
"G14": dict(
 symptom="Moving objects in a dithered animation leave a trailing \"shadow\"; file sizes balloon.",
 cause="Floyd\u2013Steinberg error diffusion carries rounding error to the right and downwards, so a change in one place re-patterns everything after it (14,683 changed pixels per frame against 597 for ordered dithering in the test).",
 fix=["Use ordered (Bayer) dithering for animation, or `--dither fs-stable`: dither the median background once and re-dither only pixels that differ from it.", "Reserve plain `fs` for stills.", "Check the effect on the PC first with `--sheet` and the changed-pixel counts."],
 code="python gif2dma.py in.gif OUT.DMA --dither fs-stable --fit fill --verify --sheet sheet.png",
 where="Chapter 10"),
"G15": dict(
 symptom="A new feature (an F6 screenshot, a fix) is missing after copying the new `.pgm` to the drive.",
 cause="Copying a file to the FAT drive doesn't change the program in flash. The old program keeps running until it is loaded again.",
 fix=["After every copy, Load Program again (and Run Program).", "Put a version or build label on the title screen so you can tell which build is loaded."],
 where="Chapters 4, 5, 6, 7, 10"),
"G16": dict(
 symptom="Key presses return unexpected codes; letters need SHIFT; arrow, F-key and screenshot keys behave differently from ASCII expectations.",
 cause="The SDK returns **key codes named after the faceplate legends** (`KEY_SIGMA`, `KEY_INV`, `KEY_SQRT`, ...), not ASCII. Letters in ALPHA mode come from a table in the OS.",
 fix=["Map the codes yourself, using the small letters printed at the lower right of each key (Appendix A).", "Use F1\u2013F6 for context-sensitive actions, and let the program read keys directly so no SHIFT or ALPHA state is needed.", "Handle KEY_EXIT and KEY_SCREENSHOT everywhere."],
 code="case KEY_SIGMA: return 'a'; case KEY_INV: return 'b'; case KEY_SQRT: return 'c';\ncase KEY_LOG:   return 'd'; case KEY_LN:  return 'e'; case KEY_XEQ:  return 'f';   /* ... */",
 where="Chapter 7"),
}


def ch11(g):
    g.h1("11  The gotchas and their solutions")
    g.p("Each entry gives the symptom you will see, the cause, the fix, and where in this guide it appeared. Some code is lifted from the finished programs; short fragments are abridged.")
    for gid, name, one in GOTCHAS:
        s = SOL[gid]
        g.h2(f"{gid}  {name}")
        g.p(f"**Symptom.** {s['symptom']}")
        g.p(f"**Cause.** {s['cause']}")
        g.p("**Solution.**", keep=True)
        g.bullets(s["fix"])
        if s.get("code"): g.code(s["code"])
        g.p(f"*Seen in:* {s['where']}.", size=10)


def appendices(g):
    g.h1("Appendix A  Key map")
    g.p("The Zork player reads letters straight from the small legend at the lower right of each key (no SHIFT or ALPHA). KEY_ names are the SDK's. Row numbers count from the soft keys.")
    g.table(["Row", "Keys (left to right) and letters"], [
        ["1 (\u03a3+ \u2026 XEQ)", "\u03a3+ **A**, 1/x **B**, \u221ax **C**, LOG **D**, LN **E**, XEQ **F**"],
        ["2", "STO **G**, RCL **H**, R\u2193 **I**, SIN **J**, COS **K**, TAN **L**"],
        ["3", "ENTER (accept/ENTER), x\u2276y **M**, +/\u2212 **N**, E **O**, \u2190 (backspace)"],
        ["4", "\u25b2 (scroll up), 7 **P**, 8 **Q**, 9 **R**, \u00f7 **S**"],
        ["5", "\u25bc (scroll down), 4 **T**, 5 **U**, 6 **V**, \u00d7 **W**"],
        ["6", "SHIFT, 1 **X**, 2 **Y**, 3 **Z**, \u2212 (hyphen)"],
        ["7", "EXIT/ON (quit), 0 (colon), . (period), R/S (question mark), + (space)"],
        ["F1\u2013F6", "Word completions in Zork; pause, mode and table in other programs; F6 = screenshot where supported"],
    ], widths=[1.4, 5.1], caption="Letters used by the Zork player (compare with the faceplate in Figure 2). SHIFT+digit types the digit, SHIFT+. a comma, SHIFT+R or S an apostrophe, SHIFT+\u2212 a quote.")
    g.fig(f("photo_keyboard.png"), "The faceplate again for reference (Steve's photo).", width=2.9)

    g.h1("Appendix B  Build, install and test recipes")
    g.h3("Build any project")
    g.code('''cd /c/Users/Steve/Documents/Projects/DM42/<Project>
export PATH="$PWD/tools:/c/VSARM/armcc/bin:/c/VSARM/mingw/mingw32/bin:$PATH"
mingw32-make                      # -> build/<NAME>.pgm''')
    g.h3("Install and run")
    g.numbered(["Calculator: SETUP > USB disk mode. PC: copy `build/<NAME>.pgm` to the root of the DM42 drive. Eject.",
                "Calculator: exit disk mode; reach the DMCP menu (EXIT from a native program, or SHIFT+SETUP > System > Reset to DMCP menu).",
                "Load Program > the `.pgm`, then Run Program. Read results later in disk mode (`/SCREENS`, `/DISKTEST.TXT`, `/ANIMTEST.TXT`)."])
    g.h3("Host-side tests")
    g.code('''# Ackermann: engine twin, closed forms, memoized evaluator against plain recursion
gcc -std=gnu99 -D__USE_MINGW_ANSI_STDIO=1 -O2 tools/ack_host_test.c src/ack_math.c -o ack_host_test
./ack_host_test            # about a second;  ./ack_host_test full  adds the 2.9-billion-step A(4,1)

# Life: the assembly's algorithm vs brute-force Life; organisms; stamper
python tools/simlife.py ;  python tools/patterns_check.py

# Animation: convert and verify
python tools/gif2dma.py in.gif OUT.DMA --dither bayer --fit fill --gamma 0.5 --verify --sheet sheet.png''')
    g.h3("Programs and sizes")
    root = r"C:\Users\Steve\Documents\Projects\DM42"
    rows = []
    for name, pgm, what, git in (("DMCP_SDK", "TESTPGM.pgm", "Hello World", "(SDK clone)"),
                                ("Life", "LIFE.pgm", "Conway's Life, 5 modes", "committed a0c5bcc, a1a8412"),
                                ("Ackermann", "ACKERMANN.pgm", "brute force / memoized / closed form", "committed 9195331, eea2181"),
                                ("ZMachine", "ZMACHINE.pgm", "Z-machine v3 player (Zork I)", "uncommitted (by request)"),
                                ("USBTest", "USBTEST.pgm", "USB serial experiment", "uncommitted"),
                                ("DiskTest", "DISKTEST.pgm", "FAT read-speed benchmark", "uncommitted"),
                                ("Anim", "ANIMPLAY.pgm", ".DMA animation player", "uncommitted")):
        p = os.path.join(root, name, "build", pgm)
        rows.append([name, pgm, f"{os.path.getsize(p):,}" if os.path.exists(p) else "?", what, git])
    g.table(["Project", "Program", "Bytes", "Purpose", "Git state"], rows, widths=[1.0, 1.3, 0.7, 2.0, 1.5], size=9, caption="Programs built (nothing has been pushed).")

    g.h1("Appendix C  Statistics")
    g.h3("Drive read speed")
    g.p("See the Chapter 9 results table for the full run-by-run numbers. Summary: USB 80 MHz 16\u201318 MB/s sequential; battery 24 MHz about 2.5 MB/s; a 12,000-byte frame reads in 0.71\u20130.76 ms (USB) or 4.7 ms (battery); open+close 0.25 ms vs 1.1 ms.")
    g.h3("LCD transfer")
    g.table(["Power", "Per line", "240 lines", "Source"], [
        ["USB 80 MHz", "about 0.095 ms", "about 23 ms", "Life redraw; player stats"],
        ["Battery 24 MHz", "about 0.29 ms", "about 69 ms (68.9 measured)", "player stats, ANIM1 on battery"],
    ], widths=[1.4, 1.4, 1.9, 1.8], caption="LCD transfer cost.")
    g.h3("Engine and kernel speeds")
    g.table(["Item", "Value"], [
        ["Life step (asm, 8 cells/op)", "3.1 ms per generation (plain C reference 28.7 ms; first cell-at-a-time asm about 30 ms)"],
        ["Life displayed rate (USB)", "36.5 generations/s full redraw; 40.9\u201373.8 with dirty-row redraw, by mode"],
        ["Ackermann engine", "3.7\u20134.0 million steps/s (USB)"],
        ["Ackermann A(4,1)", "2,862,984,010 steps; 13 min 26 s to 15 min 17 s observed; memoized 98,315 steps"],
        ["Animation, USB", "25 fps sustained with up to 240 changed lines per frame (early test) and 0 late frames in most runs"],
        ["Animation, battery", "25 fps at 110 lines/frame; 12.8 fps at 240 lines/frame"],
        ["Coriolis clip", "81 frames x 70 ms = 14.3 fps target; 957,091 bytes; not appreciably slower on battery (Steve)"],
    ], widths=[2.1, 4.4], size=9.5, caption="Key performance numbers.")

    g.h1("Appendix D  Open items and ideas")
    g.table(["Item", "Status"], [
        ["Commit ZMachine (needs README, `.gitignore` whitelist block, one commit, no push)", "preserved for later at Steve's request"],
        ["Commit Anim, DiskTest, USBTest", "ask Steve first"],
        ["Fix Life's mirrored world", "one-line fix known; offered, not requested"],
        ["Zork II and III from the historicalsource repositories", "needs Steve's approval to download"],
        ["Z-machine version 5 support (and the CZECH conformance suite)", "not started"],
        ["Byte-span delta compression for `.DMA` (smaller files, not faster)", "idea"],
        ["Automatic brightness adjustment for dark clips", "idea"],
        ["Zork screenshots for this guide", "to be captured"],
        ["DM42 as an infrared universal remote (LG TV, Panasonic DVD, Firestick)", "needs reverse engineering of the IR LED (the SDK only exposes HP-82240 printer calls); not started"],
        ["Two-way link to the ESP32-S3", "blocked by the one-way serial limit (G13)"],
    ], widths=[4.5, 2.0], size=9.5, caption="Open items.")
