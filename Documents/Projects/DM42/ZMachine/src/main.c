/*

BSD 3-Clause License

Copyright (c) 2015-2025, SwissMicros
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


  The software and related material is released as “NOMAS”  (NOt MAnufacturer Supported).

  1. Info is released to assist customers using, exploring and extending the product
  2. Do NOT contact the manufacturer with questions, seeking support, etc. regarding
     NOMAS material as no support is implied or committed-to by the Manufacturer
  3. The Manufacturer may reply and/or update materials if and when needed solely
     at their discretion

*/

#include <main.h>
#include <dmcp.h>
#include <stdlib.h>
#include "zmachine.h"

// Infocom (Z-machine version 3) story player for the DM42.
//
// Stories live in /STORIES on the DM42 drive (zork1.z3, ...); the program probes a
// list of well-known names and an optional /STORIES/INDEX.TXT (one file name per
// line). Saved games go to /SAVES (<story>.S1 ... .S9) and the "resume where you
// left off" state to <story>.AUT, written automatically when you quit or leave
// the calculator idle until it would switch itself off.
//
// Typing uses the letters printed on the keys; see key_char() below.

// ---- text helpers (as in the Ackermann program) -------------------------------

static int lh, cw, cols, rows_text;

static void text_at(disp_stat_t *ds, int x, int y, const char *s) {
  uint8_t ln = ds->lnfill, nl = ds->newln;
  ds->lnfill = 0; ds->newln = 0;
  lcd_setXY(ds, x, y);
  lcd_puts(ds, s);
  ds->lnfill = ln; ds->newln = nl;
}

// A reverse-video line across the screen at pixel row y, with `left` at the left
// edge and `right` (if any) against the right edge. The fonts are proportional, so
// the gap is measured in pixels, not characters.
static void bar_line(disp_stat_t *ds, int y, const char *left, const char *right) {
  char b[100];
  int n = 0;
  while (left[n] && n < 60) { b[n] = left[n]; n++; }
  b[n] = 0;
  if (right) {
    int ws = lcd_textWidth(ds, " ");
    if (ws < 1) ws = 5;
    int gap = LCD_X - lcd_textWidth(ds, b) - lcd_textWidth(ds, right) - 6;
    if (gap >= ws) {
      int sp = gap / ws;
      while (sp-- > 0 && n < 80) b[n++] = ' ';
      for (int i = 0; right[i] && n < 98; i++) b[n++] = right[i];
      b[n] = 0;
    }
  }
  uint8_t iv = ds->inv, nl = ds->newln;
  ds->inv = 1; ds->newln = 0;
  lcd_setXY(ds, 0, y);
  lcd_puts(ds, b);
  ds->inv = iv; ds->newln = nl;
}

static char *put_str(char *p, const char *s) { while (*s) *p++ = *s++; return p; }
static char *put_u32(char *p, uint32_t v) {
  char t[11]; int n = 0;
  do { t[n++] = (char)('0' + v % 10); v /= 10; } while (v);
  while (n) *p++ = t[--n];
  return p;
}

// ---- keys -------------------------------------------------------------------------

// Next key press, or -1. SHIFT+F6 takes a screenshot (BMP into /SCREENS).

static int poll_key(void) {
  while (!key_empty()) {
    int k = key_pop();
    if (k <= 0) continue;
    if (k == KEY_SCREENSHOT) { create_screenshot(1); continue; }
    return k;
  }
  return -1;
}

static int idle_event(void) { return is_auto_off() || ST(STAT_PGM_END); }

// Wait for a key. If the system's inactivity timer expires first, *idle is set and
// KEY_EXIT is returned.
static int wait_key(int *idle) {
  for (;;) {
    int k = poll_key();
    if (k > 0) return k;
    if (idle_event()) { if (idle) *idle = 1; return KEY_EXIT; }
    sys_sleep();
  }
}

// The letters printed beside the keys (A..Z run across the keyboard), plus
// space on +, "-" on the minus key and ":" "." "?" on 0, . and R/S.
static char key_char(int k) {
  switch (k) {
  case KEY_SIGMA: return 'a'; case KEY_INV: return 'b'; case KEY_SQRT: return 'c';
  case KEY_LOG: return 'd';   case KEY_LN: return 'e';  case KEY_XEQ: return 'f';
  case KEY_STO: return 'g';   case KEY_RCL: return 'h'; case KEY_RDN: return 'i';
  case KEY_SIN: return 'j';   case KEY_COS: return 'k'; case KEY_TAN: return 'l';
  case KEY_SWAP: return 'm';  case KEY_CHS: return 'n'; case KEY_E: return 'o';
  case KEY_7: return 'p';     case KEY_8: return 'q';   case KEY_9: return 'r';   case KEY_DIV: return 's';
  case KEY_4: return 't';     case KEY_5: return 'u';   case KEY_6: return 'v';   case KEY_MUL: return 'w';
  case KEY_1: return 'x';     case KEY_2: return 'y';   case KEY_3: return 'z';   case KEY_SUB: return '-';
  case KEY_0: return ':';     case KEY_DOT: return '.'; case KEY_RUN: return '?'; case KEY_ADD: return ' ';
  default: return 0;
  }
}

// With SHIFT: the number keys give digits; . , R/S ' and - "
static char key_char_shifted(int k) {
  switch (k) {
  case KEY_0: return '0'; case KEY_1: return '1'; case KEY_2: return '2'; case KEY_3: return '3';
  case KEY_4: return '4'; case KEY_5: return '5'; case KEY_6: return '6'; case KEY_7: return '7';
  case KEY_8: return '8'; case KEY_9: return '9';
  case KEY_DOT: return ','; case KEY_RUN: return '\''; case KEY_SUB: return '"';
  default: return 0;
  }
}

// ---- the story file, saves ----------------------------------------------------------

static FIL story_fp, save_fp;
static char story_base[10];            // upper-case file name stem, at most 8 characters
static char story_path[48];
static int  story_is_open;

// The FAT driver may only allow one open file at a time, so the story file is
// closed while a saved game is written or read, and reopened afterwards.
static void story_close(void) { if (story_is_open) { f_close(&story_fp); story_is_open = 0; } }
static int  story_reopen(void) {
  if (story_is_open) return 1;
  story_is_open = f_open(&story_fp, story_path, FA_READ) == FR_OK;
  return story_is_open;
}
static int  save_is_auto, save_prev_write;

static void make_save_path_in(char *out, const char *dir, const char *ext) {
  char *p = put_str(out, dir);
  p = put_str(p, story_base);
  *p++ = '.';
  p = put_str(p, ext);
  *p = 0;
}

static void make_save_path(char *out, const char *ext) { make_save_path_in(out, "/SAVES/", ext); }

static char *put_int(char *p, int v) { if (v < 0) { *p++ = '-'; v = -v; } return put_u32(p, (uint32_t)v); }

static int io_read(void *ctx, uint32_t off, uint8_t *buf, uint32_t len) {
  (void)ctx;
  UINT got = 0;
  for (uint32_t i = 0; i < len; i++) buf[i] = 0;
  if (!story_is_open && !story_reopen()) return 1;
  if (off >= f_size(&story_fp)) return 0;
  if (f_lseek(&story_fp, off) != FR_OK) return 1;
  if (f_read(&story_fp, buf, len, &got) != FR_OK) return 1;
  return 0;
}

// ---- the game text: word wrapping into a ring of lines ----------------------------------

#define COLS_MAX   60
#define RING_LINES 100

static char (*ring)[COLS_MAX + 1];
static int  ring_total;
static char cur[COLS_MAX + 1];
static int  curlen;
static int  lines_since_input;
static int  view;                       // lines scrolled back from the bottom

static char st_where[40];
static int  st_a, st_b, st_time;

static void screen_draw(const char *typing, const char *softkeys[6], const char *prompt_msg);

static void more_check(void) {
  if (lines_since_input >= rows_text - 1) {
    screen_draw(0, 0, "[MORE]  press any key");
    wait_key(0);
    lines_since_input = 0;
  }
}

static void push_line(void) {
  cur[curlen] = 0;
  char *d = ring[ring_total % RING_LINES];
  for (int i = 0; i <= curlen; i++) d[i] = cur[i];
  ring_total++;
  curlen = 0; cur[0] = 0;
  view = 0;
  lines_since_input++;
  more_check();
}

static void out_char(int c) {
  if (c == 13) { push_line(); return; }
  if (c < 32 || c > 126) return;
  if (curlen >= cols) {                    // wrap at the last space, if there is one
    int sp = -1;
    for (int i = curlen - 1; i > 0; i--) if (cur[i] == ' ') { sp = i; break; }
    if (sp > 0) {
      char rest[COLS_MAX + 1];
      int rl = 0;
      for (int i = sp + 1; i < curlen; i++) rest[rl++] = cur[i];
      curlen = sp;
      push_line();
      for (int i = 0; i < rl; i++) cur[i] = rest[i];
      curlen = rl;
    } else {
      push_line();
    }
  }
  cur[curlen++] = (char)c;
  cur[curlen] = 0;
}

static void out_str(const char *s) { while (*s) out_char(*s++); }

static void io_print(void *ctx, int c) { (void)ctx; out_char(c); }

static void io_status(void *ctx, const char *where, int a, int b, int is_time) {
  (void)ctx;
  int i = 0;
  while (where[i] && i < (int)sizeof st_where - 1) { st_where[i] = where[i]; i++; }
  st_where[i] = 0;
  st_a = a; st_b = b; st_time = is_time;
}

static void screen_draw(const char *typing, const char *softkeys[6], const char *prompt_msg) {
  char l[COLS_MAX + 40], r[40], *p;
  lcd_clear_buf();
  lcd_writeClr(t20);

  // status line
  if (st_time) {
    p = put_str(r, "Time: "); p = put_u32(p, (uint32_t)st_a); *p++ = ':';
    if (st_b < 10) *p++ = '0';
    p = put_u32(p, (uint32_t)st_b); *p = 0;
  } else {
    p = put_str(r, "Score: "); p = put_u32(p, (uint32_t)st_a);
    p = put_str(p, "  Moves: "); p = put_u32(p, (uint32_t)st_b); *p = 0;
  }
  bar_line(t20, 0, st_where, r);

  // text rows: the last rows_text lines (the unfinished line, with what is being typed, at the bottom)
  int have = ring_total < RING_LINES ? ring_total : RING_LINES;
  int n_virtual = have + 1;                              // plus the line being written
  int bottom = n_virtual - 1 - view;                     // index of the last line to show
  for (int row = 0; row < rows_text; row++) {
    int idx = bottom - (rows_text - 1 - row);
    if (idx < 0) continue;
    const char *s;
    if (idx == have) {
      p = put_str(l, cur);
      if (typing) { p = put_str(p, typing); *p++ = '_'; }
      *p = 0;
      int len = (int)(p - l);
      s = len > cols ? l + (len - cols) : l;              // keep the end visible when typing is long
    } else {
      s = ring[(ring_total - have + idx) % RING_LINES];
    }
    text_at(t20, 0, lh * (1 + row), s);
  }

  // bottom bar: completions (7 characters each) or a message
  int y = lh * (1 + rows_text);
  if (!prompt_msg && view > 0) prompt_msg = "[scrolled back: DOWN to return]";
  if (prompt_msg) bar_line(t20, y, prompt_msg, 0);
  else if (softkeys) {
    char sk[100]; int n = 0;
    sk[0] = 0;
    for (int i = 0; i < 6; i++) {                       // each label starts above its F key
      int target = i * (LCD_X / 6) + 3;
      while (n < 80 && lcd_textWidth(t20, sk) < target) { sk[n++] = ' '; sk[n] = 0; }
      const char *w = softkeys[i];
      for (int k = 0; w && w[k] && k < 6 && n < 98; k++) sk[n++] = w[k];
      sk[n] = 0;
    }
    bar_line(t20, y, sk, 0);
  } else bar_line(t20, y, "", 0);
  lcd_forced_refresh();
}

// ---- the game -------------------------------------------------------------------------------

static zm_t zst;
static int  quit_requested, idle_quit;

static void show_message(const char *a, const char *b) {
  lcd_clear_buf();
  lcd_writeClr(t20);
  bar_line(t20, 0, "Z-machine", 0);
  text_at(t20, 6, lh * 2, a);
  if (b) text_at(t20, 6, lh * 3, b);
  bar_line(t20, LCD_Y - lh, "press any key", 0);
  lcd_forced_refresh();
  wait_key(0);
}

// Words to complete: dictionary entries starting with the letters typed so far.
static char comp[6][12];
static const char *comp_ptr[6];
static const char *const default_words[6] = { "look", "inventory", "take", "drop", "open", "examine" };

static void find_completions(const char *typed, int len) {
  int start = len;
  while (start > 0 && typed[start - 1] != ' ') start--;
  int plen = len - start;
  int found = 0;
  if (plen == 0) {
    for (; found < 6; found++) { int k = 0; while (default_words[found][k]) { comp[found][k] = default_words[found][k]; k++; } comp[found][k] = 0; }
  } else {
    int n = zm_dict_count(&zst);
    char w[12];
    for (int i = 0; i < n && found < 6; i++) {
      zm_dict_word(&zst, i, w);
      if (w[0] < 'a' || w[0] > 'z') continue;
      int ok = 1;
      for (int k = 0; k < plen && ok; k++) if (w[k] != typed[start + k]) ok = 0;
      if (!ok || w[plen] == 0) continue;                 // must be longer than what is typed
      int k = 0; while (w[k]) { comp[found][k] = w[k]; k++; } comp[found][k] = 0;
      found++;
    }
  }
  for (int i = 0; i < 6; i++) comp_ptr[i] = i < found ? comp[i] : "";
}

static int slot_prompt(const char *what) {
  char m[48], *p = put_str(m, what);
  p = put_str(p, ": slot 1-9 (EXIT cancels)"); *p = 0;
  screen_draw(0, 0, m);
  for (;;) {
    int k = wait_key(0);
    if (k == KEY_EXIT) return 0;
    char c = key_char_shifted(k);
    if (k >= 0 && c >= '1' && c <= '9') return c - '0';
    if (k == KEY_1) return 1; if (k == KEY_2) return 2; if (k == KEY_3) return 3;
    if (k == KEY_4) return 4; if (k == KEY_5) return 5; if (k == KEY_6) return 6;
    if (k == KEY_7) return 7; if (k == KEY_8) return 8; if (k == KEY_9) return 9;
  }
}

static int io_save_begin(void *ctx) {
  (void)ctx;
  char path[40], ext[4];
  if (save_is_auto) { ext[0] = 'A'; ext[1] = 'U'; ext[2] = 'T'; ext[3] = 0; }
  else {
    int s = slot_prompt("Save");
    if (!s) return 1;
    ext[0] = 'S'; ext[1] = (char)('0' + s); ext[2] = 0;
  }
  make_save_path(path, ext);
  story_close();
  save_prev_write = sys_is_disk_write_enable();
  sys_disk_write_enable(1);
  int dir_rc = check_create_dir("/SAVES");
  FRESULT fr = f_open(&save_fp, path, FA_WRITE | FA_CREATE_ALWAYS);
  FRESULT fr2 = FR_OK;
  if (fr != FR_OK) {                                  // fall back to the drive's root folder
    char root[40];
    make_save_path_in(root, "/", ext);
    fr2 = f_open(&save_fp, root, FA_WRITE | FA_CREATE_ALWAYS);
    if (fr2 == FR_OK) fr = FR_OK;
  }
  if (fr != FR_OK) {
    char m[64], *q = put_str(m, "f_open "); q = put_int(q, (int)fr);
    q = put_str(q, "/"); q = put_int(q, (int)fr2);
    q = put_str(q, " mkdir "); q = put_int(q, dir_rc);
    q = put_str(q, " wr "); q = put_int(q, sys_is_disk_write_enable());
    q = put_str(q, " ok "); q = put_int(q, sys_disk_ok());
    q = put_str(q, " usb "); q = put_int(q, usb_powered()); *q = 0;
    sys_disk_write_enable(save_prev_write);
    story_reopen();
    show_message(save_is_auto ? "Could not write the autosave:" : "Could not create the save file:", m);
    return 1;
  }
  return 0;
}

static int io_save_write(void *ctx, const uint8_t *d, uint32_t n) {
  (void)ctx;
  UINT w = 0;
  return f_write(&save_fp, d, n, &w) != FR_OK || w != n;
}

static void io_save_end(void *ctx, int ok) {
  (void)ctx; (void)ok;
  f_close(&save_fp);
  sys_disk_write_enable(save_prev_write);
  story_reopen();
}

static int restore_is_auto;

static int io_restore_begin(void *ctx) {
  (void)ctx;
  char path[40], ext[4];
  if (restore_is_auto) { ext[0] = 'A'; ext[1] = 'U'; ext[2] = 'T'; ext[3] = 0; }
  else {
    int s = slot_prompt("Restore");
    if (!s) return 1;
    ext[0] = 'S'; ext[1] = (char)('0' + s); ext[2] = 0;
  }
  make_save_path(path, ext);
  story_close();
  if (f_open(&save_fp, path, FA_READ) == FR_OK) return 0;
  make_save_path_in(path, "/", ext);
  if (f_open(&save_fp, path, FA_READ) == FR_OK) return 0;
  story_reopen();
  return 1;
}

static int io_restore_read(void *ctx, uint8_t *d, uint32_t n) {
  (void)ctx;
  UINT g = 0;
  return f_read(&save_fp, d, n, &g) != FR_OK || g != n;
}

static void io_restore_end(void *ctx) { (void)ctx; f_close(&save_fp); story_reopen(); }

static void io_message(void *ctx, const char *m) {
  (void)ctx;
  show_message("The interpreter stopped:", m);
}

static void autosave(void) {
  save_is_auto = 1;
  zm_save_state(&zst, 1);
  save_is_auto = 0;
}

static int io_read_line(void *ctx, char *buf, int max) {
  (void)ctx;
  char typed[COLS_MAX + 8];
  int len = 0, shift = 0;
  typed[0] = 0;
  lines_since_input = 0;
  view = 0;
  for (;;) {
    find_completions(typed, len);
    screen_draw(typed, comp_ptr, shift ? "SHIFT: 0-9 digits   . ,   R/S '   - \"" : 0);
    int idle = 0;
    int k = wait_key(&idle);
    if (idle) { autosave(); idle_quit = 1; return -1; }

    if (k == KEY_SHIFT) { shift = !shift; continue; }
    int sh = shift; shift = 0;

    if (k == KEY_ENTER) {
      for (int i = 0; i < len && i < max; i++) buf[i] = typed[i];
      out_str(typed);
      out_char(13);
      return len < max ? len : max;
    }
    if (k == KEY_BSP) {
      if (sh) { len = 0; typed[0] = 0; } else if (len > 0) typed[--len] = 0;
      view = 0;
      continue;
    }
    if (k == KEY_UP || k == KEY_DOWN) {
      int have = ring_total < RING_LINES ? ring_total : RING_LINES;
      int step = sh ? rows_text - 1 : 1;
      if (k == KEY_UP) view = view + step > have ? have : view + step;
      else view = view - step < 0 ? 0 : view - step;
      continue;
    }
    if (k == KEY_EXIT) {
      screen_draw(typed, 0, "Quit? EXIT or ENTER = yes (autosaves)");
      int q = wait_key(0);
      if (q == KEY_ENTER || q == KEY_EXIT) { autosave(); quit_requested = 1; return -1; }
      continue;
    }
    if (k >= KEY_F1 && k <= KEY_F6) {
      if (sh && k == KEY_F6) { create_screenshot(1); continue; }
      const char *w = comp_ptr[k - KEY_F1];
      if (w && w[0]) {
        while (len > 0 && typed[len - 1] != ' ') len--;           // replace the word being typed
        for (int i = 0; w[i] && len < (int)sizeof typed - 3; i++) typed[len++] = w[i];
        typed[len++] = ' ';
        typed[len] = 0;
      }
      view = 0;
      continue;
    }
    char c = sh ? key_char_shifted(k) : key_char(k);
    if (c && len < max && len < cols - 4) {
      if (c == ' ' && (len == 0 || typed[len - 1] == ' ')) continue;   // no leading or doubled spaces
      typed[len++] = c;
      typed[len] = 0;
      view = 0;
    }
  }
}

// ---- story selection -----------------------------------------------------------------------------

#define MAX_STORIES 16
static char stories[MAX_STORIES][34];
static int  n_stories;

static void add_story(const char *name) {
  if (n_stories >= MAX_STORIES) return;
  char path[48]; char *p = put_str(path, "/STORIES/"); p = put_str(p, name); *p = 0;
  if (!file_exists(path)) return;
  for (int i = 0; i < n_stories; i++) {             // skip duplicates (FAT names are case-insensitive)
    int k = 0;
    while (stories[i][k] && name[k] && ((stories[i][k] | 32) == (name[k] | 32))) k++;
    if (!stories[i][k] && !name[k]) return;
  }
  int k = 0; while (name[k] && k < 33) { stories[n_stories][k] = name[k]; k++; }
  stories[n_stories][k] = 0;
  n_stories++;
}

static void find_stories(void) {
  static const char *const known[] = {
    "zork1.z3", "zork2.z3", "zork3.z3", "starcross.z3", "planetfall.z3", "planetfa.z3",
    "hitchhiker.z3", "hitch.z3", "deadline.z3", "witness.z3", "suspect.z3", "enchanter.z3",
    "sorcerer.z3", "spellbreaker.z3", "infidel.z3", "suspended.z3", "cutthroats.z3",
    "ballyhoo.z3", "seastalker.z3", "wishbringer.z3", "moonmist.z3", "lurking.z3",
    "hollywood.z3", "leather.z3", "stationfall.z3", "plundered.z3", "nordbert.z3", "story.z3"
  };
  n_stories = 0;
  for (unsigned i = 0; i < sizeof known / sizeof known[0]; i++) add_story(known[i]);

  FIL f;                                              // optional list of extra file names
  if (f_open(&f, "/STORIES/INDEX.TXT", FA_READ) == FR_OK) {
    char buf[512]; UINT got = 0;
    if (f_read(&f, buf, sizeof buf - 1, &got) == FR_OK) {
      buf[got] = 0;
      char name[34]; int n = 0;
      for (UINT i = 0; i <= got; i++) {
        char c = buf[i];
        if (c == '\r' || c == '\n' || c == 0) { name[n] = 0; if (n) add_story(name); n = 0; }
        else if (n < 33 && c != ' ') name[n++] = c;
      }
    }
    f_close(&f);
  }
}

static int pick_story(void) {
  int sel = 0;
  for (;;) {
    lcd_clear_buf();
    lcd_writeClr(t20);
    bar_line(t20, 0, "Infocom story player", "DM42 Z-machine v3");
    if (n_stories == 0) {
      text_at(t20, 6, lh * 2, "No stories found. Copy story files");
      text_at(t20, 6, lh * 3, "(zork1.z3, ...) to /STORIES on the");
      text_at(t20, 6, lh * 4, "DM42 drive, or list their names in");
      text_at(t20, 6, lh * 5, "/STORIES/INDEX.TXT, one per line.");
    }
    for (int i = 0; i < n_stories; i++) {
      if (i == sel) bar_line(t20, lh * (2 + i), stories[i], 0);
      else text_at(t20, 6, lh * (2 + i), stories[i]);
    }
    bar_line(t20, LCD_Y - lh, "UP/DOWN choose  ENTER play", "EXIT quit");
    lcd_forced_refresh();
    int idle = 0;
    int k = wait_key(&idle);
    if (idle || k == KEY_EXIT) return -1;
    if (k == KEY_UP && sel > 0) sel--;
    if (k == KEY_DOWN && sel < n_stories - 1) sel++;
    if (k == KEY_ENTER && n_stories) return sel;
  }
}

// Play one story. Returns 1 to go back to the story list, 0 to leave the program.
static int play(const char *name) {
  char path[48];
  int have_auto = 0;
  char *p = put_str(path, "/STORIES/"); p = put_str(p, name); *p = 0;
  {                                                   // save-file name stem
    int n = 0;
    for (int i = 0; name[i] && name[i] != '.' && n < 8; i++) {
      char c = name[i];
      if (c >= 'a' && c <= 'z') c = (char)(c - 32);
      story_base[n++] = c;
    }
    story_base[n] = 0;
  }
  {                                                   // look for an autosave before the story file is held open
    char ap[40];
    make_save_path(ap, "AUT");
    have_auto = file_exists(ap);
    if (!have_auto) { make_save_path_in(ap, "/", "AUT"); have_auto = file_exists(ap); }
  }
  { char *q = put_str(story_path, path); *q = 0; }
  if (!story_reopen()) { show_message("Could not open the story file.", path); return 1; }

  uint8_t hdr[64]; UINT got = 0;
  if (f_read(&story_fp, hdr, 64, &got) != FR_OK || got < 64) { story_close(); show_message("The story file is too short.", 0); return 1; }
  uint32_t dyn_size = (uint32_t)((hdr[14] << 8) | hdr[15]);

  // memory: dynamic part + stack + frames + page cache, sized to what the heap offers
  uint32_t pages = 32;
  uint32_t fixed = dyn_size + 1024 * 2 + 100 * sizeof(zm_frame_t) + RING_LINES * (COLS_MAX + 1) + 512;
  uint32_t avail = (uint32_t)sys_largest_free_mem();
  (void)avail;
  uint8_t *dyn = 0, *cache = 0; uint16_t *stack = 0; zm_frame_t *frames = 0; uint32_t *ctag = 0, *cuse = 0;
  for (;; pages /= 2) {
    free(cache); free(ctag); free(cuse); free(dyn); free(stack); free(frames); free(ring);
    dyn = malloc(dyn_size); stack = malloc(2048); frames = malloc(100 * sizeof(zm_frame_t));
    ring = malloc(RING_LINES * (COLS_MAX + 1));
    cache = malloc(pages * ZM_PAGE); ctag = malloc(pages * 4); cuse = malloc(pages * 4);
    if (dyn && stack && frames && ring && cache && ctag && cuse) break;
    if (pages <= 4) { show_message("Not enough memory for this story.", 0); goto fail; }
  }
  (void)fixed;

  zm_mem_t mem = { dyn, dyn_size, stack, 1024, frames, 100, cache, pages, ctag, cuse };
  zm_io_t io = { io_read, io_print, io_status, io_read_line, io_save_begin, io_save_write, io_save_end,
                 io_restore_begin, io_restore_read, io_restore_end, io_message };
  int e = zm_init(&zst, &io, 0, &mem, sys_current_ms() ^ get_rtc_ticks());
  if (e) { show_message("Cannot start this story (needs version 3).", 0); goto fail; }

  ring_total = 0; curlen = 0; cur[0] = 0; lines_since_input = 0; view = 0;
  st_where[0] = 0; st_a = st_b = st_time = 0;
  quit_requested = idle_quit = 0;

  char autopath[40];
  make_save_path(autopath, "AUT");
  if (have_auto) {
    screen_draw(0, 0, "Saved game found: ENTER continues, EXIT starts over");
    if (wait_key(0) == KEY_ENTER) {
      restore_is_auto = 1;
      int ok = zm_load_state(&zst);
      restore_is_auto = 0;
      if (!ok) show_message("Could not load the saved game.", "Starting a new game.");
    }
  }

  zm_run(&zst);

  if (!idle_quit && !quit_requested) {
    show_message("The game has ended.", 0);
    sys_disk_write_enable(1); f_unlink(autopath); sys_disk_write_enable(0);   // no stale resume file
  }
fail:
  story_close();
  free(cache); free(ctag); free(cuse); free(dyn); free(stack); free(frames); free(ring); ring = 0;
  return !idle_quit && !quit_requested;
}

void program_main() {
  lh = lcd_lineHeight(t20);
  cw = lcd_textWidth(t20, "0");
  cols = LCD_X / cw; if (cols > COLS_MAX) cols = COLS_MAX;
  rows_text = LCD_Y / lh - 2;

  for (;;) {
    find_stories();
    int s = pick_story();
    if (s < 0) break;
    if (!play(stories[s])) break;
  }
}
