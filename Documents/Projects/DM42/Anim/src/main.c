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

// Plays .DMA animations from /ANIMS on the DM42 drive (made by tools/gif2dma.py from an
// animated GIF). Each frame stores only the LCD lines that changed; the line copy is in
// assembly (anim.s). The next frame is read and copied while the previous one is still
// being sent to the display in the background (lcd_refresh_dma).
//
//   ENTER choose / restart    F1 pause    EXIT stop

extern void apply_frame(const uint8_t *rec, int nlines, uint8_t *(*line_addr)(int));

static int lh;

static void text_at(disp_stat_t *ds, int x, int y, const char *s) {
  uint8_t ln = ds->lnfill, nl = ds->newln;
  ds->lnfill = 0; ds->newln = 0;
  lcd_setXY(ds, x, y);
  lcd_puts(ds, s);
  ds->lnfill = ln; ds->newln = nl;
}

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
static char *put_tenths(char *p, uint32_t v10) { p = put_u32(p, v10 / 10); *p++ = '.'; return put_u32(p, v10 % 10); }

static int poll_key(void) {
  while (!key_empty()) {
    int k = key_pop();
    if (k <= 0) continue;
    if (k == KEY_SCREENSHOT || k == KEY_F6) { create_screenshot(1); continue; }   // F6: screenshot to /SCREENS
    return k;
  }
  return -1;
}

// Set when the system's inactivity timer expires while we wait for a key: the program then
// ends, so the DMCP menu can power the calculator off (waiting on regardless hangs it).
static int quit_req;

static int idle_event(void) { return is_auto_off() || ST(STAT_PGM_END); }

// Wait for a key press; returns KEY_EXIT (with quit_req set) if the timer expires first.
static int wait_key(void) {
  for (;;) {
    int k = poll_key();
    if (k > 0) return k;
    if (idle_event()) { quit_req = 1; return KEY_EXIT; }
    sys_sleep();
  }
}

static void show_message(const char *a, const char *b) {
  lcd_clear_buf();
  lcd_writeClr(t20);
  bar_line(t20, 0, "Animation player", 0);
  text_at(t20, 6, lh * 2, a);
  if (b) text_at(t20, 6, lh * 3, b);
  bar_line(t20, LCD_Y - lh, "press any key", 0);
  lcd_forced_refresh();
  wait_key();
}

// ---- finding animations ------------------------------------------------------------

#define MAX_ANIMS 12
static char anims[MAX_ANIMS][20];
static int  n_anims;

static void add_anim(const char *name) {
  if (n_anims >= MAX_ANIMS) return;
  char path[40], *p = put_str(path, "/ANIMS/"); p = put_str(p, name); *p = 0;
  if (!file_exists(path)) return;
  for (int i = 0; i < n_anims; i++) {
    int k = 0;
    while (anims[i][k] && name[k] && ((anims[i][k] | 32) == (name[k] | 32))) k++;
    if (!anims[i][k] && !name[k]) return;
  }
  int k = 0; while (name[k] && k < 19) { anims[n_anims][k] = name[k]; k++; }
  anims[n_anims][k] = 0;
  n_anims++;
}

static void find_anims(void) {
  static const char *const known[] = { "bounce.dma", "cube.dma", "anim1.dma", "anim2.dma", "anim3.dma", "demo.dma", "coriolis.dma" };
  n_anims = 0;
  for (unsigned i = 0; i < sizeof known / sizeof known[0]; i++) add_anim(known[i]);
  FIL f;                                              // optional list of more names
  if (f_open(&f, "/ANIMS/INDEX.TXT", FA_READ) == FR_OK) {
    char b[400]; UINT got = 0;
    if (f_read(&f, b, sizeof b - 1, &got) == FR_OK) {
      char name[20]; int n = 0;
      for (UINT i = 0; i <= got; i++) {
        char c = i < got ? b[i] : 0;
        if (c == '\r' || c == '\n' || c == 0) { name[n] = 0; if (n) add_anim(name); n = 0; }
        else if (n < 19 && c != ' ') name[n++] = c;
      }
    }
    f_close(&f);
  }
}

// ---- playback ------------------------------------------------------------------------------

#define LINE_REC 51

typedef struct {
  uint32_t frames, ms_total, ms_read, ms_apply, ms_dma_wait, ms_idle, late, bytes, lines;
  uint32_t first_ms;
  char     name[20];
  int      usb;
} stats_t;

static FIL fp;
static uint8_t *buf;

// Paused: wait for F1 to resume (returns 0) or EXIT / the inactivity timer (returns 1).
static int pause_wait(void) {
  lcd_refresh_wait();
  for (;;) {
    int k = wait_key();
    if (k == KEY_F1) return 0;
    if (k == KEY_EXIT) return 1;
  }
}

static int play(const char *name, stats_t *st) {
  char path[40], *p = put_str(path, "/ANIMS/"); p = put_str(p, name); *p = 0;
  UINT got = 0;
  uint8_t hdr[16];
  if (f_open(&fp, path, FA_READ) != FR_OK) { show_message("Could not open the animation.", path); return 0; }
  if (f_read(&fp, hdr, 16, &got) != FR_OK || got != 16 || hdr[0] != 'D' || hdr[1] != 'M' || hdr[2] != 'A' || hdr[3] != '1') {
    f_close(&fp); show_message("Not a DMA1 animation file.", 0); return 0;
  }
  uint32_t nframes = hdr[4] | (hdr[5] << 8);
  if (nframes == 0) { f_close(&fp); show_message("The animation has no frames.", 0); return 0; }

  *st = (stats_t){0};
  { int k = 0; while (name[k] && k < 19) { st->name[k] = name[k]; k++; } st->name[k] = 0; }
  st->usb = usb_powered();

  lcd_clear_buf();
  lcd_forced_refresh();
  uint32_t t_start = sys_current_ms(), deadline = 0;
  int stopped = 0;

  for (uint32_t f = 0; !stopped; f = (f + 1) % nframes) {
    if (f == 0 && st->frames) {                       // loop: frame 0 holds all 240 lines
      f_lseek(&fp, 16);
    }
    reset_auto_off();
    uint32_t t0 = sys_current_ms();
    uint8_t h2[2];
    if (f_read(&fp, h2, 2, &got) != FR_OK || got != 2) break;
    uint32_t delay_ms = h2[0] * 10u, n = h2[1];
    if (n && (f_read(&fp, buf, n * LINE_REC, &got) != FR_OK || got != n * LINE_REC)) break;
    uint32_t t1 = sys_current_ms();

    lcd_refresh_wait();                               // the previous frame must be on the display
    uint32_t t2 = sys_current_ms();
    apply_frame(buf, (int)n, lcd_line_addr);
    lcd_refresh_dma();
    uint32_t t3 = sys_current_ms();

    if (st->frames == 0) st->first_ms = t3 - t0;
    st->frames++; st->bytes += 2 + n * LINE_REC; st->lines += n;
    st->ms_read += t1 - t0; st->ms_dma_wait += t2 - t1; st->ms_apply += t3 - t2;

    deadline += delay_ms;
    uint32_t now = sys_current_ms() - t_start;
    if (now > deadline + 250) deadline = now;         // far behind: do not rush to catch up
    if (now > deadline) st->late++;
    uint32_t idle0 = sys_current_ms();
    while (sys_current_ms() - t_start < deadline) {
      int k = poll_key();
      if (k == KEY_EXIT) { stopped = 1; break; }
      if (k == KEY_F1) {                              // pause
        if (pause_wait()) stopped = 1;
        deadline = sys_current_ms() - t_start;
        break;
      }
    }
    st->ms_idle += sys_current_ms() - idle0;
    int k = poll_key();
    if (k == KEY_EXIT) stopped = 1;
    if (k == KEY_F1) {
      if (pause_wait()) stopped = 1;
      deadline = sys_current_ms() - t_start;
    }
  }
  lcd_refresh_wait();
  st->ms_total = sys_current_ms() - t_start;
  f_close(&fp);
  return 1;
}

static void log_stats(const stats_t *st) {
  char line[120], *q;
  int prev = sys_is_disk_write_enable();
  sys_disk_write_enable(1);
  if (f_open(&fp, "/ANIMTEST.TXT", FA_WRITE | FA_OPEN_APPEND) == FR_OK) {
    UINT w;
    uint32_t f = st->frames ? st->frames : 1;
    q = put_str(line, st->usb ? "== USB powered " : "== battery ");
    q = put_str(q, st->name); q = put_str(q, ": "); q = put_u32(q, st->frames); q = put_str(q, " frames in ");
    q = put_u32(q, st->ms_total); q = put_str(q, " ms, ");
    q = put_tenths(q, st->ms_total ? st->frames * 10000u / st->ms_total : 0); q = put_str(q, " fps\r\n"); *q = 0;
    f_write(&fp, line, (UINT)(q - line), &w);
    q = put_str(line, "   per frame: read "); q = put_tenths(q, st->ms_read * 10 / f);
    q = put_str(q, " ms, apply "); q = put_tenths(q, st->ms_apply * 10 / f);
    q = put_str(q, " ms, wait for display "); q = put_tenths(q, st->ms_dma_wait * 10 / f);
    q = put_str(q, " ms, idle "); q = put_tenths(q, st->ms_idle * 10 / f);
    q = put_str(q, " ms; lines "); q = put_u32(q, st->lines / f);
    q = put_str(q, ", bytes "); q = put_u32(q, st->bytes / f);
    q = put_str(q, ", late "); q = put_u32(q, st->late); q = put_str(q, "\r\n"); *q = 0;
    f_write(&fp, line, (UINT)(q - line), &w);
    f_close(&fp);
  }
  sys_disk_write_enable(prev);
}

static void show_stats(const stats_t *st) {
  char l[100], *p;
  uint32_t f = st->frames ? st->frames : 1;
  lcd_clear_buf();
  lcd_writeClr(t20);
  p = put_str(l, st->usb ? "USB powered" : "battery"); *p = 0;
  bar_line(t20, 0, st->name, l);
  p = put_u32(l, st->frames); p = put_str(p, " frames in "); p = put_tenths(p, st->ms_total / 100); p = put_str(p, " s  =  ");
  p = put_tenths(p, st->ms_total ? st->frames * 10000u / st->ms_total : 0); p = put_str(p, " frames/s"); *p = 0;
  text_at(t20, 6, lh * 1, l);
  p = put_str(l, "per frame:  read "); p = put_tenths(p, st->ms_read * 10 / f); p = put_str(p, " ms"); *p = 0;
  text_at(t20, 6, lh * 3, l);
  p = put_str(l, "            apply "); p = put_tenths(p, st->ms_apply * 10 / f); p = put_str(p, " ms"); *p = 0;
  text_at(t20, 6, lh * 4, l);
  p = put_str(l, "            wait for display "); p = put_tenths(p, st->ms_dma_wait * 10 / f); p = put_str(p, " ms"); *p = 0;
  text_at(t20, 6, lh * 5, l);
  p = put_str(l, "            idle "); p = put_tenths(p, st->ms_idle * 10 / f); p = put_str(p, " ms"); *p = 0;
  text_at(t20, 6, lh * 6, l);
  p = put_str(l, "avg lines "); p = put_u32(p, st->lines / f); p = put_str(p, ", bytes "); p = put_u32(p, st->bytes / f);
  p = put_str(p, ", late frames "); p = put_u32(p, st->late); *p = 0;
  text_at(t20, 6, lh * 8, l);
  bar_line(t20, LCD_Y - lh, "ENTER menu   (stats saved to /ANIMTEST.TXT)", 0);
  lcd_forced_refresh();
  for (;;) { int k = wait_key(); if (k == KEY_ENTER || k == KEY_EXIT) return; }
}

// *selected is kept by the caller, so the cursor stays on the animation just played.
static int pick(int *selected) {
  int sel = *selected;
  if (sel >= n_anims) sel = n_anims ? n_anims - 1 : 0;
  for (;;) {
    lcd_clear_buf();
    lcd_writeClr(t20);
    bar_line(t20, 0, "Animation player", "DM42 .DMA files");
    if (!n_anims) {
      text_at(t20, 6, lh * 2, "No animations found. Put .DMA files in");
      text_at(t20, 6, lh * 3, "/ANIMS on the drive (see tools/gif2dma.py),");
      text_at(t20, 6, lh * 4, "or list their names in /ANIMS/INDEX.TXT.");
    }
    for (int i = 0; i < n_anims; i++) {
      if (i == sel) bar_line(t20, lh * (2 + i), anims[i], 0);
      else text_at(t20, 6, lh * (2 + i), anims[i]);
    }
    bar_line(t20, LCD_Y - lh, "UP/DOWN choose  ENTER play  F1 pause", "EXIT quit");
    lcd_forced_refresh();
    for (;;) {
      int k = poll_key();
      if (k == KEY_EXIT) return -1;
      if (k == KEY_UP && n_anims) { sel = (sel + n_anims - 1) % n_anims; break; }   // wraps to the bottom
      if (k == KEY_DOWN && n_anims) { sel = (sel + 1) % n_anims; break; }           // wraps to the top
      if (k == KEY_ENTER && n_anims) { *selected = sel; return sel; }
      if (is_auto_off() || ST(STAT_PGM_END)) return -1;
      sys_sleep();
    }
  }
}

void program_main() {
  lh = lcd_lineHeight(t20);
  buf = malloc(240 * LINE_REC);
  if (!buf) return;
  int selected = 0;
  for (;;) {
    find_anims();
    int s = pick(&selected);
    if (s < 0) break;
    stats_t st;
    if (play(anims[s], &st)) { log_stats(&st); show_stats(&st); }
    if (quit_req) break;
  }
  free(buf);
}
