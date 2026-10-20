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

// How fast can a native program read the DM42's FAT drive? Reads /TESTDATA.BIN (1 MiB,
// copied there from the PC) in the patterns an animation player would use, shows the
// results, and appends them to /DISKTEST.TXT so they can be read from the PC.
//
//   ENTER  run the tests again      F6  screenshot      EXIT  quit

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

#define MAX_RESULTS 10
static char results[MAX_RESULTS][72];
static int  nres;
static char status[72];

static void add_result(const char *label, uint32_t bytes, uint32_t ms, uint32_t ops) {
  char *p = put_str(results[nres], label);
  p = put_str(p, ": ");
  if (ms == 0) p = put_str(p, "<1 ms");
  else {
    p = put_u32(p, ms); p = put_str(p, " ms");
    if (bytes) {
      uint32_t kbps = bytes * 1000u / 1024u / ms;
      p = put_str(p, ", "); p = put_u32(p, kbps); p = put_str(p, " KB/s");
    }
    if (ops) { p = put_str(p, ", "); p = put_u32(p, (ms * 1000) / ops); p = put_str(p, " us/op"); }
  }
  *p = 0;
  if (nres < MAX_RESULTS - 1) nres++;
}

static void draw(int running) {
  char l[96], *p;
  lcd_clear_buf();
  lcd_writeClr(t20);
  p = put_str(l, usb_powered() ? "USB powered (80 MHz)" : "battery (24 MHz)"); *p = 0;
  bar_line(t20, 0, "Drive read speed", l);
  for (int i = 0; i < nres; i++) text_at(t20, 4, lh * (1 + i), results[i]);
  if (status[0]) text_at(t20, 4, lh * (1 + nres), status);
  bar_line(t20, LCD_Y - lh, running ? "running..." : "ENTER again   F6 screenshot", "EXIT quit");
  lcd_forced_refresh();
}

static FIL fp;
static uint8_t *buf;
static uint32_t buf_size;

static uint32_t rng_state = 12345;
static uint32_t rnd(void) { rng_state ^= rng_state << 13; rng_state ^= rng_state >> 17; rng_state ^= rng_state << 5; return rng_state; }

static int seq_test(const char *label, uint32_t chunk, uint32_t total) {
  UINT got = 0;
  if (chunk > buf_size) return 1;
  if (f_lseek(&fp, 0) != FR_OK) return 0;
  uint32_t done = 0, ops = 0, t0 = sys_current_ms();
  while (done + chunk <= total) {
    if (f_read(&fp, buf, chunk, &got) != FR_OK || got != chunk) { add_result(label, 0, 0, 0); return 0; }
    done += chunk; ops++;
  }
  add_result(label, done, sys_current_ms() - t0, ops);
  return 1;
}

static void run_tests(void) {
  UINT got = 0;
  nres = 0; status[0] = 0;
  draw(1);

  if (f_open(&fp, "/TESTDATA.BIN", FA_READ) != FR_OK) {
    put_str(status, "Cannot open /TESTDATA.BIN (copy it to the drive root)");
    return;
  }
  uint32_t fsize = f_size(&fp);
  if (fsize < 262144) { put_str(status, "TESTDATA.BIN is too small"); f_close(&fp); return; }

  seq_test("seq 512 B x512", 512, 262144);   draw(1);
  seq_test("seq 4 KB x64", 4096, 262144);    draw(1);
  seq_test("seq 12000 B x21", 12000, 252000); draw(1);
  if (buf_size >= 32768) { seq_test("seq 32 KB x8", 32768, 262144); draw(1); }

  {                                          // the player's pattern: a 2-byte header, then the line records
    uint32_t t0 = sys_current_ms(), bytes = 0;
    f_lseek(&fp, 0);
    for (int i = 0; i < 40; i++) {
      f_read(&fp, buf, 2, &got); bytes += got;
      f_read(&fp, buf, 6000, &got); bytes += got;
    }
    add_result("hdr 2 + 6000 B x40", bytes, sys_current_ms() - t0, 40);
    draw(1);
  }
  {                                          // jumping around: seek then read a frame-sized block
    uint32_t t0 = sys_current_ms(), bytes = 0;
    for (int i = 0; i < 20; i++) {
      uint32_t off = (rnd() % ((fsize - 12000) / 512)) * 512;
      f_lseek(&fp, off);
      f_read(&fp, buf, 12000, &got); bytes += got;
    }
    add_result("random seek+12000 B x20", bytes, sys_current_ms() - t0, 20);
    draw(1);
  }
  f_close(&fp);
  {                                          // cost of opening and closing a file
    uint32_t t0 = sys_current_ms();
    for (int i = 0; i < 20; i++) {
      if (f_open(&fp, "/TESTDATA.BIN", FA_READ) == FR_OK) f_close(&fp);
    }
    add_result("open+close x20", 0, sys_current_ms() - t0, 20);
  }
}

static void save_results(void) {
  char line[100];
  int prev = sys_is_disk_write_enable();
  sys_disk_write_enable(1);
  if (f_open(&fp, "/DISKTEST.TXT", FA_WRITE | FA_OPEN_APPEND) == FR_OK) {
    char *p = put_str(line, usb_powered() ? "== USB powered, 80 MHz" : "== battery, 24 MHz");
    p = put_str(p, ", buffer "); p = put_u32(p, buf_size); p = put_str(p, " bytes\r\n"); *p = 0;
    UINT w;
    f_write(&fp, line, (UINT)(p - line), &w);
    for (int i = 0; i < nres; i++) {
      char *q = put_str(line, results[i]); *q++ = '\r'; *q++ = '\n'; *q = 0;
      f_write(&fp, line, (UINT)(q - line), &w);
    }
    f_close(&fp);
    put_str(status, "results appended to /DISKTEST.TXT");
  } else {
    put_str(status, "could not write /DISKTEST.TXT");
  }
  sys_disk_write_enable(prev);
}

void program_main() {
  lh = lcd_lineHeight(t20);
  buf_size = 32768;
  buf = malloc(buf_size);
  if (!buf) { buf_size = 16384; buf = malloc(buf_size); }
  if (!buf) { buf_size = 12288; buf = malloc(buf_size); }
  if (!buf) return;

  run_tests();
  save_results();
  draw(0);

  for (;;) {
    reset_auto_off();
    if (key_empty()) { sys_sleep(); continue; }
    int k = key_pop();
    if (k == KEY_EXIT) break;
    if (k == KEY_F6) create_screenshot(1);
    if (k == KEY_ENTER) { run_tests(); save_results(); draw(0); }
  }
  free(buf);
}
