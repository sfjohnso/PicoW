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

// Experiment: what can a native program do with the DM42's USB serial port?
//
//   F1  usb_acm_on()        switch the USB port to a virtual serial port
//   F2  send one line       acm_puts("line N")
//   F3  toggle sending a numbered line every second
//   F4  usb_turn_off()
//   F5  burst: 100 numbered 32-character lines, to look for lost data
//   F6  QUIET TEST: hands off for 12 s; the PC sends all 256 byte values meanwhile and
//       the screen reports how many key events appeared (0 = no receive path)
//   EXIT  turns USB off again and leaves
//
// Every key code the system delivers is logged on screen, so we can see whether
// bytes sent from the PC show up as key events (the API has no receive function).

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
static char *put_int(char *p, int v) { if (v < 0) { *p++ = '-'; v = -v; } return put_u32(p, (uint32_t)v); }

#define LOG_N 14
static int      keylog[LOG_N];
static uint32_t nkeys;
static uint32_t sent_calls, sent_bytes;
static int      auto_tx;
static int      quiet_state;                 // 0 idle, 1 running, 2 finished
static uint32_t quiet_t0, quiet_events;

static void send_line(const char *prefix, uint32_t n) {
  char b[48], *p = put_str(b, prefix);
  p = put_u32(p, n);
  *p++ = '\r'; *p++ = '\n'; *p = 0;
  acm_puts(b);
  sent_calls++;
  sent_bytes += (uint32_t)(p - b);
}

static void draw(uint32_t elapsed_ms) {
  char l[96], *p;
  lcd_clear_buf();
  lcd_writeClr(t20);
  bar_line(t20, 0, "USB serial experiment", "DM42");

  p = put_str(l, "usb_powered ");  p = put_int(p, usb_powered());
  p = put_str(p, "   usb_is_on ");  p = put_int(p, usb_is_on());
  p = put_str(p, "   disk_ok ");    p = put_int(p, sys_disk_ok()); *p = 0;
  text_at(t20, 6, lh * 1, l);

  p = put_str(l, "acm_puts calls "); p = put_u32(p, sent_calls);
  p = put_str(p, "  bytes "); p = put_u32(p, sent_bytes);
  p = put_str(p, auto_tx ? "  AUTO ON" : "  auto off"); *p = 0;
  text_at(t20, 6, lh * 2, l);

  p = put_str(l, "key events seen: "); p = put_u32(p, nkeys);
  p = put_str(p, "   up "); p = put_u32(p, elapsed_ms / 1000); p = put_str(p, " s"); *p = 0;
  text_at(t20, 6, lh * 3, l);

  p = put_str(l, "last codes: ");
  uint32_t shown = nkeys < LOG_N ? nkeys : LOG_N;
  for (uint32_t i = 0; i < shown; i++) {
    p = put_int(p, keylog[(nkeys - shown + i) % LOG_N]);
    *p++ = ' ';
  }
  *p = 0;
  text_at(t20, 6, lh * 4, l);

  if (quiet_state == 1) {
    p = put_str(l, "QUIET TEST running: hands off!  "); p = put_u32(p, (quiet_t0 + 12000 - elapsed_ms) / 1000 + 1); *p = 0;
    text_at(t20, 6, lh * 6, l);
  } else if (quiet_state == 2) {
    p = put_str(l, "QUIET RESULT: "); p = put_u32(p, quiet_events); p = put_str(p, " key events in 10 s"); *p = 0;
    text_at(t20, 6, lh * 6, l);
    text_at(t20, 6, lh * 7, quiet_events ? "-> something arrived from the PC!" : "-> nothing arrived from the PC");
  } else {
    text_at(t20, 6, lh * 6, "F1 ACM on   F2 send   F3 auto   F4 off");
    text_at(t20, 6, lh * 7, "F5 burst   F6 QUIET TEST (do F1 first)");
  }
  bar_line(t20, LCD_Y - lh, "EXIT: USB off and quit", 0);
  lcd_forced_refresh();
}

void program_main() {
  lh = lcd_lineHeight(t20);
  uint32_t t0 = sys_current_ms(), last_draw = 0, last_tx = 0, tick = 0;
  int dirty = 1;

  for (;;) {
    reset_auto_off();
    while (!key_empty()) {
      int k = key_pop();
      keylog[nkeys % LOG_N] = k; nkeys++;
      dirty = 1;
      if (quiet_state == 1 && sys_current_ms() - t0 >= quiet_t0 + 1000) quiet_events++;   // ignore the first second
      if (k == KEY_EXIT) { usb_turn_off(); return; }
      if (k == KEY_F1) usb_acm_on();
      if (k == KEY_F2) send_line("manual line ", ++tick);
      if (k == KEY_F3) auto_tx = !auto_tx;
      if (k == KEY_F4) usb_turn_off();
      if (k == KEY_F6) {
        quiet_state = 1; quiet_events = 0; quiet_t0 = sys_current_ms() - t0;
        acm_puts("QUIET-START\r\n"); sent_calls++; sent_bytes += 14;
      }
      if (k == KEY_F5) {
        for (uint32_t i = 0; i < 100; i++) {
          char b[48], *p = put_str(b, "burst ");
          p = put_u32(p, i);
          while (p - b < 30) *p++ = '.';
          *p++ = '\r'; *p++ = '\n'; *p = 0;
          acm_puts(b);
          sent_calls++; sent_bytes += (uint32_t)(p - b);
        }
      }
    }
    uint32_t now = sys_current_ms() - t0;
    if (quiet_state == 1 && now >= quiet_t0 + 11000) { quiet_state = 2; dirty = 1; acm_puts("QUIET-END\r\n"); sent_calls++; sent_bytes += 12; }
    if (auto_tx && now - last_tx >= 1000) { last_tx = now; send_line("auto tick ", ++tick); dirty = 1; }
    if (dirty || now - last_draw >= 500) { draw(now); last_draw = now; dirty = 0; }
  }
}
