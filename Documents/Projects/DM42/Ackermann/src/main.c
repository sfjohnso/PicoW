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
#include "ack_engine.h"
#include "ack_math.h"

// The Ackermann function on the DM42, three ways:
//   brute force  the assembly engine in ack.s, one call of the recursion per step,
//                with the stack kept run-length encoded (A(4,1) is 65,531 deep
//                but never more than 4 runs)
//   memo         ack_math.c: remember how far each m level has climbed; the brute
//                force step count is carried along so it can still be shown
//   formula      closed forms for m <= 3 and the known values for m = 4, 5
// All three agree, and the program checks that at start-up.

typedef uint64_t u64;

enum { MODE_BRUTE, MODE_MEMO, MODE_FORMULA, NMODES };
static const char *const mode_name[NMODES] = { "Brute force (asm)", "Memoized chain (C)", "Closed form" };

// ---- text formatting (no 64-bit division: libgcc is not linked) -------------

static char *put_str(char *p, const char *s) {
  while (*s) *p++ = *s++;
  return p;
}

static char *put_u32(char *p, uint32_t v) {
  char tmp[11];
  int n = 0;
  do { tmp[n++] = '0' + v % 10; v /= 10; } while (v);
  while (n) *p++ = tmp[--n];
  return p;
}

static char *put_tenths(char *p, uint32_t v10) {
  p = put_u32(p, v10 / 10);
  *p++ = '.';
  return put_u32(p, v10 % 10);
}

static const u64 P10[20] = {
  1ULL, 10ULL, 100ULL, 1000ULL, 10000ULL, 100000ULL, 1000000ULL, 10000000ULL,
  100000000ULL, 1000000000ULL, 10000000000ULL, 100000000000ULL, 1000000000000ULL,
  10000000000000ULL, 100000000000000ULL, 1000000000000000ULL, 10000000000000000ULL,
  100000000000000000ULL, 1000000000000000000ULL, 10000000000000000000ULL
};

// Decimal with thousands separators, by repeated subtraction.
static char *put_u64(char *p, u64 v) {
  char d[20];
  for (int i = 19, k = 0; i >= 0; i--, k++) {
    int c = 0;
    while (v >= P10[i]) { v -= P10[i]; c++; }
    d[k] = '0' + c;
  }
  int s = 0;
  while (s < 19 && d[s] == '0') s++;
  int len = 20 - s;
  for (int i = 0; i < len; i++) {
    *p++ = d[s + i];
    int rem = len - 1 - i;
    if (rem && rem % 3 == 0) *p++ = ',';
  }
  return p;
}

static float u64f(u64 v) {
  return (float)(uint32_t)(v >> 32) * 4294967296.0f + (float)(uint32_t)v;
}

static char *put_duration(char *p, float s) {
  if (s < 0.001f) return put_str(p, "under 1 ms");
  if (s < 1.0f)   { p = put_u32(p, (uint32_t)(s * 1000.0f)); return put_str(p, " ms"); }
  if (s < 100.0f) { p = put_tenths(p, (uint32_t)(s * 10.0f)); return put_str(p, " s"); }
  if (s > 3.0e9f) return put_str(p, "over 100 years");
  uint32_t t = (uint32_t)s;
  if (t < 7200)   { p = put_u32(p, t / 60); p = put_str(p, " min "); p = put_u32(p, t % 60); return put_str(p, " s"); }
  if (t < 172800) { p = put_u32(p, t / 3600); p = put_str(p, " h "); p = put_u32(p, t % 3600 / 60); return put_str(p, " min"); }
  if (t < 63072000) { p = put_u32(p, t / 86400); return put_str(p, " days"); }
  p = put_u32(p, t / 31536000);
  return put_str(p, " years");
}

// ---- keys -------------------------------------------------------------

// Next key press, or -1 if none is waiting (key releases are discarded).
static int poll_key(void) {
  while (!key_empty()) {
    int k = key_pop();
    if (k == KEY_SCREENSHOT || k == KEY_F6) { create_screenshot(1); continue; }   // BMP into /SCREENS
    if (k > 0) return k;
  }
  return -1;
}

// Set when the system's inactivity auto-off fires while we are idle: the program
// then ends, so the DMCP menu can power the calculator off properly.
static int quit_req;

static int wait_key(void) {
  for (;;) {
    int k = poll_key();
    if (k > 0) return k;
    if (is_auto_off() || ST(STAT_PGM_END)) { quit_req = 1; return KEY_EXIT; }
    sys_sleep();
  }
}

static int key_digit(int k) {
  switch (k) {
  case KEY_0: return 0; case KEY_1: return 1; case KEY_2: return 2; case KEY_3: return 3;
  case KEY_4: return 4; case KEY_5: return 5; case KEY_6: return 6; case KEY_7: return 7;
  case KEY_8: return 8; case KEY_9: return 9;
  default: return -1;
  }
}

static int memo_poll(void) { reset_auto_off(); return poll_key() == KEY_EXIT; }

// ---- self-test and speed calibration -----------------------------------

#define MAXRUNS 64
static uint8_t  run_vals[MAXRUNS];
static uint32_t run_cnts[MAXRUNS];

static void state_init(ack_state_t *s, uint32_t m, uint32_t n) {
  s->m = m; s->n = n;
  s->vals = run_vals; s->cnts = run_cnts;
  s->nruns = 0; s->topval = 0; s->topcnt = 0;
  s->depth = 0; s->peak = 0; s->maxruns = MAXRUNS;
  s->steps_lo = 0; s->steps_hi = 0;
}

static u64 state_steps(const ack_state_t *s) { return ((u64)s->steps_hi << 32) | s->steps_lo; }

typedef struct { uint32_t m, n, value; u64 steps; } known_t;
static const known_t known[] = {
  {0, 5, 6, 1}, {1, 5, 7, 12}, {2, 0, 3, 5}, {2, 3, 9, 44}, {2, 5, 13, 90}, {3, 0, 5, 15},
  {3, 2, 29, 541}, {3, 3, 61, 2432}, {3, 4, 125, 10307}, {3, 6, 509, 172233}, {4, 0, 13, 107}
};

static int     self_ok;
static float   rate_asm, rate_memo;        // steps per second, measured

static void self_test(void) {
  self_ok = 1;
  for (unsigned i = 0; i < sizeof known / sizeof known[0]; i++) {
    const known_t *k = &known[i];
    ack_state_t s; state_init(&s, k->m, k->n);
    if (ack_run(&s, 0xFFFFFFFFu) != ACK_DONE || s.n != k->value || state_steps(&s) != k->steps) self_ok = 0;
    uint32_t fv; u64 bs;
    if (ack_formula(k->m, k->n, &fv) || fv != k->value) self_ok = 0;
    if (ack_bf_steps(k->m, k->n, &bs) || bs != k->steps) self_ok = 0;
    memo_result_t mr;
    if (ack_memo(k->m, k->n, &mr, 0) != MEMO_OK || mr.value != k->value || mr.bf_steps != k->steps) self_ok = 0;
  }
  // Speed: brute force on A(3,8) (2,785,999 steps), memo on A(3,13) (98,313 steps).
  ack_state_t s; state_init(&s, 3, 8);
  uint32_t t = sys_current_ms();
  ack_run(&s, 0xFFFFFFFFu);
  uint32_t ms = sys_current_ms() - t; if (!ms) ms = 1;
  rate_asm = u64f(state_steps(&s)) * 1000.0f / (float)ms;

  memo_result_t mr;
  t = sys_current_ms();
  ack_memo(3, 13, &mr, 0);
  ms = sys_current_ms() - t; if (!ms) ms = 1;
  rate_memo = u64f(mr.real_steps) * 1000.0f / (float)ms;
}

// ---- results ------------------------------------------------------------

enum { RES_OK, RES_TOOBIG, RES_ABORT, RES_ERROR };

typedef struct {
  int      kind, mode;
  uint32_t m, n, value, ms;
  u64      bf_steps, real_steps;
  int      agrees;               // value matches the closed form
} res_t;

// ---- drawing helpers ----------------------------------------------------
//
// Two fonts: t24 (25 pixels per row, 11 wide) for titles and results, t20 for
// everything else. Positioned text uses lnfill = 0 so it does not clear the
// row it sits on (the same trick SwissMicros' SDKdemo uses).

static int lh24, lh20;

static void text_at(disp_stat_t *ds, int x, int y, const char *s) {
  uint8_t ln = ds->lnfill, nl = ds->newln;
  ds->lnfill = 0; ds->newln = 0;
  lcd_setXY(ds, x, y);
  lcd_puts(ds, s);
  ds->lnfill = ln; ds->newln = nl;
}

static int text_width(disp_stat_t *ds, const char *s) { return lcd_textWidth(ds, s); }

// A reverse-video line across the whole screen at pixel row y, with `left`
// at the left edge and `right` (if any) pushed against the right edge.
static void bar_line(disp_stat_t *ds, int y, const char *left, const char *right) {
  char b[80];
  int cw = lcd_textWidth(ds, "0");              // lcd_fontWidth() reports a wider value than the real advance
  int n = LCD_X / (cw ? cw : 10);
  if (n > 78) n = 78;
  for (int i = 0; i < n; i++) b[i] = ' ';
  b[n] = 0;
  int ll = 0; while (left[ll] && ll < n) { b[ll] = left[ll]; ll++; }
  if (right) {
    int rl = 0; while (right[rl]) rl++;
    if (rl + ll + 1 <= n) for (int i = 0; i < rl; i++) b[n - rl + i] = right[i];
  }
  uint8_t iv = ds->inv, nl = ds->newln;
  ds->inv = 1; ds->newln = 0;
  lcd_setXY(ds, 0, y);
  lcd_puts(ds, b);
  ds->inv = iv; ds->newln = nl;
}

static void top_bar(const char *left, const char *right) { bar_line(t24, 0, left, right); }
static void bottom_bar(const char *left, const char *right) { bar_line(t20, LCD_Y - lh20, left, right); }

static void frame(int x, int y, int w, int h) {
  lcd_fill_rect(x, y, w, 1, 1);
  lcd_fill_rect(x, y + h - 1, w, 1, 1);
  lcd_fill_rect(x, y, 1, h, 1);
  lcd_fill_rect(x + w - 1, y, 1, h, 1);
}

static void fill_by_value(int x, int y, int w, int h, uint32_t v) {
  if (w <= 0) return;
  if (v == 0)      lcd_fill_rect(x, y, w, h, 1);
  else if (v == 1) lcd_fill_ptrn(x, y, w, h, 0xAA, 0x55);
  else if (v == 2) lcd_fill_ptrn(x, y, w, h, 0x88, 0x22);
  else             lcd_fill_ptrn(x, y, w, h, 0xEE, 0xBB);
}

// A value in a box; the active one gets a heavy border.
static void field_box(int x, int y, const char *label, uint32_t v, int w, int active) {
  char b[16]; *put_u32(b, v) = 0;
  text_at(t24, x, y + 2, label);
  int bx = x + 24;
  frame(bx, y, w, 29);
  if (active) { frame(bx + 1, y + 1, w - 2, 27); frame(bx + 2, y + 2, w - 4, 25); }
  text_at(t24, bx + (w - text_width(t24, b)) / 2, y + 2, b);
}

// ---- setup screen -----------------------------------------------------

static void draw_setup(uint32_t m, uint32_t n, int field, int mode) {
  char l[64], *p;
  uint32_t fv = 0, peak = 0; u64 bs = 0;
  int big = ack_formula(m, n, &fv);
  if (!big) { ack_bf_steps(m, n, &bs); ack_peak_depth(m, n, &peak); }

  lcd_clear_buf();
  lcd_writeClr(t24);
  top_bar("Ackermann function", self_ok ? "self-test OK" : "SELF-TEST FAILED");

  field_box(10, 32, "m", m, 44, field == 0);
  field_box(100, 32, "n", n, 118, field == 1);

  p = put_str(l, "Mode: "); p = put_str(p, mode_name[mode]); *p = 0;
  text_at(t20, 10, 68, l);

  p = put_str(l, "A("); p = put_u32(p, m); *p++ = ','; p = put_u32(p, n); p = put_str(p, ")");
  if (big) p = put_str(p, " is beyond 32 bits");
  else { p = put_str(p, " = "); p = put_u64(p, fv); }
  *p = 0;
  text_at(t24, 10, 68 + lh20 + 4, l);

  int y = 68 + lh20 + 4 + lh24 + 6;
  if (!big) {
    p = put_u64(l, bs); p = put_str(p, " steps by brute force"); *p = 0;
    text_at(t20, 10, y, l);

    if (mode == MODE_BRUTE) {
      p = put_str(l, "est. "); p = put_duration(p, u64f(bs) / (rate_asm * 0.9f));
      p = put_str(p, " at "); p = put_tenths(p, (uint32_t)(rate_asm / 100000.0f)); p = put_str(p, "M steps/s");
      *p = 0;
    } else if (mode == MODE_MEMO) {
      u64 est = ack_memo_estimate(m, n);
      if (est) { p = put_str(l, "est. "); p = put_u64(p, est); p = put_str(p, " steps, "); p = put_duration(p, u64f(est) / rate_memo); *p = 0; }
      else { p = put_str(l, "memoized: too slow here"); *p = 0; }
    } else {
      p = put_str(l, "closed form: instant"); *p = 0;
    }
    text_at(t20, 10, y + lh20, l);

    p = put_str(l, "stack up to "); p = put_u64(p, peak); p = put_str(p, " deep"); *p = 0;
    text_at(t20, 10, y + 2 * lh20, l);
  }
  bottom_bar("ENTER run  F1 mode  F2 table", "EXIT quit");
  lcd_refresh();
}

// ---- running screen (brute force) ---------------------------------------

#define NB 200
static uint32_t graph[NB];

static void draw_run(const ack_state_t *s, uint32_t m0, uint32_t n0, float frac, uint32_t elapsed_ms,
                     uint32_t slice_ms, uint32_t slice_budget, int paused) {
  char l[64], r[32], *p;
  const int bx = 10, bw = 380;

  lcd_clear_buf();
  lcd_writeClr(t24);

  p = put_str(l, "A("); p = put_u32(p, m0); *p++ = ','; p = put_u32(p, n0); *p++ = ')'; *p = 0;
  p = put_str(r, paused ? "PAUSED " : "running ");
  p = put_u32(p, (uint32_t)(frac * 100.0f)); p = put_str(p, "%"); *p = 0;
  top_bar(l, r);

  int y = lh24 + 3;
  p = put_str(l, "steps "); p = put_u64(p, state_steps(s)); *p = 0;
  text_at(t20, 10, y, l);
  p = put_str(l, "depth "); p = put_u64(p, s->depth); p = put_str(p, "   peak "); p = put_u64(p, s->peak); *p = 0;
  text_at(t20, 10, y + lh20, l);
  p = put_str(l, "m="); p = put_u32(p, s->m); p = put_str(p, " n="); p = put_u32(p, s->n);
  if (slice_ms) {
    p = put_str(p, "   ");
    p = put_tenths(p, (uint32_t)((float)slice_budget / (float)slice_ms / 100.0f));
    p = put_str(p, "M steps/s");
  }
  *p = 0;
  text_at(t20, 10, y + 2 * lh20, l);

  // progress bar
  int y0 = y + 3 * lh20 + 4;
  frame(bx, y0, bw, 9);
  lcd_fill_rect(bx, y0, (uint32_t)(frac * (float)bw), 9, 1);

  // the stack as runs, drawn to scale against the peak depth, with labels
  int y1 = y0 + 14, hh = 14;
  frame(bx, y1, bw, hh);
  uint32_t peak = s->peak ? s->peak : 1;
  int x = bx, xlast = bx;
  uint32_t vfirst = 0, cfirst = 0, vlast = 0, clast = 0;
  for (uint32_t i = 0; i < s->nruns; i++) {
    uint32_t v = (i + 1 == s->nruns) ? s->topval : s->vals[i];
    uint32_t c = (i + 1 == s->nruns) ? s->topcnt : s->cnts[i];
    int w = (int)(c * (uint32_t)bw / peak);
    if (x + w > bx + bw) w = bx + bw - x;
    fill_by_value(x, y1 + 1, w, hh - 2, v);
    if (i == 0) { vfirst = v; cfirst = c; }
    xlast = x; vlast = v; clast = c;
    x += w;
  }
  int ly = y1 + hh + 2;
  if (s->nruns) {
    p = put_u32(l, vfirst); p = put_str(p, " x"); p = put_u64(p, cfirst); *p = 0;
    text_at(t20, bx, ly, l);
    if (s->nruns > 1) {
      int minx = bx + text_width(t20, l) + 12;
      p = put_u32(l, vlast); p = put_str(p, " x"); p = put_u64(p, clast); *p = 0;
      text_at(t20, xlast > minx ? xlast : minx, ly, l);
    }
  }

  // depth over the course of the run
  int gt = ly + lh20 + 4, gb = LCD_Y - lh20 - 6, gh = gb - gt;
  uint32_t gmax = 1;
  for (int i = 0; i < NB; i++) if (graph[i] > gmax) gmax = graph[i];
  lcd_fill_rect(bx, gb, bw, 1, 1);
  for (int i = 0; i < NB; i++) {
    uint32_t h = graph[i] * (uint32_t)gh / gmax;
    if (h) lcd_fill_rect(bx + i * 19 / 10, gb - h, 2, h, 1);
  }
  int cx = bx + (int)(frac * (float)(bw - 1));
  for (int yy = gt; yy < gb; yy += 3) lcd_fill_rect(cx, yy, 1, 2, 1);

  p = put_str(r, "time "); p = put_duration(p, (float)elapsed_ms / 1000.0f); *p = 0;
  bottom_bar(paused ? "F1 resume  EXIT stop" : "F1 pause  EXIT stop", r);
  lcd_forced_refresh();
}

static void run_brute(uint32_t m, uint32_t n, res_t *r) {
  u64 total; ack_bf_steps(m, n, &total);
  float total_f = u64f(total);
  ack_state_t s; state_init(&s, m, n);
  for (int i = 0; i < NB; i++) graph[i] = 0;

  uint32_t t_start = sys_current_ms(), paused_ms = 0, budget = 200000, last_ms = 0, last_budget = 0;
  int rc;
  r->kind = RES_OK;
  for (;;) {
    uint32_t t0 = sys_current_ms();
    rc = ack_run(&s, budget);
    last_ms = sys_current_ms() - t0; last_budget = budget;

    float frac = u64f(state_steps(&s)) / total_f;
    if (frac > 1.0f) frac = 1.0f;
    int idx = (int)(frac * 199.99f);
    if (s.depth > graph[idx]) graph[idx] = s.depth;
    if (rc != ACK_MORE) break;
    reset_auto_off();                      // a long run is activity, not idleness

    draw_run(&s, m, n, frac, sys_current_ms() - t_start - paused_ms, last_ms, last_budget, 0);

    int k;
    while ((k = poll_key()) > 0) {
      if (k == KEY_EXIT) { r->kind = RES_ABORT; return; }
      if (k == KEY_F1) {
        uint32_t tp = sys_current_ms();
        draw_run(&s, m, n, frac, tp - t_start - paused_ms, last_ms, last_budget, 1);
        for (;;) {
          int k2 = wait_key();
          if (k2 == KEY_F1) break;
          if (k2 == KEY_EXIT) { r->kind = RES_ABORT; return; }
        }
        paused_ms += sys_current_ms() - tp;
      }
    }
    // aim for slices of roughly 300 ms: the redraw costs ~30 ms, so about 9% overhead
    if (last_ms < 50) budget = budget > 16000000 ? 32000000 : budget * 2;
    else {
      budget = (uint32_t)((float)budget * 300.0f / (float)last_ms);
      if (budget < 1000) budget = 1000;
      if (budget > 32000000) budget = 32000000;
    }
  }
  r->ms = sys_current_ms() - t_start - paused_ms;
  r->real_steps = state_steps(&s);
  r->bf_steps = r->real_steps;
  r->value = s.n;
  r->kind = rc == ACK_DONE ? RES_OK : RES_ERROR;
}

static void run_memo(uint32_t m, uint32_t n, res_t *r) {
  lcd_clear_buf();
  lcd_writeClr(t24);
  top_bar("Ackermann function", 0);
  text_at(t24, 10, 60, "Working, memoized...");
  bottom_bar("EXIT stops", 0);
  lcd_refresh();
  uint32_t t0 = sys_current_ms();
  memo_result_t mr;
  int rc = ack_memo(m, n, &mr, memo_poll);
  r->ms = sys_current_ms() - t0;
  if (rc == MEMO_ABORTED) { r->kind = RES_ABORT; return; }
  if (rc != MEMO_OK)      { r->kind = RES_TOOBIG; return; }
  r->kind = RES_OK;
  r->value = mr.value; r->bf_steps = mr.bf_steps; r->real_steps = mr.real_steps;
}

static void run_formula(uint32_t m, uint32_t n, res_t *r) {
  uint32_t t0 = sys_current_ms();
  uint32_t v; u64 bs;
  if (ack_formula(m, n, &v) || ack_bf_steps(m, n, &bs)) { r->kind = RES_TOOBIG; return; }
  r->ms = sys_current_ms() - t0;
  r->kind = RES_OK; r->value = v; r->bf_steps = bs; r->real_steps = 1;
}

// ---- result screens -----------------------------------------------------

static void show_toobig(uint32_t m, uint32_t n) {
  char l[64], *p;
  p = put_str(l, "A("); p = put_u32(p, m); *p++ = ','; p = put_u32(p, n); *p++ = ')'; *p = 0;
  top_bar(l, "too big");
  int y = lh24 + 8;
  text_at(t20, 10, y, "The value does not fit in 32 bits.");
  y += lh20 + 6;
  if (m == 3) {
    p = put_str(l, "= 2^"); p = put_u32(p, n + 3); p = put_str(p, " - 3"); *p = 0;
    text_at(t24, 10, y, l);
    p = put_str(l, "about "); p = put_u32(p, (n + 3) * 30103u / 100000u + 1); p = put_str(p, " digits"); *p = 0;
    text_at(t20, 10, y + lh24 + 4, l);
  } else if (m == 4 && n == 2) {
    text_at(t24, 10, y, "= 2^65536 - 3");
    text_at(t20, 10, y + lh24 + 4, "19,729 digits");
    text_at(t20, 10, y + lh24 + 4 + lh20, "begins 20035299304068...");
    text_at(t20, 10, y + lh24 + 4 + 2 * lh20, "ends   ...905719156733");
  } else if (m == 4) {
    p = put_str(l, "= 2^^"); p = put_u32(p, n + 3); p = put_str(p, " - 3"); *p = 0;
    text_at(t24, 10, y, l);
    p = put_str(l, "a tower of "); p = put_u32(p, n + 3); p = put_str(p, " twos"); *p = 0;
    text_at(t20, 10, y + lh24 + 4, l);
  } else if (m == 5) {
    text_at(t24, 10, y, "= 2^^65536 - 3 and up");
    text_at(t20, 10, y + lh24 + 4, "(A(5,1) = A(4,65533))");
  } else {
    text_at(t24, 10, y, "beyond any notation");
    text_at(t20, 10, y + lh24 + 4, "that fits on a screen");
  }
  bottom_bar("any key: back", 0);
}

static void show_result(const res_t *r) {
  char l[64], t[32], *p;
  lcd_clear_buf();
  lcd_writeClr(t24);

  if (r->kind == RES_TOOBIG) { show_toobig(r->m, r->n); lcd_refresh(); wait_key(); return; }
  if (r->kind == RES_ERROR) {
    top_bar("Ackermann function", "error");
    text_at(t20, 10, lh24 + 8, "The engine stopped: stack or value limit.");
    bottom_bar("any key: back", 0);
    lcd_refresh(); wait_key(); return;
  }

  p = put_str(l, "A("); p = put_u32(p, r->m); *p++ = ','; p = put_u32(p, r->n);
  p = put_str(p, ") = "); p = put_u64(p, r->value); *p = 0;
  top_bar(l, 0);

  int y = lh24 + 6;
  text_at(t20, 10, y, mode_name[r->mode]);
  p = put_str(l, "steps taken  "); p = put_u64(p, r->real_steps); *p = 0;
  text_at(t20, 10, y + 2 * lh20, l);
  p = put_str(l, "brute force  "); p = put_u64(p, r->bf_steps); *p = 0;
  text_at(t20, 10, y + 3 * lh20, l);
  if (r->mode != MODE_BRUTE && r->real_steps) {
    p = put_u64(l, (u64)(u64f(r->bf_steps) / u64f(r->real_steps)));
    p = put_str(p, "x fewer steps"); *p = 0;
    text_at(t24, 10, y + 4 * lh20 + 6, l);
  }
  p = put_str(l, "time  "); p = put_duration(p, (float)r->ms / 1000.0f); *p = 0;
  text_at(t20, 10, y + 4 * lh20 + lh24 + 12, l);
  p = put_str(t, r->agrees ? "closed form agrees" : "CLOSED FORM DISAGREES");
  bottom_bar("any key: back", t);
  lcd_refresh();
  wait_key();
}

static void show_table(void) {
  lcd_clear_buf();
  lcd_writeClr(t24);
  top_bar("Ackermann table, n = 0..4", 0);
  lcd_putsAt(t24, 1, "m=0:   1   2   3   4   5");
  lcd_putsAt(t24, 2, "m=1:   2   3   4   5   6");
  lcd_putsAt(t24, 3, "m=2:   3   5   7   9  11");
  lcd_putsAt(t24, 4, "m=3:   5  13  29  61 125");
  lcd_putsAt(t24, 5, "m=4:  13  65,533  2^65536-3");
  lcd_putsAt(t24, 6, "      then 2^^(n+3) - 3");
  lcd_putsAt(t24, 7, "m=5:  65,533 then 2^^65536-3..");
  bottom_bar("any key: back", 0);
  lcd_refresh();
  wait_key();
}

static void do_run(uint32_t m, uint32_t n, int mode) {
  res_t r;
  uint32_t fv = 0;
  r.kind = RES_OK; r.mode = mode; r.m = m; r.n = n; r.value = 0; r.ms = 0;
  r.bf_steps = r.real_steps = 0; r.agrees = 0;

  if (ack_formula(m, n, &fv)) { r.kind = RES_TOOBIG; show_result(&r); return; }

  if (mode == MODE_BRUTE) run_brute(m, n, &r);
  else if (mode == MODE_MEMO) run_memo(m, n, &r);
  else run_formula(m, n, &r);

  if (r.kind == RES_ABORT) return;
  r.mode = mode; r.m = m; r.n = n;
  r.agrees = (r.kind == RES_OK && r.value == fv);
  show_result(&r);
}

// ---- main ---------------------------------------------------------------

void program_main() {
  uint32_t m = 3, n = 6;
  int field = 0, mode = MODE_BRUTE;

  lh24 = lcd_lineHeight(t24);
  lh20 = lcd_lineHeight(t20);

  lcd_clear_buf();
  lcd_writeClr(t24);
  top_bar("Ackermann function", 0);
  text_at(t24, 10, 60, "Self-test and timing...");
  lcd_refresh();
  self_test();

  for (;;) {
    if (quit_req) break;
    draw_setup(m, n, field, mode);
    int k = wait_key();
    int d = key_digit(k);
    if (d >= 0) {
      if (field == 0) m = d;
      else if (n < 100000) n = n * 10 + d;
      else n = d;
    } else if (k == KEY_BSP) {
      if (field == 0) m = 0; else n /= 10;
    } else if (k == KEY_UP || k == KEY_DOWN) {
      field = !field;
    } else if (k == KEY_F1) {
      mode = (mode + 1) % NMODES;
    } else if (k == KEY_F2) {
      show_table();
    } else if (k == KEY_ENTER) {
      do_run(m, n, mode);
    } else if (k == KEY_EXIT) {
      break;
    }
  }
}
