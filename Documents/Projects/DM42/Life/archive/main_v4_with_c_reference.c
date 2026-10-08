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

// Game of Life on a 200x120 torus, drawn as 2x2-pixel cells on the 400x240 LCD.
// The generation step is in life.s (see its header for the grid layout).
// A run ends when the user presses EXIT or when the field repeats itself with
// period 1 or 2 (still lifes, blinkers, ...) or its population count settles
// into a short cycle.

#define W      200
#define H      120
#define DBYTES 25                // data bytes per row (W/8)
#define GS     27                // stored row: left copy + 25 data + right copy
#define GROWS  (H + 2)           // plus a copy of the last row above, first row below
#define GBYTES (GS * GROWS)

#define ROW(g, y) ((g) + ((y) + 1) * GS + 1)    // first data byte of Life row y

extern void life_step(const uint8_t *src, uint8_t *dst);
extern void life_fix_edges(uint8_t *grid);

static uint32_t rng_state;

static uint32_t rng(void) {      // xorshift32
  uint32_t x = rng_state;
  x ^= x << 13;
  x ^= x >> 17;
  x ^= x << 5;
  return rng_state = x;
}

// ~37% live cells
static void randomize(uint8_t *g) {
  rng_state = (sys_current_ms() ^ get_rtc_ticks() ^ 0x9E3779B9u) | 1;
  for (int y = 0; y < H; y++) {
    uint8_t *row = ROW(g, y);
    for (int j = 0; j < DBYTES; j++) {
      uint32_t a = rng(), b = rng(), c = rng();
      row[j] = (uint8_t)((a & (b | c)) >> 8);
    }
  }
  life_fix_edges(g);
}

static int same(const uint8_t *a, const uint8_t *b) {
  for (int i = 0; i < GBYTES; i++)
    if (a[i] != b[i]) return 0;
  return 1;
}

static uint32_t popcount32(uint32_t v) {
  v = v - ((v >> 1) & 0x55555555u);
  v = (v & 0x33333333u) + ((v >> 2) & 0x33333333u);
  v = (v + (v >> 4)) & 0x0F0F0F0Fu;
  return (v * 0x01010101u) >> 24;
}

static uint32_t population(const uint8_t *g) {
  uint32_t n = 0;
  for (int y = 0; y < H; y++) {
    const uint8_t *row = ROW(g, y);
    for (int j = 0; j < DBYTES; j++) n += popcount32(row[j]);
  }
  return n;
}

// Number of cells that differ between two grids.
static uint32_t distance(const uint8_t *a, const uint8_t *b) {
  uint32_t n = 0;
  for (int y = 0; y < H; y++) {
    const uint8_t *ra = ROW(a, y), *rb = ROW(b, y);
    for (int j = 0; j < DBYTES; j++) n += popcount32(ra[j] ^ rb[j]);
  }
  return n;
}

// ---- plain-C reference step (for the self-test) -------------------------

static int cell(const uint8_t *g, int x, int y) {
  return (ROW(g, y)[x >> 3] >> (x & 7)) & 1;
}

static void ref_step(const uint8_t *s, uint8_t *d) {
  for (int y = 0; y < H; y++) {
    int ym = y ? y - 1 : H - 1, yp = (y < H - 1) ? y + 1 : 0;
    uint8_t *out = ROW(d, y);
    for (int j = 0; j < DBYTES; j++) {
      uint8_t v = 0;
      for (int k = 0; k < 8; k++) {
        int x = 8 * j + k;
        int xm = x ? x - 1 : W - 1, xp = (x < W - 1) ? x + 1 : 0;
        int n = cell(s, xm, ym) + cell(s, x, ym) + cell(s, xp, ym)
              + cell(s, xm, y)                    + cell(s, xp, y)
              + cell(s, xm, yp) + cell(s, x, yp) + cell(s, xp, yp);
        if (n == 3 || (n == 2 && cell(s, x, y))) v |= 1 << k;
      }
      out[j] = v;
    }
  }
  life_fix_edges(d);
}

// ---- display ----------------------------------------------------------

static uint8_t tbl[16];          // 4 cells -> 8 LCD pixels (one LCD byte)

// Pixel x=0 is bit 0 of the LCD line byte; a lit pixel is a 0 bit when
// LCD_EMPTY_VALUE is 0xFF.
static void build_table(void) {
  for (int n = 0; n < 16; n++) {
    uint8_t b = LCD_EMPTY_VALUE;
    for (int p = 0; p < 8; p++) {
      if ((n >> (p >> 1)) & 1) {
        uint8_t m = 1 << p;
        if (LCD_EMPTY_VALUE) b &= ~m; else b |= m;
      }
    }
    tbl[n] = b;
  }
}

static void render(const uint8_t *g) {
  for (int y = 0; y < H; y++) {
    const uint8_t *row = ROW(g, y);
    uint8_t *l0 = lcd_line_addr(2 * y);
    uint8_t *l1 = lcd_line_addr(2 * y + 1);
    for (int b = 0; b < LCD_LINE_SIZE; b++) {
      uint8_t v = (row[b >> 1] >> ((b & 1) * 4)) & 15;
      l0[b] = l1[b] = tbl[v];
    }
  }
  lcd_forced_refresh();
}

// ---- keys -------------------------------------------------------------

// Sleep until a key is pressed; returns KEY_ENTER or KEY_EXIT, ignores the rest.
static int wait_enter_or_exit(void) {
  for (;;) {
    if (key_empty()) { sys_sleep(); continue; }
    int k = key_pop();
    if (k == KEY_ENTER || k == KEY_EXIT) return k;
  }
}

static int exit_pressed(void) {
  while (!key_empty())
    if (key_pop() == KEY_EXIT) return 1;
  return 0;
}

// ---- self-test and timing ---------------------------------------------

typedef struct {
  int      ok;                 // asm and C agreed on every generation tested
  uint32_t asm10, c10;         // ms per generation, tenths
  uint32_t draw10;             // ms per frame, tenths
} bench_t;

#define CHECK_GENS  4
#define ASM_GENS    50
#define DRAW_FRAMES 10

static void selftest(uint8_t *a, uint8_t *b, uint8_t *c, uint8_t *d, bench_t *r) {
  uint32_t t, tc = 0, ta, td;
  uint8_t *x;

  randomize(a);
  for (int i = 0; i < GBYTES; i++) c[i] = a[i];

  r->ok = 1;
  for (int i = 0; i < CHECK_GENS; i++) {
    life_step(a, b);
    t = sys_current_ms(); ref_step(c, d); tc += sys_current_ms() - t;
    if (!same(b, d)) r->ok = 0;
    x = a; a = b; b = x;
    x = c; c = d; d = x;
  }

  t = sys_current_ms();
  for (int i = 0; i < ASM_GENS; i++) { life_step(a, b); x = a; a = b; b = x; }
  ta = sys_current_ms() - t;

  t = sys_current_ms();
  for (int i = 0; i < DRAW_FRAMES; i++) render(a);
  td = sys_current_ms() - t;

  r->c10    = tc * 10 / CHECK_GENS;
  r->asm10  = ta * 10 / ASM_GENS;
  r->draw10 = td * 10 / DRAW_FRAMES;
}

// ---- one run ----------------------------------------------------------

enum { END_EXIT, END_STABLE, END_PERIODIC };

typedef struct {
  int      ran;                // a run has completed
  int      how;                // END_*
  uint32_t gens;
  uint32_t ms;
  uint32_t pop;                // live cells at the end
  uint32_t diff;               // cells differing from two generations back
} result_t;

#define PHIST   128              // population history (power of 2)
#define PWINDOW 64               // generations that must repeat
#define PMAXP   32               // longest period looked for

static uint32_t pops[PHIST];

// True if the population count of the last PWINDOW generations repeats
// with some period <= PMAXP.
static int population_periodic(uint32_t g) {
  if (g < PWINDOW + PMAXP) return 0;
  for (uint32_t p = 1; p <= PMAXP; p++) {
    uint32_t i;
    for (i = 0; i < PWINDOW; i++)
      if (pops[(g - i) & (PHIST - 1)] != pops[(g - i - p) & (PHIST - 1)]) break;
    if (i == PWINDOW) return 1;
  }
  return 0;
}

// prev/cur/nxt hold generations k-1, k, k+1. Comparing k+1 with k-1 catches
// period 1 (still life) and period 2 (blinkers, crosses).
static void play(uint8_t *prev, uint8_t *cur, uint8_t *nxt, result_t *r) {
  uint32_t g = 0, t0 = sys_current_ms();
  int how = END_EXIT;

  randomize(cur);
  pops[0] = population(cur);
  for (;;) {
    render(cur);
    if (exit_pressed()) break;
    life_step(cur, nxt);
    g++;
    pops[g & (PHIST - 1)] = population(nxt);
    if (g >= 2 && same(nxt, prev)) { how = END_STABLE; break; }
    if (population_periodic(g))    { how = END_PERIODIC; break; }
    uint8_t *t = prev; prev = cur; cur = nxt; nxt = t;
  }
  r->ran  = 1;
  r->how  = how;
  r->gens = g;
  r->ms   = sys_current_ms() - t0;
  // On EXIT, nxt still holds generation k-2, so cur-vs-nxt is the 2-step distance.
  r->pop  = (how == END_EXIT) ? population(cur) : pops[g & (PHIST - 1)];
  r->diff = (how == END_EXIT && g >= 2) ? distance(cur, nxt) : 0;
}

// ---- title screen -----------------------------------------------------

static char *put_str(char *p, const char *s) {
  while (*s) *p++ = *s++;
  return p;
}

static char *put_uint(char *p, uint32_t v) {
  char tmp[11];
  int n = 0;
  do { tmp[n++] = '0' + v % 10; v /= 10; } while (v);
  while (n) *p++ = tmp[--n];
  return p;
}

static char *put_tenths(char *p, uint32_t v10) {
  p = put_uint(p, v10 / 10);
  *p++ = '.';
  return put_uint(p, v10 % 10);
}

static void show_title(const bench_t *bn, const result_t *r) {
  char l4[48], l5[48], l7[48], l8[48], *p;

  lcd_clear_buf();
  lcd_writeClr(t24);
  lcd_putsAt(t24, 0, "GAME OF LIFE");
  lcd_putsAt(t24, 1, "ENTER start, EXIT quit");
  lcd_putsAt(t24, 3, bn->ok ? "Self-test: asm = C, OK" : "Self-test: MISMATCH!");

  p = put_str(l4, "Step: asm ");
  p = put_tenths(p, bn->asm10);
  p = put_str(p, " ms, C ");
  p = put_tenths(p, bn->c10);
  p = put_str(p, " ms");
  *p = 0;
  lcd_putsAt(t24, 4, l4);

  p = put_str(l5, "Draw: ");
  p = put_tenths(p, bn->draw10);
  p = put_str(p, " ms per frame");
  *p = 0;
  lcd_putsAt(t24, 5, l5);

  if (r->ran) {
    p = put_str(l7, r->how == END_STABLE   ? "Stable after " :
                    r->how == END_PERIODIC ? "Periodic after " : "Stopped at ");
    p = put_uint(p, r->gens);
    p = put_str(p, " generations");
    *p = 0;
    lcd_putsAt(t24, 7, l7);

    p = put_tenths(l8, r->ms ? (r->gens * 10000u) / r->ms : 0);
    p = put_str(p, " gens/s, pop ");
    p = put_uint(p, r->pop);
    if (r->how == END_EXIT) {
      p = put_str(p, ", d2 ");
      p = put_uint(p, r->diff);
    }
    *p = 0;
    lcd_putsAt(t24, 8, l8);
  }
  lcd_refresh();
}

void program_main() {
  uint8_t *b0 = malloc(GBYTES);
  uint8_t *b1 = malloc(GBYTES);
  uint8_t *b2 = malloc(GBYTES);
  uint8_t *b3 = malloc(GBYTES);
  result_t res = {0, 0, 0, 0, 0, 0};
  bench_t  bn  = {0, 0, 0, 0};

  if (b0 && b1 && b2 && b3) {
    build_table();

    lcd_clear_buf();
    lcd_writeClr(t24);
    lcd_putsAt(t24, 1, "Self-test running...");
    lcd_refresh();
    selftest(b0, b1, b2, b3, &bn);

    for (;;) {
      show_title(&bn, &res);
      if (wait_enter_or_exit() == KEY_EXIT) break;
      play(b0, b1, b2, &res);
      if (res.how != END_EXIT) {         // let the final field be seen
        uint32_t t = sys_current_ms();
        while (sys_current_ms() - t < 1500) {}
        while (!key_empty()) key_pop();
      }
    }
  } else {
    lcd_clear_buf();
    lcd_writeClr(t24);
    lcd_putsAt(t24, 1, "out of memory");
    lcd_refresh();
    wait_for_key_press();
  }
  free(b0); free(b1); free(b2); free(b3);
}
