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

#include "life_grid.h"
#include "patterns.h"

extern void life_step(const uint8_t *src, uint8_t *dst);

static uint32_t rng_state;

static uint32_t rng(void) {      // xorshift32
  uint32_t x = rng_state;
  x ^= x << 13;
  x ^= x >> 17;
  x ^= x << 5;
  return rng_state = x;
}

static int rnd(int n) {          // 0 .. n-1
  return (int)((rng() >> 8) % (uint32_t)n);
}

// ---- starting fields ----------------------------------------------------

enum { MODE_SOUP, MODE_GARDEN, MODE_GUNS, MODE_HYBRID, MODE_KALEIDO, NMODES };

static const char *const mode_name[NMODES] = {
  "Soup", "Garden", "Guns", "Hybrid", "Kaleido"
};

static void clear_grid(uint8_t *g) {
  for (int i = 0; i < GBYTES; i++) g[i] = 0;
}

// Random cells; the three-way mix of rng() words gives ~37% (soup) or ~12% (sparse).
static void fill_soup(uint8_t *g, int sparse) {
  for (int y = 0; y < H; y++) {
    uint8_t *row = ROW(g, y);
    for (int j = 0; j < DBYTES; j++) {
      uint32_t a = rng(), b = rng(), c = rng();
      row[j] = (uint8_t)((sparse ? (a & b & c) : (a & (b | c))) >> 8);
    }
  }
}

// Stamp a pattern at a random orientation centred on (cx, cy), after clearing
// a one-cell margin around it so the soup does not smother it.
static void drop_organism(uint8_t *g, const char *rle, int cx, int cy, int clear) {
  int w, h, o = rnd(8);
  rle_size(rle, &w, &h);
  if (o & 4) { int t = w; w = h; h = t; }
  if (clear) clear_rect(g, cx - w / 2 - 1, cy - h / 2 - 1, w + 2, h + 2);
  rle_stamp(g, rle, cx - w / 2, cy - h / 2, o);
}

static void populate(uint8_t *g, int mode) {
  static const char *const methuselahs[] = { PAT_RPENTOMINO, PAT_ACORN, PAT_DIEHARD };
  static const char *const small[] = { PAT_RPENTOMINO, PAT_ACORN, PAT_PULSAR,
                                       PAT_PENTADECATHLON, PAT_LWSS, PAT_GLIDER };
  rng_state = (sys_current_ms() ^ get_rtc_ticks() ^ 0x9E3779B9u) | 1;

  switch (mode) {
  case MODE_SOUP:
    fill_soup(g, 0);
    break;

  case MODE_GARDEN:                       // one methuselah per quadrant
    clear_grid(g);
    for (int q = 0; q < 4; q++)
      drop_organism(g, methuselahs[rnd(3)], (q & 1) * (W / 2) + 25 + rnd(50),
                    (q >> 1) * (H / 2) + 15 + rnd(30), 0);
    break;

  case MODE_GUNS: {                       // two Gosper guns firing at each other
    int w, h, dy = rnd(30);
    clear_grid(g);
    rle_size(PAT_GOSPER_GUN, &w, &h);
    rle_stamp(g, PAT_GOSPER_GUN, 8, 8 + dy, 0);
    rle_stamp(g, PAT_GOSPER_GUN, W - 8 - w, H - 8 - h - dy, 3);
    break;
  }

  case MODE_HYBRID:                       // thin soup with a few organisms in it
    fill_soup(g, 1);
    for (int i = 0; i < 3 + rnd(3); i++)
      drop_organism(g, small[rnd(6)], 15 + rnd(W - 30), 10 + rnd(H - 20), 1);
    break;

  case MODE_KALEIDO:                      // random quadrant mirrored four ways
    clear_grid(g);
    for (int y = 0; y < H / 2; y++)
      for (int x = 0; x < W / 2; x++) {
        uint32_t a = rng(), b = rng(), c = rng();
        if ((a & (b | c)) & 0x100) {
          set_cell(g, x, y);
          set_cell(g, W - 1 - x, y);
          set_cell(g, x, H - 1 - y);
          set_cell(g, W - 1 - x, H - 1 - y);
        }
      }
    break;
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

// Diagnostics for the current run: time spent packing cells into LCD bytes,
// time spent waiting for / starting the LCD transfer, rows redrawn.
static uint32_t t_pack, t_send, n_rows, n_frames;

static int row_differs(const uint8_t *a, const uint8_t *b) {
  for (int j = 0; j < DBYTES; j++)
    if (a[j] != b[j]) return 1;
  return 0;
}

// Draw grid g. If old is the grid already on the LCD, only rows that changed
// are rewritten (and so marked dirty) and sent; with old == NULL everything is.
static void render(const uint8_t *g, const uint8_t *old) {
  uint32_t t0 = sys_current_ms();
  lcd_refresh_wait();                  // a previous transfer may still be running
  uint32_t t1 = sys_current_ms();

  uint32_t rows = 0;
  for (int y = 0; y < H; y++) {
    const uint8_t *row = ROW(g, y);
    if (old && !row_differs(row, ROW(old, y))) continue;
    uint8_t *l0 = lcd_line_addr(2 * y);
    uint8_t *l1 = lcd_line_addr(2 * y + 1);
    for (int b = 0; b < LCD_LINE_SIZE; b++) {
      uint8_t v = (row[b >> 1] >> ((b & 1) * 4)) & 15;
      l0[b] = l1[b] = tbl[v];
    }
    rows++;
  }
  uint32_t t2 = sys_current_ms();
  if (old) lcd_refresh(); else lcd_forced_refresh();
  uint32_t t3 = sys_current_ms();

  t_send += (t1 - t0) + (t3 - t2);
  t_pack += t2 - t1;
  n_rows += rows;
  n_frames++;
}

// ---- keys -------------------------------------------------------------

// Sleep until a mode key (1-5), ENTER (repeat the last mode) or EXIT is pressed.
// Returns the mode number, or -1 for EXIT.
static int wait_choice(int last_mode) {
  static const char keys[NMODES] = { KEY_1, KEY_2, KEY_3, KEY_4, KEY_5 };
  for (;;) {
    if (key_empty()) { sys_sleep(); continue; }
    int k = key_pop();
    if (k == KEY_EXIT)  return -1;
    if (k == KEY_ENTER) return last_mode;
    for (int m = 0; m < NMODES; m++)
      if (k == keys[m]) return m;
  }
}

static int exit_pressed(void) {
  while (!key_empty())
    if (key_pop() == KEY_EXIT) return 1;
  return 0;
}

// ---- one run ----------------------------------------------------------

enum { END_EXIT, END_STABLE, END_PERIODIC };

typedef struct {
  int      ran;                // a run has completed
  int      how;                // END_*
  int      mode;               // MODE_*
  uint32_t gens;
  uint32_t ms;
  uint32_t pop;                // live cells at the end
  uint32_t diff;               // cells differing from two generations back
  uint32_t pack10, send10;     // per frame, tenths of ms
  uint32_t rows;               // average rows redrawn per frame
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
static void play(uint8_t *prev, uint8_t *cur, uint8_t *nxt, int mode, result_t *r) {
  uint32_t g = 0, t0 = sys_current_ms();
  int how = END_EXIT;

  populate(cur, mode);
  t_pack = t_send = n_rows = n_frames = 0;
  pops[0] = population(cur);
  for (;;) {
    render(cur, g ? prev : NULL);
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
  r->mode = mode;
  r->gens = g;
  r->ms   = sys_current_ms() - t0;
  // On EXIT, nxt still holds generation k-2, so cur-vs-nxt is the 2-step distance.
  r->pop  = (how == END_EXIT) ? population(cur) : pops[g & (PHIST - 1)];
  r->diff = (how == END_EXIT && g >= 2) ? distance(cur, nxt) : 0;
  r->pack10 = n_frames ? t_pack * 10 / n_frames : 0;
  r->send10 = n_frames ? t_send * 10 / n_frames : 0;
  r->rows   = n_frames ? n_rows / n_frames : 0;
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

static void show_title(const result_t *r) {
  char l6[48], l7[48], l8[48], *p;

  lcd_clear_buf();
  lcd_writeClr(t24);
  lcd_putsAt(t24, 0, "GAME OF LIFE - EXIT quits");
  lcd_putsAt(t24, 1, "1  Soup (random)");
  lcd_putsAt(t24, 2, "2  Garden");
  lcd_putsAt(t24, 3, "3  Gun battle");
  lcd_putsAt(t24, 4, "4  Hybrid");
  lcd_putsAt(t24, 5, "5  Kaleidoscope");

  if (r->ran) {
    p = put_str(l6, mode_name[r->mode]);
    p = put_str(p, r->how == END_STABLE   ? ": stable" :
                   r->how == END_PERIODIC ? ": periodic" : ": stopped");
    *p = 0;
    lcd_putsAt(t24, 6, l6);

    p = put_uint(l7, r->gens);
    p = put_str(p, " gens, ");
    p = put_tenths(p, r->ms ? (r->gens * 10000u) / r->ms : 0);
    p = put_str(p, "/s");
    *p = 0;
    lcd_putsAt(t24, 7, l7);

    p = put_str(l8, "pack ");
    p = put_tenths(p, r->pack10);
    p = put_str(p, " send ");
    p = put_tenths(p, r->send10);
    p = put_str(p, " rows ");
    p = put_uint(p, r->rows);
    *p = 0;
    lcd_putsAt(t24, 8, l8);
  }
  lcd_refresh();
}

void program_main() {
  uint8_t *b0 = malloc(GBYTES);
  uint8_t *b1 = malloc(GBYTES);
  uint8_t *b2 = malloc(GBYTES);
  result_t res = {0};
  int mode = MODE_SOUP;

  if (b0 && b1 && b2) {
    build_table();
    for (;;) {
      show_title(&res);
      mode = wait_choice(mode);
      if (mode < 0) break;
      play(b0, b1, b2, mode, &res);
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
  free(b0); free(b1); free(b2);
}
