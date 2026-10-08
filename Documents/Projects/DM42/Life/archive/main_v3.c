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
// The generation step is in life.s. A run ends when the user presses EXIT or
// when the field repeats itself with period 1 or 2 (still lifes, blinkers, ...).

#define W      200
#define H      120
#define STRIDE 25                // bytes per grid row (W/8)
#define GBYTES (STRIDE * H)

extern void life_step(const uint8_t *src, uint8_t *dst);

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
  for (int i = 0; i < GBYTES; i++) {
    uint32_t a = rng(), b = rng(), c = rng();
    g[i] = (uint8_t)((a & (b | c)) >> 8);
  }
}

static int same(const uint8_t *a, const uint8_t *b) {
  const uint32_t *p = (const uint32_t *)a, *q = (const uint32_t *)b;
  for (int i = 0; i < GBYTES / 4; i++)
    if (p[i] != q[i]) return 0;
  return 1;
}

static uint32_t popcount32(uint32_t v) {
  v = v - ((v >> 1) & 0x55555555u);
  v = (v & 0x33333333u) + ((v >> 2) & 0x33333333u);
  v = (v + (v >> 4)) & 0x0F0F0F0Fu;
  return (v * 0x01010101u) >> 24;
}

static uint32_t population(const uint8_t *g) {
  const uint32_t *p = (const uint32_t *)g;
  uint32_t n = 0;
  for (int i = 0; i < GBYTES / 4; i++) n += popcount32(p[i]);
  return n;
}

// Number of cells that differ between two grids.
static uint32_t distance(const uint8_t *a, const uint8_t *b) {
  const uint32_t *p = (const uint32_t *)a, *q = (const uint32_t *)b;
  uint32_t n = 0;
  for (int i = 0; i < GBYTES / 4; i++) n += popcount32(p[i] ^ q[i]);
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

static void render(const uint8_t *g) {
  for (int y = 0; y < H; y++) {
    const uint8_t *row = g + y * STRIDE;
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

static void show_title(const result_t *r) {
  char l6[48], l7[48], l8[48], *p;

  lcd_clear_buf();
  lcd_writeClr(t24);
  lcd_putsAt(t24, 0, "GAME OF LIFE");
  lcd_putsAt(t24, 1, "200x120 cells, 2x2 pixels");
  lcd_putsAt(t24, 3, "ENTER = start");
  lcd_putsAt(t24, 4, "EXIT = quit");

  if (r->ran) {
    p = put_str(l6, r->how == END_STABLE   ? "Stable after " :
                    r->how == END_PERIODIC ? "Periodic after " : "Stopped at ");
    p = put_uint(p, r->gens);
    p = put_str(p, " generations");
    *p = 0;
    lcd_putsAt(t24, 6, l6);

    uint32_t rate10 = r->ms ? (r->gens * 10000u) / r->ms : 0;
    p = put_uint(l7, rate10 / 10);
    *p++ = '.';
    p = put_uint(p, rate10 % 10);
    p = put_str(p, " gens/sec, pop ");
    p = put_uint(p, r->pop);
    *p = 0;
    lcd_putsAt(t24, 7, l7);

    if (r->how == END_EXIT) {
      p = put_str(l8, "2-gen difference: ");
      p = put_uint(p, r->diff);
      p = put_str(p, " cells");
      *p = 0;
      lcd_putsAt(t24, 8, l8);
    }
  }
  lcd_refresh();
}

void program_main() {
  uint8_t *b0 = malloc(GBYTES);
  uint8_t *b1 = malloc(GBYTES);
  uint8_t *b2 = malloc(GBYTES);
  result_t res = {0, 0, 0, 0};

  if (b0 && b1 && b2) {
    build_table();
    for (;;) {
      show_title(&res);
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
  free(b0); free(b1); free(b2);
}
