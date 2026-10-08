#include "patterns.h"

const char PAT_GLIDER[]         = "bo$2bo$3o!";
const char PAT_LWSS[]           = "bo2bo$o$o3bo$4o!";
const char PAT_RPENTOMINO[]     = "b2o$2ob$bo!";
const char PAT_ACORN[]          = "bo$3bo$2o2b3o!";
const char PAT_DIEHARD[]        = "6bo$2o$bo3b3o!";
const char PAT_PENTADECATHLON[] = "2bo4bo$2ob4ob2o$2bo4bo!";
const char PAT_PULSAR[]         = "2b3o3b3o2$o4bobo4bo$o4bobo4bo$o4bobo4bo$2b3o3b3o2$2b3o3b3o$o4bobo4bo$o4bobo4bo$o4bobo4bo2$2b3o3b3o!";
const char PAT_GOSPER_GUN[]     = "24bo$22bobo$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o$2o8bo3bob2o4bobo$10bo5bo7bo$11bo3bo$12b2o!";

static int wrapx(int x) { x %= W; return x < 0 ? x + W : x; }
static int wrapy(int y) { y %= H; return y < 0 ? y + H : y; }

void set_cell(uint8_t *g, int x, int y) {
  x = wrapx(x); y = wrapy(y);
  ROW(g, y)[x >> 3] |= 1 << (x & 7);
}

static void clear_cell(uint8_t *g, int x, int y) {
  x = wrapx(x); y = wrapy(y);
  ROW(g, y)[x >> 3] &= ~(1 << (x & 7));
}

void clear_rect(uint8_t *g, int x0, int y0, int w, int h) {
  for (int y = 0; y < h; y++)
    for (int x = 0; x < w; x++)
      clear_cell(g, x0 + x, y0 + y);
}

// Walk the RLE, calling put(ctx, x, y) for every live cell.
typedef void (*put_fn)(void *ctx, int x, int y);

static void rle_walk(const char *s, put_fn put, void *ctx) {
  int run = 0, x = 0, y = 0;
  for (; *s && *s != '!'; s++) {
    if (*s >= '0' && *s <= '9') { run = run * 10 + (*s - '0'); continue; }
    int n = run ? run : 1;
    run = 0;
    if (*s == 'b')      x += n;
    else if (*s == 'o') { for (int k = 0; k < n; k++) put(ctx, x + k, y); x += n; }
    else if (*s == '$') { y += n; x = 0; }
  }
}

typedef struct { int w, h; } size_ctx_t;

static void size_put(void *ctx, int x, int y) {
  size_ctx_t *c = ctx;
  if (x + 1 > c->w) c->w = x + 1;
  if (y + 1 > c->h) c->h = y + 1;
}

void rle_size(const char *rle, int *w, int *h) {
  size_ctx_t c = {0, 0};
  rle_walk(rle, size_put, &c);
  *w = c.w; *h = c.h;
}

typedef struct { uint8_t *g; int x0, y0, w, h, orient; } stamp_ctx_t;

static void stamp_put(void *ctx, int x, int y) {
  stamp_ctx_t *c = ctx;
  int w = c->w, h = c->h;
  if (c->orient & 4) { int t = x; x = y; y = t; t = w; w = h; h = t; }
  if (c->orient & 1) x = w - 1 - x;
  if (c->orient & 2) y = h - 1 - y;
  set_cell(c->g, c->x0 + x, c->y0 + y);
}

void rle_stamp(uint8_t *g, const char *rle, int x0, int y0, int orient) {
  stamp_ctx_t c = {g, x0, y0, 0, 0, orient};
  rle_size(rle, &c.w, &c.h);
  rle_walk(rle, stamp_put, &c);
}
