#ifndef PATTERNS_H
#define PATTERNS_H

#include "life_grid.h"

// Organisms as RLE text (verified by simulation against their known behaviour).
extern const char PAT_GLIDER[], PAT_LWSS[], PAT_RPENTOMINO[], PAT_ACORN[],
                  PAT_DIEHARD[], PAT_PENTADECATHLON[], PAT_PULSAR[], PAT_GOSPER_GUN[];

// Bounding box of an RLE pattern.
void rle_size(const char *rle, int *w, int *h);

// Set the pattern's live cells into g with its top-left corner at (x0, y0),
// wrapping around the torus. orient: bit 2 transposes first, then bit 0 mirrors x
// and bit 1 mirrors y (so 0..7 are the eight rotations/reflections).
// Does not touch the edge copies: call life_fix_edges() when done.
void rle_stamp(uint8_t *g, const char *rle, int x0, int y0, int orient);

// Clear a w x h rectangle (with wrap), for making room before a stamp.
void clear_rect(uint8_t *g, int x0, int y0, int w, int h);

void set_cell(uint8_t *g, int x, int y);

#endif
