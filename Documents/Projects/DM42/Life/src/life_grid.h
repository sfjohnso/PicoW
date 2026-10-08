#ifndef LIFE_GRID_H
#define LIFE_GRID_H

#include <stdint.h>

// Grid geometry shared by main.c, patterns.c and life.s (see life.s for the layout).
#define W      200
#define H      120
#define DBYTES 25                // data bytes per row (W/8)
#define GS     27                // stored row: left copy + 25 data + right copy
#define GROWS  (H + 2)           // plus a copy of the last row above, first row below
#define GBYTES (GS * GROWS)

#define ROW(g, y) ((g) + ((y) + 1) * GS + 1)    // first data byte of Life row y

extern void life_fix_edges(uint8_t *grid);

#endif
