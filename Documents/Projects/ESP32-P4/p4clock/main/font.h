#pragma once
#include <stdint.h>

typedef struct {
    uint8_t w, h;      // bitmap size
    int8_t xo, yo;     // offset of bitmap's top-left from the pen position on the baseline
    uint8_t adv;       // pen advance
    uint32_t off;      // offset into the 8-bit alpha data
} glyph_t;

typedef struct {
    const glyph_t *g;        // indexed by (char - 32), Latin-1 32..255
    const uint8_t *alpha;
} font_t;

extern const font_t f_lbl, f_num, f_date, f_val, f_temp, f_time;
