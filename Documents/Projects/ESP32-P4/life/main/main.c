// Conway's Game of Life for the Waveshare ESP32-P4 3.5" board (a launcher app).
//
// A port of the DM42 version: a 240x160 torus drawn as 2x2-pixel cells on the 480x320 screen, five ways to start
// (soup, garden, gun battle, hybrid, kaleidoscope), and a run that ends by itself when the field is still, repeats
// with period 2, or its population settles into a short cycle. Tap the screen during a run to stop it.
//
// Display: ST7796 480x320 on SPI (MOSI20 SCLK21 CS23 DC26 RST27 BL28).  Touch: FT6336 at I2C 0x38 (SDA7 SCL8 RST29).
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "driver/gpio.h"
#include "driver/i2c_master.h"
#include "driver/ledc.h"
#include "driver/spi_master.h"
#include "esp_heap_caps.h"
#include "esp_lcd_panel_io.h"
#include "esp_log.h"
#include "esp_random.h"
#include "esp_timer.h"
#include "freertos/FreeRTOS.h"
#include "freertos/semphr.h"
#include "freertos/task.h"
#include "font_8x16.h"

static const char *TAG = "life";

#define LCD_W 480
#define LCD_H 320
#define PIN_MOSI 20
#define PIN_SCLK 21
#define PIN_LCD_CS 23
#define PIN_LCD_DC 26
#define PIN_LCD_RST 27
#define PIN_LCD_BL 28
#define PIN_TP_RST 29
#define PIN_SDA 7
#define PIN_SCL 8
#define SPI_MHZ 80
#define CHUNK_PX 12800                      // 25,600 bytes per SPI transfer (a multiple of the cache line)
#define BACKLIGHT_PCT 80

// RGB565 byte-swapped for the SPI wire
static inline uint16_t rgb565(int r, int g, int b)
{
    uint16_t v = ((r & 0xF8) << 8) | ((g & 0xFC) << 3) | (b >> 3);
    return (v << 8) | (v >> 8);
}

// ===========================================================================
// Display (same as the launcher's), with a partial update for the Life rows
// ===========================================================================
static esp_lcd_panel_io_handle_t lcd_io;
static SemaphoreHandle_t lcd_done;
static uint16_t *fb;                        // LCD_W x LCD_H, byte-swapped RGB565

static bool on_done(esp_lcd_panel_io_handle_t io, esp_lcd_panel_io_event_data_t *e, void *u)
{
    BaseType_t w = pdFALSE;
    xSemaphoreGiveFromISR(lcd_done, &w);
    return w == pdTRUE;
}

static void cmd(uint8_t c, const uint8_t *p, size_t n) { ESP_ERROR_CHECK(esp_lcd_panel_io_tx_param(lcd_io, c, p, n)); }

// Send screen rows y0 .. y1-1 from the frame buffer.
static void flush_rows(int y0, int y1)
{
    cmd(0x2A, (uint8_t[]){ 0, 0, (LCD_W - 1) >> 8, (LCD_W - 1) & 0xFF }, 4);
    cmd(0x2B, (uint8_t[]){ y0 >> 8, y0 & 0xFF, (y1 - 1) >> 8, (y1 - 1) & 0xFF }, 4);
    int total = (y1 - y0) * LCD_W, sent = 0, chunks = 0;
    uint16_t *p = fb + y0 * LCD_W;
    while (sent < total) {
        int n = total - sent < CHUNK_PX ? total - sent : CHUNK_PX;
        ESP_ERROR_CHECK(esp_lcd_panel_io_tx_color(lcd_io, sent ? 0x3C : 0x2C, p + sent, n * 2));
        sent += n;
        chunks++;
    }
    while (chunks--) xSemaphoreTake(lcd_done, portMAX_DELAY);
}

static void flush(void) { flush_rows(0, LCD_H); }

static void display_init(void)
{
    gpio_config_t o = { .pin_bit_mask = 1ULL << PIN_LCD_RST, .mode = GPIO_MODE_OUTPUT };
    gpio_config(&o);
    ledc_timer_config_t t = { .speed_mode = LEDC_LOW_SPEED_MODE, .duty_resolution = LEDC_TIMER_10_BIT, .timer_num = LEDC_TIMER_0,
                              .freq_hz = 5000, .clk_cfg = LEDC_AUTO_CLK };
    ESP_ERROR_CHECK(ledc_timer_config(&t));
    ledc_channel_config_t c = { .gpio_num = PIN_LCD_BL, .speed_mode = LEDC_LOW_SPEED_MODE, .channel = LEDC_CHANNEL_0,
                                .timer_sel = LEDC_TIMER_0, .duty = 0 };
    ESP_ERROR_CHECK(ledc_channel_config(&c));
    gpio_set_level(PIN_LCD_RST, 0);
    vTaskDelay(pdMS_TO_TICKS(20));
    gpio_set_level(PIN_LCD_RST, 1);
    vTaskDelay(pdMS_TO_TICKS(120));

    spi_bus_config_t bc = { .mosi_io_num = PIN_MOSI, .miso_io_num = -1, .sclk_io_num = PIN_SCLK,
                            .quadwp_io_num = -1, .quadhd_io_num = -1, .max_transfer_sz = CHUNK_PX * 2 };
    ESP_ERROR_CHECK(spi_bus_initialize(SPI2_HOST, &bc, SPI_DMA_CH_AUTO));
    lcd_done = xSemaphoreCreateCounting(32, 0);
    esp_lcd_panel_io_spi_config_t ic = {
        .dc_gpio_num = PIN_LCD_DC, .cs_gpio_num = PIN_LCD_CS, .pclk_hz = SPI_MHZ * 1000 * 1000,
        .lcd_cmd_bits = 8, .lcd_param_bits = 8, .spi_mode = 0, .trans_queue_depth = 4, .on_color_trans_done = on_done,
    };
    ESP_ERROR_CHECK(esp_lcd_new_panel_io_spi((esp_lcd_spi_bus_handle_t)SPI2_HOST, &ic, &lcd_io));
    fb = heap_caps_malloc(LCD_W * LCD_H * 2, MALLOC_CAP_DMA | MALLOC_CAP_INTERNAL);
    assert(fb);
    memset(fb, 0, LCD_W * LCD_H * 2);

    cmd(0x01, NULL, 0); vTaskDelay(pdMS_TO_TICKS(150));
    cmd(0x11, NULL, 0); vTaskDelay(pdMS_TO_TICKS(120));
    cmd(0xF0, (uint8_t[]){ 0xC3 }, 1); cmd(0xF0, (uint8_t[]){ 0x96 }, 1);
    cmd(0x36, (uint8_t[]){ 0x28 }, 1);             // landscape, BGR
    cmd(0x3A, (uint8_t[]){ 0x55 }, 1);             // 16 bits/pixel
    cmd(0xB4, (uint8_t[]){ 0x01 }, 1);
    cmd(0xF0, (uint8_t[]){ 0x3C }, 1); cmd(0xF0, (uint8_t[]){ 0x69 }, 1);
    cmd(0x21, NULL, 0);                            // inversion on (IPS panel)
    cmd(0x29, NULL, 0); vTaskDelay(pdMS_TO_TICKS(50));
    flush();                                       // black before the backlight comes on
    ledc_set_duty(LEDC_LOW_SPEED_MODE, LEDC_CHANNEL_0, 1023 * BACKLIGHT_PCT / 100);
    ledc_update_duty(LEDC_LOW_SPEED_MODE, LEDC_CHANNEL_0);
}

// ===========================================================================
// Touch (same as the launcher's)
// ===========================================================================
static i2c_master_dev_handle_t touch_dev;

static void touch_init(void)
{
    gpio_config_t o = { .pin_bit_mask = 1ULL << PIN_TP_RST, .mode = GPIO_MODE_OUTPUT };
    gpio_config(&o);
    gpio_set_level(PIN_TP_RST, 0);                 // reset is held low by a resistor until driven
    vTaskDelay(pdMS_TO_TICKS(20));
    gpio_set_level(PIN_TP_RST, 1);
    vTaskDelay(pdMS_TO_TICKS(300));
    i2c_master_bus_config_t bc = { .i2c_port = I2C_NUM_0, .sda_io_num = PIN_SDA, .scl_io_num = PIN_SCL,
                                   .clk_source = I2C_CLK_SRC_DEFAULT, .glitch_ignore_cnt = 7, .flags.enable_internal_pullup = true };
    i2c_master_bus_handle_t bus;
    ESP_ERROR_CHECK(i2c_new_master_bus(&bc, &bus));
    i2c_device_config_t dc = { .dev_addr_length = I2C_ADDR_BIT_LEN_7, .device_address = 0x38, .scl_speed_hz = 400000 };
    ESP_ERROR_CHECK(i2c_master_bus_add_device(bus, &dc, &touch_dev));
}

static bool touch_read(int *sx, int *sy)
{
    uint8_t reg = 0x02, b[5];
    if (i2c_master_transmit_receive(touch_dev, &reg, 1, b, sizeof b, 50) != ESP_OK) return false;
    int n = b[0] & 0x0F;
    if (n < 1 || n > 2) return false;
    int rx = ((b[1] & 0x0F) << 8) | b[2], ry = ((b[3] & 0x0F) << 8) | b[4];
    if (rx > 3000 || ry > 3000) return false;      // the chip reports 4095,4095 for a phantom point
    *sx = ry; *sy = LCD_H - 1 - rx;                // portrait controller -> landscape panel
    return true;
}

// Wait for the screen to be touched and released; returns where it was released.
static void wait_tap(int *x, int *y)
{
    int hx, hy;
    while (touch_read(&hx, &hy)) vTaskDelay(2);        // a touch that is still down (the one that stopped a run) is not a tap
    bool was_down = false;
    int lx = 0, ly = 0;
    while (1) {
        int tx, ty;
        bool down = touch_read(&tx, &ty);
        if (down) { lx = tx; ly = ty; }
        if (!down && was_down) break;
        was_down = down;
        vTaskDelay(2);
    }
    *x = lx; *y = ly;
}

// ===========================================================================
// Text and boxes on the frame buffer (title screen)
// ===========================================================================
static void fill_rect(int x, int y, int w, int h, uint16_t c)
{
    for (int j = y; j < y + h; j++)
        for (int i = x; i < x + w; i++) fb[j * LCD_W + i] = c;
}

static int text_width(const char *s, int k) { return (int)strlen(s) * 8 * k; }

static void draw_text(int x0, int y0, const char *s, int k, uint16_t col)
{
    for (int c = 0; s[c]; c++)
        for (int r = 0; r < 16; r++) {
            uint8_t bits = fontdata_8x16[(unsigned char)s[c] * 16 + r];
            for (int b = 0; b < 8; b++)
                if (bits & (0x80 >> b)) fill_rect(x0 + (c * 8 + b) * k, y0 + r * k, k, k, col);
        }
}

static void draw_text_c(int cx, int y0, const char *s, int k, uint16_t col) { draw_text(cx - text_width(s, k) / 2, y0, s, k, col); }

// ===========================================================================
// The Life field: W x H cells, one byte each (0 or 1), on a torus
// ===========================================================================
#define W 240
#define H 160

static uint8_t *grid[3];                    // three generations, in PSRAM (internal RAM is needed for the screen buffer)

static uint32_t rng_state;

static uint32_t rng(void)                   // xorshift32
{
    uint32_t x = rng_state;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    return rng_state = x;
}

static int rnd(int n) { return (int)((rng() >> 8) % (uint32_t)n); }       // 0 .. n-1

// One generation, src -> dst; returns the live-cell count of dst.
static uint32_t life_step(const uint8_t *s, uint8_t *d)
{
    uint32_t pop = 0;
    for (int y = 0; y < H; y++) {
        const uint8_t *a = s + ((y + H - 1) % H) * W, *b = s + y * W, *c = s + ((y + 1) % H) * W;
        uint8_t *o = d + y * W;
        for (int x = 0; x < W; x++) {
            int xl = x ? x - 1 : W - 1, xr = x < W - 1 ? x + 1 : 0;
            int n = a[xl] + a[x] + a[xr] + b[xl] + b[xr] + c[xl] + c[x] + c[xr];
            uint8_t v = n == 3 || (n == 2 && b[x]);
            o[x] = v;
            pop += v;
        }
    }
    return pop;
}

static uint32_t population(const uint8_t *g)
{
    uint32_t n = 0;
    for (int i = 0; i < W * H; i++) n += g[i];
    return n;
}

// ---- organisms (run-length encoded: b = dead, o = live, $ = next row) ------
static const char PAT_GLIDER[]         = "bo$2bo$3o!";
static const char PAT_LWSS[]           = "bo2bo$o$o3bo$4o!";
static const char PAT_RPENTOMINO[]     = "b2o$2ob$bo!";
static const char PAT_ACORN[]          = "bo$3bo$2o2b3o!";
static const char PAT_DIEHARD[]        = "6bo$2o$bo3b3o!";
static const char PAT_PENTADECATHLON[] = "2bo4bo$2ob4ob2o$2bo4bo!";
static const char PAT_PULSAR[]         = "2b3o3b3o2$o4bobo4bo$o4bobo4bo$o4bobo4bo$2b3o3b3o2$2b3o3b3o$o4bobo4bo$o4bobo4bo$o4bobo4bo2$2b3o3b3o!";
static const char PAT_GOSPER_GUN[]     = "24bo$22bobo$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o$2o8bo3bob2o4bobo$10bo5bo7bo$11bo3bo$12b2o!";

static int wrapx(int x) { x %= W; return x < 0 ? x + W : x; }
static int wrapy(int y) { y %= H; return y < 0 ? y + H : y; }

static void set_cell(uint8_t *g, int x, int y) { g[wrapy(y) * W + wrapx(x)] = 1; }

static void clear_rect(uint8_t *g, int x0, int y0, int w, int h)
{
    for (int y = 0; y < h; y++)
        for (int x = 0; x < w; x++) g[wrapy(y0 + y) * W + wrapx(x0 + x)] = 0;
}

// Walk the RLE, calling put(ctx, x, y) for every live cell.
typedef void (*put_fn)(void *ctx, int x, int y);

static void rle_walk(const char *s, put_fn put, void *ctx)
{
    int run = 0, x = 0, y = 0;
    for (; *s && *s != '!'; s++) {
        if (*s >= '0' && *s <= '9') { run = run * 10 + (*s - '0'); continue; }
        int n = run ? run : 1;
        run = 0;
        if (*s == 'b') x += n;
        else if (*s == 'o') { for (int k = 0; k < n; k++) put(ctx, x + k, y); x += n; }
        else if (*s == '$') { y += n; x = 0; }
    }
}

typedef struct { int w, h; } size_ctx_t;

static void size_put(void *ctx, int x, int y)
{
    size_ctx_t *c = ctx;
    if (x + 1 > c->w) c->w = x + 1;
    if (y + 1 > c->h) c->h = y + 1;
}

static void rle_size(const char *rle, int *w, int *h)
{
    size_ctx_t c = { 0, 0 };
    rle_walk(rle, size_put, &c);
    *w = c.w; *h = c.h;
}

typedef struct { uint8_t *g; int x0, y0, w, h, orient; } stamp_ctx_t;

static void stamp_put(void *ctx, int x, int y)
{
    stamp_ctx_t *c = ctx;
    int w = c->w, h = c->h;
    if (c->orient & 4) { int t = x; x = y; y = t; t = w; w = h; h = t; }
    if (c->orient & 1) x = w - 1 - x;
    if (c->orient & 2) y = h - 1 - y;
    set_cell(c->g, c->x0 + x, c->y0 + y);
}

// orient bits: 1 = mirror x, 2 = mirror y, 4 = transpose (eight orientations in all)
static void rle_stamp(uint8_t *g, const char *rle, int x0, int y0, int orient)
{
    stamp_ctx_t c = { g, x0, y0, 0, 0, orient };
    rle_size(rle, &c.w, &c.h);
    rle_walk(rle, stamp_put, &c);
}

// ---- starting fields ---------------------------------------------------------
enum { MODE_SOUP, MODE_GARDEN, MODE_GUNS, MODE_HYBRID, MODE_KALEIDO, NMODES };
static const char *const mode_name[NMODES] = { "Soup", "Garden", "Gun battle", "Hybrid", "Kaleidoscope" };

// Random cells: ~37% (soup) or ~12% (sparse).
static void fill_soup(uint8_t *g, int sparse)
{
    for (int i = 0; i < W * H; i++) {
        uint32_t a = rng(), b = rng(), c = rng();
        g[i] = ((sparse ? (a & b & c) : (a & (b | c))) >> 8) & 1;
    }
}

// Stamp a pattern at a random orientation centred on (cx, cy), optionally clearing a one-cell margin around it first
// so the soup does not smother it.
static void drop_organism(uint8_t *g, const char *rle, int cx, int cy, int clear)
{
    int w, h, o = rnd(8);
    rle_size(rle, &w, &h);
    if (o & 4) { int t = w; w = h; h = t; }
    if (clear) clear_rect(g, cx - w / 2 - 1, cy - h / 2 - 1, w + 2, h + 2);
    rle_stamp(g, rle, cx - w / 2, cy - h / 2, o);
}

static void populate(uint8_t *g, int mode)
{
    static const char *const methuselahs[] = { PAT_RPENTOMINO, PAT_ACORN, PAT_DIEHARD };
    static const char *const small[] = { PAT_RPENTOMINO, PAT_ACORN, PAT_PULSAR, PAT_PENTADECATHLON, PAT_LWSS, PAT_GLIDER };
    rng_state = esp_random() | 1;

    switch (mode) {
    case MODE_SOUP:
        fill_soup(g, 0);
        break;

    case MODE_GARDEN:                       // one methuselah per quadrant
        memset(g, 0, W * H);
        for (int q = 0; q < 4; q++)
            drop_organism(g, methuselahs[rnd(3)], (q & 1) * (W / 2) + 30 + rnd(60), (q >> 1) * (H / 2) + 20 + rnd(40), 0);
        break;

    case MODE_GUNS: {                       // two Gosper guns firing at each other
        int w, h, dy = rnd(40);
        memset(g, 0, W * H);
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
        memset(g, 0, W * H);
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
}

// ---- display of a field: 2x2 pixels per cell, coloured by what just happened to the cell ------------------------
// code = 2 * (alive in the previous generation) + (alive now): 0 empty, 1 born, 2 just died, 3 surviving.
static uint32_t pal[4];                     // two pixels of each colour, ready to store as one word
static uint8_t *shown;                      // the code of every cell as currently on the screen

static void palette_init(void)
{
    uint16_t c[4] = { rgb565(0, 0, 0), rgb565(255, 255, 170), rgb565(120, 30, 30), rgb565(60, 200, 90) };
    for (int i = 0; i < 4; i++) pal[i] = c[i] | ((uint32_t)c[i] << 16);
}

// Draw grid g (old = the generation before it, or NULL for the first frame, which draws everything as surviving
// cells) into the frame buffer and send it. Only cell rows whose colours changed are redrawn; runs of changed rows
// go out together.
static void render(const uint8_t *g, const uint8_t *old)
{
    uint32_t *fb32 = (uint32_t *)fb;
    uint8_t code[W];
    if (!old) memset(shown, 0xFF, W * H);
    int run_start = -1;
    for (int y = 0; y <= H; y++) {
        bool changed = false;
        if (y < H) {
            const uint8_t *row = g + y * W, *orow = old ? old + y * W : row;
            uint8_t *sh = shown + y * W;
            for (int x = 0; x < W; x++) {
                uint8_t c = (orow[x] << 1) | row[x];
                code[x] = c;
                changed |= c != sh[x];
            }
        }
        if (changed) {
            memcpy(shown + y * W, code, W);
            uint32_t *p0 = fb32 + (2 * y) * (LCD_W / 2), *p1 = p0 + LCD_W / 2;
            for (int x = 0; x < W; x++) p0[x] = p1[x] = pal[code[x]];
            if (run_start < 0) run_start = y;
        } else if (run_start >= 0) {
            flush_rows(2 * run_start, 2 * y);
            run_start = -1;
        }
    }
}

// ===========================================================================
// One run
// ===========================================================================
enum { END_TAP, END_STABLE, END_PERIODIC };

typedef struct {
    bool ran;
    int how, mode;
    uint32_t gens, pop;
    int64_t us;
} result_t;

#define PHIST 128                           // population history (power of 2)
#define PWINDOW 64                          // generations that must repeat
#define PMAXP 32                            // longest period looked for

static uint32_t pops[PHIST];

// True if the population count of the last PWINDOW generations repeats with some period <= PMAXP.
static bool population_periodic(uint32_t g)
{
    if (g < PWINDOW + PMAXP) return false;
    for (uint32_t p = 1; p <= PMAXP; p++) {
        uint32_t i;
        for (i = 0; i < PWINDOW; i++)
            if (pops[(g - i) & (PHIST - 1)] != pops[(g - i - p) & (PHIST - 1)]) break;
        if (i == PWINDOW) return true;
    }
    return false;
}

static int64_t step_us, render_us;                    // time spent in life_step and in render, for the log

static bool screen_touched(void)
{
    int x, y;
    return touch_read(&x, &y);
}

// prev/cur/nxt hold generations k-1, k, k+1. Comparing k+1 with k-1 catches period 1 (still life) and period 2 (blinkers).
static void play(int mode, result_t *r)
{
    uint8_t *prev = grid[0], *cur = grid[1], *nxt = grid[2];
    uint32_t g = 0;
    step_us = render_us = 0;
    int64_t t0 = esp_timer_get_time();
    int how = END_TAP;

    populate(cur, mode);
    pops[0] = population(cur);
    while (screen_touched()) vTaskDelay(2);           // the tap that chose the mode must be over
    for (;;) {
        int64_t a = esp_timer_get_time();
        render(cur, g ? prev : NULL);
        int64_t b = esp_timer_get_time();
        if (screen_touched()) break;
        pops[(g + 1) & (PHIST - 1)] = life_step(cur, nxt);
        render_us += b - a;
        step_us += esp_timer_get_time() - b;
        g++;
        if (g >= 2 && memcmp(nxt, prev, W * H) == 0) { how = END_STABLE; break; }
        if (population_periodic(g)) { how = END_PERIODIC; break; }
        uint8_t *t = prev; prev = cur; cur = nxt; nxt = t;
    }
    r->ran = true;
    r->how = how;
    r->mode = mode;
    r->gens = g;
    r->us = esp_timer_get_time() - t0;
    r->pop = pops[g & (PHIST - 1)];
    if (how != END_TAP) vTaskDelay(pdMS_TO_TICKS(1500));          // let the final field be seen
}

// ===========================================================================
// Title screen
// ===========================================================================
#define BTN_W 216
#define BTN_H 48
static const int BTN_X[2] = { 16, 248 };
static const int BTN_Y[3] = { 62, 120, 178 };

static void btn_pos(int i, int *x, int *y) { *x = BTN_X[i & 1]; *y = BTN_Y[i >> 1]; }

static void show_title(const result_t *r)
{
    memset(fb, 0, LCD_W * LCD_H * 2);
    fill_rect(0, 0, LCD_W, 36, rgb565(110, 255, 120));
    draw_text_c(LCD_W / 2, 2, "Conway's Game of Life", 2, rgb565(0, 0, 0));
    for (int i = 0; i < NMODES; i++) {
        int x, y;
        btn_pos(i, &x, &y);
        fill_rect(x, y, BTN_W, BTN_H, rgb565(110, 255, 120));
        fill_rect(x + 2, y + 2, BTN_W - 4, BTN_H - 4, rgb565(24, 40, 28));
        draw_text_c(x + BTN_W / 2, y + 8, mode_name[i], 2, rgb565(255, 255, 255));
    }
    if (r->ran) {
        char l1[64], l2[64];
        snprintf(l1, sizeof l1, "%s: %s", mode_name[r->mode], r->how == END_STABLE ? "stable" : r->how == END_PERIODIC ? "periodic" : "stopped");
        double per_s = r->us ? r->gens * 1e6 / (double)r->us : 0;
        snprintf(l2, sizeof l2, "%u gens, %.1f/s, pop %u", (unsigned)r->gens, per_s, (unsigned)r->pop);
        draw_text_c(LCD_W / 2, 244, l1, 1, rgb565(255, 255, 255));
        draw_text_c(LCD_W / 2, 264, l2, 1, rgb565(255, 255, 255));
    }
    draw_text_c(LCD_W / 2, 296, "Tap during a run to stop.  Reset = launcher menu", 1, rgb565(150, 150, 160));
    flush();
}

// Which mode button (0..NMODES-1) is at the point, or -1.
static int button_at(int px, int py)
{
    for (int i = 0; i < NMODES; i++) {
        int x, y;
        btn_pos(i, &x, &y);
        if (px >= x && px < x + BTN_W && py >= y && py < y + BTN_H) return i;
    }
    return -1;
}

void app_main(void)
{
    for (int i = 0; i < 3; i++) {
        grid[i] = heap_caps_malloc(W * H, MALLOC_CAP_SPIRAM);
        assert(grid[i]);
    }
    shown = heap_caps_malloc(W * H, MALLOC_CAP_SPIRAM);
    assert(shown);
    palette_init();
    display_init();
    touch_init();
    result_t res = { 0 };
    while (1) {
        show_title(&res);
        int mode = -1;
        while (mode < 0) {
            int x, y;
            wait_tap(&x, &y);
            mode = button_at(x, y);
        }
        ESP_LOGI(TAG, "mode %s", mode_name[mode]);
        play(mode, &res);
        if (res.gens) ESP_LOGI(TAG, "per generation: step %.2f ms, screen update %.2f ms", step_us / 1000.0 / res.gens, render_us / 1000.0 / res.gens);
        ESP_LOGI(TAG, "%s: end %d, %u gens, %.1f/s, pop %u", mode_name[mode], res.how, (unsigned)res.gens,
                 res.us ? res.gens * 1e6 / (double)res.us : 0, (unsigned)res.pop);
    }
}
