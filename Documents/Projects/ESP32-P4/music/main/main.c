// Music player for the Waveshare ESP32-P4 3.5" board (a launcher app).
//
// Albums are the folders in /Music on the SD card; each album's .mp3 files play in track order (natural sort, so 2 comes
// before 10). Screen 1 is a touch list of albums (same look as the launcher); screen 2 is the player with play/pause,
// next track, next album and volume. Music keeps playing while you go back to the album list. At the end of an album
// playback stops. Reset returns to the launcher.
//
// Decoding: minimp3 (CC0) in its own task on core 1. Audio: ES8311 codec at I2C 0x18 fed by I2S0 (MCLK13 BCLK12 WS10
// DOUT9), speaker amp enable on GPIO53. Track titles come from the ID3v2 TIT2 tag, else the file name.
//
// Display: ST7796 480x320 on SPI (MOSI20 SCLK21 CS23 DC26 RST27 BL28).  Touch: FT6336 at I2C 0x38 (SDA7 SCL8 RST29).
// SD card: SDMMC slot 0, 4-bit, CLK43 CMD44 D0-3 39-42, supplied by on-chip LDO4.
#include <ctype.h>
#include <dirent.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "driver/gpio.h"
#include "driver/i2c_master.h"
#include "driver/i2s_std.h"
#include "driver/ledc.h"
#include "driver/sdmmc_host.h"
#include "driver/spi_master.h"
#include "esp_heap_caps.h"
#include "esp_lcd_panel_io.h"
#include "esp_log.h"
#include "esp_timer.h"
#include "esp_vfs_fat.h"
#include "freertos/FreeRTOS.h"
#include "freertos/semphr.h"
#include "freertos/task.h"
#include "sd_pwr_ctrl_by_on_chip_ldo.h"
#include "sdmmc_cmd.h"
#include "font_8x16.h"
#define MINIMP3_IMPLEMENTATION
#define MINIMP3_ONLY_MP3
#include "minimp3.h"

static const char *TAG = "music";

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
#define PIN_PA 53
#define SPI_MHZ 80
#define CHUNK_PX 12800                      // 25,600 bytes per SPI transfer (a multiple of the cache line)
#define BACKLIGHT_PCT 60

#define MUSIC_DIR "/sdcard/Music"
#define VOL_MAX 10
#define VOL_DEFAULT 7

#define RGB(r, g, b) ((uint16_t)((((((r) & 0xF8) << 8) | (((g) & 0xFC) << 3) | ((b) >> 3)) << 8) | ((((r) & 0xF8) << 8 | (((g) & 0xFC) << 3) | ((b) >> 3)) >> 8)))
#define C_BG RGB(12, 12, 16)
#define C_HEAD RGB(30, 30, 38)
#define C_TEXT RGB(225, 225, 225)
#define C_DIM RGB(140, 140, 152)
#define C_LINE RGB(44, 44, 54)
#define C_SEL_BG RGB(58, 30, 6)
#define C_ACCENT RGB(255, 140, 0)
#define C_BTN_OFF RGB(60, 60, 68)
#define C_BTN_TEXT RGB(12, 12, 16)
#define C_ERR RGB(255, 90, 80)

// ===========================================================================
// Display
// ===========================================================================
static esp_lcd_panel_io_handle_t lcd_io;
static SemaphoreHandle_t lcd_done;
static uint16_t *fb;                        // LCD_W x LCD_H, byte-swapped RGB565
static int clip_x0, clip_y0 = 0, clip_x1 = LCD_W, clip_y1 = LCD_H;

static bool on_done(esp_lcd_panel_io_handle_t io, esp_lcd_panel_io_event_data_t *e, void *u)
{
    BaseType_t w = pdFALSE;
    xSemaphoreGiveFromISR(lcd_done, &w);
    return w == pdTRUE;
}

static void cmd(uint8_t c, const uint8_t *p, size_t n) { ESP_ERROR_CHECK(esp_lcd_panel_io_tx_param(lcd_io, c, p, n)); }

static void flush(void)
{
    cmd(0x2A, (uint8_t[]){ 0, 0, (LCD_W - 1) >> 8, (LCD_W - 1) & 0xFF }, 4);
    cmd(0x2B, (uint8_t[]){ 0, 0, (LCD_H - 1) >> 8, (LCD_H - 1) & 0xFF }, 4);
    int total = LCD_W * LCD_H, sent = 0, chunks = 0;
    while (sent < total) {
        int n = total - sent < CHUNK_PX ? total - sent : CHUNK_PX;
        ESP_ERROR_CHECK(esp_lcd_panel_io_tx_color(lcd_io, sent ? 0x3C : 0x2C, fb + sent, n * 2));
        sent += n;
        chunks++;
    }
    while (chunks--) xSemaphoreTake(lcd_done, portMAX_DELAY);
}

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

static void rect(int x, int y, int w, int h, uint16_t c)
{
    int x0 = x < clip_x0 ? clip_x0 : x, y0 = y < clip_y0 ? clip_y0 : y;
    int x1 = x + w > clip_x1 ? clip_x1 : x + w, y1 = y + h > clip_y1 ? clip_y1 : y + h;
    for (int yy = y0; yy < y1; yy++)
        for (int xx = x0; xx < x1; xx++) fb[yy * LCD_W + xx] = c;
}

static void glyph(int x, int y, unsigned char ch, uint16_t c, int s)
{
    const unsigned char *g = fontdata_8x16 + ch * 16;
    for (int r = 0; r < 16; r++)
        for (int col = 0; col < 8; col++)
            if (g[r] & (0x80 >> col)) rect(x + col * s, y + r * s, s, s, c);
}

static int text_w(const char *t, int s) { return (int)strlen(t) * 8 * s; }

static void text(int x, int y, const char *t, uint16_t c, int s)
{
    for (; *t; t++, x += 8 * s) glyph(x, y, (unsigned char)*t, c, s);
}

static void text_fit(int x, int y, const char *t, uint16_t c, int s, int max_w)
{
    char b[128];
    int n = max_w / (8 * s);
    if (n > (int)sizeof b - 1) n = sizeof b - 1;
    snprintf(b, sizeof b, "%.*s", n, t);
    text(x, y, b, c, s);
}

// Wrapped text: breaks at a space when there is one, at most max_lines lines (the last is cut with "..." if the text goes on).
// Returns the number of lines drawn.
static int text_wrap(int x, int y, const char *t, uint16_t c, int s, int max_w, int max_lines)
{
    int per = max_w / (8 * s), lines = 0;
    if (per > 100) per = 100;
    size_t len = strlen(t);
    while (len > 0 && lines < max_lines) {
        char b[104];
        size_t n = len <= (size_t)per ? len : (size_t)per;
        if (len > (size_t)per) {
            size_t sp = n;
            while (sp > 0 && t[sp] != ' ') sp--;               // last space at or before the limit
            if (sp > (size_t)per / 2) n = sp;
            if (lines == max_lines - 1) {                      // last line: cut with an ellipsis
                n = (size_t)per;
                snprintf(b, sizeof b, "%.*s...", (int)n - 3, t);
                text(x, y + lines * 16 * s, b, c, s);
                return lines + 1;
            }
        }
        snprintf(b, sizeof b, "%.*s", (int)n, t);
        text(x, y + lines * 16 * s, b, c, s);
        lines++;
        t += n; len -= n;
        while (len > 0 && *t == ' ') { t++; len--; }
    }
    return lines;
}

static void set_clip(int x0, int y0, int x1, int y1) { clip_x0 = x0; clip_y0 = y0; clip_x1 = x1; clip_y1 = y1; }

// right-pointing triangle with its left edge at x, vertical center cy
static void tri(int x, int cy, int w, int h, uint16_t c)
{
    for (int col = 0; col < w; col++) {
        int hh = (h / 2) * (w - col) / w;
        rect(x + col, cy - hh, 1, 2 * hh + 1, c);
    }
}

// ---- text: UTF-8 / Latin-1 / UTF-16 -> the 8x16 font's code page (CP437), '?' where it has no glyph
static void put_cp(char *out, size_t on, size_t *o, uint32_t cp)
{
    char tmp[4];
    int n = 1;
    if (cp < 0x80) tmp[0] = (char)cp;
    else switch (cp) {
    case 0x2026: memcpy(tmp, "...", 3); n = 3; break;
    case 0x2018: case 0x2019: case 0x2032: tmp[0] = '\''; break;
    case 0x201C: case 0x201D: tmp[0] = '"'; break;
    case 0x2013: case 0x2014: tmp[0] = '-'; break;
    case 0xE9: tmp[0] = (char)0x82; break;  case 0xE8: tmp[0] = (char)0x8A; break;  case 0xEA: tmp[0] = (char)0x88; break;
    case 0xEB: tmp[0] = (char)0x89; break;  case 0xE0: tmp[0] = (char)0x85; break;  case 0xE2: tmp[0] = (char)0x83; break;
    case 0xE4: tmp[0] = (char)0x84; break;  case 0xE5: tmp[0] = (char)0x86; break;  case 0xE7: tmp[0] = (char)0x87; break;
    case 0xFC: tmp[0] = (char)0x81; break;  case 0xF6: tmp[0] = (char)0x94; break;  case 0xF1: tmp[0] = (char)0xA4; break;
    case 0xEF: tmp[0] = (char)0x8B; break;  case 0xEE: tmp[0] = (char)0x8C; break;  case 0xF4: tmp[0] = (char)0x93; break;
    case 0xFB: tmp[0] = (char)0x96; break;  case 0xF9: tmp[0] = (char)0x97; break;  case 0xC9: tmp[0] = (char)0x90; break;
    case 0xC4: tmp[0] = (char)0x8E; break;  case 0xC5: tmp[0] = (char)0x8F; break;  case 0xD6: tmp[0] = (char)0x99; break;
    case 0xDC: tmp[0] = (char)0x9A; break;  case 0xDF: tmp[0] = (char)0xE1; break;  case 0xE1: tmp[0] = (char)0xA0; break;
    case 0xED: tmp[0] = (char)0xA1; break;  case 0xF3: tmp[0] = (char)0xA2; break;  case 0xFA: tmp[0] = (char)0xA3; break;
    case 0xD1: tmp[0] = (char)0xA5; break;  case 0xE6: tmp[0] = (char)0x91; break;  case 0xC6: tmp[0] = (char)0x92; break;
    case 0xA1: tmp[0] = (char)0xAD; break;  case 0xBF: tmp[0] = (char)0xA8; break;  case 0xB0: tmp[0] = (char)0xF8; break;
    default: tmp[0] = '?'; break;
    }
    for (int i = 0; i < n && *o + 1 < on; i++) out[(*o)++] = tmp[i];
}

static void utf8_to_disp(const char *in, char *out, size_t on)
{
    size_t o = 0;
    const unsigned char *p = (const unsigned char *)in;
    while (*p && o + 1 < on) {
        uint32_t cp;
        int extra;
        if (*p < 0x80) { cp = *p; extra = 0; }
        else if ((*p & 0xE0) == 0xC0) { cp = *p & 0x1F; extra = 1; }
        else if ((*p & 0xF0) == 0xE0) { cp = *p & 0x0F; extra = 2; }
        else if ((*p & 0xF8) == 0xF0) { cp = *p & 0x07; extra = 3; }
        else { cp = '?'; extra = 0; }
        p++;
        while (extra-- > 0 && (*p & 0xC0) == 0x80) cp = (cp << 6) | (*p++ & 0x3F);
        put_cp(out, on, &o, cp);
    }
    out[o] = 0;
}

// ===========================================================================
// I2C bus (touch + codec), touch
// ===========================================================================
static i2c_master_bus_handle_t i2c_bus;
static i2c_master_dev_handle_t touch_dev, es_dev;

static void touch_init(void)
{
    gpio_config_t o = { .pin_bit_mask = 1ULL << PIN_TP_RST, .mode = GPIO_MODE_OUTPUT };
    gpio_config(&o);
    gpio_set_level(PIN_TP_RST, 0);                 // reset is held low by a resistor until driven
    vTaskDelay(pdMS_TO_TICKS(20));
    gpio_set_level(PIN_TP_RST, 1);
    vTaskDelay(pdMS_TO_TICKS(300));
    i2c_device_config_t dc = { .dev_addr_length = I2C_ADDR_BIT_LEN_7, .device_address = 0x38, .scl_speed_hz = 400000 };
    ESP_ERROR_CHECK(i2c_master_bus_add_device(i2c_bus, &dc, &touch_dev));
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

// ===========================================================================
// SD card, albums
// ===========================================================================
static bool sd_mount(void)
{
    sd_pwr_ctrl_handle_t pwr = NULL;
    sd_pwr_ctrl_ldo_config_t ldo = { .ldo_chan_id = 4 };
    if (sd_pwr_ctrl_new_on_chip_ldo(&ldo, &pwr) != ESP_OK) return false;
    sdmmc_host_t host = SDMMC_HOST_DEFAULT();
    host.slot = SDMMC_HOST_SLOT_0;
    host.max_freq_khz = SDMMC_FREQ_HIGHSPEED;
    host.pwr_ctrl_handle = pwr;
    sdmmc_slot_config_t slot = SDMMC_SLOT_CONFIG_DEFAULT();
    slot.width = 4;
    slot.flags |= SDMMC_SLOT_FLAG_INTERNAL_PULLUP;
    slot.clk = 43; slot.cmd = 44; slot.d0 = 39; slot.d1 = 40; slot.d2 = 41; slot.d3 = 42;
    esp_vfs_fat_sdmmc_mount_config_t mc = { .format_if_mount_failed = false, .max_files = 4, .allocation_unit_size = 16 * 1024 };
    sdmmc_card_t *card;
    esp_err_t r = esp_vfs_fat_sdmmc_mount("/sdcard", &host, &slot, &mc, &card);
    if (r != ESP_OK) { ESP_LOGW(TAG, "SD mount failed: %s", esp_err_to_name(r)); return false; }
    return true;
}

static bool is_mp3(const char *n)
{
    size_t l = strlen(n);
    return l > 4 && n[0] != '.' && strcasecmp(n + l - 4, ".mp3") == 0;
}

// track numbers sort numerically ("2 x" before "10 x"), everything else case-insensitively
static int natcmp(const char *a, const char *b)
{
    while (*a && *b) {
        if (isdigit((unsigned char)*a) && isdigit((unsigned char)*b)) {
            const char *pa = a, *pb = b;
            while (*pa == '0') pa++;
            while (*pb == '0') pb++;
            int la = 0, lb = 0;
            while (isdigit((unsigned char)pa[la])) la++;
            while (isdigit((unsigned char)pb[lb])) lb++;
            if (la != lb) return la - lb;
            int c = strncmp(pa, pb, la);
            if (c) return c;
            a = pa + la; b = pb + lb;
            continue;
        }
        int ca = tolower((unsigned char)*a), cb = tolower((unsigned char)*b);
        if (ca != cb) return ca - cb;
        a++; b++;
    }
    return (unsigned char)*a - (unsigned char)*b;
}

static int by_natural(const void *a, const void *b) { return natcmp(*(char *const *)a, *(char *const *)b); }

typedef struct { char *name; int ntracks; } album_t;
static album_t *albums;
static int nalbums;

static int by_album(const void *a, const void *b) { return natcmp(((const album_t *)a)->name, ((const album_t *)b)->name); }

static void scan_albums(void)
{
    DIR *d = opendir(MUSIC_DIR);
    if (!d) return;
    int cap = 0;
    struct dirent *de;
    while ((de = readdir(d))) {
        if (de->d_type != DT_DIR || de->d_name[0] == '.') continue;
        char path[300];
        snprintf(path, sizeof path, MUSIC_DIR "/%s", de->d_name);
        DIR *a = opendir(path);
        if (!a) continue;
        int n = 0;
        struct dirent *e;
        while ((e = readdir(a))) if (e->d_type != DT_DIR && is_mp3(e->d_name)) n++;
        closedir(a);
        if (!n) continue;
        if (nalbums == cap) { cap = cap ? cap * 2 : 16; albums = realloc(albums, cap * sizeof *albums); }
        albums[nalbums].name = strdup(de->d_name);
        albums[nalbums].ntracks = n;
        nalbums++;
    }
    closedir(d);
    qsort(albums, nalbums, sizeof *albums, by_album);
}

// ===========================================================================
// Audio hardware
// ===========================================================================
static i2s_chan_handle_t tx;
static uint32_t cur_rate;
static int volume = VOL_DEFAULT;

static void es_w(uint8_t reg, uint8_t val)
{
    uint8_t b[2] = { reg, val };
    esp_err_t r = i2c_master_transmit(es_dev, b, 2, 100);
    if (r != ESP_OK) ESP_LOGW(TAG, "codec write %02x failed: %s", reg, esp_err_to_name(r));
}

static i2s_std_clk_config_t clk_for(uint32_t rate)
{
    i2s_std_clk_config_t c = I2S_STD_CLK_DEFAULT_CONFIG(rate);
    c.mclk_multiple = I2S_MCLK_MULTIPLE_256;
    return c;
}

static void audio_init(void)
{
    i2c_device_config_t dc = { .dev_addr_length = I2C_ADDR_BIT_LEN_7, .device_address = 0x18, .scl_speed_hz = 100000 };
    ESP_ERROR_CHECK(i2c_master_bus_add_device(i2c_bus, &dc, &es_dev));

    // I2S first so MCLK is running before the codec is configured
    i2s_chan_config_t cc = I2S_CHANNEL_DEFAULT_CONFIG(I2S_NUM_0, I2S_ROLE_MASTER);
    cc.auto_clear = true;
    ESP_ERROR_CHECK(i2s_new_channel(&cc, &tx, NULL));
    cur_rate = 44100;
    i2s_std_config_t sc = {
        .clk_cfg = clk_for(cur_rate),
        .slot_cfg = I2S_STD_PHILIPS_SLOT_DEFAULT_CONFIG(I2S_DATA_BIT_WIDTH_16BIT, I2S_SLOT_MODE_STEREO),
        .gpio_cfg = { .mclk = 13, .bclk = 12, .ws = 10, .dout = 9, .din = I2S_GPIO_UNUSED },
    };
    ESP_ERROR_CHECK(i2s_channel_init_std_mode(tx, &sc));
    ESP_ERROR_CHECK(i2s_channel_enable(tx));
    vTaskDelay(pdMS_TO_TICKS(50));

    // ES8311 init (256*fs MCLK from pin, I2S 16-bit slave, DAC only)
    es_w(0x44, 0x08); es_w(0x44, 0x08);
    es_w(0x01, 0x30); es_w(0x02, 0x00); es_w(0x03, 0x10); es_w(0x16, 0x24);
    es_w(0x04, 0x10); es_w(0x05, 0x00); es_w(0x0B, 0x00); es_w(0x0C, 0x00);
    es_w(0x10, 0x1F); es_w(0x11, 0x7F);
    es_w(0x00, 0x80);                 // power on, slave mode
    es_w(0x01, 0x3F);                 // all clocks on, MCLK from pin
    es_w(0x02, 0x00); es_w(0x05, 0x00);
    es_w(0x03, 0x10); es_w(0x04, 0x10);
    es_w(0x07, 0x00); es_w(0x08, 0xFF);
    es_w(0x06, 0x03);                 // BCLK = MCLK/4
    es_w(0x09, 0x0C); es_w(0x0A, 0x0C); // I2S, 16-bit
    es_w(0x0D, 0x01); es_w(0x0E, 0x02); es_w(0x12, 0x00); es_w(0x13, 0x10);
    es_w(0x1C, 0x6A); es_w(0x37, 0x08);
    es_w(0x31, 0x00);                 // unmute DAC

    gpio_config_t g = { .pin_bit_mask = 1ULL << PIN_PA, .mode = GPIO_MODE_OUTPUT };
    ESP_ERROR_CHECK(gpio_config(&g));
    gpio_set_level(PIN_PA, 1);        // speaker amp on
}

// 0..VOL_MAX in 3 dB steps: 10 = 0 dB (register 0xBF), 1 = -27 dB, 0 = off
static void set_volume(int v)
{
    if (v < 0) v = 0;
    if (v > VOL_MAX) v = VOL_MAX;
    volume = v;
    es_w(0x32, v == 0 ? 0x00 : (uint8_t)(0xBF - 6 * (VOL_MAX - v)));
}

static void set_rate(uint32_t rate)
{
    if (rate == cur_rate) return;
    i2s_std_clk_config_t c = clk_for(rate);
    ESP_ERROR_CHECK(i2s_channel_disable(tx));
    ESP_ERROR_CHECK(i2s_channel_reconfig_std_clock(tx, &c));
    ESP_ERROR_CHECK(i2s_channel_enable(tx));
    cur_rate = rate;
}

// ===========================================================================
// Player task
// ===========================================================================
typedef struct {
    volatile int album;             // album playing or last played
    volatile int track, ntracks;    // 0-based track, count
    volatile int active;            // 1 while an album is playing or paused, 0 when idle or finished
    volatile int finished;          // 1 after the last track ended
    volatile uint32_t elapsed_s;
    volatile int progress;          // 0..1000 through the current file
    volatile uint32_t rev;          // bumped whenever the title changes
    char title[80];
} pstate_t;

static pstate_t st = { .album = -1 };
static SemaphoreHandle_t title_mux;
static volatile int req_album = -1;     // UI -> task: start this album
static volatile int req_skip;           // UI -> task: go to the next track
static volatile int paused;

static char **tracks;
static int ntracks;

static void load_tracks(int album)
{
    for (int i = 0; i < ntracks; i++) free(tracks[i]);
    free(tracks);
    tracks = NULL; ntracks = 0;
    char path[300];
    snprintf(path, sizeof path, MUSIC_DIR "/%s", albums[album].name);
    DIR *d = opendir(path);
    if (!d) return;
    int cap = 0;
    struct dirent *de;
    while ((de = readdir(d))) {
        if (de->d_type == DT_DIR || !is_mp3(de->d_name)) continue;
        if (ntracks == cap) { cap = cap ? cap * 2 : 32; tracks = realloc(tracks, cap * sizeof *tracks); }
        tracks[ntracks++] = strdup(de->d_name);
    }
    closedir(d);
    qsort(tracks, ntracks, sizeof *tracks, by_natural);
}

static void set_title(const char *t)
{
    xSemaphoreTake(title_mux, portMAX_DELAY);
    snprintf(st.title, sizeof st.title, "%s", t);
    st.rev++;
    xSemaphoreGive(title_mux);
}

// ---- ID3v2: skip the tag and pull out the title (TIT2)
static uint32_t syncsafe(const uint8_t *p) { return (p[0] << 21) | (p[1] << 14) | (p[2] << 7) | p[3]; }
static uint32_t be32(const uint8_t *p) { return ((uint32_t)p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3]; }

static void id3_text(const uint8_t *d, int n, char *out, size_t on)
{
    size_t o = 0;
    out[0] = 0;
    if (n < 2) return;
    int enc = d[0];
    const uint8_t *p = d + 1;
    n--;
    if (enc == 0) {
        for (int i = 0; i < n && p[i]; i++) put_cp(out, on, &o, p[i]);
    } else if (enc == 3) {
        char tmp[260];
        snprintf(tmp, sizeof tmp, "%.*s", n, (const char *)p);
        utf8_to_disp(tmp, out, on);
        return;
    } else {                                           // UTF-16, with or without BOM
        bool le = true;
        if (enc == 1 && n >= 2) {
            if (p[0] == 0xFE && p[1] == 0xFF) { le = false; p += 2; n -= 2; }
            else if (p[0] == 0xFF && p[1] == 0xFE) { p += 2; n -= 2; }
        } else if (enc == 2) le = false;
        for (int i = 0; i + 1 < n; i += 2) {
            uint32_t cp = le ? p[i] | (p[i + 1] << 8) : (p[i] << 8) | p[i + 1];
            if (!cp) break;
            put_cp(out, on, &o, cp >= 0xD800 && cp < 0xE000 ? '?' : cp);
        }
    }
    out[o] = 0;
}

// Returns the offset of the first byte after the tag (0 if none) and leaves the file positioned there.
static long id3_read(FILE *f, char *title, size_t tn)
{
    uint8_t h[10];
    title[0] = 0;
    if (fread(h, 1, 10, f) != 10 || memcmp(h, "ID3", 3)) { fseek(f, 0, SEEK_SET); return 0; }
    int ver = h[3];
    long end = 10 + (long)syncsafe(h + 6) + ((h[5] & 0x10) ? 10 : 0);
    long pos = 10;
    if (ver >= 3 && (h[5] & 0x40)) {                    // extended header
        uint8_t e[4];
        if (fread(e, 1, 4, f) == 4) pos += ver == 4 ? syncsafe(e) : be32(e) + 4;
    }
    while (ver >= 3 && pos + 10 <= end) {
        uint8_t fh[10];
        if (fseek(f, pos, SEEK_SET) || fread(fh, 1, 10, f) != 10 || fh[0] == 0) break;
        uint32_t fs = ver == 4 ? syncsafe(fh + 4) : be32(fh + 4);
        if (fs == 0 || pos + 10 + (long)fs > end) break;
        if (!memcmp(fh, "TIT2", 4) && !title[0]) {
            uint8_t buf[256];
            int n = fs < sizeof buf ? (int)fs : (int)sizeof buf;
            if (fread(buf, 1, n, f) == (size_t)n) id3_text(buf, n, title, tn);
        }
        pos += 10 + fs;
    }
    fseek(f, end, SEEK_SET);
    return end;
}

typedef enum { END_EOF, END_SKIP, END_NEWREQ, END_ERROR } end_t;

static mp3dec_t dec;
static uint8_t inbuf[16 * 1024];
static mp3d_sample_t pcm[MINIMP3_MAX_SAMPLES_PER_FRAME];
static int16_t stereo[MINIMP3_MAX_SAMPLES_PER_FRAME * 2];

static end_t play_file(const char *name, const char *path)
{
    FILE *f = fopen(path, "rb");
    if (!f) { ESP_LOGW(TAG, "cannot open %s", path); return END_ERROR; }
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);
    char title[80];
    long consumed = id3_read(f, title, sizeof title);
    if (!title[0]) {                                   // no tag: the file name without ".mp3"
        char base[300];
        snprintf(base, sizeof base, "%.*s", (int)strlen(name) - 4, name);
        utf8_to_disp(base, title, sizeof title);
    }
    set_title(title);
    ESP_LOGI(TAG, "playing %s (%s)", name, title);

    mp3dec_init(&dec);
    int have = 0, pos = 0;
    bool eof = false;
    uint64_t samples = 0;
    uint32_t rate = 0;
    end_t result = END_EOF;
    st.elapsed_s = 0;
    st.progress = 0;
    for (;;) {
        if (req_album >= 0) { result = END_NEWREQ; break; }
        if (req_skip) { result = END_SKIP; break; }
        if (paused) {
            es_w(0x31, 0x60);                          // mute the DAC while paused (no click, no hiss)
            while (paused && req_album < 0 && !req_skip) vTaskDelay(pdMS_TO_TICKS(20));
            es_w(0x31, 0x00);
            continue;
        }
        if (have < 6144 && !eof) {                     // keep the input buffer topped up
            memmove(inbuf, inbuf + pos, have);
            pos = 0;
            size_t n = fread(inbuf + have, 1, sizeof inbuf - have, f);
            if (n == 0) eof = true;
            have += n;
        }
        if (have == 0) break;
        mp3dec_frame_info_t info;
        int ns = mp3dec_decode_frame(&dec, inbuf + pos, have, pcm, &info);
        if (info.frame_bytes == 0) {                   // not enough data for a frame: the end of the file
            if (eof) break;
            have = 0;
            continue;
        }
        pos += info.frame_bytes;
        have -= info.frame_bytes;
        consumed += info.frame_bytes;
        if (ns == 0) continue;                         // skipped junk
        if (info.hz != rate) { rate = info.hz; set_rate(rate); }
        const int16_t *out = pcm;
        if (info.channels == 1) {
            for (int i = 0; i < ns; i++) stereo[2 * i] = stereo[2 * i + 1] = pcm[i];
            out = stereo;
        }
        size_t w;
        i2s_channel_write(tx, out, ns * 2 * sizeof(int16_t), &w, portMAX_DELAY);
        samples += ns;
        st.elapsed_s = (uint32_t)(samples / rate);
        st.progress = fsize > 0 ? (int)(consumed * 1000 / fsize) : 0;
    }
    fclose(f);
    return result;
}

static void player_task(void *arg)
{
    int cur_album = -1;
    bool running = false;
    for (;;) {
        if (req_album >= 0) {                          // start an album
            cur_album = req_album;
            req_album = -1;
            req_skip = 0;
            paused = 0;
            load_tracks(cur_album);
            st.album = cur_album;
            st.ntracks = ntracks;
            st.track = 0;
            st.finished = 0;
            running = ntracks > 0;
            st.active = running;
            ESP_LOGI(TAG, "album '%s': %d track(s)", albums[cur_album].name, ntracks);
        }
        if (!running) { vTaskDelay(pdMS_TO_TICKS(20)); continue; }

        char path[600];
        snprintf(path, sizeof path, MUSIC_DIR "/%s/%s", albums[cur_album].name, tracks[st.track]);
        end_t r = play_file(tracks[st.track], path);
        if (r == END_NEWREQ) continue;
        if (r == END_SKIP) { req_skip = 0; paused = 0; }       // next track (on the last one: the album ends)
        if (++st.track >= ntracks) {                   // last track done: stop and stand by at track 1
            st.track = 0;
            running = false;
            st.active = 0;
            st.finished = 1;
            st.elapsed_s = 0;
            st.progress = 0;
            ESP_LOGI(TAG, "album finished");
        }
    }
}

// ===========================================================================
// UI
// ===========================================================================
#define HEAD_H 36
#define FOOT_Y 264
#define LIST_Y HEAD_H
#define LISTV_H (FOOT_Y - HEAD_H)
#define ROW_H 56
#define BTN_X 318
#define BTN_Y 272
#define BTN_W 154
#define BTN_H 40

typedef struct { int x, y, w, h; } box_t;
static const box_t B_PLAY = { 20, 156, 170, 70 }, B_NEXT = { 200, 156, 130, 70 }, B_ALBUM = { 340, 156, 120, 70 };
static const box_t B_VDN = { 20, 244, 80, 56 }, B_VUP = { 380, 244, 80, 56 };
static const box_t B_ALBUMS = { 350, 0, 130, HEAD_H }, B_NOWPLAYING = { 300, 0, 180, HEAD_H };

static bool hit(const box_t *b, int x, int y) { return x >= b->x && x < b->x + b->w && y >= b->y && y < b->y + b->h; }

typedef enum { SCREEN_LIST, SCREEN_PLAYER } screen_t;
static screen_t screen = SCREEN_LIST;
static int sel;
static float scroll_px;

static float max_scroll(void)
{
    float m = nalbums * ROW_H - LISTV_H;
    return m > 0 ? m : 0;
}

static void clamp_scroll(void)
{
    float m = max_scroll();
    if (scroll_px < 0) scroll_px = 0;
    if (scroll_px > m) scroll_px = m;
}

static void draw_list(void)
{
    set_clip(0, 0, LCD_W, LCD_H);
    rect(0, 0, LCD_W, LCD_H, C_BG);
    set_clip(0, LIST_Y, LCD_W, FOOT_Y);
    for (int i = 0; i < nalbums; i++) {
        int y = LIST_Y + i * ROW_H - (int)scroll_px;
        if (y + ROW_H <= LIST_Y || y >= FOOT_Y) continue;
        if (i == sel) { rect(0, y, LCD_W, ROW_H, C_SEL_BG); rect(0, y, 6, ROW_H, C_ACCENT); }
        char nm[128], sub[40];
        utf8_to_disp(albums[i].name, nm, sizeof nm);
        snprintf(sub, sizeof sub, "%d track%s%s", albums[i].ntracks, albums[i].ntracks == 1 ? "" : "s",
                 st.active && st.album == i ? "   (playing)" : "");
        text_wrap(18, y + 6, nm, C_TEXT, 1, LCD_W - 40, 2);
        text_fit(18, y + 38, sub, C_DIM, 1, LCD_W - 40);
        rect(0, y + ROW_H - 1, LCD_W, 1, C_LINE);
    }
    float m = max_scroll();
    if (m > 0) {
        int bh = (int)(LISTV_H * (float)LISTV_H / (nalbums * ROW_H));
        int by = LIST_Y + (int)((LISTV_H - bh) * scroll_px / m);
        rect(LCD_W - 5, by, 4, bh, C_DIM);
    }
    set_clip(0, 0, LCD_W, LCD_H);
    rect(0, 0, LCD_W, HEAD_H, C_HEAD);
    text(10, 10, "Music: pick an album", C_TEXT, 1);
    if (st.active) text(LCD_W - text_w("NOW PLAYING >", 1) - 10, 10, "NOW PLAYING >", C_ACCENT, 1);
    rect(0, HEAD_H - 1, LCD_W, 1, C_LINE);
    rect(0, FOOT_Y, LCD_W, LCD_H - FOOT_Y, C_HEAD);
    rect(0, FOOT_Y, LCD_W, 1, C_LINE);
    text_fit(10, FOOT_Y + 6, "Tap an album, then Play", C_DIM, 1, BTN_X - 20);
    if (sel >= 0 && sel < nalbums) {
        char nm[128];
        utf8_to_disp(albums[sel].name, nm, sizeof nm);
        text_wrap(10, FOOT_Y + 26, nm, C_TEXT, 1, BTN_X - 20, 2);
    }
    rect(BTN_X, BTN_Y, BTN_W, BTN_H, C_ACCENT);
    text(BTN_X + (BTN_W - text_w("PLAY", 2)) / 2, BTN_Y + 4, "PLAY", C_BTN_TEXT, 2);
}

static void box_fill(const box_t *b, uint16_t c) { rect(b->x, b->y, b->w, b->h, c); }

static void draw_player(void)
{
    set_clip(0, 0, LCD_W, LCD_H);
    rect(0, 0, LCD_W, LCD_H, C_BG);
    rect(0, 0, LCD_W, HEAD_H, C_HEAD);
    rect(0, HEAD_H - 1, LCD_W, 1, C_LINE);
    text(10, 10, "Music", C_TEXT, 1);
    text(B_ALBUMS.x + (B_ALBUMS.w - text_w("< ALBUMS", 1)) / 2, 10, "< ALBUMS", C_ACCENT, 1);

    char nm[128], title[80];
    if (st.album >= 0 && st.album < nalbums) utf8_to_disp(albums[st.album].name, nm, sizeof nm); else nm[0] = 0;
    xSemaphoreTake(title_mux, portMAX_DELAY);
    snprintf(title, sizeof title, "%s", st.title);
    xSemaphoreGive(title_mux);
    text_wrap(20, 46, nm, C_DIM, 1, 440, 1);
    if (st.finished) text(20, 70, "Album finished", C_TEXT, 2);
    else if (text_w(title, 2) <= 440) text(20, 70, title, C_TEXT, 2);     // short titles big
    else text_wrap(20, 68, title, C_TEXT, 1, 440, 2);                      // long ones normal size, two lines

    char b[48];
    snprintf(b, sizeof b, "Track %d of %d", st.track + 1, st.ntracks);
    text(20, 108, b, C_DIM, 1);
    snprintf(b, sizeof b, "%u:%02u", (unsigned)(st.elapsed_s / 60), (unsigned)(st.elapsed_s % 60));
    text(460 - text_w(b, 1), 108, b, C_DIM, 1);
    rect(20, 130, 440, 8, C_LINE);
    rect(20, 130, 440 * st.progress / 1000, 8, C_ACCENT);

    // play/pause
    box_fill(&B_PLAY, C_ACCENT);
    int cx = B_PLAY.x + B_PLAY.w / 2, cy = B_PLAY.y + B_PLAY.h / 2;
    if (!st.active || paused) tri(cx - 12, cy, 28, 40, C_BTN_TEXT);
    else { rect(cx - 16, cy - 20, 11, 40, C_BTN_TEXT); rect(cx + 5, cy - 20, 11, 40, C_BTN_TEXT); }
    // next track
    box_fill(&B_NEXT, C_HEAD);
    cx = B_NEXT.x + B_NEXT.w / 2; cy = B_NEXT.y + B_NEXT.h / 2;
    tri(cx - 20, cy, 26, 36, C_TEXT);
    rect(cx + 10, cy - 18, 8, 37, C_TEXT);
    // next album
    box_fill(&B_ALBUM, C_HEAD);
    cx = B_ALBUM.x + B_ALBUM.w / 2; cy = B_ALBUM.y + B_ALBUM.h / 2;
    tri(cx - 28, cy, 22, 36, C_TEXT);
    tri(cx - 6, cy, 22, 36, C_TEXT);
    rect(cx + 20, cy - 18, 8, 37, C_TEXT);
    text(B_NEXT.x + (B_NEXT.w - text_w("TRACK", 1)) / 2, B_NEXT.y + B_NEXT.h - 17, "TRACK", C_DIM, 1);
    text(B_ALBUM.x + (B_ALBUM.w - text_w("ALBUM", 1)) / 2, B_ALBUM.y + B_ALBUM.h - 17, "ALBUM", C_DIM, 1);

    // volume
    box_fill(&B_VDN, C_HEAD);
    rect(B_VDN.x + 26, B_VDN.y + B_VDN.h / 2 - 3, 28, 6, C_TEXT);
    box_fill(&B_VUP, C_HEAD);
    rect(B_VUP.x + 26, B_VUP.y + B_VUP.h / 2 - 3, 28, 6, C_TEXT);
    rect(B_VUP.x + B_VUP.w / 2 - 3, B_VUP.y + 14, 6, 28, C_TEXT);
    for (int i = 0; i < VOL_MAX; i++) {
        int seg_h = 14 + i * 3;
        rect(110 + i * 26, B_VDN.y + B_VDN.h - 10 - seg_h, 22, seg_h, i < volume ? C_ACCENT : C_LINE);
    }
}

static void start_album(int a)
{
    if (st.active && st.album == a) { screen = SCREEN_PLAYER; return; }     // already on: just show the player
    st.album = a;
    st.track = 0;
    st.ntracks = albums[a].ntracks;
    st.elapsed_s = 0;
    st.progress = 0;
    st.finished = 0;
    set_title("Loading...");
    paused = 0;
    req_album = a;
    screen = SCREEN_PLAYER;
}

void app_main(void)
{
    title_mux = xSemaphoreCreateMutex();
    display_init();
    i2c_master_bus_config_t bc = { .i2c_port = I2C_NUM_0, .sda_io_num = PIN_SDA, .scl_io_num = PIN_SCL,
                                   .clk_source = I2C_CLK_SRC_DEFAULT, .glitch_ignore_cnt = 7, .flags.enable_internal_pullup = true };
    ESP_ERROR_CHECK(i2c_new_master_bus(&bc, &i2c_bus));
    touch_init();
    audio_init();
    set_volume(VOL_DEFAULT);

    if (!sd_mount()) {
        set_clip(0, 0, LCD_W, LCD_H);
        rect(0, 0, LCD_W, LCD_H, C_BG);
        text(40, 140, "No SD card", C_ERR, 2);
        flush();
        vTaskDelay(portMAX_DELAY);
    }
    scan_albums();
    if (nalbums == 0) {
        set_clip(0, 0, LCD_W, LCD_H);
        rect(0, 0, LCD_W, LCD_H, C_BG);
        text(40, 120, "No albums found", C_ERR, 2);
        text(40, 160, "Put album folders of .mp3 files", C_DIM, 1);
        text(40, 180, "in /Music on the SD card", C_DIM, 1);
        flush();
        vTaskDelay(portMAX_DELAY);
    }
    ESP_LOGI(TAG, "%d album(s)", nalbums);
    xTaskCreatePinnedToCore(player_task, "player", 32768, NULL, 6, NULL, 1);   // minimp3 keeps ~12 KB of scratch on the stack

    bool was_down = false, in_list = false, dragging = false, pressed_button = false;
    int start_y = 0, last_x = 0, last_y = 0;
    float start_scroll = 0, vel = 0;
    int64_t vol_repeat_us = 0;
    // what the screen currently shows, to redraw only on change
    screen_t drawn_screen = (screen_t)-1;
    uint32_t drawn_sig = 0;
    bool dirty = true;
    while (1) {
        int x, y;
        bool down = touch_read(&x, &y);
        if (down) { last_x = x; last_y = y; }

        if (screen == SCREEN_LIST) {
            if (down && !was_down) {
                in_list = y < FOOT_Y;
                pressed_button = false;
                dragging = false;
                start_y = y;
                start_scroll = scroll_px;
                vel = 0;
                if (y < HEAD_H && st.active && hit(&B_NOWPLAYING, x, y)) { screen = SCREEN_PLAYER; dirty = true; }
                else pressed_button = !in_list && x >= BTN_X && y >= BTN_Y && y < BTN_Y + BTN_H;
            } else if (down && in_list) {
                int dy = y - start_y;
                if (!dragging && abs(dy) > 8) dragging = true;
                if (dragging) {
                    float before = scroll_px;
                    scroll_px = start_scroll - dy;
                    clamp_scroll();
                    vel = scroll_px - before;
                    dirty = true;
                }
            } else if (!down && was_down) {
                if (in_list && !dragging) {
                    int idx = (int)((last_y - LIST_Y + scroll_px) / ROW_H);
                    if (last_y >= LIST_Y && idx >= 0 && idx < nalbums) { sel = idx; dirty = true; }
                } else if (pressed_button && last_x >= BTN_X && last_y >= BTN_Y && last_y < BTN_Y + BTN_H) {
                    start_album(sel);
                    dirty = true;
                }
                if (!in_list || !dragging) vel = 0;
            }
            if (!down && fabsf(vel) > 0.5f) {
                scroll_px += vel;
                vel *= 0.92f;
                clamp_scroll();
                dirty = true;
            }
        } else {
            int64_t now = esp_timer_get_time();
            if (down && !was_down) {
                if (y < HEAD_H && hit(&B_ALBUMS, x, y)) { screen = SCREEN_LIST; dirty = true; }
                else if (hit(&B_PLAY, x, y)) {
                    if (st.active) paused = !paused;
                    else if (st.album >= 0) start_album(st.album);        // finished or idle: play the album again
                } else if (hit(&B_NEXT, x, y) && st.active) req_skip = 1;
                else if (hit(&B_ALBUM, x, y)) { st.active = 0; start_album((st.album + 1) % nalbums); }   // always a fresh start
                else if (hit(&B_VDN, x, y)) { set_volume(volume - 1); vol_repeat_us = now + 400000; }
                else if (hit(&B_VUP, x, y)) { set_volume(volume + 1); vol_repeat_us = now + 400000; }
            } else if (down && was_down && now >= vol_repeat_us && vol_repeat_us) {   // hold to ramp the volume
                if (hit(&B_VDN, x, y)) { set_volume(volume - 1); vol_repeat_us = now + 150000; }
                else if (hit(&B_VUP, x, y)) { set_volume(volume + 1); vol_repeat_us = now + 150000; }
            }
            if (!down) vol_repeat_us = 0;
        }
        was_down = down;

        // redraw when the screen changed or any displayed value did
        uint32_t sig = (uint32_t)screen;
        sig = sig * 31 + st.elapsed_s;
        sig = sig * 31 + (st.progress / 4);
        sig = sig * 31 + st.track;
        sig = sig * 31 + st.rev;
        sig = sig * 31 + st.active;
        sig = sig * 31 + st.finished;
        sig = sig * 31 + st.album;
        sig = sig * 31 + paused;
        sig = sig * 31 + volume;
        if (screen != drawn_screen || (screen == SCREEN_PLAYER && sig != drawn_sig) || (screen == SCREEN_LIST && st.active != (drawn_sig & 1))) dirty = true;
        if (dirty) {
            if (screen == SCREEN_LIST) { draw_list(); drawn_sig = st.active; }
            else { draw_player(); drawn_sig = sig; }
            flush();
            drawn_screen = screen;
            dirty = false;
        }
        vTaskDelay(1);
    }
}
