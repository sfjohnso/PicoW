// Photo slideshow for the Waveshare ESP32-P4 3.5" board (a launcher app).
//
// Cycles through the .jpg/.jpeg/.png files in /Photos on the SD card, in name order. A photo larger than the 480x320
// screen is scaled down to fit with its aspect ratio kept; a smaller one is shown at its own size. Either way it is
// centered on black. JPEGs honor the EXIF orientation tag (phone photos). Tap the right half of the screen for the
// next photo, the left half for the previous one; otherwise it advances by itself.
//
// Decoding: JPEG with esp_jpeg (TJpgDec, decodes at 1/1, 1/2, 1/4 or 1/8 size), PNG with libpng row by row. Both feed
// a box-filter accumulator the size of the final picture, so memory stays small however large the photo is.
// Progressive JPEG goes through lowzag (reduced-scale, low memory). Not supported (shown as a message): interlaced PNG.
//
// Display: ST7796 480x320 on SPI (MOSI20 SCLK21 CS23 DC26 RST27 BL28).  Touch: FT6336 at I2C 0x38 (SDA7 SCL8 RST29).
// SD card: SDMMC slot 0, 4-bit, CLK43 CMD44 D0-3 39-42, supplied by on-chip LDO4.
#include <dirent.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "driver/gpio.h"
#include "driver/i2c_master.h"
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
#include "driver/jpeg_decode.h"
#include "jpeg_decoder.h"
#include "lowzag.h"
#include "png.h"
#include "sd_pwr_ctrl_by_on_chip_ldo.h"
#include "sdmmc_cmd.h"
#include "font_8x16.h"

static const char *TAG = "photos";

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

#define PHOTO_DIR "/sdcard/Photos"
#define SLIDE_SECONDS 8
#define MAX_PHOTOS 2000
#define MAX_JPEG_BYTES (24 * 1024 * 1024)

// RGB565 byte-swapped for the SPI wire
static inline uint16_t rgb565(int r, int g, int b)
{
    uint16_t v = ((r & 0xF8) << 8) | ((g & 0xFC) << 3) | (b >> 3);
    return (v << 8) | (v >> 8);
}

// ===========================================================================
// Display (same as the launcher's)
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

// ---- a line or two of text on black (messages only)
static void message(const char *l1, const char *l2)
{
    memset(fb, 0, LCD_W * LCD_H * 2);
    const char *lines[2] = { l1, l2 };
    for (int i = 0; i < 2; i++) {
        if (!lines[i]) continue;
        int n = strlen(lines[i]);
        if (n > LCD_W / 8 - 2) n = LCD_W / 8 - 2;
        int x0 = (LCD_W - n * 8) / 2, y0 = 130 + i * 26;
        for (int c = 0; c < n; c++)
            for (int r = 0; r < 16; r++) {
                uint8_t bits = fontdata_8x16[(unsigned char)lines[i][c] * 16 + r];
                for (int b = 0; b < 8; b++)
                    if (bits & (0x80 >> b)) fb[(y0 + r) * LCD_W + x0 + c * 8 + b] = rgb565(i ? 160 : 255, i ? 160 : 255, i ? 170 : 255);
            }
    }
    flush();
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

// ===========================================================================
// SD card and photo list
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

static char *names[MAX_PHOTOS];
static int nphotos;

static bool has_ext(const char *n, const char *ext)
{
    size_t a = strlen(n), b = strlen(ext);
    return a > b && strcasecmp(n + a - b, ext) == 0;
}

static bool is_png(const char *n) { return has_ext(n, ".png"); }

static int by_name(const void *a, const void *b) { return strcasecmp(*(char *const *)a, *(char *const *)b); }

static void scan_photos(void)
{
    DIR *d = opendir(PHOTO_DIR);
    if (!d) return;
    struct dirent *de;
    while ((de = readdir(d)) && nphotos < MAX_PHOTOS) {
        if (de->d_type == DT_DIR || de->d_name[0] == '.') continue;      // "._x.jpg" files are macOS metadata
        if (has_ext(de->d_name, ".jpg") || has_ext(de->d_name, ".jpeg") || has_ext(de->d_name, ".png"))
            names[nphotos++] = strdup(de->d_name);
    }
    closedir(d);
    qsort(names, nphotos, sizeof names[0], by_name);
}

// ===========================================================================
// Scaling: decoders feed source pixels to acc_add(); acc_present() draws the averaged result centered on black.
// ===========================================================================
typedef struct {
    int w, h;            // source size
    int orient;          // EXIF orientation 1..8 (1 = as stored)
    int ow, oh;          // size after orientation
    int fw, fh;          // final size on screen
} xf_t;

static uint32_t *acc_sum;       // 3 x (LCD_W * LCD_H) channel sums (allocated per photo, only by the PNG and software-JPEG paths)
static uint16_t *acc_cnt;       // samples per cell
static xf_t X;

static void acc_free(void)
{
    heap_caps_free(acc_sum); heap_caps_free(acc_cnt);
    acc_sum = NULL; acc_cnt = NULL;
}

static void fit(int ow, int oh, int *fw, int *fh)
{
    if (ow <= LCD_W && oh <= LCD_H) { *fw = ow; *fh = oh; return; }      // never enlarge
    long a = (long)LCD_W * oh, b = (long)LCD_H * ow;                     // compare LCD_W/ow with LCD_H/oh
    if (a <= b) { *fw = LCD_W; *fh = (int)(((long)oh * LCD_W + ow / 2) / ow); }
    else { *fh = LCD_H; *fw = (int)(((long)ow * LCD_H + oh / 2) / oh); }
    if (*fw < 1) *fw = 1;
    if (*fh < 1) *fh = 1;
    if (*fw > LCD_W) *fw = LCD_W;
    if (*fh > LCD_H) *fh = LCD_H;
}

// orig_w/orig_h: the whole image; w/h: the size of the pixels that will be fed in (a JPEG decoded at 1/n size)
static bool acc_begin(int orig_w, int orig_h, int w, int h, int orient)
{
    acc_sum = heap_caps_malloc(sizeof(uint32_t) * 3 * LCD_W * LCD_H, MALLOC_CAP_SPIRAM);
    acc_cnt = heap_caps_malloc(sizeof(uint16_t) * LCD_W * LCD_H, MALLOC_CAP_SPIRAM);
    if (!acc_sum || !acc_cnt) { acc_free(); return false; }
    X.w = w; X.h = h; X.orient = orient;
    bool swap = orient >= 5;
    X.ow = swap ? h : w;
    X.oh = swap ? w : h;
    fit(swap ? orig_h : orig_w, swap ? orig_w : orig_h, &X.fw, &X.fh);
    memset(acc_sum, 0, sizeof(uint32_t) * 3 * X.fw * X.fh);
    memset(acc_cnt, 0, sizeof(uint16_t) * X.fw * X.fh);
    return true;
}

static inline void acc_add(int x, int y, int r, int g, int b)
{
    int ox, oy;
    switch (X.orient) {
    default:
    case 1: ox = x;             oy = y;             break;
    case 2: ox = X.w - 1 - x;   oy = y;             break;
    case 3: ox = X.w - 1 - x;   oy = X.h - 1 - y;   break;
    case 4: ox = x;             oy = X.h - 1 - y;   break;
    case 5: ox = y;             oy = x;             break;
    case 6: ox = X.h - 1 - y;   oy = x;             break;
    case 7: ox = X.h - 1 - y;   oy = X.w - 1 - x;   break;
    case 8: ox = y;             oy = X.w - 1 - x;   break;
    }
    int dx = (int)((long)ox * X.fw / X.ow), dy = (int)((long)oy * X.fh / X.oh);
    int i = dy * X.fw + dx;
    acc_sum[3 * i] += r; acc_sum[3 * i + 1] += g; acc_sum[3 * i + 2] += b;
    acc_cnt[i]++;
}

static void acc_present(void)
{
    memset(fb, 0, LCD_W * LCD_H * 2);
    int x0 = (LCD_W - X.fw) / 2, y0 = (LCD_H - X.fh) / 2;
    for (int y = 0; y < X.fh; y++) {
        uint16_t *row = fb + (y0 + y) * LCD_W + x0;
        for (int x = 0; x < X.fw; x++) {
            int i = y * X.fw + x, n = acc_cnt[i];
            if (n) row[x] = rgb565(acc_sum[3 * i] / n, acc_sum[3 * i + 1] / n, acc_sum[3 * i + 2] / n);
        }
    }
    flush();
    acc_free();
}

// ---- hardware-decoded RGB565 frame -> screen: exact box average, source read in row order, orientation applied by
// mapping each screen cell back to its rectangle in the source (no per-pixel division, no accumulator arrays).
static void inv_map(int orient, int w, int h, int u, int v, int *x, int *y)
{
    switch (orient) {
    default:
    case 1: *x = u;         *y = v;         break;
    case 2: *x = w - 1 - u; *y = v;         break;
    case 3: *x = w - 1 - u; *y = h - 1 - v; break;
    case 4: *x = u;         *y = h - 1 - v; break;
    case 5: *x = v;         *y = u;         break;
    case 6: *x = v;         *y = h - 1 - u; break;
    case 7: *x = w - 1 - v; *y = h - 1 - u; break;
    case 8: *x = w - 1 - v; *y = u;         break;
    }
}

static void present_rgb565(const uint16_t *src, int pitch, int w, int h, int orient)
{
    bool swap = orient >= 5;
    int ow = swap ? h : w, oh = swap ? w : h, fw, fh;
    fit(ow, oh, &fw, &fh);
    X.fw = fw; X.fh = fh;
    memset(fb, 0, LCD_W * LCD_H * 2);
    int x0 = (LCD_W - fw) / 2, y0 = (LCD_H - fh) / 2;
    for (int dy = 0; dy < fh; dy++) {
        int v0 = (dy * oh + fh - 1) / fh, v1 = ((dy + 1) * oh + fh - 1) / fh;      // oriented rows of this screen row
        uint16_t *row = fb + (y0 + dy) * LCD_W + x0;
        for (int dx = 0; dx < fw; dx++) {
            int u0 = (dx * ow + fw - 1) / fw, u1 = ((dx + 1) * ow + fw - 1) / fw;
            int xa, ya, xb, yb;
            inv_map(orient, w, h, u0, v0, &xa, &ya);
            inv_map(orient, w, h, u1 - 1, v1 - 1, &xb, &yb);
            int sx0 = xa < xb ? xa : xb, sx1 = xa < xb ? xb : xa, sy0 = ya < yb ? ya : yb, sy1 = ya < yb ? yb : ya;
            uint32_t sr = 0, sg = 0, sb = 0;
            for (int sy = sy0; sy <= sy1; sy++) {
                const uint16_t *p = src + (size_t)sy * pitch + sx0;
                for (int sx = sx0; sx <= sx1; sx++) {
                    uint32_t c = *p++;
                    sr += c >> 11; sg += (c >> 5) & 63; sb += c & 31;
                }
            }
            uint32_t n = (uint32_t)(sx1 - sx0 + 1) * (sy1 - sy0 + 1);
            uint16_t v = ((sr / n) << 11) | ((sg / n) << 5) | (sb / n);
            row[dx] = (v << 8) | (v >> 8);
        }
    }
    flush();
}

// ===========================================================================
// JPEG
// ===========================================================================
static uint16_t rd16(const uint8_t *p, bool le) { return le ? p[0] | (p[1] << 8) : (p[0] << 8) | p[1]; }
static uint32_t rd32(const uint8_t *p, bool le) { return le ? rd16(p, 1) | ((uint32_t)rd16(p + 2, 1) << 16) : ((uint32_t)rd16(p, 0) << 16) | rd16(p + 2, 0); }

// EXIF orientation (1..8) from the APP1 segment, 1 if there is none.
static int jpeg_orientation(const uint8_t *d, size_t n)
{
    if (n < 4 || d[0] != 0xFF || d[1] != 0xD8) return 1;
    size_t p = 2;
    while (p + 4 <= n && d[p] == 0xFF) {
        uint8_t m = d[p + 1];
        if (m == 0xFF) { p++; continue; }
        if (m == 0xD9 || m == 0xDA) break;                                // end of image / start of scan
        size_t len = ((size_t)d[p + 2] << 8) | d[p + 3];
        if (len < 2) break;
        if (m == 0xE1 && len >= 14 && p + 2 + len <= n && !memcmp(d + p + 4, "Exif\0\0", 6)) {
            const uint8_t *t = d + p + 10;
            size_t tl = len - 8;
            if (tl < 8 || (memcmp(t, "II", 2) && memcmp(t, "MM", 2))) return 1;
            bool le = t[0] == 'I';
            uint32_t ifd = rd32(t + 4, le);
            if (ifd + 2 > tl) return 1;
            int cnt = rd16(t + ifd, le);
            for (int i = 0; i < cnt && ifd + 2 + (size_t)(i + 1) * 12 <= tl; i++) {
                const uint8_t *e = t + ifd + 2 + i * 12;
                if (rd16(e, le) == 0x0112) {
                    int v = rd16(e + 8, le);
                    return v >= 1 && v <= 8 ? v : 1;
                }
            }
            return 1;
        }
        p += 2 + len;
    }
    return 1;
}

// TJpgDec's scratch pool (stream buffer, quantization + Huffman tables, block buffer). The library's default of 3100
// bytes is too small for ordinary 4:2:0 JPEGs (decode fails with error 3 = JDR_MEM1).
static uint8_t jpeg_work[16 * 1024];

static jpeg_decoder_handle_t hw_engine;

// Hardware decode (baseline JPEG, whole frame, RGB565) then box-filter to the screen. Returns true if it drew the photo;
// false means "use the software decoder" (unsupported layout, or not enough memory for the full-size frame).
static bool show_jpeg_hw(const uint8_t *in, size_t size, int w, int h, int orient, const char *path)
{
    if (!hw_engine) {
        jpeg_decode_engine_cfg_t ec = { .intr_priority = 0, .timeout_ms = 20000 };
        if (jpeg_new_decoder_engine(&ec, &hw_engine) != ESP_OK) { hw_engine = NULL; return false; }
    }
    jpeg_decode_picture_info_t pi;
    if (jpeg_decoder_get_info(in, size, &pi) != ESP_OK) return false;
    int mx, my;                                                            // MCU size: the frame is padded to whole MCUs
    switch (pi.sample_method) {
    case JPEG_DOWN_SAMPLING_YUV420: mx = 16; my = 16; break;
    case JPEG_DOWN_SAMPLING_YUV422: mx = 16; my = 8;  break;
    case JPEG_DOWN_SAMPLING_YUV444:
    case JPEG_DOWN_SAMPLING_GRAY:   mx = 8;  my = 8;  break;
    default: return false;
    }
    int pw = (w + mx - 1) / mx * mx, ph = (h + my - 1) / my * my;
    jpeg_decode_memory_alloc_cfg_t mc = { .buffer_direction = JPEG_DEC_ALLOC_OUTPUT_BUFFER };
    size_t got;
    uint16_t *out = jpeg_alloc_decoder_mem((size_t)pw * ph * 2, &mc, &got);
    if (!out) { ESP_LOGW(TAG, "no memory for the %dx%d frame, using the software decoder", pw, ph); return false; }
    // BGR order = little-endian halfwords, i.e. native RGB565 (the RGB setting writes byte-swapped halfwords)
    jpeg_decode_cfg_t dc = { .output_format = JPEG_DECODE_OUT_FORMAT_RGB565, .rgb_order = JPEG_DEC_RGB_ELEMENT_ORDER_BGR,
                             .conv_std = JPEG_YUV_RGB_CONV_STD_BT601 };
    uint32_t out_len = 0;
    int64_t t0 = esp_timer_get_time();
    esp_err_t r = jpeg_decoder_process(hw_engine, &dc, in, size, (uint8_t *)out, got, &out_len);
    int64_t t1 = esp_timer_get_time();
    if (r != ESP_OK) {
        ESP_LOGW(TAG, "hardware decode failed (%s), using the software decoder", esp_err_to_name(r));
        heap_caps_free(out);
        return false;
    }
    present_rgb565(out, pw, w, h, orient);
    heap_caps_free(out);
    ESP_LOGI(TAG, "%s: %dx%d orient %d, HARDWARE decode %lld ms, scaled+drawn in %lld ms -> %dx%d", path, w, h, orient,
             (long long)((t1 - t0) / 1000), (long long)((esp_timer_get_time() - t1) / 1000), X.fw, X.fh);
    return true;
}

// RGB888 pixels (w x h: the whole picture, or a reduced-size decode of an orig_w x orig_h one) -> the screen, scaled to fit.
static bool draw_rgb888(const uint8_t *px, int orig_w, int orig_h, int w, int h, int orient)
{
    if (!acc_begin(orig_w, orig_h, w, h, orient)) return false;
    for (int y = 0; y < h; y++)
        for (int x = 0; x < w; x++, px += 3) acc_add(x, y, px[0], px[1], px[2]);
    acc_present();
    return true;
}

// Progressive JPEG: neither the hardware decoder nor TJpgDec can do it, so lowzag (MIT, a registry component) does.
// It decodes at 1/1, 1/2, 1/4 or 1/8 and keeps only the low-frequency corner of each block, which is all a reduced
// IDCT reads: a 12 MP photo needs about 3.6 MB at 1/8 instead of the 36 MB of coefficients a general decoder holds.
// Memory still follows the source size, so it is capped; over the cap it gives up chroma detail, then a scale step.
#define PROGRESSIVE_MEM_LIMIT (16 * 1024 * 1024)

static const char *show_jpeg_progressive(const uint8_t *in, size_t size, const char *path)
{
    int w, h;
    lowzag_err_t e = lowzag_probe(in, size, &w, &h);
    if (e == LOWZAG_ERR_NOT_PROGRESSIVE || e == LOWZAG_ERR_UNSUPPORTED) return "JPEG type not supported";
    if (e != LOWZAG_OK) return "not a valid JPEG";
    int orient = jpeg_orientation(in, size);
    bool swap = orient >= 5;
    int fw, fh;
    fit(swap ? h : w, swap ? w : h, &fw, &fh);
    lowzag_cfg_t cfg = LOWZAG_CFG_INIT;
    cfg.target_w = swap ? fh : fw;                      // the decoder's output grid is the stored, unrotated picture
    cfg.target_h = swap ? fw : fh;
    cfg.mem_limit = PROGRESSIVE_MEM_LIMIT;
    lowzag_info_t info = LOWZAG_INFO_INIT;
    e = lowzag_decode_mem(&cfg, in, size, NULL, 0, &info);              // size query: picks the scale, allocates nothing
    if (e != LOWZAG_ERR_BUFFER_TOO_SMALL) return e == LOWZAG_ERR_NOMEM ? "too large for memory" : "progressive JPEG not supported";
    size_t out_len = (size_t)info.out_w * info.out_h * 3;
    uint8_t *out = heap_caps_malloc(out_len, MALLOC_CAP_SPIRAM);
    if (!out) return "out of memory";
    int64_t t0 = esp_timer_get_time();
    e = lowzag_decode_mem(&cfg, in, size, out, out_len, &info);
    int64_t t1 = esp_timer_get_time();
    if (e != LOWZAG_OK) {
        heap_caps_free(out);
        return e == LOWZAG_ERR_NOMEM ? "too large for memory" : "progressive JPEG decode failed";
    }
    bool ok = draw_rgb888(out, w, h, info.out_w, info.out_h, orient);
    heap_caps_free(out);
    if (!ok) return "out of memory";
    ESP_LOGI(TAG, "%s: %dx%d orient %d, PROGRESSIVE decode 1/%d in %lld ms (peak %u KB), scaled+drawn in %lld ms -> %dx%d", path, w, h,
             orient, info.scale, (long long)((t1 - t0) / 1000), (unsigned)(info.peak_alloc / 1024),
             (long long)((esp_timer_get_time() - t1) / 1000), X.fw, X.fh);
    return NULL;
}

static const char *show_jpeg(const char *path)
{
    FILE *f = fopen(path, "rb");
    if (!f) return "cannot open the file";
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    if (size < 4 || size > MAX_JPEG_BYTES) { fclose(f); return "file size not supported"; }
    jpeg_decode_memory_alloc_cfg_t imc = { .buffer_direction = JPEG_DEC_ALLOC_INPUT_BUFFER };
    size_t in_alloc;
    uint8_t *in = jpeg_alloc_decoder_mem(size, &imc, &in_alloc);
    if (!in) { fclose(f); return "out of memory"; }
    size_t got = fread(in, 1, size, f);
    fclose(f);
    if (got != (size_t)size) { heap_caps_free(in); return "read error"; }

    esp_jpeg_image_cfg_t cfg = { .indata = in, .indata_size = size, .out_format = JPEG_IMAGE_FORMAT_RGB888, .out_scale = JPEG_IMAGE_SCALE_0 };
    esp_jpeg_image_output_t info;
    if (esp_jpeg_get_image_info(&cfg, &info) != ESP_OK) {               // not baseline: try progressive
        const char *err = show_jpeg_progressive(in, size, path);
        heap_caps_free(in);
        return err;
    }
    int w = info.width, h = info.height;
    int orient = jpeg_orientation(in, size);

    if (show_jpeg_hw(in, size, w, h, orient, path)) { heap_caps_free(in); return NULL; }

    // software fallback: the largest 1/2^k reduction that still leaves at least the final size
    bool swap = orient >= 5;
    int fw, fh;
    fit(swap ? h : w, swap ? w : h, &fw, &fh);
    int k = 3;
    while (k > 0 && (((swap ? h : w) >> k) < fw || ((swap ? w : h) >> k) < fh)) k--;
    cfg.out_scale = (esp_jpeg_image_scale_t)k;
    cfg.advanced.working_buffer = jpeg_work;
    cfg.advanced.working_buffer_size = sizeof jpeg_work;
    size_t out_len = (size_t)(w >> k) * (h >> k) * 3;                      // the decoder outputs width/2^k x height/2^k
    uint8_t *out = heap_caps_malloc(out_len, MALLOC_CAP_SPIRAM);
    if (!out) { heap_caps_free(in); return "out of memory"; }
    cfg.outbuf = out;
    cfg.outbuf_size = out_len;
    int64_t t0 = esp_timer_get_time();
    esp_err_t r = esp_jpeg_decode(&cfg, &info);
    heap_caps_free(in);
    if (r != ESP_OK) { heap_caps_free(out); return "JPEG decode failed"; }
    int64_t t1 = esp_timer_get_time();

    bool drawn = draw_rgb888(out, w, h, info.width, info.height, orient);
    heap_caps_free(out);
    if (!drawn) return "out of memory";
    ESP_LOGI(TAG, "%s: %dx%d orient %d, software decode 1/%d in %lld ms, scaled+drawn in %lld ms -> %dx%d", path, w, h, orient, 1 << k,
             (long long)((t1 - t0) / 1000), (long long)((esp_timer_get_time() - t1) / 1000), X.fw, X.fh);
    return NULL;
}

// ===========================================================================
// PNG
// ===========================================================================
static FILE *rd_file;
static png_structp rd_png;
static png_infop rd_info;
static uint8_t *rd_row;

static void png_cleanup(void)
{
    if (rd_png) png_destroy_read_struct(&rd_png, &rd_info, NULL);
    if (rd_file) fclose(rd_file);
    free(rd_row);
    rd_png = NULL; rd_info = NULL; rd_file = NULL; rd_row = NULL;
}

static const char *show_png(const char *path)
{
    static const char *err;
    err = NULL;
    rd_file = fopen(path, "rb");
    if (!rd_file) return "cannot open the file";
    rd_png = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    rd_info = rd_png ? png_create_info_struct(rd_png) : NULL;
    if (!rd_info) { png_cleanup(); return "out of memory"; }
    if (setjmp(png_jmpbuf(rd_png))) { png_cleanup(); acc_free(); return err ? err : "PNG decode failed"; }

    png_init_io(rd_png, rd_file);
    png_read_info(rd_png, rd_info);
    png_uint_32 w, h;
    int bd, ct, il;
    png_get_IHDR(rd_png, rd_info, &w, &h, &bd, &ct, &il, NULL, NULL);
    if (il != PNG_INTERLACE_NONE) { png_cleanup(); return "interlaced PNG not supported"; }

    if (ct == PNG_COLOR_TYPE_PALETTE) png_set_palette_to_rgb(rd_png);
    if (ct == PNG_COLOR_TYPE_GRAY && bd < 8) png_set_expand_gray_1_2_4_to_8(rd_png);
    if (png_get_valid(rd_png, rd_info, PNG_INFO_tRNS)) png_set_tRNS_to_alpha(rd_png);
    if (bd == 16) png_set_strip_16(rd_png);
    if (ct == PNG_COLOR_TYPE_GRAY || ct == PNG_COLOR_TYPE_GRAY_ALPHA) png_set_gray_to_rgb(rd_png);
    png_set_filler(rd_png, 0xFF, PNG_FILLER_AFTER);                       // always 4 bytes per pixel: R G B A
    png_read_update_info(rd_png, rd_info);

    rd_row = malloc((size_t)w * 4);
    if (!rd_row) { err = "out of memory"; png_error(rd_png, "oom"); }
    int64_t t0 = esp_timer_get_time();
    if (!acc_begin(w, h, w, h, 1)) { err = "out of memory"; png_error(rd_png, "oom"); }
    for (png_uint_32 y = 0; y < h; y++) {
        png_read_row(rd_png, rd_row, NULL);
        const uint8_t *p = rd_row;
        for (png_uint_32 x = 0; x < w; x++, p += 4) {
            int a = p[3];                                                  // composite over black
            acc_add(x, y, a == 255 ? p[0] : p[0] * a / 255, a == 255 ? p[1] : p[1] * a / 255, a == 255 ? p[2] : p[2] * a / 255);
        }
    }
    png_cleanup();
    int64_t t1 = esp_timer_get_time();
    acc_present();
    ESP_LOGI(TAG, "%s: %ux%u PNG, decoded+scaled in %lld ms, drawn in %lld ms -> %dx%d", path, (unsigned)w, (unsigned)h,
             (long long)((t1 - t0) / 1000), (long long)((esp_timer_get_time() - t1) / 1000), X.fw, X.fh);
    return NULL;
}

// ===========================================================================
// Slideshow
// ===========================================================================
// Returns true if the photo was drawn; otherwise a message is on screen.
static bool show(int idx)
{
    char path[320];
    snprintf(path, sizeof path, PHOTO_DIR "/%s", names[idx]);
    const char *err = is_png(names[idx]) ? show_png(path) : show_jpeg(path);
    if (err) {
        ESP_LOGW(TAG, "%s: %s", names[idx], err);
        message(names[idx], err);
        return false;
    }
    return true;
}

void app_main(void)
{
    display_init();
    touch_init();

    if (!sd_mount()) { message("No SD card", "Photos need a card with a Photos folder"); while (1) vTaskDelay(portMAX_DELAY); }
    scan_photos();
    if (nphotos == 0) { message("No photos found", "Put .jpg or .png files in /Photos on the SD card"); while (1) vTaskDelay(portMAX_DELAY); }
    ESP_LOGI(TAG, "%d photo(s)", nphotos);

    int idx = 0, step = 1;
    bool was_down = false;
    int last_x = 0;
    int64_t shown_us = 0;
    bool need_show = true;
    while (1) {
        if (need_show) {
            bool ok = show(idx);
            shown_us = esp_timer_get_time() - (ok ? 0 : (SLIDE_SECONDS - 2) * 1000000LL);   // a message stays 2 s
            need_show = false;
        }
        int x, y;
        bool down = touch_read(&x, &y);
        if (down) last_x = x;
        if (!down && was_down) {                                          // a tap: left half back, right half forward
            step = last_x < LCD_W / 2 ? -1 : 1;
            idx = (idx + step + nphotos) % nphotos;
            need_show = true;
        } else if (nphotos > 1 && esp_timer_get_time() - shown_us >= SLIDE_SECONDS * 1000000LL) {
            idx = (idx + 1) % nphotos;
            need_show = true;
        } else if (nphotos == 1 && esp_timer_get_time() - shown_us >= SLIDE_SECONDS * 1000000LL) {
            shown_us = esp_timer_get_time();                              // one photo: nothing to cycle to
        }
        was_down = down;
        vTaskDelay(1);
    }
}
