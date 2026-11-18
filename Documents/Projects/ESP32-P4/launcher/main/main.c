// Launcher for the Waveshare ESP32-P4 3.5" board.
//
// Shows a touch menu of the built-in clock plus every /apps/*.bin on the SD card. Picking one copies it into the
// ota_1 flash slot (skipped when that exact image is already there), points the bootloader at it and reboots.
// Apps never mark themselves valid, so with app rollback enabled the next reset of any kind (Reset button, power
// cycle, crash) comes back here. If nothing is touched for a few seconds, the built-in clock starts.
//
// Display: ST7796 480x320 on SPI (MOSI20 SCLK21 CS23 DC26 RST27 BL28).  Touch: FT6336 at I2C 0x38 (SDA7 SCL8 RST29).
// SD card: SDMMC slot 0, 4-bit, CLK43 CMD44 D0-3 39-42, supplied by on-chip LDO4.
#include <dirent.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "driver/gpio.h"
#include "driver/i2c_master.h"
#include "driver/ledc.h"
#include "driver/sdmmc_host.h"
#include "driver/spi_master.h"
#include "esp_app_format.h"
#include "esp_heap_caps.h"
#include "esp_lcd_panel_io.h"
#include "esp_log.h"
#include "esp_ota_ops.h"
#include "esp_partition.h"
#include "esp_system.h"
#include "esp_timer.h"
#include "esp_vfs_fat.h"
#include "freertos/FreeRTOS.h"
#include "freertos/semphr.h"
#include "freertos/task.h"
#include "sd_pwr_ctrl_by_on_chip_ldo.h"
#include "sdmmc_cmd.h"
#include "font_8x16.h"

static const char *TAG = "launcher";

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
#define BACKLIGHT_PCT 40                    // moderate: the menu also appears after every night-time reset

#define APPS_DIR "/sdcard/apps"
#define MAX_ENTRIES 64
#define COUNTDOWN_S 3

// ---- layout
#define HEAD_H 36
#define FOOT_Y 264
#define LIST_Y HEAD_H
#define LIST_H (FOOT_Y - HEAD_H)            // 228
#define ROW_H 56
#define BTN_X 318
#define BTN_Y 272
#define BTN_W 154
#define BTN_H 40

// ---- colors (stored byte-swapped, ready for the SPI wire)
#define RGB(r, g, b) ((uint16_t)((((((r) & 0xF8) << 8) | (((g) & 0xFC) << 3) | ((b) >> 3)) << 8) | ((((r) & 0xF8) << 8 | (((g) & 0xFC) << 3) | ((b) >> 3)) >> 8)))
#define C_BG RGB(12, 12, 16)
#define C_HEAD RGB(30, 30, 38)
#define C_TEXT RGB(225, 225, 225)
#define C_DIM RGB(140, 140, 152)
#define C_LINE RGB(44, 44, 54)
#define C_SEL_BG RGB(58, 30, 6)
#define C_ACCENT RGB(255, 140, 0)
#define C_BTN RGB(255, 140, 0)
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
    for (int i = 0; i < LCD_W * LCD_H; i++) fb[i] = C_BG;

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

// ---- drawing primitives (all clipped to clip_*)
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
    char b[96];
    int n = max_w / (8 * s);
    if (n > (int)sizeof b - 1) n = sizeof b - 1;
    snprintf(b, sizeof b, "%.*s", n, t);
    text(x, y, b, c, s);
}

static void set_clip(int x0, int y0, int x1, int y1) { clip_x0 = x0; clip_y0 = y0; clip_x1 = x1; clip_y1 = y1; }

// ===========================================================================
// Touch
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

// Returns true and the landscape screen position while a finger is down.
static bool touch_read(int *sx, int *sy)
{
    static int lx, ly;
    uint8_t reg = 0x02, b[5];
    if (i2c_master_transmit_receive(touch_dev, &reg, 1, b, sizeof b, 50) != ESP_OK) return false;   // keep last state on a bus error
    int n = b[0] & 0x0F;
    if (n < 1 || n > 2) return false;
    int rx = ((b[1] & 0x0F) << 8) | b[2], ry = ((b[3] & 0x0F) << 8) | b[4];
    if (rx > 3000 || ry > 3000) return false;      // the chip reports 4095,4095 for a phantom point
    lx = ry; ly = LCD_H - 1 - rx;                  // portrait controller -> landscape panel: swap axes, flip vertical (checked on the board)
    *sx = lx; *sy = ly;
    return true;
}

// ===========================================================================
// Entries: the built-in clock (ota_0) and the apps found on the SD card
// ===========================================================================
typedef struct {
    char name[48];
    char sub[72];
    char path[300];
    uint32_t size;
    uint8_t elf_sha[32];
    bool builtin;
} entry_t;

static entry_t entries[MAX_ENTRIES];
static int nentries;
static char status[96];
static bool status_err;
static int64_t status_until_us;

static void set_status(const char *s, bool err, int seconds)
{
    snprintf(status, sizeof status, "%s", s);
    status_err = err;
    status_until_us = esp_timer_get_time() + seconds * 1000000LL;
}

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

// Reads the image header and app descriptor of a .bin; false if it is not an ESP32-P4 app image.
static bool read_info(const char *path, entry_t *e)
{
    FILE *f = fopen(path, "rb");
    if (!f) return false;
    uint8_t h[sizeof(esp_image_header_t) + sizeof(esp_image_segment_header_t) + sizeof(esp_app_desc_t)];
    bool ok = fread(h, 1, sizeof h, f) == sizeof h;
    fseek(f, 0, SEEK_END);
    e->size = (uint32_t)ftell(f);
    fclose(f);
    if (!ok) return false;
    const esp_image_header_t *ih = (const esp_image_header_t *)h;
    const esp_app_desc_t *d = (const esp_app_desc_t *)(h + sizeof(esp_image_header_t) + sizeof(esp_image_segment_header_t));
    if (ih->magic != ESP_IMAGE_HEADER_MAGIC || ih->chip_id != ESP_CHIP_ID_ESP32P4 || d->magic_word != ESP_APP_DESC_MAGIC_WORD) return false;
    memcpy(e->elf_sha, d->app_elf_sha256, 32);
    snprintf(e->sub, sizeof e->sub, "%.20s  %.20s  %.1f MB", d->project_name, d->version, e->size / 1048576.0);
    return true;
}

static int by_name(const void *a, const void *b) { return strcasecmp(((const entry_t *)a)->name, ((const entry_t *)b)->name); }

static void scan_sd(const esp_partition_t *slot)
{
    if (!sd_mount()) { set_status("No SD card (only the built-in clock is available)", true, 3600); return; }
    DIR *d = opendir(APPS_DIR);
    if (!d) { set_status("No /apps folder on the SD card", true, 3600); return; }
    int skipped = 0;
    struct dirent *de;
    while ((de = readdir(d)) && nentries < MAX_ENTRIES) {
        size_t n = strlen(de->d_name);
        if (de->d_type == DT_DIR || n < 5 || strcasecmp(de->d_name + n - 4, ".bin")) continue;
        entry_t *e = &entries[nentries];
        memset(e, 0, sizeof *e);
        snprintf(e->path, sizeof e->path, APPS_DIR "/%s", de->d_name);
        if (!read_info(e->path, e) || e->size > slot->size) { skipped++; continue; }
        snprintf(e->name, sizeof e->name, "%.*s", (int)(n - 4 < sizeof e->name - 1 ? n - 4 : sizeof e->name - 1), de->d_name);
        nentries++;
    }
    closedir(d);
    qsort(&entries[1], nentries - 1, sizeof(entry_t), by_name);
    if (nentries == 1) set_status("No apps in /apps on the SD card", true, 3600);
    else if (skipped) { char b[64]; snprintf(b, sizeof b, "%d file(s) in /apps are not ESP32-P4 apps", skipped); set_status(b, true, 6); }
    ESP_LOGI(TAG, "%d app(s) on the SD card, %d skipped", nentries - 1, skipped);
}

// ===========================================================================
// UI
// ===========================================================================
static int sel = 0;
static float scroll_px;
static int64_t countdown_end_us;                   // 0 when cancelled

static float max_scroll(void)
{
    float m = nentries * ROW_H - LIST_H;
    return m > 0 ? m : 0;
}

static void clamp_scroll(void)
{
    float m = max_scroll();
    if (scroll_px < 0) scroll_px = 0;
    if (scroll_px > m) scroll_px = m;
}

static void draw_all(void)
{
    set_clip(0, 0, LCD_W, LCD_H);
    rect(0, 0, LCD_W, LCD_H, C_BG);
    // list
    set_clip(0, LIST_Y, LCD_W, FOOT_Y);
    for (int i = 0; i < nentries; i++) {
        int y = LIST_Y + i * ROW_H - (int)scroll_px;
        if (y + ROW_H <= LIST_Y || y >= FOOT_Y) continue;
        bool s = i == sel;
        if (s) { rect(0, y, LCD_W, ROW_H, C_SEL_BG); rect(0, y, 6, ROW_H, C_ACCENT); }
        text_fit(18, y + 8, entries[i].name, C_TEXT, 2, LCD_W - 40);
        text_fit(18, y + 36, entries[i].sub, C_DIM, 1, LCD_W - 40);
        rect(0, y + ROW_H - 1, LCD_W, 1, C_LINE);
    }
    float m = max_scroll();
    if (m > 0) {                                   // scroll bar
        int bh = (int)(LIST_H * (float)LIST_H / (nentries * ROW_H));
        int by = LIST_Y + (int)((LIST_H - bh) * scroll_px / m);
        rect(LCD_W - 5, by, 4, bh, C_DIM);
    }
    // header
    set_clip(0, 0, LCD_W, LCD_H);
    rect(0, 0, LCD_W, HEAD_H, C_HEAD);
    text(10, 10, "ESP32-P4 Launcher", C_TEXT, 1);
    if (countdown_end_us) {
        char b[64];
        int left = (int)((countdown_end_us - esp_timer_get_time() + 999999) / 1000000);
        if (left < 1) left = 1;
        snprintf(b, sizeof b, "Starting clock in %d  (touch to stay)", left);
        text(LCD_W - text_w(b, 1) - 10, 10, b, C_ACCENT, 1);
    }
    rect(0, HEAD_H - 1, LCD_W, 1, C_LINE);
    // footer
    rect(0, FOOT_Y, LCD_W, LCD_H - FOOT_Y, C_HEAD);
    rect(0, FOOT_Y, LCD_W, 1, C_LINE);
    bool st = status[0] && esp_timer_get_time() < status_until_us;
    if (st) text_fit(10, FOOT_Y + 10, status, status_err ? C_ERR : C_TEXT, 1, BTN_X - 20);
    else text_fit(10, FOOT_Y + 10, "Tap an app, then Launch", C_DIM, 1, BTN_X - 20);
    if (sel >= 0) text_fit(10, FOOT_Y + 32, entries[sel].name, C_TEXT, 2, BTN_X - 20);
    rect(BTN_X, BTN_Y, BTN_W, BTN_H, sel >= 0 ? C_BTN : C_BTN_OFF);
    text(BTN_X + (BTN_W - text_w("LAUNCH", 2)) / 2, BTN_Y + 4, "LAUNCH", C_BTN_TEXT, 2);
}

static void draw_progress(const char *name, const char *msg, int pct)
{
    set_clip(0, 0, LCD_W, LCD_H);
    rect(0, 0, LCD_W, LCD_H, C_BG);
    text_fit(24, 110, name, C_TEXT, 2, LCD_W - 48);
    text_fit(24, 150, msg, C_DIM, 2, LCD_W - 48);
    rect(24, 196, LCD_W - 48, 20, C_LINE);
    if (pct > 0) rect(24, 196, (LCD_W - 48) * pct / 100, 20, C_ACCENT);
    flush();
}

// ===========================================================================
// Launching
// ===========================================================================
// True when the ota_1 slot already holds exactly this image (same ELF hash in the descriptor and same last 32 bytes,
// which are the image's own SHA-256; the tail guards against an interrupted earlier copy).
static bool slot_has_image(const esp_partition_t *slot, const entry_t *e, FILE *f)
{
    esp_app_desc_t d;
    if (esp_ota_get_partition_description(slot, &d) != ESP_OK || memcmp(d.app_elf_sha256, e->elf_sha, 32)) return false;
    uint8_t a[32], b[32];
    if (e->size < 64 || fseek(f, e->size - 32, SEEK_SET) || fread(a, 1, 32, f) != 32) return false;
    if (esp_partition_read(slot, e->size - 32, b, 32) != ESP_OK) return false;
    return memcmp(a, b, 32) == 0;
}

static void fail(const char *msg, esp_err_t err)
{
    char b[96];
    snprintf(b, sizeof b, "%s (%s)", msg, esp_err_to_name(err));
    ESP_LOGE(TAG, "%s", b);
    set_status(b, true, 8);
}

// Returns only on failure.
static void launch(const entry_t *e)
{
    const esp_partition_t *target;
    if (e->builtin) {
        target = esp_partition_find_first(ESP_PARTITION_TYPE_APP, ESP_PARTITION_SUBTYPE_APP_OTA_0, NULL);
        draw_progress(e->name, "Starting...", 0);
    } else {
        target = esp_partition_find_first(ESP_PARTITION_TYPE_APP, ESP_PARTITION_SUBTYPE_APP_OTA_1, NULL);
        FILE *f = fopen(e->path, "rb");
        if (!f) { fail("Cannot open the file", ESP_FAIL); return; }
        if (slot_has_image(target, e, f)) {
            ESP_LOGI(TAG, "%s is already in the app slot", e->name);
            draw_progress(e->name, "Starting...", 100);
        } else {
            esp_ota_handle_t h;
            esp_err_t r = esp_ota_begin(target, OTA_WITH_SEQUENTIAL_WRITES, &h);
            if (r != ESP_OK) { fclose(f); fail("Cannot prepare the app slot", r); return; }
            fseek(f, 0, SEEK_SET);
            uint8_t *buf = malloc(32768);
            uint32_t done = 0;
            int64_t t0 = esp_timer_get_time();
            int next_draw = 0;
            while (r == ESP_OK) {
                size_t n = fread(buf, 1, 32768, f);
                if (n == 0) break;
                r = esp_ota_write(h, buf, n);
                done += n;
                if (done >= (uint32_t)next_draw) {
                    draw_progress(e->name, "Loading into flash...", (int)(100ULL * done / e->size));
                    next_draw = done + 131072;
                }
            }
            free(buf);
            fclose(f);
            if (r == ESP_OK && done != e->size) r = ESP_FAIL;
            if (r != ESP_OK) { esp_ota_abort(h); fail("Copy failed", r); return; }
            r = esp_ota_end(h);
            if (r != ESP_OK) { fail("Image check failed", r); return; }
            ESP_LOGI(TAG, "copied %u bytes in %lld ms", (unsigned)done, (long long)((esp_timer_get_time() - t0) / 1000));
            draw_progress(e->name, "Starting...", 100);
            f = NULL;
        }
        if (f) fclose(f);
    }
    if (!target) { fail("No app partition", ESP_ERR_NOT_FOUND); return; }
    esp_err_t r = esp_ota_set_boot_partition(target);
    if (r != ESP_OK) { fail(e->builtin ? "The built-in clock is missing" : "Cannot select the app", r); return; }
    ESP_LOGI(TAG, "booting %s", e->name);
    esp_restart();
}

// ===========================================================================
// Main loop
// ===========================================================================
void app_main(void)
{
    display_init();
    touch_init();

    const esp_partition_t *slot = esp_partition_find_first(ESP_PARTITION_TYPE_APP, ESP_PARTITION_SUBTYPE_APP_OTA_1, NULL);
    snprintf(entries[0].name, sizeof entries[0].name, "Clock");
    snprintf(entries[0].sub, sizeof entries[0].sub, "built-in: time, date and weather");
    entries[0].builtin = true;
    nentries = 1;

    draw_all();
    flush();
    scan_sd(slot);
    sel = 0;
    countdown_end_us = esp_timer_get_time() + COUNTDOWN_S * 1000000LL;

    bool was_down = false, in_list = false, dragging = false, on_button = false;
    int start_y = 0, last_x = 0, last_y = 0;
    float start_scroll = 0, vel = 0;
    int last_left = -1;
    bool dirty = true;
    while (1) {
        int x, y;
        bool down = touch_read(&x, &y);
        if (down) { last_x = x; last_y = y; }
        if (down && !was_down) {                     // finger down
            if (countdown_end_us) { countdown_end_us = 0; dirty = true; }
            in_list = y < FOOT_Y;
            on_button = !in_list && x >= BTN_X && y >= BTN_Y && y < BTN_Y + BTN_H;
            dragging = false;
            start_y = y;
            start_scroll = scroll_px;
            vel = 0;
        } else if (down && in_list) {                // drag
            int dy = y - start_y;
            if (!dragging && abs(dy) > 8) dragging = true;
            if (dragging) {
                float before = scroll_px;
                scroll_px = start_scroll - dy;
                clamp_scroll();
                vel = scroll_px - before;
                dirty = true;
            }
        } else if (!down && was_down) {              // finger up
            if (in_list && !dragging) {
                int idx = (int)((last_y - LIST_Y + scroll_px) / ROW_H);
                if (last_y >= LIST_Y && idx >= 0 && idx < nentries) { sel = idx; dirty = true; }
            } else if (on_button && sel >= 0 && last_x >= BTN_X && last_y >= BTN_Y && last_y < BTN_Y + BTN_H) {
                launch(&entries[sel]);               // returns only on failure
                dirty = true;
            }
            if (!in_list || !dragging) vel = 0;
        }
        if (!down && fabsf(vel) > 0.5f) {            // coast after a flick
            scroll_px += vel;
            vel *= 0.92f;
            clamp_scroll();
            dirty = true;
        }
        was_down = down;

        if (countdown_end_us) {
            int64_t now = esp_timer_get_time();
            if (now >= countdown_end_us) {
                countdown_end_us = 0;
                launch(&entries[0]);
                dirty = true;
            } else {
                int left = (int)((countdown_end_us - now + 999999) / 1000000);
                if (left != last_left) { last_left = left; dirty = true; }
            }
        }
        if (status[0] && esp_timer_get_time() >= status_until_us && status_until_us) { status[0] = 0; status_until_us = 0; dirty = true; }
        if (dirty) { draw_all(); flush(); dirty = false; }
        vTaskDelay(1);
    }
}
