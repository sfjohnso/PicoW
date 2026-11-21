// Sound meter for the Waveshare ESP32-P4 3.5" board (a launcher app).
//
// Listens to the board's microphone and shows either a VU meter (RMS level with peak hold and a 15 s history) or a
// spectrogram (live spectrum plus a scrolling waterfall of an FFT, log frequency axis 50 Hz - 16 kHz). The + and - buttons
// change the microphone gain. Levels are in dBFS (decibels below digital full scale), not calibrated sound pressure.
//
// Audio: ES8311 codec at I2C 0x18 with its analog microphone input; I2S0 (MCLK13 BCLK12 WS10 DIN11), 44.1 kHz, 16 bit.
// Display: ST7796 480x320 on SPI (MOSI20 SCLK21 CS23 DC26 RST27 BL28).  Touch: FT6336 at I2C 0x38 (SDA7 SCL8 RST29).
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "driver/gpio.h"
#include "driver/i2c_master.h"
#include "driver/i2s_std.h"
#include "driver/ledc.h"
#include "driver/spi_master.h"
#include "esp_heap_caps.h"
#include "esp_lcd_panel_io.h"
#include "esp_log.h"
#include "esp_timer.h"
#include "freertos/FreeRTOS.h"
#include "freertos/semphr.h"
#include "freertos/task.h"
#include "font_8x16.h"

static const char *TAG = "sound";

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
#define PIN_PA 53                           // speaker amplifier enable (kept off: this app only listens)
#define SPI_MHZ 80
#define CHUNK_PX 12800                      // 25,600 bytes per SPI transfer (a multiple of the cache line)
#define BACKLIGHT_PCT 80

#define FS 44100
#define CHUNK 1470                          // samples per update: 44100 / 1470 = 30 updates a second
#define NFFT 2048
#define NBIN (NFFT / 2)
#define FMIN 50.0f                          // spectrogram frequency range (log axis)
#define FMAX 16000.0f
#define DB_LO (-100.0f)                     // spectrum colour range, dBFS
#define DB_HI (-30.0f)

// RGB565 byte-swapped for the SPI wire
static inline uint16_t rgb565(int r, int g, int b)
{
    uint16_t v = ((r & 0xF8) << 8) | ((g & 0xFC) << 3) | (b >> 3);
    return (v << 8) | (v >> 8);
}

// ===========================================================================
// Display (same as the Life app's)
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
    flush_rows(0, LCD_H);                          // black before the backlight comes on
    ledc_set_duty(LEDC_LOW_SPEED_MODE, LEDC_CHANNEL_0, 1023 * BACKLIGHT_PCT / 100);
    ledc_update_duty(LEDC_LOW_SPEED_MODE, LEDC_CHANNEL_0);
}

// ===========================================================================
// Touch (same as the Life app's); the I2C bus is shared with the codec
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
    i2c_master_bus_config_t bc = { .i2c_port = I2C_NUM_0, .sda_io_num = PIN_SDA, .scl_io_num = PIN_SCL,
                                   .clk_source = I2C_CLK_SRC_DEFAULT, .glitch_ignore_cnt = 7, .flags.enable_internal_pullup = true };
    ESP_ERROR_CHECK(i2c_new_master_bus(&bc, &i2c_bus));
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
// Drawing helpers on the frame buffer
// ===========================================================================
static void fill_rect(int x, int y, int w, int h, uint16_t c)
{
    if (x < 0) { w += x; x = 0; }
    if (y < 0) { h += y; y = 0; }
    if (x + w > LCD_W) w = LCD_W - x;
    if (y + h > LCD_H) h = LCD_H - y;
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
// Audio hardware: ES8311 microphone input
// ===========================================================================
static i2s_chan_handle_t rx;
static int pga = 4;                         // microphone preamp, 0..10 in 3 dB steps
#define ADC_BASE_DB 24                      // fixed ADC scale (register 0x16 = 0x24)

static void es_w(uint8_t reg, uint8_t val)
{
    uint8_t b[2] = { reg, val };
    esp_err_t r = i2c_master_transmit(es_dev, b, 2, 100);
    if (r != ESP_OK) ESP_LOGW(TAG, "codec write %02x failed: %s", reg, esp_err_to_name(r));
}

static int gain_db(void) { return ADC_BASE_DB + 3 * pga; }

static void set_pga(int g)
{
    if (g < 0) g = 0;
    if (g > 10) g = 10;
    pga = g;
    es_w(0x14, 0x10 | pga);                 // analog microphone, preamp gain
}

static void audio_init(void)
{
    i2c_device_config_t dc = { .dev_addr_length = I2C_ADDR_BIT_LEN_7, .device_address = 0x18, .scl_speed_hz = 100000 };
    ESP_ERROR_CHECK(i2c_master_bus_add_device(i2c_bus, &dc, &es_dev));

    // I2S first so MCLK is running before the codec is configured; receive only, with deep DMA buffers (8 x 1024 frames)
    i2s_chan_config_t cc = I2S_CHANNEL_DEFAULT_CONFIG(I2S_NUM_0, I2S_ROLE_MASTER);
    cc.dma_desc_num = 8;
    cc.dma_frame_num = 1024;
    ESP_ERROR_CHECK(i2s_new_channel(&cc, NULL, &rx));
    i2s_std_config_t sc = {
        .clk_cfg = I2S_STD_CLK_DEFAULT_CONFIG(FS),
        .slot_cfg = I2S_STD_PHILIPS_SLOT_DEFAULT_CONFIG(I2S_DATA_BIT_WIDTH_16BIT, I2S_SLOT_MODE_STEREO),
        .gpio_cfg = { .mclk = 13, .bclk = 12, .ws = 10, .dout = I2S_GPIO_UNUSED, .din = 11 },
    };
    sc.clk_cfg.mclk_multiple = I2S_MCLK_MULTIPLE_256;
    ESP_ERROR_CHECK(i2s_channel_init_std_mode(rx, &sc));
    ESP_ERROR_CHECK(i2s_channel_enable(rx));
    vTaskDelay(pdMS_TO_TICKS(50));

    // ES8311 init (256*fs MCLK from pin, I2S 16-bit slave), as in the Music app, plus the analog microphone input
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
    es_w(0x17, 0xBF);                 // ADC volume 0 dB
    es_w(0x31, 0x60);                 // mute the DAC: nothing is played
    set_pga(pga);

    gpio_config_t g = { .pin_bit_mask = 1ULL << PIN_PA, .mode = GPIO_MODE_OUTPUT };
    ESP_ERROR_CHECK(gpio_config(&g));
    gpio_set_level(PIN_PA, 0);        // speaker amp off
}

// ===========================================================================
// Analysis (audio task): level and a Hann-windowed 2048-point FFT, 30 times a second
// ===========================================================================
// The big tables and buffers live in PSRAM: static arrays in internal RAM would starve the 307 KB screen buffer.
static float *hann, *tw_c, *tw_s;
static uint16_t *bitrev;

static void *psram(size_t n)
{
    void *p = heap_caps_calloc(1, n, MALLOC_CAP_SPIRAM);
    assert(p);
    return p;
}

static void fft_tables(void)
{
    hann = psram(NFFT * sizeof(float));
    tw_c = psram(NFFT / 2 * sizeof(float));
    tw_s = psram(NFFT / 2 * sizeof(float));
    bitrev = psram(NFFT * sizeof(uint16_t));
    for (int i = 0; i < NFFT; i++) hann[i] = 0.5f - 0.5f * cosf(2.0f * (float)M_PI * i / NFFT);
    for (int i = 0; i < NFFT / 2; i++) { tw_c[i] = cosf(2.0f * (float)M_PI * i / NFFT); tw_s[i] = -sinf(2.0f * (float)M_PI * i / NFFT); }
    int bits = 0;
    while ((1 << bits) < NFFT) bits++;
    for (int i = 0; i < NFFT; i++) {
        int r = 0;
        for (int b = 0; b < bits; b++) if (i & (1 << b)) r |= 1 << (bits - 1 - b);
        bitrev[i] = (uint16_t)r;
    }
}

static void fft(float *re, float *im)
{
    for (int len = 2; len <= NFFT; len <<= 1) {
        int half = len / 2, step = NFFT / len;
        for (int i = 0; i < NFFT; i += len)
            for (int j = 0; j < half; j++) {
                float wr = tw_c[j * step], wi = tw_s[j * step];
                int a = i + j, b = a + half;
                float tr = re[b] * wr - im[b] * wi, ti = re[b] * wi + im[b] * wr;
                re[b] = re[a] - tr; im[b] = im[a] - ti;
                re[a] += tr;        im[a] += ti;
            }
    }
}

static portMUX_TYPE mux = portMUX_INITIALIZER_UNLOCKED;
static struct {
    uint32_t seq;
    float rms_db, peak_db;                  // of the last chunk, dBFS
    bool clip;
    float *spec;                            // NBIN dBFS values, one per FFT bin (PSRAM)
} shared;

static void audio_task(void *arg)
{
    int16_t *buf = psram(CHUNK * 2 * sizeof(int16_t));
    float *xs = psram(CHUNK * sizeof(float)), *win = psram(NFFT * sizeof(float)), *re = psram(NFFT * sizeof(float));
    float *im = psram(NFFT * sizeof(float)), *spec = psram(NBIN * sizeof(float));
    int wpos = 0, chunks = 0, use_r = 0;
    double el = 0, er = 0;
    while (1) {
        size_t got = 0;
        if (i2s_channel_read(rx, buf, CHUNK * 4, &got, 1000) != ESP_OK || got < CHUNK * 4) continue;
        if (chunks < 20) {                  // the microphone is on one of the two I2S slots: pick the louder one
            for (int i = 0; i < CHUNK; i++) { el += (double)buf[2 * i] * buf[2 * i]; er += (double)buf[2 * i + 1] * buf[2 * i + 1]; }
            if (++chunks == 20) {
                use_r = er > el;
                ESP_LOGI(TAG, "microphone on the %s slot (rms L %.1f, R %.1f)", use_r ? "right" : "left", sqrt(el / (20.0 * CHUNK)), sqrt(er / (20.0 * CHUNK)));
            }
        }
        float mean = 0;
        bool clip = false;
        for (int i = 0; i < CHUNK; i++) {
            int16_t s = buf[2 * i + use_r];
            if (s > 32000 || s < -32000) clip = true;
            xs[i] = s / 32768.0f;
            mean += xs[i];
        }
        mean /= CHUNK;
        double ss = 0;
        float pk = 0;
        for (int i = 0; i < CHUNK; i++) {
            float v = xs[i] - mean;
            ss += (double)v * v;
            if (fabsf(v) > pk) pk = fabsf(v);
            win[(wpos + i) & (NFFT - 1)] = v;
        }
        wpos = (wpos + CHUNK) & (NFFT - 1);
        for (int i = 0; i < NFFT; i++) { re[bitrev[i]] = win[(wpos + i) & (NFFT - 1)] * hann[i]; im[bitrev[i]] = 0; }
        fft(re, im);
        for (int k = 0; k < NBIN; k++) spec[k] = 20.0f * log10f(sqrtf(re[k] * re[k] + im[k] * im[k]) / (NFFT / 4) + 1e-9f);
        float rms = sqrtf((float)(ss / CHUNK));
        portENTER_CRITICAL(&mux);
        shared.rms_db = 20.0f * log10f(rms + 1e-9f);
        shared.peak_db = 20.0f * log10f(pk + 1e-9f);
        shared.clip = clip;
        memcpy(shared.spec, spec, NBIN * sizeof(float));
        shared.seq++;
        portEXIT_CRITICAL(&mux);
    }
}

// ===========================================================================
// The two screens
// ===========================================================================
#define BODY_Y 40
#define BAR_X 16
#define BAR_W 448
#define HIST_N 448                          // 15 s of history

static uint16_t C_BG, C_BAR, C_TXT, C_DIM, C_LINE, C_BTN, C_BTN_ON, C_GREEN, C_YEL, C_RED, C_CYAN;
static uint16_t pal[256];
static float colpos[LCD_W + 1];             // FFT bin position of each spectrogram column

static void colours_init(void)
{
    C_BG = rgb565(8, 8, 12); C_BAR = rgb565(30, 30, 38); C_TXT = rgb565(225, 225, 225); C_DIM = rgb565(140, 140, 152);
    C_LINE = rgb565(50, 50, 62); C_BTN = rgb565(60, 60, 72); C_BTN_ON = rgb565(255, 140, 0);
    C_GREEN = rgb565(60, 210, 90); C_YEL = rgb565(240, 210, 50); C_RED = rgb565(240, 70, 60); C_CYAN = rgb565(80, 200, 230);
    static const float st[6][4] = { { 0, 0, 0, 0 }, { 0.2f, 10, 0, 90 }, { 0.4f, 120, 0, 140 }, { 0.6f, 220, 40, 40 },
                                    { 0.8f, 255, 170, 0 }, { 1.0f, 255, 255, 200 } };
    for (int i = 0; i < 256; i++) {
        float t = i / 255.0f;
        int s = 0;
        while (s < 4 && t > st[s + 1][0]) s++;
        float u = (t - st[s][0]) / (st[s + 1][0] - st[s][0]);
        pal[i] = rgb565((int)(st[s][1] + u * (st[s + 1][1] - st[s][1])), (int)(st[s][2] + u * (st[s + 1][2] - st[s][2])),
                        (int)(st[s][3] + u * (st[s + 1][3] - st[s][3])));
    }
    float binhz = (float)FS / NFFT, ratio = FMAX / FMIN;
    for (int x = 0; x <= LCD_W; x++) colpos[x] = FMIN * powf(ratio, (float)x / LCD_W) / binhz;
}

static float level_of(float db_lo, float db_hi, float db)
{
    float t = (db - db_lo) / (db_hi - db_lo);
    return t < 0 ? 0 : (t > 1 ? 1 : t);
}

// Spectrum value for a column: the loudest bin it covers, or an interpolation between two bins when it covers less than one.
static float col_db(const float *spec, int x)
{
    float a = colpos[x], b = colpos[x + 1];
    int i0 = (int)a;
    if (b - a >= 1.0f) {
        int i1 = (int)b;
        if (i1 > NBIN - 1) i1 = NBIN - 1;
        float m = spec[i0];
        for (int i = i0 + 1; i <= i1; i++) if (spec[i] > m) m = spec[i];
        return m;
    }
    if (i0 > NBIN - 2) i0 = NBIN - 2;
    float f = a - i0;
    return spec[i0] * (1 - f) + spec[i0 + 1] * f;
}

static int mode;                            // 0 = VU meter, 1 = spectrogram
static float vu_db = -90, hold_db = -90, hold_t;
static float hist[HIST_N];
static float spec_peak[LCD_W];
static bool clip_hold;
static int clip_frames;

static uint16_t zone_colour(float db) { return db > -6 ? C_RED : (db > -18 ? C_YEL : C_GREEN); }
static uint16_t dim(uint16_t c) { uint16_t v = (c << 8) | (c >> 8); return rgb565(((v >> 11) & 31) * 8 / 5, ((v >> 5) & 63) * 4 / 5, (v & 31) * 8 / 5); }

static void draw_topbar(void)
{
    fill_rect(0, 0, LCD_W, BODY_Y, C_BAR);
    fill_rect(0, BODY_Y - 1, LCD_W, 1, C_LINE);
    fill_rect(4, 4, 68, 32, mode == 0 ? C_BTN_ON : C_BTN);
    draw_text_c(38, 12, "VU", 1, mode == 0 ? rgb565(12, 12, 16) : C_TXT);
    fill_rect(78, 4, 100, 32, mode == 1 ? C_BTN_ON : C_BTN);
    draw_text_c(128, 12, "SPECTRUM", 1, mode == 1 ? rgb565(12, 12, 16) : C_TXT);
    fill_rect(262, 2, 44, 36, C_BTN); draw_text_c(284, 4, "-", 2, C_TXT);
    char b[32];
    snprintf(b, sizeof b, "GAIN %d dB", gain_db());
    draw_text_c(346, 12, b, 1, C_TXT);
    fill_rect(386, 2, 44, 36, C_BTN); draw_text_c(408, 4, "+", 2, C_TXT);
}

static void draw_vu(void)
{
    char b[64];
    fill_rect(0, BODY_Y, LCD_W, LCD_H - BODY_Y, C_BG);
    // numbers
    draw_text(BAR_X, 52, "LEVEL", 1, C_DIM);
    snprintf(b, sizeof b, "%.1f dBFS", vu_db < -99 ? -99.9 : vu_db);
    draw_text(BAR_X, 70, b, 3, C_TXT);
    draw_text(300, 52, "PEAK HOLD", 1, C_DIM);
    snprintf(b, sizeof b, "%.1f", hold_db < -99 ? -99.9 : hold_db);
    draw_text(300, 74, b, 2, zone_colour(hold_db));
    if (clip_hold) draw_text(404, 74, "CLIP", 2, C_RED);
    // segmented bar, -60 .. 0 dBFS
    for (int s = 0; s < 32; s++) {
        float db = -60.0f + (s + 0.5f) * 60.0f / 32.0f;
        uint16_t c = zone_colour(db);
        fill_rect(BAR_X + s * 14, 118, 12, 56, db <= vu_db ? c : dim(dim(dim(c))));
    }
    int px = BAR_X + (int)(level_of(-60, 0, hold_db) * (BAR_W - 1));
    fill_rect(px - 1, 112, 3, 68, C_TXT);
    for (int d = -60; d <= 0; d += 10) {
        int x = BAR_X + (int)((d + 60) / 60.0f * (BAR_W - 1));
        fill_rect(x, 180, 1, 5, C_DIM);
        snprintf(b, sizeof b, "%d", d);
        draw_text_c(x, 186, b, 1, C_DIM);
    }
    // history of the last 15 s
    draw_text(BAR_X, 208, "LAST 15 SECONDS", 1, C_DIM);
    int y0 = 228, h = 84;
    fill_rect(BAR_X, y0, HIST_N, h, rgb565(14, 14, 20));
    for (int i = 0; i < HIST_N; i++) {
        float db = hist[i];
        int bh = (int)(level_of(-80, 0, db) * h);
        if (bh > 0) fill_rect(BAR_X + i, y0 + h - bh, 1, bh, zone_colour(db));
    }
}

static void spec_scale(void)
{
    static const struct { float f; const char *t; } ticks[] = { { 100, "100" }, { 200, "200" }, { 500, "500" }, { 1000, "1k" },
        { 2000, "2k" }, { 5000, "5k" }, { 10000, "10k" } };
    for (int i = 0; i < 7; i++) {
        int x = (int)(LCD_W * logf(ticks[i].f / FMIN) / logf(FMAX / FMIN));
        fill_rect(x, 298, 1, 4, C_DIM);
        draw_text_c(x, 302, ticks[i].t, 1, C_DIM);
    }
    draw_text(LCD_W - 24, 302, "Hz", 1, C_DIM);
}

#define SP_Y0 44                            // live spectrum area
#define SP_H 92
#define WF_Y0 140                           // waterfall: newest row on top, scrolls down
#define WF_Y1 298

static void draw_spec_new(const float *spec)
{
    char b[64];
    // live spectrum
    fill_rect(0, SP_Y0, LCD_W, SP_H, C_BG);
    for (int d = -40; d >= -80; d -= 20) fill_rect(0, SP_Y0 + SP_H - 1 - (int)(level_of(DB_LO, DB_HI, (float)d) * (SP_H - 1)), LCD_W, 1, C_LINE);
    for (int x = 0; x < LCD_W; x++) {
        float db = col_db(spec, x);
        int h = (int)(level_of(DB_LO, DB_HI, db) * (SP_H - 1));
        if (h > 0) fill_rect(x, SP_Y0 + SP_H - 1 - h, 1, h + 1, pal[(int)(level_of(DB_LO, DB_HI, db) * 255)]);
        spec_peak[x] = db > spec_peak[x] ? db : spec_peak[x] - 0.6f;
        int ph = (int)(level_of(DB_LO, DB_HI, spec_peak[x]) * (SP_H - 1));
        fill_rect(x, SP_Y0 + SP_H - 1 - ph, 1, 1, C_TXT);
    }
    // strongest component
    int kb = 2;
    for (int k = 2; k < NBIN; k++) if (spec[k] > spec[kb]) kb = k;
    snprintf(b, sizeof b, "PEAK %.0f Hz  %.0f dB", kb * (float)FS / NFFT, spec[kb]);
    draw_text(6, SP_Y0 + 2, b, 1, C_CYAN);
    // waterfall: scroll down one row, new row on top
    memmove(fb + (WF_Y0 + 1) * LCD_W, fb + WF_Y0 * LCD_W, (size_t)(WF_Y1 - WF_Y0 - 1) * LCD_W * 2);
    for (int x = 0; x < LCD_W; x++) fb[WF_Y0 * LCD_W + x] = pal[(int)(level_of(DB_LO, DB_HI, col_db(spec, x)) * 255)];
    fill_rect(0, WF_Y1, LCD_W, LCD_H - WF_Y1, C_BG);
    spec_scale();
}

static void enter_mode(int m)
{
    mode = m;
    fill_rect(0, BODY_Y, LCD_W, LCD_H - BODY_Y, C_BG);
    for (int x = 0; x < LCD_W; x++) spec_peak[x] = DB_LO;
    for (int i = 0; i < HIST_N; i++) hist[i] = -90;
    vu_db = hold_db = -90;
}

// ===========================================================================
// Main loop
// ===========================================================================
void app_main(void)
{
    display_init();
    touch_init();
    audio_init();
    shared.spec = psram(NBIN * sizeof(float));
    fft_tables();
    colours_init();
    xTaskCreatePinnedToCore(audio_task, "audio", 8192, NULL, 5, NULL, 1);
    ESP_LOGI(TAG, "started");

    enter_mode(0);
    draw_topbar();
    flush_rows(0, LCD_H);
    uint32_t last = 0;
    bool was_down = false, top_dirty = false;
    int64_t t_prev = esp_timer_get_time(), t_log = t_prev;
    int frames = 0;
    float *spec = psram(NBIN * sizeof(float));
    while (1) {
        int tx, ty;
        bool down = touch_read(&tx, &ty);
        if (down && !was_down) {
            if (ty < BODY_Y) {
                if (tx >= 4 && tx < 72 && mode != 0) { enter_mode(0); top_dirty = true; }
                else if (tx >= 78 && tx < 178 && mode != 1) { enter_mode(1); top_dirty = true; }
                else if (tx >= 262 && tx < 306) { set_pga(pga - 1); top_dirty = true; }
                else if (tx >= 386 && tx < 430) { set_pga(pga + 1); top_dirty = true; }
            }
        }
        was_down = down;

        float rms, pk;
        bool clip;
        uint32_t seq;
        portENTER_CRITICAL(&mux);
        seq = shared.seq;
        rms = shared.rms_db; pk = shared.peak_db; clip = shared.clip;
        if (seq != last) memcpy(spec, shared.spec, NBIN * sizeof(float));
        portEXIT_CRITICAL(&mux);
        if (seq == last) { vTaskDelay(1); continue; }
        last = seq;

        int64_t now = esp_timer_get_time();
        float dt = (now - t_prev) / 1e6f;
        t_prev = now;
        // meter ballistics: fast attack, slow release; peak hold 1.5 s then falls 20 dB/s
        vu_db += (rms > vu_db ? 0.7f : 0.15f) * (rms - vu_db);
        if (pk > hold_db) { hold_db = pk; hold_t = 0; }
        else { hold_t += dt; if (hold_t > 1.5f) hold_db -= 20.0f * dt; }
        if (hold_db < -90) hold_db = -90;
        memmove(hist, hist + 1, (HIST_N - 1) * sizeof hist[0]);
        hist[HIST_N - 1] = rms;
        if (clip) clip_frames = 30;
        clip_hold = clip_frames > 0;
        if (clip_frames > 0) clip_frames--;

        if (mode == 0) draw_vu(); else draw_spec_new(spec);
        if (top_dirty) { draw_topbar(); flush_rows(0, LCD_H); top_dirty = false; }
        else flush_rows(BODY_Y, LCD_H);

        frames++;
        if (now - t_log > 5000000) {
            ESP_LOGI(TAG, "%.1f frames/s, level %.1f dBFS, peak %.1f dBFS, gain %d dB", frames * 1e6 / (double)(now - t_log), rms, pk, gain_db());
            frames = 0; t_log = now;
        }
    }
}
