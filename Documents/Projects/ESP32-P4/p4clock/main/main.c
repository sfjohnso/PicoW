// Clock + weather display for the Waveshare ESP32-P4 3.5" board (ST7796, 480x320 landscape).
//
// Layout and colors follow the ESP32-S3 web page: analog dial, digital time, date,
// weather tiles for the location set in user_config.h. Between 22:00 and 07:00 local time every
// color is replaced by a muted red of the same brightness.
//
// Wi-Fi: esp_wifi_remote -> SDIO -> ESP32-C6.  Time: SNTP.  Weather: Open-Meteo over HTTPS.
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#include <time.h>
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "freertos/event_groups.h"
#include "freertos/semphr.h"
#include "driver/gpio.h"
#include "driver/ledc.h"
#include "driver/i2c_master.h"
#include "driver/spi_master.h"
#include "esp_lcd_panel_io.h"
#include "esp_heap_caps.h"
#include "esp_wifi.h"
#include "esp_event.h"
#include "esp_netif.h"
#include "esp_netif_sntp.h"
#include "esp_log.h"
#include "esp_timer.h"
#include "esp_http_client.h"
#include "esp_crt_bundle.h"
#include "nvs_flash.h"
#include "cJSON.h"
#include "font.h"
#include "settings.h"
#include "portal.h"
#include "ambient.h"
#include "qrcode.h"

static const char *TAG = "p4clock";

#define NIGHT_START_HOUR 22
#define NIGHT_END_HOUR 7
#define WEATHER_EVERY_S (30 * 60)

// ---------------------------------------------------------------------------
// ST7796 LCD over SPI: MOSI20 SCLK21 CS23 DC26 RST27 BL28
// ---------------------------------------------------------------------------
#define LCD_W 480
#define LCD_H 320
#define PIN_MOSI 20
#define PIN_SCLK 21
#define PIN_LCD_CS 23
#define PIN_LCD_DC 26
#define PIN_LCD_RST 27
#define PIN_LCD_BL 28
#define BOUNCE_PX 12800           // 25,600 bytes per SPI transfer

static esp_lcd_panel_io_handle_t lcd_io;
static SemaphoreHandle_t lcd_done;
static uint16_t *bounce;
static const uint8_t MADCTL[2] = { 0x28, 0xE8 };   // landscape, and the same rotated 180 degrees
static int rot;                                    // 0/1: index into MADCTL
static volatile bool flip_req;                     // set by touch_task, handled by ui_task

static bool on_done(esp_lcd_panel_io_handle_t io, esp_lcd_panel_io_event_data_t *e, void *u)
{
    BaseType_t w = pdFALSE;
    xSemaphoreGiveFromISR(lcd_done, &w);
    return w == pdTRUE;
}

static void cmd(uint8_t c, const uint8_t *p, size_t n)
{
    ESP_ERROR_CHECK(esp_lcd_panel_io_tx_param(lcd_io, c, p, n));
}

// Push a w*h block of native-order RGB565 (stride w) to the screen at (x,y).
// If src is NULL the block is filled with 'color' instead.
static void push(int x, int y, int w, int h, const uint16_t *src, uint16_t color)
{
    int x1 = x + w - 1;
    uint8_t ca[4] = { x >> 8, x & 0xFF, x1 >> 8, x1 & 0xFF };
    cmd(0x2A, ca, 4);
    int per = BOUNCE_PX / w;
    for (int r = 0; r < h; r += per) {
        int n = (h - r < per) ? h - r : per;
        int ya = y + r, yb = ya + n - 1;
        uint8_t ra[4] = { ya >> 8, ya & 0xFF, yb >> 8, yb & 0xFF };
        cmd(0x2B, ra, 4);
        if (src) {
            const uint16_t *p = src + r * w;
            for (int i = 0; i < w * n; i++) bounce[i] = (p[i] << 8) | (p[i] >> 8);
        } else {
            uint16_t sw = (color << 8) | (color >> 8);
            for (int i = 0; i < w * n; i++) bounce[i] = sw;
        }
        ESP_ERROR_CHECK(esp_lcd_panel_io_tx_color(lcd_io, 0x2C, bounce, w * n * 2));
        xSemaphoreTake(lcd_done, portMAX_DELAY);
    }
}

// ---------------------------------------------------------------------------
// Backlight: PWM on the N-FET gate (5 kHz, 10 bit). Dimmed at night.
// ---------------------------------------------------------------------------
#define NIGHT_BACKLIGHT_PCT 8                // brightness in a dark room (percent of full)
#define BL_FADE_MS 1500                      // fade time between day and night levels
#define BL_MODE LEDC_LOW_SPEED_MODE
#define BL_CHANNEL LEDC_CHANNEL_0

static void backlight_init(void)
{
    ledc_timer_config_t t = { .speed_mode = BL_MODE, .duty_resolution = LEDC_TIMER_10_BIT, .timer_num = LEDC_TIMER_0,
                              .freq_hz = 5000, .clk_cfg = LEDC_AUTO_CLK };
    ESP_ERROR_CHECK(ledc_timer_config(&t));
    ledc_channel_config_t c = { .gpio_num = PIN_LCD_BL, .speed_mode = BL_MODE, .channel = BL_CHANNEL,
                                .timer_sel = LEDC_TIMER_0, .duty = 0, .hpoint = 0 };
    ESP_ERROR_CHECK(ledc_channel_config(&c));
    ledc_fade_func_install(0);
}

static void backlight_set(int percent, int fade_ms)
{
    uint32_t duty = 1023u * percent / 100;
    ledc_fade_stop(BL_MODE, BL_CHANNEL);
    if (fade_ms > 0) {
        ledc_set_fade_with_time(BL_MODE, BL_CHANNEL, duty, fade_ms);
        ledc_fade_start(BL_MODE, BL_CHANNEL, LEDC_FADE_NO_WAIT);
    } else {
        ledc_set_duty(BL_MODE, BL_CHANNEL, duty);
        ledc_update_duty(BL_MODE, BL_CHANNEL);
    }
}

static void lcd_init(void)
{
    gpio_config_t o = { .pin_bit_mask = (1ULL << PIN_LCD_RST), .mode = GPIO_MODE_OUTPUT };
    gpio_config(&o);
    backlight_init();                        // off until the panel is initialised
    gpio_set_level(PIN_LCD_RST, 0);
    vTaskDelay(pdMS_TO_TICKS(20));
    gpio_set_level(PIN_LCD_RST, 1);
    vTaskDelay(pdMS_TO_TICKS(120));

    spi_bus_config_t bc = {
        .mosi_io_num = PIN_MOSI, .miso_io_num = -1, .sclk_io_num = PIN_SCLK,
        .quadwp_io_num = -1, .quadhd_io_num = -1,
        .max_transfer_sz = BOUNCE_PX * 2,
    };
    ESP_ERROR_CHECK(spi_bus_initialize(SPI2_HOST, &bc, SPI_DMA_CH_AUTO));
    lcd_done = xSemaphoreCreateBinary();
    esp_lcd_panel_io_spi_config_t ic = {
        .dc_gpio_num = PIN_LCD_DC, .cs_gpio_num = PIN_LCD_CS,
        .pclk_hz = 40 * 1000 * 1000, .lcd_cmd_bits = 8, .lcd_param_bits = 8,
        .spi_mode = 0, .trans_queue_depth = 4,
        .on_color_trans_done = on_done,
    };
    ESP_ERROR_CHECK(esp_lcd_new_panel_io_spi((esp_lcd_spi_bus_handle_t)SPI2_HOST, &ic, &lcd_io));
    bounce = heap_caps_malloc(BOUNCE_PX * 2, MALLOC_CAP_DMA);

    cmd(0x01, NULL, 0);
    vTaskDelay(pdMS_TO_TICKS(150));
    cmd(0x11, NULL, 0);
    vTaskDelay(pdMS_TO_TICKS(120));
    cmd(0xF0, (uint8_t[]){0xC3}, 1);
    cmd(0xF0, (uint8_t[]){0x96}, 1);
    cmd(0x36, &MADCTL[rot], 1);               // landscape (MV), BGR
    cmd(0x3A, (uint8_t[]){0x55}, 1);          // 16 bits/pixel
    cmd(0xB4, (uint8_t[]){0x01}, 1);
    cmd(0xF0, (uint8_t[]){0x3C}, 1);
    cmd(0xF0, (uint8_t[]){0x69}, 1);
    cmd(0x21, NULL, 0);                       // inversion on (IPS panel)
    cmd(0x29, NULL, 0);
    vTaskDelay(pdMS_TO_TICKS(50));
    push(0, 0, LCD_W, LCD_H, NULL, 0);
    backlight_set(NIGHT_BACKLIGHT_PCT, 0);   // start dim: a bright flash at night is worse than a dim second by day
}

// ---------------------------------------------------------------------------
// Palette: day colors are the ones from the S3 web page; night = muted red
// ---------------------------------------------------------------------------
typedef struct {
    uint16_t bg, dial, edge, tick, tickm, num, hand, sec, text, loc, date, tz,
             tbg, tedge, label, cond, foot;
} pal_t;
static pal_t P;

static uint16_t rgb565(int r, int g, int b)
{
    return ((r & 0xF8) << 8) | ((g & 0xFC) << 3) | (b >> 3);
}

static uint16_t mk(uint32_t rgb, bool night)
{
    int r = (rgb >> 16) & 255, g = (rgb >> 8) & 255, b = rgb & 255;
    if (!night) return rgb565(r, g, b);
    int y = (299 * r + 587 * g + 114 * b) / 1000;     // brightness
    return rgb565(y * 75 / 100, y * 15 / 200, y * 15 / 200);      // muted red at 75% of the day brightness
}

static void set_palette(bool n)
{
    P.bg    = mk(0x121826, n);  P.dial  = mk(0x0e1422, n);  P.edge  = mk(0x3b4869, n);
    P.tick  = mk(0x7686a9, n);  P.tickm = mk(0xaebbd6, n);  P.num   = mk(0xcfe0ff, n);
    P.hand  = mk(0xe8edf6, n);  P.text  = mk(0xe8edf6, n);  P.loc   = mk(0x8aa0c6, n);
    P.date  = mk(0xaebbd6, n);  P.tz    = mk(0x7f8db0, n);  P.tbg   = mk(0x0e1422, n);
    P.tedge = mk(0x232d44, n);  P.label = mk(0x7f8db0, n);  P.cond  = mk(0xcfe0ff, n);
    P.foot  = mk(0x8795bb, n);
    P.sec   = n ? rgb565(212, 30, 30) : rgb565(0xff, 0x6b, 0x6b);   // keep the second hand visible
}

// ---------------------------------------------------------------------------
// Drawing primitives on an RGB565 surface
// ---------------------------------------------------------------------------
typedef struct { uint16_t *px; int w, h; } surf_t;

static inline void blend(uint16_t *d, uint16_t c, int a)
{
    if (a <= 0) return;
    if (a >= 255) { *d = c; return; }
    int dr = *d >> 11, dg = (*d >> 5) & 63, db = *d & 31;
    int cr = c >> 11,  cg = (c >> 5) & 63,  cb = c & 31;
    dr = (cr * a + dr * (255 - a)) / 255;
    dg = (cg * a + dg * (255 - a)) / 255;
    db = (cb * a + db * (255 - a)) / 255;
    *d = (dr << 11) | (dg << 5) | db;
}

static void surf_fill(surf_t *s, uint16_t c)
{
    for (int i = 0; i < s->w * s->h; i++) s->px[i] = c;
}

static int text_w(const font_t *f, const char *str)
{
    int w = 0;
    for (const unsigned char *p = (const unsigned char *)str; *p; p++)
        if (*p >= 32) w += f->g[*p - 32].adv;
    return w;
}

static int draw_text(surf_t *s, const font_t *f, int x, int base, const char *str, uint16_t c)
{
    for (const unsigned char *p = (const unsigned char *)str; *p; p++) {
        if (*p < 32) continue;
        const glyph_t *g = &f->g[*p - 32];
        const uint8_t *a = f->alpha + g->off;
        for (int j = 0; j < g->h; j++) {
            int py = base + g->yo + j;
            if (py < 0 || py >= s->h) continue;
            for (int i = 0; i < g->w; i++) {
                int px = x + g->xo + i;
                if (px < 0 || px >= s->w) continue;
                blend(&s->px[py * s->w + px], c, a[j * g->w + i]);
            }
        }
        x += g->adv;
    }
    return x;
}

static void draw_text_c(surf_t *s, const font_t *f, int cx, int base, const char *str, uint16_t c)
{
    draw_text(s, f, cx - text_w(f, str) / 2, base, str, c);
}

static inline int clampi(int v, int lo, int hi) { return v < lo ? lo : (v > hi ? hi : v); }

// Anti-aliased line segment with round ends.
static void aa_line(surf_t *s, float x0, float y0, float x1, float y1, float width, uint16_t c)
{
    float hw = width / 2;
    int xa = clampi((int)floorf(fminf(x0, x1) - hw - 1), 0, s->w - 1);
    int xb = clampi((int)ceilf(fmaxf(x0, x1) + hw + 1), 0, s->w - 1);
    int ya = clampi((int)floorf(fminf(y0, y1) - hw - 1), 0, s->h - 1);
    int yb = clampi((int)ceilf(fmaxf(y0, y1) + hw + 1), 0, s->h - 1);
    float dx = x1 - x0, dy = y1 - y0, l2 = dx * dx + dy * dy;
    for (int y = ya; y <= yb; y++) {
        for (int x = xa; x <= xb; x++) {
            float px = x + 0.5f - x0, py = y + 0.5f - y0;
            float t = l2 > 0 ? (px * dx + py * dy) / l2 : 0;
            t = t < 0 ? 0 : (t > 1 ? 1 : t);
            float ex = px - t * dx, ey = py - t * dy;
            float d = sqrtf(ex * ex + ey * ey);
            int a = (int)((hw - d + 0.5f) * 255);
            blend(&s->px[y * s->w + x], c, a);
        }
    }
}

static void aa_disc(surf_t *s, float cx, float cy, float r, uint16_t c)
{
    int xa = clampi((int)(cx - r - 1), 0, s->w - 1), xb = clampi((int)(cx + r + 1), 0, s->w - 1);
    int ya = clampi((int)(cy - r - 1), 0, s->h - 1), yb = clampi((int)(cy + r + 1), 0, s->h - 1);
    for (int y = ya; y <= yb; y++)
        for (int x = xa; x <= xb; x++) {
            float d = sqrtf((x + 0.5f - cx) * (x + 0.5f - cx) + (y + 0.5f - cy) * (y + 0.5f - cy));
            blend(&s->px[y * s->w + x], c, (int)((r - d + 0.5f) * 255));
        }
}

static void rrect(surf_t *s, int x, int y, int w, int h, float r, uint16_t c)
{
    float cx = x + w / 2.0f, cy = y + h / 2.0f, hx = w / 2.0f - r, hy = h / 2.0f - r;
    for (int py = clampi(y - 1, 0, s->h - 1); py <= clampi(y + h, 0, s->h - 1); py++)
        for (int px = clampi(x - 1, 0, s->w - 1); px <= clampi(x + w, 0, s->w - 1); px++) {
            float qx = fabsf(px + 0.5f - cx) - hx, qy = fabsf(py + 0.5f - cy) - hy;
            float ox = qx > 0 ? qx : 0, oy = qy > 0 ? qy : 0;
            float d = sqrtf(ox * ox + oy * oy) + fminf(fmaxf(qx, qy), 0) - r;
            blend(&s->px[py * s->w + px], c, (int)((0.5f - d) * 255));
        }
}

// ---------------------------------------------------------------------------
// Layout (screen coordinates)
// ---------------------------------------------------------------------------
#define LEFT_W 240                       // left column: dial, time, date
#define DIAL 220
#define DIAL_X ((LEFT_W - DIAL) / 2)
#define DIAL_Y 39                        // visible ring spans y 43.4 .. 254.6
#define K (DIAL / 100.0f)                // S3 page used a 100-unit SVG viewBox
// Left column, top to bottom, with ~15.7 px between date digits, ring, time digits and footer text:
#define DATE_Y 12                        // date baseline at y 28 (digits 12 px tall)
#define DATE_H 24
#define TIME_Y 264                       // time baseline at y 286 (digits 16 px tall)
#define TIME_H 28
#define RX 252                           // right column: x of tiles, width RW
#define RW 216
#define HDR_Y 6
#define COND_Y 30
#define COND_H 32
#define TILE_Y0 70
#define TILE_STEP 56
#define TILE_H 50
#define FOOT_Y 298
#define FOOT_H 18
#define SCRATCH_H 62

static uint16_t *face_bg, *dial_buf, *scratch;
static surf_t sc;                         // scratch region currently being drawn

static void region_begin(int w, int h)
{
    sc.px = scratch; sc.w = w; sc.h = h;
    surf_fill(&sc, P.bg);
}

static void region_end(int x, int y) { push(x, y, sc.w, sc.h, sc.px, 0); }

static void build_face(void)
{
    surf_t s = { face_bg, DIAL, DIAL };
    surf_fill(&s, P.bg);
    float c = DIAL / 2.0f, R = 48 * K;
    aa_disc(&s, c, c, R, P.edge);
    aa_disc(&s, c, c, R - 1.5f * K - 1.0f, P.dial);   // ring = 1.5 units + 1 px
    for (int i = 0; i < 60; i++) {
        bool maj = (i % 5 == 0);
        float a = i * 6 * (float)M_PI / 180, sn = sinf(a), cs = cosf(a);
        float r1 = 46 * K, r2 = (maj ? 41.2f : 43.5f) * K;
        aa_line(&s, c + r1 * sn, c - r1 * cs, c + r2 * sn, c - r2 * cs,
                (maj ? 1.4f : 0.6f) * K, maj ? P.tickm : P.tick);
    }
    for (int i = 1; i <= 12; i++) {
        float a = i * 30 * (float)M_PI / 180;
        char t[4];
        snprintf(t, sizeof t, "%d", i);
        draw_text_c(&s, &f_num, (int)(c + 37 * K * sinf(a) + 0.5f),
                    (int)(c - 37 * K * cosf(a) + 5.5f), t, P.num);
    }
}

static void draw_dial(float hours, float minutes, float seconds)
{
    memcpy(dial_buf, face_bg, DIAL * DIAL * 2);
    surf_t s = { dial_buf, DIAL, DIAL };
    float c = DIAL / 2.0f;
    float ah = hours * 30 * (float)M_PI / 180, am = minutes * 6 * (float)M_PI / 180,
          as = seconds * 6 * (float)M_PI / 180;
    aa_line(&s, c, c, c + 21 * K * sinf(ah), c - 21 * K * cosf(ah), 3 * K, P.hand);
    aa_line(&s, c, c, c + 31 * K * sinf(am), c - 31 * K * cosf(am), 2 * K, P.hand);
    aa_line(&s, c, c, c + 35 * K * sinf(as), c - 35 * K * cosf(as), 1 * K, P.sec);
    aa_disc(&s, c, c, 2.4f * K, P.sec);
    push(DIAL_X, DIAL_Y, DIAL, DIAL, dial_buf, 0);
}

// ---------------------------------------------------------------------------
// Weather data (written by net_task, read by the UI task)
// ---------------------------------------------------------------------------
typedef struct { float temp, feels, wind; int hum, code, wdir; int64_t stamp_s; } wx_t;
static wx_t wx;
static int wx_ver;                        // 0 = none yet
static SemaphoreHandle_t wx_lock;
static volatile int net_state;            // 0 connecting, 1 have IP
static char ip_str[16] = "";

static const struct { int code; const char *txt; } WMO[] = {
    {0,"Clear sky"},{1,"Mainly clear"},{2,"Partly cloudy"},{3,"Overcast"},{45,"Fog"},
    {48,"Rime fog"},{51,"Light drizzle"},{53,"Drizzle"},{55,"Heavy drizzle"},
    {56,"Freezing drizzle"},{57,"Freezing drizzle"},{61,"Light rain"},{63,"Rain"},
    {65,"Heavy rain"},{66,"Freezing rain"},{67,"Freezing rain"},{71,"Light snow"},
    {73,"Snow"},{75,"Heavy snow"},{77,"Snow grains"},{80,"Light showers"},{81,"Showers"},
    {82,"Violent showers"},{85,"Snow showers"},{86,"Snow showers"},{95,"Thunderstorm"},
    {96,"Thunderstorm + hail"},{99,"Thunderstorm + hail"},
};

static const char *wmo_text(int code)
{
    static char buf[16];
    for (unsigned i = 0; i < sizeof WMO / sizeof WMO[0]; i++)
        if (WMO[i].code == code) return WMO[i].txt;
    snprintf(buf, sizeof buf, "code %d", code);
    return buf;
}

static const char *compass(int deg)
{
    static const char *n[] = { "N", "NE", "E", "SE", "S", "SW", "W", "NW" };
    return n[((deg + 22) / 45) % 8];
}

// ---------------------------------------------------------------------------
// UI blocks
// ---------------------------------------------------------------------------
static void draw_header(void)
{
    region_begin(RW, 20);
    char lbl[sizeof settings.label];
    strlcpy(lbl, settings.label, sizeof lbl);
    while (lbl[0] && text_w(&f_lbl, lbl) > RW - 8) lbl[strlen(lbl) - 1] = 0;   // keep it inside the column
    draw_text_c(&sc, &f_lbl, RW / 2, 14, lbl, P.loc);
    region_end(RX, HDR_Y);
}

static void draw_time(const struct tm *tm)
{
    char t[16], ap[3], zn[8];
    int h12 = tm->tm_hour % 12 ? tm->tm_hour % 12 : 12;
    snprintf(t, sizeof t, "%d:%02d:%02d", h12, tm->tm_min, tm->tm_sec);
    snprintf(ap, sizeof ap, "%s", tm->tm_hour < 12 ? "AM" : "PM");
    strftime(zn, sizeof zn, "%Z", tm);                   // PST / PDT / ... from TZ_POSIX
    int tw = text_w(&f_time, t), aw = text_w(&f_val, ap), zw = text_w(&f_val, zn);
    int x = (LEFT_W - (tw + 6 + aw + 6 + zw)) / 2;
    region_begin(LEFT_W, TIME_H);
    draw_text(&sc, &f_time, x, 22, t, P.text);
    draw_text(&sc, &f_val, x + tw + 6, 22, ap, P.date);
    draw_text(&sc, &f_val, x + tw + 6 + aw + 6, 22, zn, P.date);
    region_end(0, TIME_Y);
}

static void draw_status(const char *msg)
{
    region_begin(LEFT_W, TIME_H);
    draw_text_c(&sc, &f_val, LEFT_W / 2, 28, msg, P.label);
    region_end(0, TIME_Y);
}

static void draw_date(const struct tm *tm)
{
    static const char *WD[] = { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" };
    static const char *MO[] = { "January", "February", "March", "April", "May", "June", "July",
                                "August", "September", "October", "November", "December" };
    char d[48];
    snprintf(d, sizeof d, "%s, %s %d, %d", WD[tm->tm_wday], MO[tm->tm_mon], tm->tm_mday, tm->tm_year + 1900);
    if (text_w(&f_date, d) > LEFT_W - 8)             // too wide for the column: abbreviate the month
        snprintf(d, sizeof d, "%s, %.3s %d, %d", WD[tm->tm_wday], MO[tm->tm_mon], tm->tm_mday, tm->tm_year + 1900);
    region_begin(LEFT_W, DATE_H);
    draw_text_c(&sc, &f_date, LEFT_W / 2, 16, d, P.date);
    region_end(0, DATE_Y);
}

static void tile(int x, int y, int w, int h, const char *label, const char *val, const font_t *vf, int vbase)
{
    rrect(&sc, x, y, w, h, 9, P.tedge);
    rrect(&sc, x + 1, y + 1, w - 2, h - 2, 8, P.tbg);
    draw_text(&sc, &f_lbl, x + 12, y + 17, label, P.label);
    draw_text(&sc, vf, x + 12, y + vbase, val, P.text);
}

static void draw_weather(void)
{
    wx_t w;
    xSemaphoreTake(wx_lock, portMAX_DELAY);
    w = wx;
    xSemaphoreGive(wx_lock);

    region_begin(RW, COND_H);
    rrect(&sc, 0, 0, RW, COND_H, 9, P.tedge);
    rrect(&sc, 1, 1, RW - 2, COND_H - 2, 8, P.tbg);
    draw_text_c(&sc, &f_val, RW / 2, 23, wmo_text(w.code), P.cond);
    region_end(RX, COND_Y);

    const char *lab[4] = { "TEMPERATURE", "FEELS LIKE", "HUMIDITY", "WIND" };
    const font_t *vf[4] = { &f_temp, &f_val, &f_val, &f_val };
    const int vb[4] = { 43, 41, 41, 41 };
    char v[4][24];
    snprintf(v[0], sizeof v[0], "%.0f°F", w.temp);
    snprintf(v[1], sizeof v[1], "%.0f°F", w.feels);
    snprintf(v[2], sizeof v[2], "%d%%", w.hum);
    snprintf(v[3], sizeof v[3], "%.0f mph %s", w.wind, compass(w.wdir));
    for (int i = 0; i < 4; i++) {
        region_begin(RW, TILE_H);
        tile(0, 0, RW, TILE_H, lab[i], v[i], vf[i], vb[i]);
        region_end(RX, TILE_Y0 + i * TILE_STEP);
    }
}

static void draw_footer(void)
{
    char b[64];
    int64_t now = esp_timer_get_time() / 1000000;
    xSemaphoreTake(wx_lock, portMAX_DELAY);
    int ver = wx_ver;
    int64_t age = now - wx.stamp_s;
    xSemaphoreGive(wx_lock);
    if (!ver) snprintf(b, sizeof b, "weather not loaded yet \267 %s", ip_str);
    else if (age < 60) snprintf(b, sizeof b, "weather updated just now \267 %s", ip_str);
    else snprintf(b, sizeof b, "weather updated %d min ago \267 %s", (int)(age / 60), ip_str);
    region_begin(LCD_W, FOOT_H);
    draw_text_c(&sc, &f_lbl, LCD_W / 2, 13, b, P.foot);
    region_end(0, FOOT_Y);
}

// ---------------------------------------------------------------------------
// Setup screen: QR code that joins the clock's own Wi-Fi network, plus the steps in words
// ---------------------------------------------------------------------------
static uint16_t *qr_px;
static int qr_side, qr_scale;

static void qr_render(esp_qrcode_handle_t q)
{
    const int quiet = 2;                                     // quiet-zone modules
    int n = esp_qrcode_get_size(q);
    int side = (n + 2 * quiet) * qr_scale;
    uint16_t *px = malloc(side * side * 2);
    if (!px) return;
    for (int i = 0; i < side * side; i++) px[i] = 0xFFFF;    // white, so phone cameras read it in any palette
    for (int y = 0; y < n; y++)
        for (int x = 0; x < n; x++)
            if (esp_qrcode_get_module(q, x, y))
                for (int dy = 0; dy < qr_scale; dy++)
                    for (int dx = 0; dx < qr_scale; dx++)
                        px[((y + quiet) * qr_scale + dy) * side + (x + quiet) * qr_scale + dx] = 0x0000;
    qr_px = px;
    qr_side = side;
}

// Draws a QR code for `text` with its top edge at y and its horizontal centre at cx.
static void draw_qr(const char *text, int scale, int cx, int y)
{
    esp_qrcode_config_t qc = { .display_func = qr_render, .max_qrcode_version = 6, .qrcode_ecc_level = ESP_QRCODE_ECC_MED };
    qr_px = NULL;
    qr_scale = scale;
    if (esp_qrcode_generate(&qc, text) == ESP_OK && qr_px) {
        push(cx - qr_side / 2, y, qr_side, qr_side, qr_px, 0);
        free(qr_px);
        qr_px = NULL;
    }
}

static void centred_line(int base, const font_t *f, uint16_t c, const char *txt)
{
    region_begin(LCD_W, 24);
    draw_text_c(&sc, f, LCD_W / 2, 18, txt, c);
    region_end(0, base - 18);
}

static void draw_portal(void)
{
    push(0, 0, LCD_W, LCD_H, NULL, P.bg);
    centred_line(30, &f_val, P.text, "Set up your clock");

    char wifi[80];
    snprintf(wifi, sizeof wifi, "WIFI:T:WPA;S:%s;P:%s;;", portal_ssid, portal_pass);
    draw_qr(wifi, 5, 125, 44);                               // left: join the clock's Wi-Fi network
    draw_qr("http://192.168.4.1/", 6, 355, 44);              // right: open the setup page (after joining)

    // captions under each code, centred on it
    region_begin(240, 24);
    draw_text_c(&sc, &f_date, 120, 18, "1. Scan to join the Wi-Fi", P.text);
    region_end(5, 228 - 18);
    region_begin(240, 24);
    draw_text_c(&sc, &f_date, 120, 18, "2. Then scan to open the page", P.text);
    region_end(235, 228 - 18);

    char line[96];
    snprintf(line, sizeof line, "By hand: join %s, password %s, then open 192.168.4.1", portal_ssid, portal_pass);
    centred_line(256, &f_lbl, P.cond, line);

    // status, wrapped over two lines if it is too wide
    char st[sizeof portal_status];
    strlcpy(st, portal_status, sizeof st);
    char *second = NULL;
    if (text_w(&f_date, st) > LCD_W - 16) {
        char *cut = NULL;
        for (char *c = st; *c; c++) {
            if (*c == ' ' && c > st) {
                char keep = *c; *c = 0;
                int fits = text_w(&f_date, st) <= LCD_W - 16;
                *c = keep;
                if (!fits) break;
                cut = c;
            }
        }
        if (cut) { *cut = 0; second = cut + 1; }
    }
    centred_line(second ? 282 : 292, &f_date, P.cond, st);
    if (second) centred_line(304, &f_date, P.cond, second);
}

static void ui_task(void *arg)
{
    face_bg = malloc(DIAL * DIAL * 2);
    dial_buf = malloc(DIAL * DIAL * 2);
    scratch = malloc(LCD_W * SCRATCH_H * 2);
    assert(face_bg && dial_buf && scratch);

    int cur_night = -1, last_state = -1, last_sec = -1, last_min = -1, last_yday = -1, last_wxv = 0, last_sv = 0, last_pv = -1, cur_bl = -1;
    bool in_portal = false;
    bool was_synced = false;
    while (1) {
        struct timeval tv;
        gettimeofday(&tv, NULL);
        bool synced = tv.tv_sec > 1700000000;
        time_t t = tv.tv_sec;
        struct tm tm;
        localtime_r(&t, &tm);
        // Dark red when the room is dark, judged by the camera. Without a working camera: by the time of day
        // (and not while the time zone is still unknown, when the time would be UTC).
        int night;
        if (ambient_state == AMBIENT_DAY || ambient_state == AMBIENT_NIGHT) night = ambient_state == AMBIENT_NIGHT;
        else if (ambient_state == AMBIENT_UNAVAILABLE)
            night = synced && !settings.tz_needed && (tm.tm_hour >= NIGHT_START_HOUR || tm.tm_hour < NIGHT_END_HOUR);
        else night = 0;                                  // the camera has not been read yet

        if (flip_req) {                                  // screen was touched: rotate 180 degrees
            flip_req = false;
            rot ^= 1;
            cmd(0x36, &MADCTL[rot], 1);
            cur_night = -1;                              // forces the full repaint below
            last_pv = -1;                                // and a repaint of the setup screen
            ESP_LOGI(TAG, "touch: display rotated %s", rot ? "180" : "back to normal");
        }
        if (portal_active) {                             // the setup page is open: show its screen instead
            if (!in_portal) { in_portal = true; set_palette(0); last_pv = -1; backlight_set(100, 0); cur_bl = 100; }
            if (portal_version != last_pv) { last_pv = portal_version; draw_portal(); }
            vTaskDelay(pdMS_TO_TICKS(100));
            continue;
        }
        if (in_portal) { in_portal = false; cur_night = -1; cur_bl = -1; }
        // The backlight follows the room too: dim at night, and also until the camera's first reading.
        int want_bl = (night || ambient_state == AMBIENT_UNKNOWN) ? NIGHT_BACKLIGHT_PCT : 100;
        if (want_bl != cur_bl) { cur_bl = want_bl; backlight_set(want_bl, BL_FADE_MS); }
        if (night != cur_night) {                        // palette changed: repaint everything
            cur_night = night;
            set_palette(night);
            build_face();
            push(0, 0, LCD_W, LCD_H, NULL, P.bg);
            draw_header();
            push(DIAL_X, DIAL_Y, DIAL, DIAL, face_bg, 0);
            last_state = last_sec = last_min = last_yday = -1;
            last_wxv = 0;
            was_synced = false;
            ESP_LOGI(TAG, "palette: %s", night ? "night (muted red)" : "day");
        }

        if (settings_version != last_sv) {               // place name changed (address looked up)
            last_sv = settings_version;
            draw_header();
        }
        xSemaphoreTake(wx_lock, portMAX_DELAY);
        int wxv = wx_ver;
        xSemaphoreGive(wx_lock);
        if (wxv != last_wxv) {
            last_wxv = wxv;
            draw_weather();
            last_min = -1;                               // refresh footer too
        }
        if (tm.tm_min != last_min) {
            last_min = tm.tm_min;
            draw_footer();
        }

        if (!synced) {
            if (net_state != last_state) {
                last_state = net_state;
                draw_status(net_state ? "Setting the clock..." : "Connecting to Wi-Fi...");
            }
            vTaskDelay(pdMS_TO_TICKS(200));
            continue;
        }
        if (!was_synced) { was_synced = true; last_sec = -1; last_yday = -1; }

        if (tm.tm_sec != last_sec) {
            last_sec = tm.tm_sec;
            draw_time(&tm);
        }
        if (tm.tm_yday != last_yday) {
            last_yday = tm.tm_yday;
            draw_date(&tm);
        }
        float sf = tm.tm_sec + tv.tv_usec / 1e6f;
        float mf = tm.tm_min + sf / 60;
        float hf = (tm.tm_hour % 12) + mf / 60;
        draw_dial(hf, mf, sf);
        vTaskDelay(1);
    }
}

// ---------------------------------------------------------------------------
// Touch: FT6336-style chip at I2C 0x38 (SDA7 SCL8, reset GPIO29). Any new touch flips the screen.
// ---------------------------------------------------------------------------
#define PIN_TP_RST 29
#define PIN_SDA 7
#define PIN_SCL 8
static i2c_master_bus_handle_t i2c_bus;       // shared by the touch chip and the camera (created in app_main)
#define FLIP_LOCKOUT_MS 800                  // ignore touches right after a flip
#define HOLD_MS 5000                         // holding the screen this long opens the setup page

static void touch_task(void *arg)
{
    gpio_config_t o = { .pin_bit_mask = 1ULL << PIN_TP_RST, .mode = GPIO_MODE_OUTPUT };
    gpio_config(&o);
    gpio_set_level(PIN_TP_RST, 0);           // reset is held low by a resistor until driven
    vTaskDelay(pdMS_TO_TICKS(20));
    gpio_set_level(PIN_TP_RST, 1);
    vTaskDelay(pdMS_TO_TICKS(300));

    i2c_device_config_t dc = { .dev_addr_length = I2C_ADDR_BIT_LEN_7, .device_address = 0x38,
                               .scl_speed_hz = 400000 };
    i2c_master_dev_handle_t dev;
    ESP_ERROR_CHECK(i2c_master_bus_add_device(i2c_bus, &dc, &dev));

    bool was_down = false, flipped_at_press = false, hold_done = false;
    int64_t last_flip_ms = -FLIP_LOCKOUT_MS, down_since_ms = 0;
    while (1) {
        uint8_t reg = 0x02, n = 0;                       // register 2 = number of touch points
        if (i2c_master_transmit_receive(dev, &reg, 1, &n, 1, 100) == ESP_OK) {
            bool down = (n & 0x0F) > 0 && (n & 0x0F) < 3;
            int64_t now_ms = esp_timer_get_time() / 1000;
            if (down && !was_down) {
                down_since_ms = now_ms;
                hold_done = false;
                flipped_at_press = now_ms - last_flip_ms >= FLIP_LOCKOUT_MS;
                if (flipped_at_press) {
                    flip_req = true;
                    last_flip_ms = now_ms;
                }
            }
            if (down && !hold_done && now_ms - down_since_ms >= HOLD_MS) {
                hold_done = true;
                if (flipped_at_press) flip_req = true;     // undo the flip this touch started
                ESP_LOGI(TAG, "touch held: opening the setup page");
                portal_start(NULL);
            }
            was_down = down;
        }
        vTaskDelay(pdMS_TO_TICKS(30));
    }
}

// ---------------------------------------------------------------------------
// Wi-Fi, SNTP, weather
// ---------------------------------------------------------------------------
EventGroupHandle_t net_ev;
volatile int sta_fail;
volatile bool sta_retry = true;
static bool ever_got_ip;
#define PORTAL_AFTER_MS (3 * 60 * 1000)      // no address this long after power-up: offer the setup page

static void on_event(void *arg, esp_event_base_t base, int32_t id, void *data)
{
    if (base == WIFI_EVENT && id == WIFI_EVENT_STA_START) {
        if (sta_retry) esp_wifi_connect();
    } else if (base == WIFI_EVENT && id == WIFI_EVENT_STA_DISCONNECTED) {
        sta_fail++;
        if (!sta_retry) return;                          // the setup page is in charge of the connection
        if (!ever_got_ip && esp_timer_get_time() / 1000 > PORTAL_AFTER_MS) {
            ESP_LOGW(TAG, "still no Wi-Fi: opening the setup page");
            portal_start(NULL);
            return;
        }
        ESP_LOGW(TAG, "Wi-Fi disconnected, retrying");
        vTaskDelay(pdMS_TO_TICKS(1000));
        esp_wifi_connect();
    } else if (base == IP_EVENT && id == IP_EVENT_STA_GOT_IP) {
        ip_event_got_ip_t *e = data;
        snprintf(ip_str, sizeof ip_str, IPSTR, IP2STR(&e->ip_info.ip));
        ESP_LOGI(TAG, "got IP %s", ip_str);
        net_state = 1;
        ever_got_ip = true;
        xEventGroupSetBits(net_ev, GOT_IP);
    }
}

static char wbuf[2048];

static bool fetch_weather(void)
{
    char url[320];
    snprintf(url, sizeof url,
        "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s"
        "&current=temperature_2m,apparent_temperature,relative_humidity_2m,weather_code,"
        "wind_speed_10m,wind_direction_10m,is_day"
        "&temperature_unit=fahrenheit&wind_speed_unit=mph&timezone=auto",
        settings.lat, settings.lon);
    if (!http_get_text(url, wbuf, sizeof wbuf)) return false;
    cJSON *root = cJSON_Parse(wbuf);
    cJSON *cur = root ? cJSON_GetObjectItem(root, "current") : NULL;
    if (!cur) { ESP_LOGW(TAG, "weather JSON unusable"); cJSON_Delete(root); return false; }
    wx_t n = {
        .temp = cJSON_GetObjectItem(cur, "temperature_2m")->valuedouble,
        .feels = cJSON_GetObjectItem(cur, "apparent_temperature")->valuedouble,
        .hum = cJSON_GetObjectItem(cur, "relative_humidity_2m")->valueint,
        .code = cJSON_GetObjectItem(cur, "weather_code")->valueint,
        .wind = cJSON_GetObjectItem(cur, "wind_speed_10m")->valuedouble,
        .wdir = cJSON_GetObjectItem(cur, "wind_direction_10m")->valueint,
        .stamp_s = esp_timer_get_time() / 1000000,
    };
    cJSON *zone = cJSON_GetObjectItem(root, "timezone");     // e.g. "America/Los_Angeles"
    if (settings.tz_needed && cJSON_IsString(zone)) apply_timezone_name(zone->valuestring);
    cJSON_Delete(root);
    xSemaphoreTake(wx_lock, portMAX_DELAY);
    wx = n;
    wx_ver++;
    xSemaphoreGive(wx_lock);
    ESP_LOGI(TAG, "weather: %.0fF feels %.0fF hum %d%% wind %.0f mph code %d (%s)",
             n.temp, n.feels, n.hum, n.wind, n.code, wmo_text(n.code));
    return true;
}

static void net_task(void *arg)
{
    xEventGroupWaitBits(net_ev, GOT_IP, pdFALSE, pdFALSE, portMAX_DELAY);
    // An address that arrives while the setup page is open is its connection test, with the
    // placeholder location still in place. The setup page saves the real settings and restarts.
    if (portal_active) vTaskDelay(portMAX_DELAY);
    esp_sntp_config_t sc_cfg = ESP_NETIF_SNTP_DEFAULT_CONFIG("pool.ntp.org");
    ESP_ERROR_CHECK(esp_netif_sntp_init(&sc_cfg));
    while (settings.pending[0]) {
        int r = resolve_address(settings.pending);
        if (r == 1) {
            ESP_LOGI(TAG, "address found: '%s' (%s, %s)", settings.label, settings.lat, settings.lon);
            settings.pending[0] = 0;
            save_settings();
            break;
        }
        if (r == 0) {                                    // the geocoder does not know it: ask for another
            ESP_LOGW(TAG, "address not found: '%s'", settings.pending);
            portal_start("Place not found. Use a street address, a city, a ZIP code, or latitude, longitude.");
            vTaskDelay(portMAX_DELAY);
        }
        vTaskDelay(pdMS_TO_TICKS(30 * 1000));            // network trouble: try again
    }
    while (1) {
        bool ok = fetch_weather();
        vTaskDelay(pdMS_TO_TICKS((ok ? WEATHER_EVERY_S : 60) * 1000));
    }
}

void app_main(void)
{
    printf("\n=== p4clock start ===\n");
    load_settings();
    setenv("TZ", settings.tz, 1);
    tzset();
    wx_lock = xSemaphoreCreateMutex();
    lcd_init();
    i2c_master_bus_config_t bc = {
        .i2c_port = I2C_NUM_0, .sda_io_num = PIN_SDA, .scl_io_num = PIN_SCL,
        .clk_source = I2C_CLK_SRC_DEFAULT, .glitch_ignore_cnt = 7,
        .flags.enable_internal_pullup = true,
    };
    ESP_ERROR_CHECK(i2c_new_master_bus(&bc, &i2c_bus));
    xTaskCreate(ui_task, "ui", 8192, NULL, 5, NULL);
    xTaskCreate(touch_task, "touch", 4096, NULL, 4, NULL);
    ambient_start(i2c_bus);                              // the camera as a light sensor (palette: dark in a dark room)

    esp_err_t r = nvs_flash_init();
    if (r == ESP_ERR_NVS_NO_FREE_PAGES || r == ESP_ERR_NVS_NEW_VERSION_FOUND) {
        ESP_ERROR_CHECK(nvs_flash_erase());
        r = nvs_flash_init();
    }
    ESP_ERROR_CHECK(r);
    net_ev = xEventGroupCreate();
    ESP_ERROR_CHECK(esp_netif_init());
    ESP_ERROR_CHECK(esp_event_loop_create_default());
    esp_netif_create_default_wifi_sta();
    esp_netif_create_default_wifi_ap();          // the setup network (portal.c)
    wifi_init_config_t cfg = WIFI_INIT_CONFIG_DEFAULT();
    ESP_ERROR_CHECK(esp_wifi_init(&cfg));
    ESP_ERROR_CHECK(esp_event_handler_register(WIFI_EVENT, ESP_EVENT_ANY_ID, on_event, NULL));
    ESP_ERROR_CHECK(esp_event_handler_register(IP_EVENT, IP_EVENT_STA_GOT_IP, on_event, NULL));
    sta_retry = settings.configured;                     // no Wi-Fi settings yet: the setup page runs instead
    wifi_config_t wc = { 0 };
    strlcpy((char *)wc.sta.ssid, settings.ssid, sizeof(wc.sta.ssid));
    strlcpy((char *)wc.sta.password, settings.pass, sizeof(wc.sta.password));
    wc.sta.threshold.authmode = WIFI_AUTH_WPA2_PSK;
    ESP_ERROR_CHECK(esp_wifi_set_mode(settings.configured ? WIFI_MODE_STA : WIFI_MODE_APSTA));
    ESP_ERROR_CHECK(esp_wifi_set_config(WIFI_IF_STA, &wc));
    ESP_ERROR_CHECK(esp_wifi_start());
    if (!settings.configured) portal_start(NULL);
    xTaskCreate(net_task, "net", 10240, NULL, 4, NULL);
}
