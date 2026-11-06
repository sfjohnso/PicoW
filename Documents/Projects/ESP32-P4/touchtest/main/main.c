// Touch test for Waveshare ESP32-P4-SIFI6-Touch-LCD-3.5.
// 1. Drive TP_RST (GPIO29) high to release the touch chip from reset.
// 2. Scan the I2C bus (SDA=GPIO7, SCL=GPIO8) and print every address that answers.
// 3. Identify the touch chip (FT6336 at 0x38, or GT911 at 0x5D/0x14) and print
//    touch coordinates on the console.
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "driver/gpio.h"
#include "driver/i2c_master.h"
#include "driver/spi_master.h"
#include "esp_lcd_panel_io.h"
#include "esp_heap_caps.h"
#include "freertos/semphr.h"

#define PIN_SDA   7
#define PIN_SCL   8
#define PIN_TP_RST 29
#define PIN_TP_INT 50

static i2c_master_bus_handle_t bus;

static i2c_master_dev_handle_t add_dev(uint8_t addr)
{
    i2c_device_config_t cfg = {
        .dev_addr_length = I2C_ADDR_BIT_LEN_7,
        .device_address = addr,
        .scl_speed_hz = 400000,
    };
    i2c_master_dev_handle_t dev;
    ESP_ERROR_CHECK(i2c_master_bus_add_device(bus, &cfg, &dev));
    return dev;
}


// ---- ST7796 LCD over SPI: MOSI20 SCLK21 CS23 DC26 RST27 BL28 ----
#define LCD_W 320
#define LCD_H 480
#define PIN_MOSI 20
#define PIN_SCLK 21
#define PIN_LCD_CS 23
#define PIN_LCD_DC 26
#define PIN_LCD_RST 27
#define PIN_LCD_BL 28
#define CHUNK_ROWS 40
// RGB565, byte-swapped for the SPI byte order
#define C565(r,g,b) ((uint16_t)((((((r)>>3)<<11)|(((g)>>2)<<5)|((b)>>3))&0xFF)<<8 | ((((r)>>3)<<11)|(((g)>>2)<<5)|((b)>>3))>>8))

static esp_lcd_panel_io_handle_t lcd_io;
static SemaphoreHandle_t lcd_done;
static uint16_t *lcd_buf;

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

static void fill_rect(int x, int y, int w, int h, uint16_t color)
{
    if (x < 0) { w += x; x = 0; }
    if (y < 0) { h += y; y = 0; }
    if (x + w > LCD_W) w = LCD_W - x;
    if (y + h > LCD_H) h = LCD_H - y;
    if (w <= 0 || h <= 0) return;
    int x1 = x + w - 1;
    uint8_t ca[4] = { x >> 8, x & 0xFF, x1 >> 8, x1 & 0xFF };
    cmd(0x2A, ca, 4);
    int rows_left = h, yy = y;
    while (rows_left > 0) {
        int r = rows_left > CHUNK_ROWS ? CHUNK_ROWS : rows_left;
        int y1 = yy + r - 1;
        uint8_t ra[4] = { yy >> 8, yy & 0xFF, y1 >> 8, y1 & 0xFF };
        cmd(0x2B, ra, 4);
        for (int i = 0; i < w * r; i++) lcd_buf[i] = color;
        ESP_ERROR_CHECK(esp_lcd_panel_io_tx_color(lcd_io, 0x2C, lcd_buf, w * r * 2));
        xSemaphoreTake(lcd_done, portMAX_DELAY);
        yy += r;
        rows_left -= r;
    }
}

static void lcd_init(void)
{
    gpio_config_t o = { .pin_bit_mask = (1ULL << PIN_LCD_RST) | (1ULL << PIN_LCD_BL), .mode = GPIO_MODE_OUTPUT };
    gpio_config(&o);
    gpio_set_level(PIN_LCD_BL, 0);
    gpio_set_level(PIN_LCD_RST, 0);
    vTaskDelay(pdMS_TO_TICKS(20));
    gpio_set_level(PIN_LCD_RST, 1);
    vTaskDelay(pdMS_TO_TICKS(120));

    spi_bus_config_t bc = {
        .mosi_io_num = PIN_MOSI, .miso_io_num = -1, .sclk_io_num = PIN_SCLK,
        .quadwp_io_num = -1, .quadhd_io_num = -1,
        .max_transfer_sz = LCD_W * CHUNK_ROWS * 2,
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
    lcd_buf = heap_caps_malloc(LCD_W * CHUNK_ROWS * 2, MALLOC_CAP_DMA);

    cmd(0x01, NULL, 0);                       // software reset
    vTaskDelay(pdMS_TO_TICKS(150));
    cmd(0x11, NULL, 0);                       // sleep out
    vTaskDelay(pdMS_TO_TICKS(120));
    cmd(0xF0, (uint8_t[]){0xC3}, 1);          // enable extended commands
    cmd(0xF0, (uint8_t[]){0x96}, 1);
    cmd(0x36, (uint8_t[]){0x48}, 1);          // MADCTL: portrait, BGR
    cmd(0x3A, (uint8_t[]){0x55}, 1);          // 16 bits/pixel
    cmd(0xB4, (uint8_t[]){0x01}, 1);          // 1-dot inversion
    cmd(0xF0, (uint8_t[]){0x3C}, 1);          // disable extended commands
    cmd(0xF0, (uint8_t[]){0x69}, 1);
    cmd(0x21, NULL, 0);                       // display inversion on (IPS panel)
    cmd(0x29, NULL, 0);                       // display on
    vTaskDelay(pdMS_TO_TICKS(50));

    fill_rect(0, 0, LCD_W, LCD_H, C565(0, 0, 0));
    gpio_set_level(PIN_LCD_BL, 1);
}

static void draw_test_pattern(void)
{
    fill_rect(0, 0, 40, 40, C565(255, 0, 0));                   // top-left red
    fill_rect(LCD_W - 40, 0, 40, 40, C565(0, 255, 0));          // top-right green
    fill_rect(0, LCD_H - 40, 40, 40, C565(0, 0, 255));          // bottom-left blue
    fill_rect(LCD_W - 40, LCD_H - 40, 40, 40, C565(255, 255, 255)); // bottom-right white
}

static void on_touch(int x, int y)
{
    fill_rect(x - 3, y - 3, 7, 7, C565(255, 255, 0));
}

static void poll_ft(i2c_master_dev_handle_t dev)
{
    uint8_t reg = 0x02, b[5];
    int last_n = 0;
    while (1) {
        if (i2c_master_transmit_receive(dev, &reg, 1, b, 5, 100) == ESP_OK) {
            int n = b[0] & 0x0F;
            if (n > 0 && n < 3) {
                int x = ((b[1] & 0x0F) << 8) | b[2];
                int y = ((b[3] & 0x0F) << 8) | b[4];
                on_touch(x, y);
            } else if (n == 0 && last_n) {
                printf("release\n");
            }
            last_n = (n > 0 && n < 3) ? n : 0;
        }
        vTaskDelay(pdMS_TO_TICKS(20));
    }
}

static void poll_gt(i2c_master_dev_handle_t dev)
{
    uint8_t st_reg[2] = {0x81, 0x4E}, pt_reg[2] = {0x81, 0x50}, zero[3] = {0x81, 0x4E, 0x00};
    uint8_t st, p[8];
    int last_n = 0;
    while (1) {
        if (i2c_master_transmit_receive(dev, st_reg, 2, &st, 1, 100) == ESP_OK && (st & 0x80)) {
            int n = st & 0x0F;
            if (n > 0 && i2c_master_transmit_receive(dev, pt_reg, 2, p, 8, 100) == ESP_OK) {
                int x = p[1] | (p[2] << 8);
                int y = p[3] | (p[4] << 8);
                printf("touch n=%d x=%d y=%d (INT=%d)\n", n, x, y, gpio_get_level(PIN_TP_INT));
            } else if (n == 0 && last_n) {
                printf("release\n");
            }
            last_n = n;
            i2c_master_transmit(dev, zero, 3, 100);
        }
        vTaskDelay(pdMS_TO_TICKS(20));
    }
}

void app_main(void)
{
    printf("\n=== touchtest start ===\n");
    lcd_init();
    draw_test_pattern();
    printf("LCD initialised\n");

    // Touch reset: held low by R32 until driven. Pulse low then release high.
    gpio_config_t out = { .pin_bit_mask = 1ULL << PIN_TP_RST, .mode = GPIO_MODE_OUTPUT };
    gpio_config(&out);
    gpio_set_level(PIN_TP_RST, 0);
    vTaskDelay(pdMS_TO_TICKS(20));
    gpio_set_level(PIN_TP_RST, 1);
    vTaskDelay(pdMS_TO_TICKS(300));
    gpio_config_t in = { .pin_bit_mask = 1ULL << PIN_TP_INT, .mode = GPIO_MODE_INPUT };
    gpio_config(&in);
    printf("TP_RST driven high, TP_INT level = %d\n", gpio_get_level(PIN_TP_INT));

    i2c_master_bus_config_t bc = {
        .i2c_port = I2C_NUM_0,
        .sda_io_num = PIN_SDA,
        .scl_io_num = PIN_SCL,
        .clk_source = I2C_CLK_SRC_DEFAULT,
        .glitch_ignore_cnt = 7,
        .flags.enable_internal_pullup = true,
    };
    ESP_ERROR_CHECK(i2c_new_master_bus(&bc, &bus));

    printf("I2C scan (SDA=%d SCL=%d):\n", PIN_SDA, PIN_SCL);
    int found = 0;
    uint8_t touch = 0;
    for (uint8_t a = 0x08; a < 0x78; a++) {
        if (i2c_master_probe(bus, a, 50) == ESP_OK) {
            printf("  device at 0x%02X\n", a);
            found++;
            if (a == 0x38 || a == 0x5D || a == 0x14) touch = a;
        }
    }
    printf("%d device(s) found\n", found);

    if (touch == 0x38) {
        printf("Touch chip: FT-style at 0x38. Touch the screen.\n");
        poll_ft(add_dev(0x38));
    } else if (touch) {
        printf("Touch chip: GT911-style at 0x%02X. Touch the screen.\n", touch);
        poll_gt(add_dev(touch));
    } else {
        printf("No touch chip answered.\n");
    }
    while (1) vTaskDelay(pdMS_TO_TICKS(1000));
}
