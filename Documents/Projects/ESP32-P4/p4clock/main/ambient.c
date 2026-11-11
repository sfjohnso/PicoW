#include <math.h>
#include <stdio.h>
#include "ambient.h"
#include "esp_cam_sensor.h"
#include "esp_ldo_regulator.h"
#include "esp_log.h"
#include "esp_sccb_i2c.h"
#include "esp_sccb_intf.h"
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "ov5647.h"

static const char *TAG = "ambient";

volatile int ambient_state = AMBIENT_UNKNOWN;

// The sensor's gain in stops above its minimum (1x = 0, 64x = 6). Hysteresis: the dark palette starts at
// NIGHT_STOPS and ends at DAY_STOPS. Measured: a lit room reads 0, a covered lens 6. A room that still
// looks bright to a person read about 3.6 (10x gain), so the dark palette waits for a much darker room.
#define NIGHT_STOPS 5.0f                         // 32x gain
#define DAY_STOPS 4.0f                           // 16x gain
#define SAMPLE_MS 5000                           // one reading every 5 seconds
#define AVERAGE_OF 6                             // averaged over the last 30 seconds

static i2c_master_bus_handle_t g_bus;
static esp_sccb_io_handle_t io;

static int rd(uint16_t reg)
{
    uint8_t v = 0;
    return esp_sccb_transmit_receive_reg_a16v8(io, reg, &v) == ESP_OK ? v : -1;
}

static void wr(uint16_t reg, uint8_t val)
{
    if (esp_sccb_transmit_reg_a16v8(io, reg, val) != ESP_OK) ESP_LOGW(TAG, "write 0x%04X failed", reg);
}

// Settings applied on top of the sensor's default mode (found by experiment, see the notes in the repo):
// a long fixed exposure with only the gain automatic turns the sensor's gain into a light meter that
// spans six stops, from a lit room to a dark one. The picture itself is never used.
static void tune_sensor(void)
{
    static const struct { uint16_t reg; uint8_t val; } t[] = {
        { 0x380E, 0x41 }, { 0x380F, 0xA0 },                      // frame length 16800 lines (about 3 frames a second)
        { 0x3A00, 0x58 },                                        // banding filter off
        { 0x3A02, 0xFF }, { 0x3A03, 0xF0 }, { 0x3A14, 0xFF }, { 0x3A15, 0xF0 },   // exposure ceiling: 16-bit maximum
        { 0x3A18, 0x03 }, { 0x3A19, 0xFF },                      // gain ceiling 63.9x
        { 0x3503, 0x01 },                                        // exposure manual, gain automatic
        { 0x3500, 0x03 }, { 0x3501, 0xCF }, { 0x3502, 0x00 },    // exposure 249600 (1/16 line), about 310 ms: 4x the old 80 ms, so a dim scene needs 2 stops less gain
        { 0x3A0F, 25 }, { 0x3A10, 22 }, { 0x3A1B, 25 }, { 0x3A1E, 22 }, { 0x3A11, 50 }, { 0x3A1F, 11 },   // aim for a dark picture
    };
    for (unsigned i = 0; i < sizeof t / sizeof t[0]; i++) wr(t[i].reg, t[i].val);
}

static void ambient_task(void *arg)
{
    esp_ldo_channel_handle_t ldo = NULL;                        // supply for the camera's MIPI pins, as the video code does
    esp_ldo_channel_config_t lc = { .chan_id = 3, .voltage_mv = 2500 };
    if (esp_ldo_acquire_channel(&lc, &ldo) != ESP_OK) ESP_LOGW(TAG, "MIPI supply not enabled");

    sccb_i2c_config_t sc = { .dev_addr_length = I2C_ADDR_BIT_LEN_7, .device_address = OV5647_SCCB_ADDR, .scl_speed_hz = 400000 };
    if (sccb_new_i2c_io(g_bus, &sc, &io) != ESP_OK) {
        ESP_LOGW(TAG, "camera bus not available");
        ambient_state = AMBIENT_UNAVAILABLE;
        vTaskDelete(NULL);
    }
    esp_cam_sensor_config_t cfg = { .sccb_handle = io, .reset_pin = -1, .pwdn_pin = -1, .xclk_pin = -1,
                                    .xclk_freq_hz = 0, .sensor_port = ESP_CAM_SENSOR_MIPI_CSI };
    esp_cam_sensor_device_t *dev = ov5647_detect(&cfg);
    if (!dev) {
        ESP_LOGW(TAG, "no camera found: the dark palette follows the time of day");
        ambient_state = AMBIENT_UNAVAILABLE;
        vTaskDelete(NULL);
    }
    int on = 1;
    if (esp_cam_sensor_set_format(dev, NULL) != ESP_OK || esp_cam_sensor_ioctl(dev, ESP_CAM_SENSOR_IOC_S_STREAM, &on) != ESP_OK) {
        ESP_LOGW(TAG, "camera would not start: the dark palette follows the time of day");
        ambient_state = AMBIENT_UNAVAILABLE;
        vTaskDelete(NULL);
    }
    tune_sensor();
    vTaskDelay(pdMS_TO_TICKS(10000));                           // let the automatic gain settle (slower frames now)

    float ring[AVERAGE_OF];
    int n = 0, next = 0, ticks = 0;
    while (1) {
        int hi = rd(0x350A), lo = rd(0x350B);
        if (hi >= 0 && lo >= 0) {
            int gain = ((hi & 0x03) << 8) | lo;                 // 16 = 1.0x
            float stops = log2f((gain < 16 ? 16 : gain) / 16.0f);
            ring[next] = stops;
            next = (next + 1) % AVERAGE_OF;
            if (n < AVERAGE_OF) n++;
            float avg = 0;
            for (int i = 0; i < n; i++) avg += ring[i];
            avg /= n;
            int before = ambient_state;
            if (before == AMBIENT_UNKNOWN) ambient_state = avg >= (NIGHT_STOPS + DAY_STOPS) / 2 ? AMBIENT_NIGHT : AMBIENT_DAY;
            else if (before != AMBIENT_NIGHT && avg >= NIGHT_STOPS) ambient_state = AMBIENT_NIGHT;
            else if (before != AMBIENT_DAY && avg <= DAY_STOPS) ambient_state = AMBIENT_DAY;
            if (ambient_state != before || ticks++ % 12 == 0)
                ESP_LOGI(TAG, "gain %d (%.1fx), average %.1f stops: %s", gain, gain / 16.0, avg,
                         ambient_state == AMBIENT_NIGHT ? "dark" : "bright");
        }
        vTaskDelay(pdMS_TO_TICKS(SAMPLE_MS));
    }
}

void ambient_start(i2c_master_bus_handle_t bus)
{
    g_bus = bus;
    xTaskCreate(ambient_task, "ambient", 4096, NULL, 2, NULL);
}
