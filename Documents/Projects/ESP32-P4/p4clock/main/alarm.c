#include <math.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include "alarm.h"
#include "driver/gpio.h"
#include "driver/i2s_std.h"
#include "esp_log.h"
#include "esp_timer.h"
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "portal.h"
#include "settings.h"

static const char *TAG = "alarm";

#define PIN_PA 53                       // speaker amplifier enable
#define PIN_BOOT 35                     // BOOT button, low when pressed
#define SNOOZE_S (ALARM_SNOOZE_MIN * 60)
#define RING_MAX_S (5 * 60)             // an alarm nobody answers stops after this long
#define SAMPLE_RATE 44100
#define CODEC_VOLUME 0xBF               // ES8311 register 0x32: 0xBF = 0 dB, 0.5 dB per step
#define NOTE_COUNT 4                    // the chime: four rising bell notes, then a pause, repeated ...
#define CYCLE_FRAMES (SAMPLE_RATE * 36 / 10)
#define NOTE_GAP_FRAMES (SAMPLE_RATE * 3 / 10)
#define NOTE_LIFE_FRAMES (SAMPLE_RATE * 2)
#define NOTE_AMP 0.3f
#define START_GAIN 0.04f                // ... starting very quietly, each repeat this much louder
#define GAIN_STEP 1.25f                 // (full volume after about 45 s)

#define TAP_MAX_MS 500                  // a press shorter than this is a tap
#define DOUBLE_TAP_MS 600               // two taps this close together make a double tap
#define SETTINGS_HOLD_MS 1500           // held this long: alarm setting screen (opens while still held)
#define SETUP_HOLD_MS 8000              // held this long: phone setup page

volatile int alarm_state = ALARM_IDLE;
volatile int alarm_version;
volatile int alarm_ui_request;
volatile time_t alarm_snooze_until;

static i2c_master_dev_handle_t es_dev;
static i2s_chan_handle_t tx;
static volatile bool snooze_req, stop_req;
static volatile int last_key = -1;              // day-of-year * 1440 + minute of the last alarm that rang
static time_t ring_started;
static uint32_t frame_pos;
typedef struct { bool on; float p1, p2, w1, w2, e1, e2; uint32_t age; } note_t;
static note_t notes[6];
static const float NOTE_HZ[NOTE_COUNT] = { 1046.5f, 1318.5f, 1568.0f, 2093.0f };      // C6 E6 G6 C7
static int16_t buf[1024 * 2];

static void es_w(uint8_t reg, uint8_t val)
{
    uint8_t b[2] = { reg, val };
    esp_err_t r = i2c_master_transmit(es_dev, b, 2, 100);
    if (r != ESP_OK) ESP_LOGW(TAG, "codec write %02x failed: %s", reg, esp_err_to_name(r));
}

static void sound(bool on)
{
    if (on) {
        es_w(0x31, 0x00);               // unmute the DAC, then the amplifier
        gpio_set_level(PIN_PA, 1);
    } else {
        gpio_set_level(PIN_PA, 0);
        es_w(0x31, 0x60);               // mute: no hiss while idle
    }
}

static void audio_init(i2c_master_bus_handle_t bus)
{
    i2c_device_config_t dc = { .dev_addr_length = I2C_ADDR_BIT_LEN_7, .device_address = 0x18, .scl_speed_hz = 100000 };
    ESP_ERROR_CHECK(i2c_master_bus_add_device(bus, &dc, &es_dev));
    gpio_config_t g = { .pin_bit_mask = 1ULL << PIN_PA, .mode = GPIO_MODE_OUTPUT };
    ESP_ERROR_CHECK(gpio_config(&g));
    gpio_set_level(PIN_PA, 0);

    // I2S first so that MCLK runs before the codec is configured (same sequence as the Music app)
    i2s_chan_config_t cc = I2S_CHANNEL_DEFAULT_CONFIG(I2S_NUM_0, I2S_ROLE_MASTER);
    cc.auto_clear = true;
    ESP_ERROR_CHECK(i2s_new_channel(&cc, &tx, NULL));
    i2s_std_clk_config_t clk = I2S_STD_CLK_DEFAULT_CONFIG(SAMPLE_RATE);
    clk.mclk_multiple = I2S_MCLK_MULTIPLE_256;
    i2s_std_config_t sc = {
        .clk_cfg = clk,
        .slot_cfg = I2S_STD_PHILIPS_SLOT_DEFAULT_CONFIG(I2S_DATA_BIT_WIDTH_16BIT, I2S_SLOT_MODE_STEREO),
        .gpio_cfg = { .mclk = 13, .bclk = 12, .ws = 10, .dout = 9, .din = I2S_GPIO_UNUSED },
    };
    ESP_ERROR_CHECK(i2s_channel_init_std_mode(tx, &sc));
    ESP_ERROR_CHECK(i2s_channel_enable(tx));
    vTaskDelay(pdMS_TO_TICKS(50));

    es_w(0x44, 0x08); es_w(0x44, 0x08);
    es_w(0x01, 0x30); es_w(0x02, 0x00); es_w(0x03, 0x10); es_w(0x16, 0x24);
    es_w(0x04, 0x10); es_w(0x05, 0x00); es_w(0x0B, 0x00); es_w(0x0C, 0x00);
    es_w(0x10, 0x1F); es_w(0x11, 0x7F);
    es_w(0x00, 0x80);
    es_w(0x01, 0x3F);
    es_w(0x02, 0x00); es_w(0x05, 0x00);
    es_w(0x03, 0x10); es_w(0x04, 0x10);
    es_w(0x07, 0x00); es_w(0x08, 0xFF);
    es_w(0x06, 0x03);
    es_w(0x09, 0x0C); es_w(0x0A, 0x0C);
    es_w(0x0D, 0x01); es_w(0x0E, 0x02); es_w(0x12, 0x00); es_w(0x13, 0x10);
    es_w(0x1C, 0x6A); es_w(0x37, 0x08);
    es_w(0x32, CODEC_VOLUME);
    sound(false);
}

// A soft four-note bell chime. Each note is two decaying sines (the second a bell-like overtone that dies
// quickly); the volume grows with every repeat so that the alarm wakes you gently.
static void make_audio(int16_t *out, int frames)
{
    const float two_pi = 6.2831853f;
    const float d1 = expf(-1.0f / (0.35f * SAMPLE_RATE)), d2 = expf(-1.0f / (0.15f * SAMPLE_RATE));
    for (int i = 0; i < frames; i++, frame_pos++) {
        uint32_t c = frame_pos % CYCLE_FRAMES;
        if (c % NOTE_GAP_FRAMES == 0 && c / NOTE_GAP_FRAMES < NOTE_COUNT) {
            for (int n = 0; n < 6; n++)
                if (!notes[n].on) {
                    float w = two_pi * NOTE_HZ[c / NOTE_GAP_FRAMES] / SAMPLE_RATE;
                    notes[n] = (note_t){ true, 0, 0, w, w * 2.76f, 1, 1, 0 };
                    break;
                }
        }
        float gain = START_GAIN * powf(GAIN_STEP, (float)(frame_pos / CYCLE_FRAMES));
        if (gain > 1) gain = 1;
        float sum = 0;
        for (int n = 0; n < 6; n++) {
            note_t *k = &notes[n];
            if (!k->on) continue;
            float attack = k->age < 100 ? k->age / 100.0f : 1;       // ~2 ms: no click
            sum += attack * k->e1 * (sinf(k->p1) + 0.4f * k->e2 * sinf(k->p2));
            k->p1 += k->w1; if (k->p1 > two_pi) k->p1 -= two_pi;
            k->p2 += k->w2; if (k->p2 > two_pi) k->p2 -= two_pi;
            k->e1 *= d1; k->e2 *= d2;
            if (++k->age >= NOTE_LIFE_FRAMES) k->on = false;
        }
        sum *= gain * NOTE_AMP;
        if (sum > 0.95f) sum = 0.95f; else if (sum < -0.95f) sum = -0.95f;
        int16_t v = (int16_t)(sum * 32767);
        out[2 * i] = v;
        out[2 * i + 1] = v;
    }
}

static void start_ring(time_t now)
{
    ring_started = now;
    frame_pos = 0;
    memset(notes, 0, sizeof notes);
    alarm_state = ALARM_RINGING;
    alarm_version++;
    sound(true);
    ESP_LOGI(TAG, "ringing");
}

static void end_ring(int next_state, const char *why)
{
    sound(false);
    alarm_state = next_state;
    alarm_version++;
    ESP_LOGI(TAG, "%s", why);
}

static void alarm_task(void *arg)
{
    while (1) {
        time_t now = time(NULL);
        bool synced = now > 1700000000;
        if (stop_req) {
            stop_req = snooze_req = false;
            if (alarm_state != ALARM_IDLE) end_ring(ALARM_IDLE, "stopped");
        }
        if (snooze_req) {
            snooze_req = false;
            if (alarm_state == ALARM_RINGING) {
                alarm_snooze_until = now + SNOOZE_S;
                end_ring(ALARM_SNOOZED, "snoozed");
            }
        }
        switch (alarm_state) {
        case ALARM_IDLE:
            if (settings.alarm_on && synced) {
                struct tm tm;
                localtime_r(&now, &tm);
                int key = tm.tm_yday * 1440 + tm.tm_hour * 60 + tm.tm_min;
                if (tm.tm_hour * 60 + tm.tm_min == settings.alarm_min && key != last_key) {
                    last_key = key;                  // one ring per minute, even after "stop"
                    start_ring(now);
                }
            }
            break;
        case ALARM_SNOOZED:
            if (!settings.alarm_on) end_ring(ALARM_IDLE, "alarm switched off while snoozed");
            else if (now >= alarm_snooze_until) start_ring(now);
            break;
        case ALARM_RINGING:
            if (now - ring_started >= RING_MAX_S) end_ring(ALARM_IDLE, "nobody answered: stopped");
            break;
        }
        if (alarm_state == ALARM_RINGING) {
            make_audio(buf, 1024);
            size_t w;
            i2s_channel_write(tx, buf, sizeof buf, &w, 200);     // blocks about 23 ms: sets the pace
        } else {
            vTaskDelay(pdMS_TO_TICKS(100));
        }
    }
}

// BOOT button: double tap stops an alarm, a long press opens the alarm setting screen, a very long one the
// phone setup page. Short single taps do nothing.
static void button_task(void *arg)
{
    gpio_config_t c = { .pin_bit_mask = 1ULL << PIN_BOOT, .mode = GPIO_MODE_INPUT, .pull_up_en = GPIO_PULLUP_ENABLE };
    gpio_config(&c);
    bool down = false, setup_done = false, settings_done = false;
    int64_t down_ms = 0, last_tap_ms = -100000;
    while (1) {
        bool pressed = gpio_get_level(PIN_BOOT) == 0;
        int64_t now = esp_timer_get_time() / 1000;
        if (pressed && !down) {
            down = true;
            setup_done = settings_done = false;
            down_ms = now;
        } else if (pressed && down) {
            if (!settings_done && now - down_ms >= SETTINGS_HOLD_MS) {
                settings_done = true;                // act now so there is feedback before the finger lifts
                if (alarm_state != ALARM_RINGING) alarm_ui_request = 1;
            }
            if (!setup_done && now - down_ms >= SETUP_HOLD_MS) {
                setup_done = true;
                if (alarm_state != ALARM_RINGING) {
                    ESP_LOGI(TAG, "BOOT held: opening the setup page");
                    portal_start(NULL);
                }
            }
        } else if (!pressed && down) {
            down = false;
            int64_t dur = now - down_ms;
            if (settings_done || dur < 30) {
                // already acted on, or contact bounce
            } else if (dur < TAP_MAX_MS) {
                if (now - last_tap_ms <= DOUBLE_TAP_MS) {
                    last_tap_ms = -100000;
                    ESP_LOGI(TAG, "BOOT double tap");
                    alarm_stop();
                } else {
                    last_tap_ms = now;
                }
            }
        }
        vTaskDelay(pdMS_TO_TICKS(20));
    }
}

void alarm_snooze(void) { snooze_req = true; }
void alarm_stop(void) { stop_req = true; }

void alarm_changed(void)
{
    last_key = -1;                                   // a new time may be the current minute: let it ring
    if (alarm_state == ALARM_SNOOZED) stop_req = true;
    alarm_version++;
}

void alarm_start(i2c_master_bus_handle_t bus)
{
    audio_init(bus);
    xTaskCreate(alarm_task, "alarm", 4096, NULL, 6, NULL);
    xTaskCreate(button_task, "button", 3072, NULL, 4, NULL);
}
