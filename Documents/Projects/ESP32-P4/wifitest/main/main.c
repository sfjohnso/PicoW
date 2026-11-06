// Wi-Fi test for Waveshare ESP32-P4 board. The P4 has no radio; esp_wifi_remote
// forwards the normal esp_wifi API over SDIO to the ESP32-C6 (esp_hosted).
#include <stdio.h>
#include <string.h>
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "freertos/event_groups.h"
#include "esp_wifi.h"
#include "esp_event.h"
#include "esp_netif.h"
#include "esp_log.h"
#include "nvs_flash.h"
#include "wifi_creds.h"

static const char *TAG = "wifitest";
static EventGroupHandle_t ev;
#define GOT_IP BIT0

static void on_event(void *arg, esp_event_base_t base, int32_t id, void *data)
{
    if (base == WIFI_EVENT && id == WIFI_EVENT_STA_START) {
        esp_wifi_connect();
    } else if (base == WIFI_EVENT && id == WIFI_EVENT_STA_DISCONNECTED) {
        wifi_event_sta_disconnected_t *d = data;
        ESP_LOGW(TAG, "disconnected, reason %d; retrying", d->reason);
        vTaskDelay(pdMS_TO_TICKS(1000));
        esp_wifi_connect();
    } else if (base == WIFI_EVENT && id == WIFI_EVENT_STA_CONNECTED) {
        ESP_LOGI(TAG, "associated");
    } else if (base == IP_EVENT && id == IP_EVENT_STA_GOT_IP) {
        ip_event_got_ip_t *e = data;
        ESP_LOGI(TAG, "got IP " IPSTR "  gw " IPSTR "  mask " IPSTR,
                 IP2STR(&e->ip_info.ip), IP2STR(&e->ip_info.gw), IP2STR(&e->ip_info.netmask));
        xEventGroupSetBits(ev, GOT_IP);
    }
}

void app_main(void)
{
    printf("\n=== wifitest start ===\n");
    esp_err_t r = nvs_flash_init();
    if (r == ESP_ERR_NVS_NO_FREE_PAGES || r == ESP_ERR_NVS_NEW_VERSION_FOUND) {
        ESP_ERROR_CHECK(nvs_flash_erase());
        r = nvs_flash_init();
    }
    ESP_ERROR_CHECK(r);
    ev = xEventGroupCreate();
    ESP_ERROR_CHECK(esp_netif_init());
    ESP_ERROR_CHECK(esp_event_loop_create_default());
    esp_netif_create_default_wifi_sta();

    wifi_init_config_t cfg = WIFI_INIT_CONFIG_DEFAULT();
    ESP_ERROR_CHECK(esp_wifi_init(&cfg));
    ESP_ERROR_CHECK(esp_event_handler_register(WIFI_EVENT, ESP_EVENT_ANY_ID, on_event, NULL));
    ESP_ERROR_CHECK(esp_event_handler_register(IP_EVENT, IP_EVENT_STA_GOT_IP, on_event, NULL));

    wifi_config_t wc = { 0 };
    strlcpy((char *)wc.sta.ssid, WIFI_SSID, sizeof(wc.sta.ssid));
    strlcpy((char *)wc.sta.password, WIFI_PASS, sizeof(wc.sta.password));
    wc.sta.threshold.authmode = WIFI_AUTH_WPA2_PSK;
    ESP_ERROR_CHECK(esp_wifi_set_mode(WIFI_MODE_STA));
    ESP_ERROR_CHECK(esp_wifi_set_config(WIFI_IF_STA, &wc));
    ESP_ERROR_CHECK(esp_wifi_start());

    if (xEventGroupWaitBits(ev, GOT_IP, pdFALSE, pdFALSE, pdMS_TO_TICKS(30000)) & GOT_IP) {
        uint8_t mac[6];
        esp_wifi_get_mac(WIFI_IF_STA, mac);
        printf("Wi-Fi up. STA MAC %02x:%02x:%02x:%02x:%02x:%02x\n", mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);
    } else {
        printf("Wi-Fi: no IP within 30 s\n");
    }
    while (1) {
        wifi_ap_record_t ap;
        if (esp_wifi_sta_get_ap_info(&ap) == ESP_OK)
            printf("SSID %s  RSSI %d dBm  channel %d\n", ap.ssid, ap.rssi, ap.primary);
        vTaskDelay(pdMS_TO_TICKS(5000));
    }
}
