#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "esp_event.h"
#include "esp_http_server.h"
#include "esp_log.h"
#include "esp_mac.h"
#include "esp_netif.h"
#include "esp_random.h"
#include "esp_system.h"
#include "esp_timer.h"
#include "esp_wifi.h"
#include "freertos/FreeRTOS.h"
#include "freertos/queue.h"
#include "freertos/task.h"
#include "lwip/sockets.h"
#include "portal.h"
#include "settings.h"

static const char *TAG = "portal";
static const char AP_IP[] = "192.168.4.1";
#define IDLE_RESTART_MS (10 * 60 * 1000)          // a configured clock leaves the portal after this long idle

volatile bool portal_active;
volatile int portal_version;
char portal_ssid[24], portal_pass[12], portal_status[100];

typedef struct { char ssid[33], pass[65], loc[96]; } form_t;
static QueueHandle_t save_q;
static volatile int64_t last_activity_ms;
static char scan_json[1536] = "[]";
static char start_note[sizeof portal_status];

static void set_status(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(portal_status, sizeof portal_status, fmt, ap);
    va_end(ap);
    portal_version++;
    ESP_LOGI(TAG, "%s", portal_status);
}

static void touch_activity(void) { last_activity_ms = esp_timer_get_time() / 1000; }

// ---------------------------------------------------------------------------
// The setup page
// ---------------------------------------------------------------------------
static const char PAGE[] =
    "<!doctype html><html><head><meta charset=utf-8>"
    "<meta name=viewport content='width=device-width,initial-scale=1'><title>Clock setup</title><style>"
    "body{font-family:system-ui,sans-serif;max-width:28rem;margin:0 auto;padding:1rem;background:#121826;color:#e8edf6}"
    "h1{font-size:1.4rem}label{display:block;margin-top:1rem;font-weight:600}"
    "input,select,button{width:100%;padding:.7rem;font-size:1rem;margin-top:.3rem;box-sizing:border-box;"
    "border-radius:.4rem;border:1px solid #3b4869;background:#0e1422;color:#e8edf6}"
    "button{background:#ff6b6b;color:#121826;font-weight:700;border:0;margin-top:1.5rem}"
    "small{color:#8aa0c6;display:block;margin-top:.3rem}input[type=checkbox]{width:auto;margin-right:.5rem}"
    "</style></head><body><h1>Set up your clock</h1>"
    "<form method=post action=/save>"
    "<label>Wi-Fi network<select id=net><option>Looking for networks...</option></select></label>"
    "<input id=ssid name=ssid placeholder='Network name' style='display:none'>"
    "<small>The clock can only use 2.4 GHz networks.</small>"
    "<label>Wi-Fi password<input id=pw name=pass type=password autocomplete=off required minlength=8></label>"
    "<label style='font-weight:400'><input type=checkbox onclick=\"pw.type=this.checked?'text':'password'\">Show password</label>"
    "<label>Location<input name=loc placeholder='123 Main St, City, ST 12345 or just a city' required></label>"
    "<small>A street address (U.S.), a city, a ZIP code, or a latitude and longitude like 51.5074, -0.1278.</small>"
    "<button>Save</button></form>"
    "<script>var sel=document.getElementById('net'),ssid=document.getElementById('ssid'),tries=0;"
    "function upd(){var o=sel.value=='__other__';ssid.style.display=o?'block':'none';ssid.required=o;"
    "if(!o)ssid.value=sel.value}"
    "sel.onchange=upd;"
    "function load(){fetch('/scan').then(function(r){return r.json()}).then(function(l){"
    "if(!l.length&&tries++<6){setTimeout(load,2000);return}"
    "sel.innerHTML='';l.forEach(function(n){var o=document.createElement('option');o.textContent=n;o.value=n;"
    "sel.appendChild(o)});var o=document.createElement('option');o.textContent='Other...';o.value='__other__';"
    "sel.appendChild(o);upd()})}load();</script></body></html>";

static const char DONE_PAGE[] =
    "<!doctype html><meta charset=utf-8><meta name=viewport content='width=device-width,initial-scale=1'>"
    "<body style='font-family:system-ui,sans-serif;max-width:28rem;margin:0 auto;padding:1rem;"
    "background:#121826;color:#e8edf6'><h1>Saved</h1>"
    "<p>The clock is now joining your Wi-Fi network. Watch the clock's screen: it says <b>Connected</b> and "
    "starts by itself. This phone will drop off the clock's network at that point.</p>"
    "<p>If the clock cannot join, it says so and keeps this setup network open, so you can try again.</p>";

static esp_err_t send_text(httpd_req_t *req, const char *status, const char *type, const char *body)
{
    httpd_resp_set_status(req, status);
    httpd_resp_set_type(req, type);
    return httpd_resp_send(req, body, HTTPD_RESP_USE_STRLEN);
}

// Anything a phone asks for (its "is there internet?" probes included) gets the page, or a
// redirect to it, which is what makes the phone offer to "sign in to the network".
static esp_err_t page_get(httpd_req_t *req)
{
    touch_activity();
    char host[64] = "";
    httpd_req_get_hdr_value_str(req, "Host", host, sizeof host);
    ESP_LOGI(TAG, "request: http://%s%s", host, req->uri);
    if (host[0] && strcmp(host, AP_IP) != 0) {
        httpd_resp_set_status(req, "302 Found");
        httpd_resp_set_hdr(req, "Location", "http://192.168.4.1/");
        return httpd_resp_send(req, NULL, 0);
    }
    return send_text(req, "200 OK", "text/html", PAGE);
}

static esp_err_t scan_get(httpd_req_t *req)
{
    touch_activity();
    return send_text(req, "200 OK", "application/json", scan_json);
}

static bool form_get(const char *body, const char *key, char *out, size_t n)
{
    size_t kl = strlen(key);
    for (const char *p = body; p && *p;) {
        if (!strncmp(p, key, kl) && p[kl] == '=') {
            p += kl + 1;
            size_t o = 0;
            while (*p && *p != '&' && o + 1 < n) {
                if (*p == '+') { out[o++] = ' '; p++; }
                else if (*p == '%' && isxdigit((unsigned char)p[1]) && isxdigit((unsigned char)p[2])) {
                    char h[3] = { p[1], p[2], 0 };
                    out[o++] = (char)strtol(h, NULL, 16);
                    p += 3;
                } else out[o++] = *p++;
            }
            out[o] = 0;
            return true;
        }
        p = strchr(p, '&');
        if (p) p++;
    }
    return false;
}

static esp_err_t save_post(httpd_req_t *req)
{
    touch_activity();
    char body[512];
    int len = req->content_len;
    if (len <= 0 || len >= (int)sizeof body) return send_text(req, "400 Bad Request", "text/plain", "Form too large");
    for (int got = 0; got < len;) {
        int r = httpd_req_recv(req, body + got, len - got);
        if (r <= 0) return ESP_FAIL;
        got += r;
    }
    body[len] = 0;
    form_t f = { 0 };
    form_get(body, "ssid", f.ssid, sizeof f.ssid);
    form_get(body, "pass", f.pass, sizeof f.pass);
    form_get(body, "loc", f.loc, sizeof f.loc);
    if (!f.ssid[0] || strlen(f.pass) < 8 || !f.loc[0])
        return send_text(req, "400 Bad Request", "text/html",
                         "<meta charset=utf-8><p>Please go back and fill in the network name, a password of at "
                         "least 8 characters, and the location.</p>");
    xQueueOverwrite(save_q, &f);
    return send_text(req, "200 OK", "text/html", DONE_PAGE);
}

// ---------------------------------------------------------------------------
// DNS: answer every name with the clock's own address, so any web address reaches the page
// ---------------------------------------------------------------------------
static void dns_task(void *arg)
{
    int s = socket(AF_INET, SOCK_DGRAM, IPPROTO_IP);
    struct sockaddr_in a = { .sin_family = AF_INET, .sin_port = htons(53), .sin_addr.s_addr = htonl(INADDR_ANY) };
    if (s < 0 || bind(s, (struct sockaddr *)&a, sizeof a) < 0) {
        ESP_LOGE(TAG, "DNS server could not start");
        vTaskDelete(NULL);
    }
    uint8_t buf[512];
    while (1) {
        struct sockaddr_in from;
        socklen_t fl = sizeof from;
        int n = recvfrom(s, buf, sizeof buf - 16, 0, (struct sockaddr *)&from, &fl);
        if (n < 12) continue;
        int q = 12;                                        // skip the question name, then type and class
        while (q < n && buf[q]) q += buf[q] + 1;
        if (q + 5 > n) continue;
        int end = q + 5;
        int qtype = (buf[q + 1] << 8) | buf[q + 2];
        buf[2] = 0x81; buf[3] = 0x80;                      // response, recursion available
        buf[6] = 0; buf[7] = 0; buf[8] = buf[9] = buf[10] = buf[11] = 0;
        if (qtype == 1) {                                  // an A record: the clock's address; others: no answer
            static const uint8_t ans[] = { 0xC0, 0x0C, 0, 1, 0, 1, 0, 0, 0, 60, 0, 4, 192, 168, 4, 1 };
            memcpy(buf + end, ans, sizeof ans);
            buf[7] = 1;
            end += sizeof ans;
        }
        sendto(s, buf, end, 0, (struct sockaddr *)&from, fl);
    }
}

// ---------------------------------------------------------------------------
// Network list for the page: unique names, strongest first
// ---------------------------------------------------------------------------
static void scan_networks(void)
{
    wifi_scan_config_t sc = { 0 };
    if (esp_wifi_scan_start(&sc, true) != ESP_OK) return;
    wifi_ap_record_t rec[20];
    uint16_t n = 20;
    if (esp_wifi_scan_get_ap_records(&n, rec) != ESP_OK) return;
    size_t o = 0;
    scan_json[o++] = '[';
    for (int i = 0; i < n; i++) {
        const char *ssid = (const char *)rec[i].ssid;
        if (!ssid[0]) continue;
        bool dup = false;
        for (int j = 0; j < i && !dup; j++) dup = !strcmp(ssid, (const char *)rec[j].ssid);
        if (dup || o + 2 * strlen(ssid) + 8 > sizeof scan_json - 2) continue;
        if (o > 1) scan_json[o++] = ',';
        scan_json[o++] = '"';
        for (const char *p = ssid; *p; p++) {
            if (*p == '"' || *p == '\\') scan_json[o++] = '\\';
            if ((unsigned char)*p >= 32) scan_json[o++] = *p;
        }
        scan_json[o++] = '"';
    }
    scan_json[o++] = ']';
    scan_json[o] = 0;
    ESP_LOGI(TAG, "network list: %s", scan_json);
}

// ---------------------------------------------------------------------------
// The task: bring the access point up, then handle each submitted form
// ---------------------------------------------------------------------------
static void on_ap_event(void *arg, esp_event_base_t base, int32_t id, void *data)
{
    touch_activity();
    if (id == WIFI_EVENT_AP_STACONNECTED) {
        wifi_event_ap_staconnected_t *e = data;
        ESP_LOGI(TAG, "phone joined " MACSTR, MAC2STR(e->mac));
    } else if (id == WIFI_EVENT_AP_STADISCONNECTED) {
        wifi_event_ap_stadisconnected_t *e = data;
        ESP_LOGI(TAG, "phone left " MACSTR, MAC2STR(e->mac));
    }
}

// Phones check for internet by asking a DNS server about well-known names. Offer the clock itself
// as that server (the DNS task answers everything with the clock's address) so they find the
// "sign in" page by themselves.
static void offer_clock_as_dns(void)
{
    esp_netif_t *ap = esp_netif_get_handle_from_ifkey("WIFI_AP_DEF");
    if (!ap) { ESP_LOGW(TAG, "no access point interface; phones will not find the page by themselves"); return; }
    esp_netif_dns_info_t dns = { 0 };
    dns.ip.type = IPADDR_TYPE_V4;
    dns.ip.u_addr.ip4.addr = esp_ip4addr_aton(AP_IP);
    uint8_t offer = 2;                                    // DHCPS_OFFER_DNS
    esp_netif_dhcps_stop(ap);
    esp_netif_dhcps_option(ap, ESP_NETIF_OP_SET, ESP_NETIF_DOMAIN_NAME_SERVER, &offer, sizeof offer);
    esp_netif_set_dns_info(ap, ESP_NETIF_DNS_MAIN, &dns);
    esp_netif_dhcps_start(ap);
}

static bool try_join(const form_t *f)
{
    wifi_config_t wc = { 0 };
    strlcpy((char *)wc.sta.ssid, f->ssid, sizeof wc.sta.ssid);
    strlcpy((char *)wc.sta.password, f->pass, sizeof wc.sta.password);
    wc.sta.threshold.authmode = WIFI_AUTH_WPA2_PSK;
    esp_wifi_disconnect();
    esp_wifi_set_config(WIFI_IF_STA, &wc);
    xEventGroupClearBits(net_ev, GOT_IP);
    sta_fail = 0;
    esp_wifi_connect();
    for (int i = 0; i < 100; i++) {                       // up to 25 seconds
        if (xEventGroupGetBits(net_ev) & GOT_IP) return true;
        if (sta_fail > 0) return false;
        vTaskDelay(pdMS_TO_TICKS(250));
    }
    return false;
}

static void portal_task(void *arg)
{
    save_q = xQueueCreate(1, sizeof(form_t));
    sta_retry = false;
    esp_wifi_disconnect();

    uint8_t mac[6];
    // The radio is a separate chip (ESP32-C6), so esp_read_mac() finds no Wi-Fi address on this board;
    // ask the radio itself. Its station address is unique to each clock.
    if (esp_wifi_get_mac(WIFI_IF_STA, mac) != ESP_OK) memset(mac, 0, sizeof mac);
    snprintf(portal_ssid, sizeof portal_ssid, "P4Clock-%02X%02X", mac[4], mac[5]);
    static const char alphabet[] = "abcdefghjkmnpqrstuvwxyz23456789";   // no look-alike characters
    for (int i = 0; i < 8; i++) portal_pass[i] = alphabet[esp_random() % (sizeof alphabet - 1)];
    portal_pass[8] = 0;

    wifi_config_t ap = { 0 };
    strlcpy((char *)ap.ap.ssid, portal_ssid, sizeof ap.ap.ssid);
    ap.ap.ssid_len = strlen(portal_ssid);
    strlcpy((char *)ap.ap.password, portal_pass, sizeof ap.ap.password);
    ap.ap.channel = 1;
    ap.ap.max_connection = 4;
    ap.ap.authmode = WIFI_AUTH_WPA2_PSK;
    ESP_ERROR_CHECK(esp_wifi_set_mode(WIFI_MODE_APSTA));
    ESP_ERROR_CHECK(esp_wifi_set_config(WIFI_IF_AP, &ap));
    ESP_ERROR_CHECK(esp_event_handler_register(WIFI_EVENT, WIFI_EVENT_AP_STACONNECTED, on_ap_event, NULL));
    ESP_ERROR_CHECK(esp_event_handler_register(WIFI_EVENT, WIFI_EVENT_AP_STADISCONNECTED, on_ap_event, NULL));
    offer_clock_as_dns();
    ESP_LOGI(TAG, "setup network '%s' started", portal_ssid);

    httpd_handle_t srv = NULL;
    httpd_config_t hc = HTTPD_DEFAULT_CONFIG();
    hc.uri_match_fn = httpd_uri_match_wildcard;
    hc.stack_size = 6144;
    hc.lru_purge_enable = true;                           // phones open many connections at once: drop the oldest
    ESP_ERROR_CHECK(httpd_start(&srv, &hc));
    httpd_uri_t scan = { .uri = "/scan", .method = HTTP_GET, .handler = scan_get };
    httpd_uri_t save = { .uri = "/save", .method = HTTP_POST, .handler = save_post };
    httpd_uri_t any = { .uri = "/*", .method = HTTP_GET, .handler = page_get };
    httpd_register_uri_handler(srv, &scan);
    httpd_register_uri_handler(srv, &save);
    httpd_register_uri_handler(srv, &any);
    xTaskCreate(dns_task, "dns", 4096, NULL, 3, NULL);

    touch_activity();
    set_status("%s", start_note[0] ? start_note : "Waiting for your phone...");
    portal_active = true;                                 // the screen switches to the setup page now
    scan_networks();                                      // once, before a phone joins and the scan disturbs it

    while (1) {
        form_t f;
        if (xQueueReceive(save_q, &f, pdMS_TO_TICKS(500)) == pdTRUE) {
            set_status("Joining %s...", f.ssid);
            if (try_join(&f)) {
                strlcpy(settings.ssid, f.ssid, sizeof settings.ssid);
                strlcpy(settings.pass, f.pass, sizeof settings.pass);
                double lat, lon;
                settings.pending[0] = 0;
                if (parse_coords(f.loc, &lat, &lon)) {
                    set_location_from_coords(lat, lon);
                } else {                                  // a street address: looked up after the restart
                    strlcpy(settings.pending, f.loc, sizeof settings.pending);
                    strlcpy(settings.label, "LOOKING UP LOCATION", sizeof settings.label);
                    strlcpy(settings.lat, "0", sizeof settings.lat);
                    strlcpy(settings.lon, "0", sizeof settings.lon);
                }
                settings.configured = true;
                settings.tz_needed = true;
                if (save_settings()) {
                    set_status("Connected! Starting the clock...");
                    vTaskDelay(pdMS_TO_TICKS(2500));
                    esp_restart();
                }
                set_status("Joined, but the settings could not be saved. Try again.");
            } else {
                esp_wifi_disconnect();
                set_status("Could not join %s. Check the password and use the page again.", f.ssid);
            }
            touch_activity();
        }
        // A clock that already has working settings gives up on an abandoned setup and carries on.
        if (settings.configured && esp_timer_get_time() / 1000 - last_activity_ms > IDLE_RESTART_MS) {
            ESP_LOGI(TAG, "no one used the setup page; restarting");
            esp_restart();
        }
    }
}

void portal_start(const char *note)
{
    static bool started;
    if (started) return;
    started = true;
    if (note) strlcpy(start_note, note, sizeof start_note);
    xTaskCreate(portal_task, "portal", 8192, NULL, 4, NULL);
}
