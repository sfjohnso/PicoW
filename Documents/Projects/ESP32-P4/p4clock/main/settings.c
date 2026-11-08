#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "esp_crt_bundle.h"
#include "esp_http_client.h"
#include "esp_log.h"
#include "esp_partition.h"
#include "cJSON.h"
#include "settings.h"
#ifdef P4CLOCK_INSTALLER_BUILD
#include "installer_defaults.h"   // placeholders; the installer or the setup page writes the real settings
#else
#include "user_config.h"          // Wi-Fi, location, time zone: copy from user_config.h.example
#endif
#include "tz_table.h"

static const char *TAG = "settings";

settings_t settings;
volatile int settings_version;

static void json_string(char *dst, size_t n, cJSON *json, const char *key)
{
    cJSON *v = cJSON_GetObjectItem(json, key);
    if (cJSON_IsString(v) && v->valuestring[0]) strlcpy(dst, v->valuestring, n);
}

static const esp_partition_t *config_partition(void)
{
    return esp_partition_find_first(ESP_PARTITION_TYPE_DATA, ESP_PARTITION_SUBTYPE_ANY, "config");
}

void load_settings(void)
{
    strlcpy(settings.ssid, WIFI_SSID, sizeof settings.ssid);
    strlcpy(settings.pass, WIFI_PASS, sizeof settings.pass);
    strlcpy(settings.label, LOCATION_LABEL, sizeof settings.label);
    strlcpy(settings.lat, LAT, sizeof settings.lat);
    strlcpy(settings.lon, LON, sizeof settings.lon);
    strlcpy(settings.tz, TZ_POSIX, sizeof settings.tz);
    settings.pending[0] = 0;
    settings.configured = strcmp(WIFI_SSID, "not-configured") != 0;
    settings.tz_needed = false;

    const esp_partition_t *p = config_partition();
    if (!p) return;
    static char buf[1024];
    if (esp_partition_read(p, 0, buf, sizeof buf - 1) != ESP_OK) return;
    buf[sizeof buf - 1] = 0;
    if (buf[0] != '{') return;                           // erased flash reads 0xFF: keep the defaults
    cJSON *json = cJSON_Parse(buf);
    if (!json) { ESP_LOGW(TAG, "config partition is not valid JSON; using the built-in settings"); return; }
    json_string(settings.ssid, sizeof settings.ssid, json, "ssid");
    json_string(settings.pass, sizeof settings.pass, json, "pass");
    json_string(settings.label, sizeof settings.label, json, "label");
    json_string(settings.lat, sizeof settings.lat, json, "lat");
    json_string(settings.lon, sizeof settings.lon, json, "lon");
    json_string(settings.tz, sizeof settings.tz, json, "tz");
    json_string(settings.pending, sizeof settings.pending, json, "pending");
    cJSON *ssid = cJSON_GetObjectItem(json, "ssid");
    if (cJSON_IsString(ssid) && ssid->valuestring[0]) settings.configured = true;
    cJSON *tz = cJSON_GetObjectItem(json, "tz");
    settings.tz_needed = settings.configured && !(cJSON_IsString(tz) && tz->valuestring[0]);
    cJSON_Delete(json);
    ESP_LOGI(TAG, "settings from the config partition: network '%s', place '%s'%s%s", settings.ssid, settings.label,
             settings.pending[0] ? ", address still to look up" : "", settings.tz_needed ? ", time zone to look up" : "");
}

bool save_settings(void)
{
    const esp_partition_t *p = config_partition();
    if (!p) return false;
    cJSON *json = cJSON_CreateObject();
    cJSON_AddStringToObject(json, "ssid", settings.ssid);
    cJSON_AddStringToObject(json, "pass", settings.pass);
    cJSON_AddStringToObject(json, "label", settings.label);
    cJSON_AddStringToObject(json, "lat", settings.lat);
    cJSON_AddStringToObject(json, "lon", settings.lon);
    cJSON_AddStringToObject(json, "tz", settings.tz_needed ? "" : settings.tz);
    if (settings.pending[0]) cJSON_AddStringToObject(json, "pending", settings.pending);
    char *text = cJSON_PrintUnformatted(json);
    cJSON_Delete(json);
    if (!text) return false;
    size_t n = strlen(text) + 1;
    bool ok = n <= 1024 && esp_partition_erase_range(p, 0, p->erase_size) == ESP_OK &&
              esp_partition_write(p, 0, text, n) == ESP_OK;
    free(text);
    if (!ok) ESP_LOGE(TAG, "could not write the config partition");
    return ok;
}

// ---------------------------------------------------------------------------
// "latitude, longitude" in decimal degrees: latitude first, south and west negative or
// marked with N/S/E/W, an optional degree sign, separated by a comma, semicolon, slash or space.
// ---------------------------------------------------------------------------
static const char *skip_ws(const char *s) { while (*s == ' ' || *s == '\t') s++; return s; }

static const char *skip_degree(const char *s)
{
    s = skip_ws(s);
    if ((unsigned char)s[0] == 0xC2 && (unsigned char)s[1] == 0xB0) s += 2;     // UTF-8 degree sign
    return skip_ws(s);
}

static bool read_number(const char **ps, double *out)
{
    char *end;
    double v = strtod(*ps, &end);
    if (end == *ps) return false;
    *out = v;
    *ps = end;
    return true;
}

bool parse_coords(const char *text, double *lat, double *lon)
{
    const char *s = skip_ws(text);
    double a, b;
    if (!read_number(&s, &a)) return false;
    s = skip_degree(s);
    char la = 0, lb = 0;
    if (*s && strchr("NSns", *s)) la = *s++;
    const char *before = s;
    s = skip_ws(s);
    bool separated = s != before;
    if (*s == ',' || *s == ';' || *s == '/') { s++; separated = true; }
    if (!separated) return false;
    s = skip_ws(s);
    if (!read_number(&s, &b)) return false;
    s = skip_degree(s);
    if (*s && strchr("EWew", *s)) lb = *s++;
    if (*skip_ws(s)) return false;
    if (la) a = (la == 'S' || la == 's') ? -fabs(a) : fabs(a);
    if (lb) b = (lb == 'W' || lb == 'w') ? -fabs(b) : fabs(b);
    if (!(a >= -90 && a <= 90 && b >= -180 && b <= 180)) return false;       // also rejects NaN
    *lat = a;
    *lon = b;
    return true;
}

void set_location_from_coords(double lat, double lon)
{
    snprintf(settings.lat, sizeof settings.lat, "%.4f", lat);
    snprintf(settings.lon, sizeof settings.lon, "%.4f", lon);
    snprintf(settings.label, sizeof settings.label, "%.2f %c, %.2f %c",
             fabs(lat), lat >= 0 ? 'N' : 'S', fabs(lon), lon >= 0 ? 'E' : 'W');
    settings_version++;
}

// ---------------------------------------------------------------------------
// HTTPS GET, address lookup, time zone
// ---------------------------------------------------------------------------
static char *g_buf;
static size_t g_cap, g_len;

static esp_err_t http_event(esp_http_client_event_t *e)
{
    if (e->event_id == HTTP_EVENT_ON_DATA && g_len + e->data_len < g_cap - 1) {
        memcpy(g_buf + g_len, e->data, e->data_len);
        g_len += e->data_len;
    }
    return ESP_OK;
}

bool http_get_text(const char *url, char *buf, size_t cap)
{
    g_buf = buf; g_cap = cap; g_len = 0;
    esp_http_client_config_t cfg = {
        .url = url, .event_handler = http_event, .timeout_ms = 15000,
        .crt_bundle_attach = esp_crt_bundle_attach,
    };
    esp_http_client_handle_t c = esp_http_client_init(&cfg);
    esp_err_t err = esp_http_client_perform(c);
    int status = esp_http_client_get_status_code(c);
    esp_http_client_cleanup(c);
    buf[g_len] = 0;
    if (err != ESP_OK || status != 200) {
        ESP_LOGW(TAG, "request failed: %s, HTTP %d", esp_err_to_name(err), status);
        return false;
    }
    return true;
}

static void url_encode(const char *in, char *out, size_t n)
{
    size_t o = 0;
    for (const unsigned char *p = (const unsigned char *)in; *p && o + 4 < n; p++) {
        if (isalnum(*p) || *p == '-' || *p == '_' || *p == '.' || *p == '~') out[o++] = *p;
        else o += snprintf(out + o, n - o, "%%%02X", *p);
    }
    out[o] = 0;
}

// Upper case, printable ASCII only (the clock font's range), as the place name on the screen.
static void set_label_ascii(const char *text)
{
    size_t o = 0;
    for (const char *p = text; *p && o < sizeof settings.label - 1; p++)
        if ((unsigned char)*p >= 32 && (unsigned char)*p < 127) settings.label[o++] = toupper((unsigned char)*p);
    settings.label[o] = 0;
    settings_version++;
}

static int census_lookup(const char *address)
{
    char enc[300], url[480];
    url_encode(address, enc, sizeof enc);
    snprintf(url, sizeof url, "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress"
             "?address=%s&benchmark=Public_AR_Current&format=json", enc);
    static char buf[8192];
    if (!http_get_text(url, buf, sizeof buf)) return -1;
    cJSON *root = cJSON_Parse(buf);
    cJSON *result = root ? cJSON_GetObjectItem(root, "result") : NULL;
    cJSON *matches = result ? cJSON_GetObjectItem(result, "addressMatches") : NULL;
    if (!cJSON_IsArray(matches)) { ESP_LOGW(TAG, "geocoder reply unusable"); cJSON_Delete(root); return -1; }
    cJSON *m = cJSON_GetArrayItem(matches, 0);
    cJSON *coords = m ? cJSON_GetObjectItem(m, "coordinates") : NULL;
    cJSON *x = coords ? cJSON_GetObjectItem(coords, "x") : NULL;
    cJSON *y = coords ? cJSON_GetObjectItem(coords, "y") : NULL;
    if (!cJSON_IsNumber(x) || !cJSON_IsNumber(y)) { cJSON_Delete(root); return 0; }
    cJSON *comp = cJSON_GetObjectItem(m, "addressComponents");
    cJSON *city = comp ? cJSON_GetObjectItem(comp, "city") : NULL;
    cJSON *state = comp ? cJSON_GetObjectItem(comp, "state") : NULL;
    cJSON *zip = comp ? cJSON_GetObjectItem(comp, "zip") : NULL;
    char label[64];
    snprintf(label, sizeof label, "%s, %s %s", cJSON_IsString(city) ? city->valuestring : "",
             cJSON_IsString(state) ? state->valuestring : "", cJSON_IsString(zip) ? zip->valuestring : "");
    set_label_ascii(label);
    snprintf(settings.lat, sizeof settings.lat, "%.4f", y->valuedouble);
    snprintf(settings.lon, sizeof settings.lon, "%.4f", x->valuedouble);
    cJSON_Delete(root);
    return 1;
}

// Open-Meteo's place search: cities and towns anywhere, and postal codes. Also returns the time zone.
static int place_search(const char *name, const char *country)
{
    char enc[200], url[400];
    url_encode(name, enc, sizeof enc);
    snprintf(url, sizeof url, "https://geocoding-api.open-meteo.com/v1/search?name=%s&count=1&language=en&format=json%s%s",
             enc, country ? "&countryCode=" : "", country ? country : "");
    static char buf[2048];
    if (!http_get_text(url, buf, sizeof buf)) return -1;
    cJSON *root = cJSON_Parse(buf);
    if (!root) return -1;
    cJSON *results = cJSON_GetObjectItem(root, "results");          // absent when nothing matches
    cJSON *r0 = cJSON_IsArray(results) ? cJSON_GetArrayItem(results, 0) : NULL;
    cJSON *lat = r0 ? cJSON_GetObjectItem(r0, "latitude") : NULL;
    cJSON *lon = r0 ? cJSON_GetObjectItem(r0, "longitude") : NULL;
    if (!cJSON_IsNumber(lat) || !cJSON_IsNumber(lon)) { cJSON_Delete(root); return 0; }
    cJSON *nm = cJSON_GetObjectItem(r0, "name");
    cJSON *adm = cJSON_GetObjectItem(r0, "admin1");
    cJSON *cc = cJSON_GetObjectItem(r0, "country_code");
    cJSON *tz = cJSON_GetObjectItem(r0, "timezone");
    char label[96];
    snprintf(label, sizeof label, "%s, %s", cJSON_IsString(nm) ? nm->valuestring : "",
             cJSON_IsString(adm) && adm->valuestring[0] ? adm->valuestring : (cJSON_IsString(cc) ? cc->valuestring : ""));
    set_label_ascii(label);
    snprintf(settings.lat, sizeof settings.lat, "%.4f", lat->valuedouble);
    snprintf(settings.lon, sizeof settings.lon, "%.4f", lon->valuedouble);
    char zone[64] = "";
    if (cJSON_IsString(tz)) strlcpy(zone, tz->valuestring, sizeof zone);
    cJSON_Delete(root);
    if (settings.tz_needed && zone[0]) apply_timezone_name(zone);
    return 1;
}

// "91320" or "91320-1234" (a U.S. ZIP code, optionally with spaces around it): the five digits.
static bool zip5(const char *text, char out[6])
{
    while (*text == ' ') text++;
    if (strspn(text, "0123456789") != 5) return false;
    const char *rest = text + 5;
    if (*rest == '-') {
        if (strspn(rest + 1, "0123456789") != 4) return false;
        rest += 5;
    }
    while (*rest == ' ') rest++;
    if (*rest) return false;
    memcpy(out, text, 5);
    out[5] = 0;
    return true;
}

// A street address (U.S. Census), else a place name or postal code (Open-Meteo), whole or in pieces.
int resolve_address(const char *text)
{
    int r = census_lookup(text);
    if (r != 0) return r;                                 // found, or network trouble
    char zip[6];
    if (zip5(text, zip)) {                                // a bare ZIP: the U.S. one first (91320 is Newbury Park,
        r = place_search(zip, "US");                      // California, not a town in France)
        if (r != 0) return r;
    }
    r = place_search(text, NULL);
    if (r != 0) return r;
    char copy[96];                                        // "Newbury Park, 91320": try each piece alone
    strlcpy(copy, text, sizeof copy);
    char *save = NULL;
    for (char *t = strtok_r(copy, ",", &save); t; t = strtok_r(NULL, ",", &save)) {
        while (*t == ' ') t++;
        size_t n = strlen(t);
        while (n && t[n - 1] == ' ') t[--n] = 0;
        if (!n) continue;
        char piece_zip[6];
        bool is_zip = zip5(t, piece_zip);                          // a ZIP piece is a U.S. one
        r = place_search(is_zip ? piece_zip : t, is_zip ? "US" : NULL);
        if (r != 0) return r;
    }
    return 0;
}

static int tz_cmp(const void *key, const void *item)
{
    return strcmp((const char *)key, ((const typeof(TZ_TABLE[0]) *)item)->name);
}

bool apply_timezone_name(const char *iana)
{
    const typeof(TZ_TABLE[0]) *e = bsearch(iana, TZ_TABLE, sizeof TZ_TABLE / sizeof TZ_TABLE[0], sizeof TZ_TABLE[0], tz_cmp);
    if (!e) { ESP_LOGW(TAG, "time zone '%s' is not in the table", iana); return false; }
    strlcpy(settings.tz, e->rule, sizeof settings.tz);
    setenv("TZ", settings.tz, 1);
    tzset();
    settings.tz_needed = false;
    ESP_LOGI(TAG, "time zone %s -> %s", iana, settings.tz);
    save_settings();
    return true;
}
