// Settings: Wi-Fi, location and time zone. Built-in defaults come from user_config.h; the
// "config" flash partition (JSON, written by the installer or by the setup page) overrides them.
#pragma once
#include <stdbool.h>
#include <stddef.h>

typedef struct {
    char ssid[33], pass[65], label[40], lat[16], lon[16], tz[64];
    char pending[96];     // a street address typed on the setup page, not looked up yet ("" if none)
    bool configured;      // Wi-Fi settings exist (stored, or compiled in)
    bool tz_needed;       // stored settings carry no time zone: take it from the weather reply
} settings_t;

extern settings_t settings;
extern volatile int settings_version;     // bumped when the place name changes (the screen redraws)

void load_settings(void);
bool save_settings(void);                                   // writes settings to the config partition
bool parse_coords(const char *text, double *lat, double *lon);   // "lat, lon" in decimal degrees
void set_location_from_coords(double lat, double lon);      // sets lat, lon and the place name
int resolve_address(const char *text);                      // street address, city or ZIP: 1 ok, 0 not found, -1 network
bool apply_timezone_name(const char *iana);                 // IANA name -> POSIX rule; tzset(); saves
bool http_get_text(const char *url, char *buf, size_t cap); // HTTPS GET into buf, true on HTTP 200
