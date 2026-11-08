// Placeholder settings for the installer build (see installer/package.py). The installer writes
// the real Wi-Fi, location and time zone to the "config" partition, which overrides these.
#pragma once

#define WIFI_SSID "not-configured"
#define WIFI_PASS "not-configured"
#define LOCATION_LABEL "NOT CONFIGURED"
#define LAT "0"
#define LON "0"
#define TZ_POSIX "UTC0"
