// Setup portal: the clock opens its own Wi-Fi network, a phone joins it (the screen shows a QR code),
// and a web page collects the home Wi-Fi name and password and the location.
#pragma once
#include <stdbool.h>
#include "freertos/FreeRTOS.h"
#include "freertos/event_groups.h"

#define GOT_IP BIT0
extern EventGroupHandle_t net_ev;         // main.c: GOT_IP is set while the station has an address
extern volatile int sta_fail;             // main.c: station disconnect events (the portal clears it)
extern volatile bool sta_retry;           // main.c: reconnect automatically after a disconnect

extern volatile bool portal_active;       // true while the setup screen should be shown
extern volatile int portal_version;       // bumped whenever the on-screen text changes
extern char portal_ssid[24], portal_pass[12], portal_status[100];

void portal_start(const char *note);      // safe to call more than once; note: first message on the screen, or NULL
