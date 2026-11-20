// Alarm clock: one daily alarm (time stored in the config partition), a warbling tone on the speaker, snooze
// by tapping the screen, stop by double-pressing the BOOT button. The BOOT button (GPIO35, active low) also opens
// the alarm setting screen (hold 1.5 s) and the phone setup page (hold 8 s).
#pragma once
#include <stdbool.h>
#include <time.h>
#include "driver/i2c_master.h"

#define ALARM_SNOOZE_MIN 10               // a tap on the screen silences the alarm for this long

enum { ALARM_IDLE, ALARM_RINGING, ALARM_SNOOZED };
extern volatile int alarm_state;
extern volatile int alarm_version;               // bumped whenever what the footer shows about the alarm changes
extern volatile int alarm_ui_request;            // 1 = the BOOT button asks for the alarm setting screen
extern volatile time_t alarm_snooze_until;       // valid while ALARM_SNOOZED

void alarm_start(i2c_master_bus_handle_t bus);   // codec, tone, alarm and BOOT button tasks (bus is shared with touch)
void alarm_snooze(void);                         // while ringing: silence it and ring again in a few minutes
void alarm_stop(void);                           // ringing or snoozed: end it until the next day
void alarm_changed(void);                        // call after settings.alarm_on / alarm_min were edited
