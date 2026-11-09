// Ambient light from the camera: the OV5647 runs with a fixed long exposure and automatic gain, and the
// gain it settles on says how bright the scene is (about 1x in a lit room, up to 64x in the dark).
// This replaces time of day for choosing the dark red palette; without a working camera the clock falls
// back to the time-of-day rule.
#pragma once
#include "driver/i2c_master.h"

enum { AMBIENT_UNKNOWN, AMBIENT_DAY, AMBIENT_NIGHT, AMBIENT_UNAVAILABLE };
extern volatile int ambient_state;               // UNKNOWN until the camera has been read; UNAVAILABLE if it cannot be used

void ambient_start(i2c_master_bus_handle_t bus); // the camera shares the touch chip's I2C bus
