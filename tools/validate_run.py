#!/usr/bin/env python3
"""
RUN a program on the board, let it execute for a few seconds (long enough
to compile, draw, and do the startup weather fetch), then Ctrl-C and print
everything the board emitted.  Compile errors appear immediately after RUN;
runtime errors appear as the program runs.

Usage:  python validate_run.py COM10 WXCLOCK.BAS 12
"""
import sys, time, serial

PORT   = sys.argv[1]
REMOTE = sys.argv[2]
SECS   = float(sys.argv[3]) if len(sys.argv) > 3 else 10.0

ser = serial.Serial(PORT, 115200, timeout=0.3)

def read_now():
    d = b""
    while ser.in_waiting:
        d += ser.read(ser.in_waiting)
        time.sleep(0.03)
    return d

# clean prompt
ser.write(b"\x03"); time.sleep(0.4); read_now()
ser.write(b"\r");   time.sleep(0.3); read_now()

ser.write(('RUN "%s"\r' % REMOTE).encode())
sys.stderr.write('[sent] RUN "%s"\n' % REMOTE)

# collect output while it runs
buf = b""
t0 = time.time()
while time.time() - t0 < SECS:
    buf += read_now()
    time.sleep(0.2)

# break back into the prompt
ser.write(b"\x03"); time.sleep(0.5)
buf += read_now()
ser.close()

sys.stdout.write(buf.decode("latin-1"))
