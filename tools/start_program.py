#!/usr/bin/env python3
"""
RUN a program on the board and LEAVE it running (no Ctrl-C at the end).
Captures the first few seconds of output to confirm a clean launch, then
closes the serial port -- which does not stop the program on the Pico.

Usage:  python start_program.py COM10 PicoWClockDST.bas 12
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

ser.write(b"\x03"); time.sleep(0.4); read_now()
ser.write(b"\r");   time.sleep(0.3); read_now()

ser.write(('RUN "%s"\r' % REMOTE).encode())
sys.stderr.write('[sent] RUN "%s"\n' % REMOTE)

buf = b""
t0 = time.time()
while time.time() - t0 < SECS:
    buf += read_now()
    time.sleep(0.2)

# Close WITHOUT sending Ctrl-C so the clock keeps running.
ser.close()
sys.stdout.write(buf.decode("latin-1"))
