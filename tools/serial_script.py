#!/usr/bin/env python3
"""
Send REPL commands to an MMBasic board, reading them from a text file
(one command per line).  Reading from a file keeps embedded double-quotes
intact, which PowerShell would otherwise strip when passing args to python.

Usage:  python serial_script.py COM10 board_cmds.txt
"""
import sys, time, serial

PORT     = sys.argv[1]
CMDFILE  = sys.argv[2]

with open(CMDFILE, "r", encoding="utf-8") as f:
    cmds = [ln.rstrip("\r\n") for ln in f if ln.strip()]

ser = serial.Serial(PORT, 115200, timeout=1)

def read_all(wait=0.6):
    time.sleep(wait)
    d = b""
    while ser.in_waiting:
        d += ser.read(ser.in_waiting)
        time.sleep(0.05)
    return d

ser.write(b"\x03"); read_all(0.4)
ser.write(b"\r");   read_all(0.3)

for c in cmds:
    ser.write((c + "\r").encode())
    sys.stdout.write(read_all().decode("latin-1"))

ser.close()
