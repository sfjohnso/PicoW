#!/usr/bin/env python3
"""
Receive a file from an MMBasic board (Pico-Eval-Board) over USB serial
using XMODEM.  The board is told to send via:  XMODEM SEND "name.bas"
and this script acts as the XMODEM receiver.

Usage:  python xmodem_recv.py COM10 "PicoWClockDST.bas" out.bas
"""
import sys, time, serial
from xmodem import XMODEM

PORT     = sys.argv[1]
REMOTE   = sys.argv[2]
OUTFILE  = sys.argv[3]

ser = serial.Serial(PORT, 115200, timeout=1)

def drain(label, wait=0.6):
    """Read and return whatever is sitting in the serial buffer."""
    time.sleep(wait)
    data = b""
    while ser.in_waiting:
        data += ser.read(ser.in_waiting)
        time.sleep(0.05)
    if data:
        sys.stderr.write("[%s] %r\n" % (label, data[:200]))
    return data

# 1) Break into any running program and get a clean '>' prompt.
ser.write(b"\x03")                 # Ctrl-C
drain("after ctrl-c")
ser.write(b"\r")
drain("after CR")

# 2) Ask the board to send the file.
cmd = ('XMODEM SEND "%s"\r' % REMOTE).encode()
ser.write(cmd)
sys.stderr.write("[sent] %r\n" % cmd)

# 3) Let the command echo print, then drain it so it isn't fed to the
#    XMODEM receiver as if it were file data.  The board then sits
#    silently waiting for the receiver to start the handshake.
drain("cmd echo", wait=1.0)

# 4) Run the XMODEM receive.
def getc(size, timeout=2):
    ser.timeout = timeout
    d = ser.read(size)
    return d or None

def putc(data, timeout=2):
    ser.write_timeout = timeout
    return ser.write(data)

stream = open(OUTFILE, "wb")
modem = XMODEM(getc, putc)
recvd = modem.recv(stream, retry=20, timeout=3)
stream.close()
ser.close()

if recvd is None:
    sys.stderr.write("XMODEM receive FAILED\n")
    sys.exit(1)

# 5) Strip XMODEM's trailing Ctrl-Z (0x1A) padding from the last block.
with open(OUTFILE, "rb") as f:
    raw = f.read()
raw = raw.rstrip(b"\x1a")
with open(OUTFILE, "wb") as f:
    f.write(raw)

sys.stderr.write("OK: received %d bytes -> %s\n" % (len(raw), OUTFILE))
