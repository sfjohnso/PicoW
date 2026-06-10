#!/usr/bin/env python3
"""
Send a local file TO an MMBasic board over USB serial using XMODEM.

Two modes:
  python xmodem_send.py COM10 local.bas REMOTENAME.BAS
        Board does  XMODEM RECEIVE "REMOTENAME.BAS"  -> saved to the SD card.

  python xmodem_send.py COM10 local.bas --mem
        Board does a bare  XMODEM RECEIVE  -> loaded straight into program
        memory (RAM), the SD card is NOT touched, then the script issues RUN
        and leaves the program running.  Good for visual verification before
        committing a version to a filename.

The remote filename's quotes are added here in Python (sent as literal serial
bytes), so they never pass through PowerShell's arg parser.
"""
import sys, time, serial
from xmodem import XMODEM

PORT   = sys.argv[1]
LOCAL  = sys.argv[2]
REMOTE = sys.argv[3]            # plain name, or "--mem" for the RAM mode

MEM = (REMOTE == "--mem")

ser = serial.Serial(PORT, 115200, timeout=1)

def drain(label, wait=0.5):
    time.sleep(wait)
    d = b""
    while ser.in_waiting:
        d += ser.read(ser.in_waiting)
        time.sleep(0.05)
    if d:
        sys.stderr.write("[%s] %r\n" % (label, d[:200]))
    return d

# Break into the running program, get a clean prompt.
ser.write(b"\x03"); drain("ctrl-c")
ser.write(b"\r");   drain("cr")

# Tell the board to receive.  Bare RECEIVE -> into RAM; quoted -> to a file.
if MEM:
    cmd = b"XMODEM RECEIVE\r"
else:
    cmd = ('XMODEM RECEIVE "%s"\r' % REMOTE).encode()
ser.write(cmd)
sys.stderr.write("[sent] %r\n" % cmd)
drain("cmd echo", wait=1.0)

def getc(size, timeout=2):
    ser.timeout = timeout
    return ser.read(size) or None

def putc(data, timeout=2):
    ser.write_timeout = timeout
    return ser.write(data)

stream = open(LOCAL, "rb")
modem = XMODEM(getc, putc)
ok = modem.send(stream, retry=20, timeout=5)
stream.close()
drain("after send", wait=1.2)

if MEM and ok:
    # Run the just-loaded in-memory program and leave it running (no Ctrl-C).
    ser.write(b"RUN\r")
    sys.stderr.write("[sent] RUN\n")
    buf = b""
    t0 = time.time()
    while time.time() - t0 < 12:
        time.sleep(0.2)
        while ser.in_waiting:
            buf += ser.read(ser.in_waiting); time.sleep(0.03)
    sys.stdout.write(buf.decode("latin-1"))

ser.close()
sys.stderr.write("send result: %s\n" % ok)
sys.exit(0 if ok else 1)
