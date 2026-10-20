"""PC side of the DM42 USB serial experiment.

Waits for a serial port that was not present at start-up (the DM42 once its
program calls usb_acm_on()), then logs everything it receives with timestamps,
sends numbered test lines to it every 2 s, and notes whether the DM42's FAT
drive letter is still visible while the serial port is up.

    python serial_monitor.py LOGFILE [seconds] [drive-letter]
"""
import os
import sys
import time

import serial
import serial.tools.list_ports as lp

log_path = sys.argv[1]
duration = float(sys.argv[2]) if len(sys.argv) > 2 else 900.0
drive = (sys.argv[3] if len(sys.argv) > 3 else "E") + ":\\"
t0 = time.time()
logf = open(log_path, "w", buffering=1, encoding="utf-8")


def log(msg):
    logf.write(f"[{time.time() - t0:7.1f}s] {msg}\n")


def ports():
    return {p.device: p for p in lp.comports()}


baseline = set(ports())
log(f"start; existing ports {sorted(baseline)}; watching drive {drive}")

port = None
dev = None
last_drive = os.path.exists(drive)
last_tx = 0.0
tx_n = 0
rx_total = 0
log(f"drive present: {last_drive}")

while time.time() - t0 < duration:
    now_drive = os.path.exists(drive)
    if now_drive != last_drive:
        log(f"drive {drive} {'APPEARED' if now_drive else 'DISAPPEARED'}")
        last_drive = now_drive

    if port is None:
        current = ports()
        new = [d for d in current if d not in baseline]
        if new:
            dev = new[0]
            p = current[dev]
            log(f"NEW PORT {dev}: vid={p.vid and hex(p.vid)} pid={p.pid and hex(p.pid)} "
                f"manufacturer={p.manufacturer!r} product={p.product!r} desc={p.description!r} hwid={p.hwid}")
            try:
                port = serial.Serial(dev, 115200, timeout=0.1, write_timeout=1)
                log(f"opened {dev}")
            except Exception as e:                      # keep trying; the device may still be enumerating
                log(f"open failed: {e!r}")
                port = None
                time.sleep(0.5)
        else:
            time.sleep(0.2)
        continue

    # port is open
    if dev not in ports():
        log(f"port {dev} DISAPPEARED (rx total {rx_total} bytes)")
        try:
            port.close()
        except Exception:
            pass
        port = None
        baseline = set(ports())          # wait for the next new port
        continue
    try:
        data = port.read(256)
        if data:
            rx_total += len(data)
            log(f"RX {len(data)} bytes: {data!r}")
            if b"QUIET-START" in data:
                log("quiet test: sending all 256 byte values over ~5 s")
                time.sleep(1.5)                   # the calculator ignores its first second
                for base in range(0, 256, 16):
                    port.write(bytes(range(base, base + 16)))
                    time.sleep(0.25)
                port.write(b"hello from the PC, quiet test\r\n")
                log("quiet test: sent")
        if time.time() - last_tx >= 2.0:
            tx_n += 1
            port.write(f"PC to DM42 test {tx_n}\r\n".encode())
            if tx_n % 3 == 0:
                port.write(b"A")             # a lone byte as well
            last_tx = time.time()
    except Exception as e:
        log(f"I/O error: {e!r}")
        try:
            port.close()
        except Exception:
            pass
        port = None
        baseline = set(ports())

log(f"end; rx total {rx_total} bytes, test lines sent {tx_n}")
