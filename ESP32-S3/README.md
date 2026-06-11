# ESP32-S3 Clock + Weather

MicroPython (ESP32_GENERIC_S3, v1.28) board that syncs time via NTP and pulls
current conditions for Newbury Park, CA (91320) from Open-Meteo over HTTPS.

Two programs are included:

| File | Role |
|------|------|
| `esp32s3_weather_https.py` | **HTTPS web server** — serves a styled clock + weather page at `https://<board-ip>/`. This is the active `main.py`. |
| `esp32s3_clock_weather.py` | Earlier variant that prints a live clock + weather dashboard to the USB serial. |

## Board / port quirk
- **Bootloader (flashing):** the board enumerates as **COM8, VID 303A PID 1001**
  (USB-Serial/JTAG) — this is what `esptool` talks to.
- **MicroPython running (REPL/deploy):** it switches to **COM11, PID 4001**
  (TinyUSB CDC) — this is what `mpremote` talks to. The COM8 port disappears
  while the app runs.
- Software DTR/RTS resets trap the chip in ROM **download** mode. To cold-boot
  into the app, press the physical **RESET** button (never hold BOOT).

## Files needed on the board (not in git)
- `secrets.py` — WiFi credentials (`SSID`, `PASSWORD`). See `secrets_example.py`.
- `cert.pem`, `key.pem` — self-signed TLS cert + key for the HTTPS server.

### Regenerate the TLS cert (EC P-256, 10-year, IP in SAN)
```sh
MSYS_NO_PATHCONV=1 openssl req -x509 -newkey ec \
  -pkeyopt ec_paramgen_curve:prime256v1 \
  -keyout key.pem -out cert.pem -days 3650 -nodes \
  -subj "/CN=esp32s3-weather" \
  -addext "subjectAltName=IP:192.168.0.157"
```
The cert is self-signed, so browsers show a one-time "not secure" warning
(expected for a LAN device); `curl -k` skips the check.

## Deploy
```sh
python -m mpremote connect COM11 cp secrets.py :secrets.py
python -m mpremote connect COM11 cp certs/cert.pem :cert.pem
python -m mpremote connect COM11 cp certs/key.pem :key.pem
python -m mpremote connect COM11 cp esp32s3_weather_https.py :main.py
python -m mpremote connect COM11 reset
```
Then browse to `https://192.168.0.157/`.

## Static IP
The board uses DHCP (currently `192.168.0.157`). For a stable URL, either add a
DHCP reservation for MAC `f0:9e:9e:74:c9:b4` on the router, or set `STATIC_IP`
in `esp32s3_weather_https.py`.
