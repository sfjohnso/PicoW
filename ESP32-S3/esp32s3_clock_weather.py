# ESP32-S3 Clock + Weather  (MicroPython, ESP32_GENERIC_S3)
# ---------------------------------------------------------------------------
# Joins WiFi, keeps time synced via NTP, and fetches current conditions for
# Newbury Park CA (91320) from Open-Meteo over HTTPS.  Prints a live, in-place
# clock line over the USB serial and a full weather block on each refresh.
#
# Deploy as main.py so it runs on boot.  REPL note: this board enumerates as
# a DIFFERENT COM port when MicroPython is running (TinyUSB CDC, VID 303A
# PID 4001) vs. the ROM bootloader (USB-Serial/JTAG, PID 1001) used by esptool.
# ---------------------------------------------------------------------------
import sys, time, network, ntptime, requests, machine

# --- configuration ---------------------------------------------------------
# WiFi credentials live in secrets.py on the board (NOT committed to git),
# mirroring how the PicoW programs keep creds in OPTION WIFI device config.
try:
    from secrets import SSID, PASSWORD
except ImportError:
    SSID, PASSWORD = "your-ssid", "your-password"   # placeholder; create secrets.py

LAT, LON = 34.1872, -118.9114            # Newbury Park, CA 91320
TZ       = "America/Los_Angeles"

WEATHER_EVERY = 1800                     # refresh weather every 30 min
NTP_EVERY     = 6 * 3600                 # re-sync clock every 6 hours

WDAY = ("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
MON  = ("", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# WMO weather interpretation codes -> short text
WMO = {
    0: "Clear sky", 1: "Mainly clear", 2: "Partly cloudy", 3: "Overcast",
    45: "Fog", 48: "Rime fog", 51: "Light drizzle", 53: "Drizzle",
    55: "Heavy drizzle", 56: "Freezing drizzle", 57: "Freezing drizzle",
    61: "Light rain", 63: "Rain", 65: "Heavy rain", 66: "Freezing rain",
    67: "Freezing rain", 71: "Light snow", 73: "Snow", 75: "Heavy snow",
    77: "Snow grains", 80: "Light showers", 81: "Showers", 82: "Violent showers",
    85: "Snow showers", 86: "Snow showers", 95: "Thunderstorm",
    96: "Thunderstorm + hail", 99: "Thunderstorm + hail",
}

# --- runtime state ---------------------------------------------------------
tz_offset = -7 * 3600                    # seconds; corrected from API (DST-aware)
wx = None                                # last weather dict


def connect_wifi():
    w = network.WLAN(network.STA_IF)
    w.active(True)
    if not w.isconnected():
        sys.stdout.write("\nConnecting to %s ... " % SSID)
        w.connect(SSID, PASSWORD)
        for _ in range(60):
            if w.isconnected():
                break
            time.sleep(0.5)
    if w.isconnected():
        print("OK", w.ifconfig()[0])
        return True
    print("FAILED (status=%s)" % w.status())
    return False


def sync_ntp():
    ntptime.host = "pool.ntp.org"
    for _ in range(3):
        try:
            ntptime.settime()            # RTC <- UTC
            print("NTP synced (UTC %04d-%02d-%02d %02d:%02d:%02d)" % time.gmtime()[:6])
            return True
        except Exception as e:
            print("NTP retry:", e)
            time.sleep(1)
    return False


def fetch_weather():
    global tz_offset, wx
    url = ("https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s"
           "&current=temperature_2m,apparent_temperature,relative_humidity_2m,"
           "weather_code,wind_speed_10m,wind_direction_10m,is_day"
           "&temperature_unit=fahrenheit&wind_speed_unit=mph"
           "&timezone=%s") % (LAT, LON, TZ.replace("/", "%2F"))
    r = requests.get(url)
    data = r.json()
    r.close()
    tz_offset = data.get("utc_offset_seconds", tz_offset)
    wx = data["current"]
    print_weather()


def compass(deg):
    pts = ("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    return pts[int((deg + 22.5) // 45) % 8]


def print_weather():
    if not wx:
        return
    code = wx["weather_code"]
    sys.stdout.write("\n")               # leave the live clock line
    print("-" * 52)
    print(" Newbury Park, CA            %s" % WMO.get(code, "code %s" % code))
    print(" Temp %.0f F  (feels %.0f F)   Humidity %s%%" % (
        wx["temperature_2m"], wx["apparent_temperature"], wx["relative_humidity_2m"]))
    print(" Wind %.0f mph %s            %s" % (
        wx["wind_speed_10m"], compass(wx["wind_direction_10m"]),
        "Day" if wx["is_day"] else "Night"))
    print("-" * 52)


def clock_line():
    lt = time.localtime(time.time() + tz_offset)
    yr, mo, dy, hh, mm, ss, wd, _ = lt
    ampm = "AM" if hh < 12 else "PM"
    h12 = hh % 12 or 12
    abbr = "PDT" if tz_offset == -7 * 3600 else "PST"
    temp = "  --" if not wx else "%3.0fF" % wx["temperature_2m"]
    sys.stdout.write("\r %s %s %2d %4d   %2d:%02d:%02d %s %s   %s   " % (
        WDAY[wd], MON[mo], dy, yr, h12, mm, ss, ampm, abbr, temp))


def main():
    print("\n=== ESP32-S3 Clock + Weather ===")
    while not connect_wifi():
        time.sleep(5)
    sync_ntp()
    try:
        fetch_weather()
    except Exception as e:
        print("weather error:", e)

    last_wx = last_ntp = time.time()
    while True:
        try:
            now = time.time()
            if now - last_ntp >= NTP_EVERY:
                if sync_ntp():
                    last_ntp = now
            if now - last_wx >= WEATHER_EVERY:
                try:
                    fetch_weather()
                    last_wx = now
                except Exception as e:
                    print("\nweather error:", e)
                    last_wx = now - WEATHER_EVERY + 60   # retry in ~1 min
            if not network.WLAN(network.STA_IF).isconnected():
                connect_wifi()
            clock_line()
            time.sleep(1)
        except KeyboardInterrupt:
            print("\n[stopped]")
            break
        except Exception as e:
            print("\nloop error:", e)
            time.sleep(2)


main()
