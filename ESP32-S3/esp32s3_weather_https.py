# ESP32-S3 HTTPS Clock + Weather server  (MicroPython, ESP32_GENERIC_S3)
# ---------------------------------------------------------------------------
# Joins WiFi, keeps time synced via NTP, fetches Newbury Park (91320) weather
# from Open-Meteo over HTTPS, and serves a styled status page over HTTPS
# (self-signed EC cert) on port 443.  Browse to  https://<board-ip>/
#
# Files required on the board:  secrets.py, cert.pem, key.pem
# Deploy as main.py to run on boot.
#
# Port note: in the bootloader the board is COM8 / PID 1001 (esptool); while
# MicroPython runs it is COM11 / PID 4001 (mpremote / REPL).
# ---------------------------------------------------------------------------
import sys, time, gc, network, ntptime, requests, socket, tls

# --- configuration ---------------------------------------------------------
try:
    from secrets import SSID, PASSWORD
except ImportError:
    SSID, PASSWORD = "your-ssid", "your-password"

LAT, LON = 34.1872, -118.9114            # Newbury Park, CA 91320
TZ       = "America/Los_Angeles"
PORT     = 443

# Optional static IP so the URL never changes.  Leave STATIC_IP = None to use
# DHCP.  If you set one, also add a DHCP reservation on the router to be safe.
STATIC_IP = None                         # e.g. ("192.168.0.157","255.255.255.0","192.168.0.1","192.168.0.1")

WEATHER_EVERY = 1800                     # refresh weather every 30 min
NTP_EVERY     = 6 * 3600                 # re-sync clock every 6 hours

WDAY = ("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
MON  = ("","January","February","March","April","May","June","July","August",
        "September","October","November","December")
WMO = {
    0:"Clear sky",1:"Mainly clear",2:"Partly cloudy",3:"Overcast",45:"Fog",
    48:"Rime fog",51:"Light drizzle",53:"Drizzle",55:"Heavy drizzle",
    56:"Freezing drizzle",57:"Freezing drizzle",61:"Light rain",63:"Rain",
    65:"Heavy rain",66:"Freezing rain",67:"Freezing rain",71:"Light snow",
    73:"Snow",75:"Heavy snow",77:"Snow grains",80:"Light showers",81:"Showers",
    82:"Violent showers",85:"Snow showers",86:"Snow showers",95:"Thunderstorm",
    96:"Thunderstorm + hail",99:"Thunderstorm + hail",
}

# --- runtime state ---------------------------------------------------------
tz_offset = -7 * 3600
wx = None
wx_stamp = 0
ip_addr = "0.0.0.0"


def connect_wifi():
    global ip_addr
    w = network.WLAN(network.STA_IF)
    w.active(True)
    if STATIC_IP and not w.isconnected():
        try: w.ifconfig(STATIC_IP)
        except Exception as e: print("static ip set failed:", e)
    if not w.isconnected():
        print("Connecting to %s ..." % SSID)
        w.connect(SSID, PASSWORD)
        for _ in range(60):
            if w.isconnected(): break
            time.sleep(0.5)
    if w.isconnected():
        ip_addr = w.ifconfig()[0]
        print("WiFi OK:", ip_addr)
        return True
    print("WiFi FAILED status=%s" % w.status())
    return False


def sync_ntp():
    ntptime.host = "pool.ntp.org"
    for _ in range(3):
        try:
            ntptime.settime()
            print("NTP synced UTC %04d-%02d-%02d %02d:%02d:%02d" % time.gmtime()[:6])
            return True
        except Exception as e:
            print("NTP retry:", e); time.sleep(1)
    return False


def fetch_weather():
    global tz_offset, wx, wx_stamp
    url = ("https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s"
           "&current=temperature_2m,apparent_temperature,relative_humidity_2m,"
           "weather_code,wind_speed_10m,wind_direction_10m,is_day"
           "&temperature_unit=fahrenheit&wind_speed_unit=mph"
           "&timezone=%s") % (LAT, LON, TZ.replace("/", "%2F"))
    r = requests.get(url)
    data = r.json(); r.close()
    tz_offset = data.get("utc_offset_seconds", tz_offset)
    wx = data["current"]; wx_stamp = time.time()
    print("weather: %.0fF  %s" % (wx["temperature_2m"], WMO.get(wx["weather_code"], "?")))


def compass(deg):
    return ("N","NE","E","SE","S","SW","W","NW")[int((deg + 22.5) // 45) % 8]


def render_html():
    lt = time.localtime(time.time() + tz_offset)
    yr, mo, dy, hh, mm, ss, wd, _ = lt
    abbr = "PDT" if tz_offset == -7 * 3600 else "PST"
    ampm = "AM" if hh < 12 else "PM"        # 12-hour display (hh stays 24h for JS Date seed)
    h12  = hh % 12 or 12
    age = (time.time() - wx_stamp) if wx else 0

    if wx:
        code = wx["weather_code"]
        cond = WMO.get(code, "code %s" % code)
        temp = "%.0f" % wx["temperature_2m"]
        feels = "%.0f" % wx["apparent_temperature"]
        hum = "%s" % wx["relative_humidity_2m"]
        wind = "%.0f mph %s" % (wx["wind_speed_10m"], compass(wx["wind_direction_10m"]))
        daynight = "Day" if wx["is_day"] else "Night"
    else:
        cond, temp, feels, hum, wind, daynight = "(loading)", "--", "--", "--", "--", "--"

    return """<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>ESP32-S3 Clock &amp; Weather</title>
<style>
 :root{color-scheme:dark}
 *{box-sizing:border-box;margin:0;padding:0}
 body{font-family:-apple-system,Segoe UI,Roboto,sans-serif;
  background:radial-gradient(1200px 800px at 50%% -10%%,#1b2a4a,#0a0e17);
  color:#e8edf6;min-height:100vh;display:flex;align-items:center;justify-content:center}
 .card{width:min(92vw,460px);background:#121826cc;border:1px solid #28324a;
  border-radius:20px;padding:28px 30px;box-shadow:0 18px 50px #0008;backdrop-filter:blur(6px)}
 .loc{font-size:14px;letter-spacing:.14em;text-transform:uppercase;color:#8aa0c6}
 #clock{font-weight:700;letter-spacing:.02em;margin:6px 0 2px;white-space:nowrap;
  line-height:1.05;font-variant-numeric:tabular-nums;font-size:clamp(30px,13vw,72px)}
 #ticktime{display:inline-block}
 .ap{font-size:.6em;font-weight:600;color:#aebbd6;margin-left:.04em}
 #date{color:#aebbd6;font-size:16px;margin-bottom:20px}
 .tz{font-size:14px;color:#7f8db0}
 .wx{display:grid;grid-template-columns:1fr 1fr;gap:12px;margin-top:18px}
 .tile{background:#0e1422;border:1px solid #232d44;border-radius:12px;padding:12px 14px}
 .tile .k{font-size:11px;letter-spacing:.1em;text-transform:uppercase;color:#7f8db0}
 .tile .v{font-size:22px;font-weight:600;margin-top:3px}
 .cond{grid-column:1/3;text-align:center;font-size:20px;font-weight:600;color:#cfe0ff}
 .temp{font-size:30px}
 footer{margin-top:20px;font-size:12px;color:#66749a;text-align:center;line-height:1.6}
</style></head><body><div class="card">
 <div class="loc">Newbury Park, CA &middot; 91320</div>
 <div id="clock"><span id="ticktime">%d:%02d:%02d<span class="ap"> %s</span></span></div>
 <div id="date">%s, %s %d, %d</div>
 <div class="tz">%s</div>
 <div class="wx">
   <div class="tile cond">%s</div>
   <div class="tile"><div class="k">Temperature</div><div class="v temp">%s&deg;F</div></div>
   <div class="tile"><div class="k">Feels like</div><div class="v">%s&deg;F</div></div>
   <div class="tile"><div class="k">Humidity</div><div class="v">%s%%</div></div>
   <div class="tile"><div class="k">Wind</div><div class="v">%s</div></div>
 </div>
 <footer>%s &middot; weather updated %ds ago<br>served by ESP32-S3 &middot; %s</footer>
</div>
<script>
 // Live client-side clock seeded from the server's local time.
 var t=new Date(%d,%d,%d,%d,%d,%d);
 var D=["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"];
 var M=["January","February","March","April","May","June","July","August","September","October","November","December"];
 function p(n){return (n<10?"0":"")+n;}
 function fmt(H,Mi,S){var ap=H<12?"AM":"PM",h=H%%12;if(h==0)h=12;
   return h+":"+p(Mi)+":"+p(S)+'<span class="ap"> '+ap+'</span>';}
 var box=document.getElementById("clock"),inner=document.getElementById("ticktime");
 function fitClock(){                       // size widest time (12:55:55 PM) to fit width
   var avail=box.clientWidth,save=inner.innerHTML;
   inner.innerHTML=fmt(12,55,55);
   box.style.fontSize="100px";
   var w=inner.getBoundingClientRect().width;
   box.style.fontSize=(100*avail/w*0.97)+"px";
   inner.innerHTML=save;
 }
 function tick(){
   inner.innerHTML=fmt(t.getHours(),t.getMinutes(),t.getSeconds());
   document.getElementById("date").textContent=D[t.getDay()]+", "+M[t.getMonth()]+" "+t.getDate()+", "+t.getFullYear();
   t=new Date(t.getTime()+1000);
 }
 tick(); fitClock(); setInterval(tick,1000);
 window.addEventListener("resize",fitClock);
 window.addEventListener("orientationchange",fitClock);
 // Reload to refresh weather every 5 minutes.
 setTimeout(function(){location.reload();},300000);
</script></body></html>""" % (
        h12, mm, ss, ampm, WDAY[wd], MON[mo], dy, yr, abbr,
        cond, temp, feels, hum, wind,
        daynight, age, ip_addr,
        yr, mo - 1, dy, hh, mm, ss)


def make_server_ctx():
    ctx = tls.SSLContext(tls.PROTOCOL_TLS_SERVER)
    with open("cert.pem", "rb") as f: cert = f.read()
    with open("key.pem", "rb") as f: key = f.read()
    ctx.load_cert_chain(cert, key)
    return ctx


def maintenance(state):
    global wx_stamp
    now = time.time()
    if now - state["ntp"] >= NTP_EVERY:
        if sync_ntp(): state["ntp"] = now
    if wx is None or (now - wx_stamp) >= WEATHER_EVERY:
        try: fetch_weather()
        except Exception as e: print("weather error:", e); wx_stamp = now
    if not network.WLAN(network.STA_IF).isconnected():
        connect_wifi()


def handle(cl, raddr, ctx):
    scl = None
    try:
        cl.settimeout(5)                         # raw-socket timeout (SSLSocket has none)
        scl = ctx.wrap_socket(cl, server_side=True)
        try:
            scl.readline()                       # request line
            while True:                          # drain headers
                h = scl.readline()
                if not h or h == b"\r\n": break
        except Exception:
            pass
        body = render_html().encode("utf-8")
        scl.write(b"HTTP/1.1 200 OK\r\n")
        scl.write(b"Content-Type: text/html; charset=utf-8\r\n")
        scl.write(b"Connection: close\r\n")
        scl.write(b"Content-Length: %d\r\n\r\n" % len(body))
        scl.write(body)
        print("served", raddr[0], len(body), "bytes")
    except Exception as e:
        print("conn error:", e)
    finally:
        if scl:
            try: scl.close()
            except Exception: pass
        try: cl.close()
        except Exception: pass
        gc.collect()


def serve():
    ctx = make_server_ctx()
    addr = socket.getaddrinfo("0.0.0.0", PORT)[0][-1]
    s = socket.socket()
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(addr)
    s.listen(2)
    s.settimeout(15)                     # so we can run maintenance between hits
    print("HTTPS server up:  https://%s/" % ip_addr)

    state = {"ntp": time.time()}
    while True:
        try:
            maintenance(state)
            try:
                cl, raddr = s.accept()
            except OSError:
                continue                 # accept timeout -> loop for maintenance
            handle(cl, raddr, ctx)
        except Exception as e:
            print("loop error:", e)      # never let the listen socket get rebound
            time.sleep(1)


def main():
    print("\n=== ESP32-S3 HTTPS Clock + Weather ===")
    while not connect_wifi():
        time.sleep(5)
    sync_ntp()
    try: fetch_weather()
    except Exception as e: print("weather error:", e)
    while True:
        try:
            serve()
        except Exception as e:
            print("server crashed, restarting:", e)
            time.sleep(3)


main()
