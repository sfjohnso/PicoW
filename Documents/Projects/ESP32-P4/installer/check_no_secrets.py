"""Fails (exit 1) if any firmware file contains the Wi-Fi name or password from user_config.h.

usage: check_no_secrets.py <user_config.h> <firmware folder>
"""
import os
import re
import sys

header, folder = sys.argv[1], sys.argv[2]
text = open(header, encoding="latin-1").read()
secrets = {}
for name in ("WIFI_SSID", "WIFI_PASS"):
    m = re.search(r'#define\s+%s\s+"([^"]+)"' % name, text)
    if m:
        secrets[name] = m.group(1).encode("latin-1")
if not secrets:
    sys.exit("no WIFI_SSID/WIFI_PASS found in %s: nothing to check against" % header)

bad = 0
for fname in sorted(os.listdir(folder)):
    data = open(os.path.join(folder, fname), "rb").read()
    for name, value in secrets.items():
        if value in data:
            print("FOUND %s in %s" % (name, fname))
            bad += 1
print("secret check: %s" % ("FAILED" if bad else "clean (%d file(s) checked)" % len(os.listdir(folder))))
sys.exit(1 if bad else 0)
