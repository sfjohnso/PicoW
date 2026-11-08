# Builds the installer: placeholder-settings firmware -> installer\firmware -> dist\P4ClockInstaller.exe
# Run from a normal PowerShell (not the IDF one); it activates ESP-IDF itself.
$ErrorActionPreference = 'Continue'   # native-command failures are caught via $LASTEXITCODE below
$here = $PSScriptRoot
$proj = Join-Path $here '..\p4clock' | Resolve-Path
$fw = Join-Path $here 'firmware'

# 1. Firmware with placeholder settings (installer_defaults.h), in its own build folder.
. C:\Espressif\tools\Microsoft.v5.5.6.PowerShell_profile.ps1 | Out-Null
$env:P4CLOCK_INSTALLER_BUILD = '1'
Push-Location $proj
idf.py -B build_installer build
if ($LASTEXITCODE -ne 0) { throw 'firmware build failed' }
Pop-Location
Remove-Item Env:P4CLOCK_INSTALLER_BUILD

# 2. Copy the three program files (offsets are in installer.py and build_installer\flash_args).
New-Item -ItemType Directory -Force $fw | Out-Null
Copy-Item "$proj\build_installer\bootloader\bootloader.bin" "$fw\bootloader.bin"
Copy-Item "$proj\build_installer\partition_table\partition-table.bin" "$fw\partition-table.bin"
Copy-Item "$proj\build_installer\p4clock.bin" "$fw\p4clock.bin"

# 3. Safety check: the real Wi-Fi name and password from main\user_config.h must not be in the bundle.
$py = Join-Path $here '.venv\Scripts\python.exe'
& $py (Join-Path $here 'check_no_secrets.py') "$proj\main\user_config.h" $fw
if ($LASTEXITCODE -ne 0) { throw 'secret check failed: firmware contains real settings' }

# 4. One-file Windows program with the firmware inside.
& (Join-Path $here '.venv\Scripts\pyinstaller.exe') --noconfirm --clean --onefile --windowed `
    --name P4ClockInstaller --collect-all esptool --collect-data tzdata --add-data "$fw;firmware" `
    --distpath (Join-Path $here 'dist') --workpath (Join-Path $here 'build') --specpath (Join-Path $here 'build') `
    (Join-Path $here 'installer.py')
if ($LASTEXITCODE -ne 0) { throw 'PyInstaller failed' }
Write-Host "Done: $(Join-Path $here 'dist\P4ClockInstaller.exe')"
