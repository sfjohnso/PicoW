# Builds the installer: launcher + clock + apps -> installer\firmware and installer\apps -> dist\P4ClockInstaller.exe
# Run from a normal PowerShell (not the IDF one); it activates ESP-IDF itself.
$ErrorActionPreference = 'Continue'   # native-command failures are caught via $LASTEXITCODE below
$here = $PSScriptRoot
$projects = Join-Path $here '..' | Resolve-Path
$clock = Join-Path $projects 'p4clock'
$fw = Join-Path $here 'firmware'
$apps = Join-Path $here 'apps'
. C:\Espressif\tools\Microsoft.v5.5.6.PowerShell_profile.ps1 | Out-Null

# 1. The clock with placeholder settings (installer_defaults.h), in its own build folder.
$env:P4CLOCK_INSTALLER_BUILD = '1'
Push-Location $clock
idf.py -B build_installer build
if ($LASTEXITCODE -ne 0) { throw 'clock build failed' }
Pop-Location
Remove-Item Env:P4CLOCK_INSTALLER_BUILD

# 2. The launcher and the apps (their normal build folders; configured once with set-target esp32p4).
foreach ($name in 'launcher', 'photos', 'music', 'life', 'quake') {
    Push-Location (Join-Path $projects $name)
    idf.py build
    if ($LASTEXITCODE -ne 0) { throw "$name build failed" }
    Pop-Location
}

# 3. Collect the files. The bootloader and partition table must be the launcher's (app rollback enabled, launcher layout).
# Flash offsets are in installer.py (FLASH_FILES) and launcher\build\flash_args.
Remove-Item "$fw\*", "$apps\*" -ErrorAction SilentlyContinue
New-Item -ItemType Directory -Force $fw, $apps | Out-Null
$l = Join-Path $projects 'launcher\build'
Copy-Item "$l\bootloader\bootloader.bin" "$fw\bootloader.bin"
Copy-Item "$l\partition_table\partition-table.bin" "$fw\partition-table.bin"
Copy-Item "$l\launcher.bin" "$fw\launcher.bin"
Copy-Item "$l\ota_data_initial.bin" "$fw\ota_data_initial.bin"
Copy-Item "$clock\build_installer\p4clock.bin" "$fw\p4clock.bin"
Copy-Item (Join-Path $projects 'quake\build\quake.bin') "$apps\Quake Demo.bin"
Copy-Item (Join-Path $projects 'photos\build\photos.bin') "$apps\Photos.bin"
Copy-Item (Join-Path $projects 'music\build\music.bin') "$apps\Music.bin"
Copy-Item (Join-Path $projects 'life\build\life.bin') "$apps\Life.bin"

# 4. Safety check: the real Wi-Fi name and password from main\user_config.h must not be in the bundle.
$py = Join-Path $here '.venv\Scripts\python.exe'
foreach ($dir in $fw, $apps) {
    & $py (Join-Path $here 'check_no_secrets.py') "$clock\main\user_config.h" $dir
    if ($LASTEXITCODE -ne 0) { throw "secret check failed: $dir contains real settings" }
}

# 5. One-file Windows program with the firmware and apps inside.
& (Join-Path $here '.venv\Scripts\pyinstaller.exe') --noconfirm --clean --onefile --windowed `
    --name P4ClockInstaller --collect-all esptool --collect-data tzdata `
    --add-data "$fw;firmware" --add-data "$apps;apps" `
    --distpath (Join-Path $here 'dist') --workpath (Join-Path $here 'build') --specpath (Join-Path $here 'build') `
    (Join-Path $here 'installer.py')
if ($LASTEXITCODE -ne 0) { throw 'PyInstaller failed' }
Write-Host "Done: $(Join-Path $here 'dist\P4ClockInstaller.exe')"
