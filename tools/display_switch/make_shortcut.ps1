# Creates the "Switch Display" shortcut in the Start menu Programs folder (pin it to the taskbar from there).
$dir = $PSScriptRoot
$lnkPath = Join-Path $env:APPDATA 'Microsoft\Windows\Start Menu\Programs\Switch Display.lnk'
$s = (New-Object -ComObject WScript.Shell).CreateShortcut($lnkPath)
$s.TargetPath = "$env:WINDIR\System32\wscript.exe"
$s.Arguments = "`"$dir\SwitchDisplay.vbs`""
$s.WorkingDirectory = $dir
$s.IconLocation = "$dir\display.ico,0"
$s.Description = 'Detect the external display and switch to it (laptop screen off)'
$s.Save()
"created $lnkPath"
