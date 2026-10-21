# Creates the "Fix Headphones" shortcut in the Start menu Programs folder, then tries the taskbar/Start pin verbs.
$dir = 'C:\Users\Steve\tools\headphones_icon'
$lnkPath = Join-Path $env:APPDATA 'Microsoft\Windows\Start Menu\Programs\Fix Headphones.lnk'
$ws = New-Object -ComObject WScript.Shell
$s = $ws.CreateShortcut($lnkPath)
$s.TargetPath = "$env:WINDIR\System32\wscript.exe"
$s.Arguments = "`"$dir\FixHeadphones.vbs`""
$s.WorkingDirectory = $dir
$s.IconLocation = "$dir\headphones.ico,0"
$s.Description = 'Re-detect headphones plugged into the Realtek jack'
$s.Save()
"created $lnkPath"

$folder = (New-Object -ComObject Shell.Application).Namespace((Split-Path $lnkPath))
$item = $folder.ParseName('Fix Headphones.lnk')
"verbs: " + (($item.Verbs() | % { $_.Name }) -join ' | ')
