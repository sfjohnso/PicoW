' Opens the live headphone-status popup, then starts the SYSTEM scheduled task that re-reads the
' headphone jack (no UAC prompt, no console windows).
Set sh = CreateObject("WScript.Shell")
dir = CreateObject("Scripting.FileSystemObject").GetParentFolderName(WScript.ScriptFullName)
sh.Run "powershell.exe -NoProfile -ExecutionPolicy Bypass -STA -WindowStyle Hidden -File """ & dir & "\HeadphonesStatus.ps1""", 0, False
sh.Run "schtasks.exe /run /tn RestartRealtekAudio", 0, False
