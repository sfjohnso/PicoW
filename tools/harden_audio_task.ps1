# One-time setup (run elevated): move the audio-fix script out of the user-writable tools\ folder into an
# admin-only folder, repoint the SYSTEM scheduled task at it, and let Steve start (not edit) the task.
$ErrorActionPreference = 'Stop'
$logf = "$env:TEMP\harden_audio_task.txt"
$o = @()
try {
  $dir = 'C:\ProgramData\RealtekAudioFix'
  $src = 'C:\Users\Steve\tools\restart_realtek_audio.ps1'
  $dst = Join-Path $dir 'restart_realtek_audio.ps1'
  $sid = 'S-1-5-21-1540116631-343097745-601316261-1001'

  New-Item -ItemType Directory -Force $dir | Out-Null
  icacls $dir /inheritance:r /grant:r '*S-1-5-18:(OI)(CI)F' '*S-1-5-32-544:(OI)(CI)F' '*S-1-5-32-545:(OI)(CI)RX' | Out-Null
  Copy-Item $src $dst -Force
  $old = 'C:\Users\Steve\tools\realtek_audio_restart.log'
  if (Test-Path $old) { Copy-Item $old (Join-Path $dir 'realtek_audio_restart.log') -Force }
  $o += (icacls $dir | Out-String).Trim()

  $act = New-ScheduledTaskAction -Execute 'powershell.exe' -Argument "-NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -File `"$dst`""
  Set-ScheduledTask -TaskName 'RestartRealtekAudio' -Action $act | Out-Null

  $svc = New-Object -ComObject Schedule.Service
  $svc.Connect()
  $task = $svc.GetFolder('\').GetTask('RestartRealtekAudio')
  $sddl = $task.GetSecurityDescriptor(4)
  if ($sddl -notmatch [regex]::Escape($sid)) { $sddl += "(A;;GRGX;;;$sid)" }
  $task.SetSecurityDescriptor($sddl, 0)
  $o += "Task SDDL now: " + $task.GetSecurityDescriptor(4)
  $o += "Task action now: " + (Get-ScheduledTask 'RestartRealtekAudio').Actions[0].Arguments
  $o += 'DONE'
} catch { $o += "ERROR: $($_.Exception.Message)" }
$o | Out-File $logf -Encoding utf8
