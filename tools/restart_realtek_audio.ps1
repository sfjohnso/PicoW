# Restarts the Realtek codec so the headphone-jack state is re-read. Needs elevation.
# Appends before/after endpoint states to realtek_audio_restart.log next to this script.
$log = Join-Path $PSScriptRoot 'realtek_audio_restart.log'
$id = 'INTELAUDIO\FUNC_01&VEN_10EC&DEV_0236&SUBSYS_102809C2\5&250355F&0&0001'
$root = 'HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\MMDevices\Audio\Render'

function Get-JackState {
  $s = foreach ($k in Get-ChildItem $root) {
    $props = Get-ItemProperty "$($k.PSPath)\Properties" -ErrorAction SilentlyContinue
    if ($props.'{b3f8fa53-0004-438e-9003-51a46e139bfc},6' -match 'Realtek') {
      $name = $props.'{a45c254e-df1c-4efd-8020-67d146a850e0},2'
      $state = (Get-ItemProperty $k.PSPath).DeviceState
      if ($state -ne 4) { "$name=$state" }
    }
  }
  $s -join ' '
}

$before = Get-JackState
# pnputil /restart-device works only once per boot here (then "pending system reboot"), so cycle the device instead.
# Audiosrv is stopped first: with an app holding an open audio stream, Disable-PnpDevice fails with "Generic failure".
try {
  Stop-Service Audiosrv -Force
  Disable-PnpDevice -InstanceId $id -Confirm:$false -ErrorAction Stop
  Start-Sleep 2
  Enable-PnpDevice -InstanceId $id -Confirm:$false -ErrorAction Stop
  $code = 0; $out = 'disable/enable ok'
} catch {
  $code = 1; $out = $_.Exception.Message
} finally {
  Start-Service Audiosrv
}
Start-Sleep 4
$after = Get-JackState
$ok = if ($code -eq 0) { 'ok' } else { "RESTART-FAILED(exit $code)" }
"{0}  {1}  before[{2}]  after[{3}]  pnputil: {4}" -f (Get-Date -Format 'yyyy-MM-dd HH:mm:ss'), $ok, $before, $after, (($out -split "`r?`n" | ? { $_.Trim() }) -join ' | ') | Add-Content $log -Encoding utf8
