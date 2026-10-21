# Registers the "RestartRealtekAudio" scheduled task (SYSTEM, highest privileges).
# Triggers: 45 s after startup, and 15 s after resume from sleep. Needs elevation.
# The script runs as SYSTEM, so it must live in an admin-only folder (see harden_audio_task.ps1), not in tools\.
$script = 'C:\ProgramData\RealtekAudioFix\restart_realtek_audio.ps1'
$xml = @"
<?xml version="1.0" encoding="UTF-16"?>
<Task version="1.4" xmlns="http://schemas.microsoft.com/windows/2004/02/mit/task">
  <RegistrationInfo><Description>Restart Realtek codec so headphone-jack detection is refreshed (boot + resume).</Description></RegistrationInfo>
  <Triggers>
    <BootTrigger><Enabled>true</Enabled><Delay>PT45S</Delay></BootTrigger>
    <EventTrigger>
      <Enabled>true</Enabled>
      <Delay>PT15S</Delay>
      <Subscription>&lt;QueryList&gt;&lt;Query Id="0" Path="System"&gt;&lt;Select Path="System"&gt;*[System[Provider[@Name='Microsoft-Windows-Power-Troubleshooter'] and EventID=1]]&lt;/Select&gt;&lt;/Query&gt;&lt;/QueryList&gt;</Subscription>
    </EventTrigger>
  </Triggers>
  <Principals><Principal id="Author"><UserId>S-1-5-18</UserId><RunLevel>HighestAvailable</RunLevel></Principal></Principals>
  <Settings>
    <MultipleInstancesPolicy>IgnoreNew</MultipleInstancesPolicy>
    <DisallowStartIfOnBatteries>false</DisallowStartIfOnBatteries>
    <StopIfGoingOnBatteries>false</StopIfGoingOnBatteries>
    <StartWhenAvailable>true</StartWhenAvailable>
    <ExecutionTimeLimit>PT2M</ExecutionTimeLimit>
  </Settings>
  <Actions Context="Author">
    <Exec><Command>powershell.exe</Command><Arguments>-NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -File "$script"</Arguments></Exec>
  </Actions>
</Task>
"@
Register-ScheduledTask -TaskName 'RestartRealtekAudio' -Xml $xml -Force | Out-Null
Get-ScheduledTask -TaskName 'RestartRealtekAudio' | Out-File "$env:TEMP\register_audio_task.txt"
