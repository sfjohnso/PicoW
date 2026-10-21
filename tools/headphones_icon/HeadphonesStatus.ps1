# Small live popup: shows whether Windows currently detects a plug in the Realtek headphone jack.
# Launched by FixHeadphones.vbs right before it starts the re-detect task. Close with the X or Esc.
# -SnapshotPath/-SnapshotDelaySec are for testing: save a picture of the window after a delay, then exit.
param([string]$SnapshotPath, [int]$SnapshotDelaySec = 2)

Add-Type -AssemblyName System.Windows.Forms, System.Drawing
[System.Windows.Forms.Application]::EnableVisualStyles()

$mutex = New-Object System.Threading.Mutex($false, 'Local\FixHeadphonesStatusPopup')
if (-not $mutex.WaitOne(0)) { exit }   # a popup is already open; it keeps tracking the jack on its own

$script:started = Get-Date
$logFile = 'C:\ProgramData\RealtekAudioFix\realtek_audio_restart.log'
$renderKey = 'SOFTWARE\Microsoft\Windows\CurrentVersion\MMDevices\Audio\Render'
$pendingTimeoutSec = 25

# True if any Realtek "Headphones" endpoint is Active (DeviceState 1); 8 = unplugged, 4 = not present.
function Test-HeadphonesDetected {
  $render = [Microsoft.Win32.Registry]::LocalMachine.OpenSubKey($renderKey)
  foreach ($name in $render.GetSubKeyNames()) {
    $ep = $render.OpenSubKey($name)
    $props = $ep.OpenSubKey('Properties')
    $adapter = $props.GetValue('{b3f8fa53-0004-438e-9003-51a46e139bfc},6')
    $friendly = $props.GetValue('{a45c254e-df1c-4efd-8020-67d146a850e0},2')
    if ($adapter -match 'Realtek' -and $friendly -eq 'Headphones' -and $ep.GetValue('DeviceState') -eq 1) { return $true }
  }
  return $false
}

$green = [System.Drawing.Color]::FromArgb(46, 160, 80)
$red   = [System.Drawing.Color]::FromArgb(200, 70, 70)
$blue  = [System.Drawing.Color]::FromArgb(70, 130, 200)
$script:dot = $blue
$script:lastState = $null
$script:lastChange = $script:started

$form = New-Object System.Windows.Forms.Form
$form.Text = 'Fix Headphones'
$form.ClientSize = New-Object System.Drawing.Size(360, 130)
$form.FormBorderStyle = 'FixedDialog'
$form.MaximizeBox = $false
$form.MinimizeBox = $false
$form.TopMost = $true
$form.KeyPreview = $true
$form.StartPosition = 'Manual'
$wa = [System.Windows.Forms.Screen]::PrimaryScreen.WorkingArea
$form.Location = New-Object System.Drawing.Point(($wa.Right - $form.Width - 16), ($wa.Bottom - $form.Height - 16))
try { $form.Icon = New-Object System.Drawing.Icon((Join-Path $PSScriptRoot 'headphones.ico')) } catch [System.ArgumentException] { }

$dotPanel = New-Object System.Windows.Forms.Panel
$dotPanel.Location = New-Object System.Drawing.Point(18, 24)
$dotPanel.Size = New-Object System.Drawing.Size(34, 34)
$dotPanel.Add_Paint({
  param($s, $e)
  $e.Graphics.SmoothingMode = 'AntiAlias'
  $brush = New-Object System.Drawing.SolidBrush($script:dot)
  $e.Graphics.FillEllipse($brush, 2, 2, 28, 28)
  $brush.Dispose()
})

$title = New-Object System.Windows.Forms.Label
$title.Location = New-Object System.Drawing.Point(66, 20)
$title.Size = New-Object System.Drawing.Size(282, 30)
$title.Font = New-Object System.Drawing.Font('Segoe UI Semibold', 14)

$hint = New-Object System.Windows.Forms.Label
$hint.Location = New-Object System.Drawing.Point(66, 52)
$hint.Size = New-Object System.Drawing.Size(282, 40)
$hint.Font = New-Object System.Drawing.Font('Segoe UI', 9)
$hint.ForeColor = [System.Drawing.Color]::DimGray

$since = New-Object System.Windows.Forms.Label
$since.Location = New-Object System.Drawing.Point(18, 100)
$since.Size = New-Object System.Drawing.Size(230, 20)
$since.Font = New-Object System.Drawing.Font('Segoe UI', 8.5)
$since.ForeColor = [System.Drawing.Color]::Gray

# Re-runs the re-detect task; disabled while a re-detect is in progress so repeat clicks can't cut the audio again.
$redetect = New-Object System.Windows.Forms.Button
$redetect.Text = 'Re-detect'
$redetect.Location = New-Object System.Drawing.Point(258, 94)
$redetect.Size = New-Object System.Drawing.Size(90, 28)
$redetect.Add_Click({
  $script:started = Get-Date
  Start-Process schtasks.exe -ArgumentList '/run', '/tn', 'RestartRealtekAudio' -WindowStyle Hidden
  Update-Popup
})

$form.Controls.AddRange(@($dotPanel, $title, $hint, $since, $redetect))
$form.Add_KeyDown({ param($s, $e) if ($e.KeyCode -eq 'Escape') { $form.Close() } })

function Update-Popup {
  $logTime = if (Test-Path $logFile) { (Get-Item $logFile).LastWriteTime } else { [datetime]::MinValue }
  $waiting = ($logTime -le $script:started) -and (((Get-Date) - $script:started).TotalSeconds -lt $pendingTimeoutSec)
  $redetect.Enabled = -not $waiting
  if ($waiting) {
    $script:dot = $blue
    $title.Text = 'Re-detecting headphone jack...'
    $hint.Text = 'This takes a few seconds.'
    $since.Text = ''
  } else {
    $detected = Test-HeadphonesDetected
    if ($detected -ne $script:lastState) { $script:lastState = $detected; $script:lastChange = Get-Date }
    if ($detected) {
      $script:dot = $green
      $title.Text = 'Headphones detected'
      $hint.Text = 'Updates live. Unplug them to see it change.'
    } else {
      $script:dot = $red
      $title.Text = 'No headphones detected'
      $hint.Text = "Plug them in. If this doesn't update, press Re-detect."
    }
    $since.Text = 'Since ' + $script:lastChange.ToString('h:mm:ss tt')
  }
  $dotPanel.Invalidate()
}

$timer = New-Object System.Windows.Forms.Timer
$timer.Interval = 400
$timer.Add_Tick({ Update-Popup })
$form.Add_Shown({ Update-Popup; $timer.Start() })
$form.Add_FormClosed({ $timer.Stop(); $mutex.ReleaseMutex() })

if ($SnapshotPath) {
  $snap = New-Object System.Windows.Forms.Timer
  $snap.Interval = $SnapshotDelaySec * 1000
  $snap.Add_Tick({
    $snap.Stop()
    Update-Popup
    $bmp = New-Object System.Drawing.Bitmap($form.Width, $form.Height)
    $form.DrawToBitmap($bmp, (New-Object System.Drawing.Rectangle(0, 0, $form.Width, $form.Height)))
    $bmp.Save($SnapshotPath, [System.Drawing.Imaging.ImageFormat]::Png)
    $form.Close()
  })
  $snap.Start()
}

[void]$form.ShowDialog()
