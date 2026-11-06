# Detects an external display and switches Windows to "second screen only" (laptop panel off), retrying until
# it sticks. Shows a small progress popup; every attempt is logged to display_switch.log next to this script.
#   -NoUi     run headless, print progress to the console, exit code 0 = switched, 1 = failed
#   -DryRun   only validate the switch with Windows (SDC_VALIDATE); change nothing
param([switch]$NoUi, [switch]$DryRun, [int]$Attempts = 6)

$logFile = Join-Path $PSScriptRoot 'display_switch.log'
function Write-Log([string]$m) { ('{0}  {1}' -f (Get-Date -Format 'yyyy-MM-dd HH:mm:ss'), $m) | Add-Content $logFile -Encoding utf8 }

Add-Type -TypeDefinition @'
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

public static class Ccd {
  [DllImport("user32.dll")] static extern int GetDisplayConfigBufferSizes(uint flags, out uint np, out uint nm);
  [DllImport("user32.dll")] static extern int QueryDisplayConfig(uint flags, ref uint np, IntPtr paths, ref uint nm, IntPtr modes, IntPtr topo);
  [DllImport("user32.dll")] static extern int SetDisplayConfig(uint np, IntPtr paths, uint nm, IntPtr modes, uint flags);
  [DllImport("user32.dll")] static extern int DisplayConfigGetDeviceInfo(IntPtr packet);
  [DllImport("user32.dll")] static extern void keybd_event(byte vk, byte scan, uint flags, UIntPtr extra);

  const uint USE_DB_CURRENT = 0xF, TOPO_EXTERNAL = 0x8, TOPO_SUPPLIED = 0x10, VALIDATE = 0x40, APPLY = 0x80,
             ALLOW_CHANGES = 0x400, FORCE_ENUM = 0x1000, ALLOW_ORDER = 0x2000;
  const int PATH = 72, MODE = 64;

  public class Row {
    public byte[] Raw;
    public uint TgtLow, TgtId, Tech;
    public int TgtHigh;
    public bool Available, Active;
    public string Name;
    public bool Internal { get { return Tech == 0x80000000 || Tech == 11 || Tech == 13; } }
    public string Key { get { return TgtLow + ":" + TgtHigh + ":" + TgtId; } }
  }

  static string TargetName(uint low, int high, uint id) {
    IntPtr p = Marshal.AllocHGlobal(420);
    try {
      for (int i = 0; i < 420; i++) Marshal.WriteByte(p, i, 0);
      Marshal.WriteInt32(p, 0, 2); Marshal.WriteInt32(p, 4, 420);          // GET_TARGET_NAME
      Marshal.WriteInt32(p, 8, (int)low); Marshal.WriteInt32(p, 12, high); Marshal.WriteInt32(p, 16, (int)id);
      if (DisplayConfigGetDeviceInfo(p) != 0) return "";
      return Marshal.PtrToStringUni(new IntPtr(p.ToInt64() + 36), 64).TrimEnd('\0');
    } finally { Marshal.FreeHGlobal(p); }
  }

  // flags: 1 = all paths (incl. inactive), 2 = active paths only
  public static List<Row> Query(uint flags) {
    uint np, nm;
    GetDisplayConfigBufferSizes(flags, out np, out nm);
    IntPtr paths = Marshal.AllocHGlobal((int)np * PATH), modes = Marshal.AllocHGlobal((int)nm * MODE);
    try {
      int rc = QueryDisplayConfig(flags, ref np, paths, ref nm, modes, IntPtr.Zero);
      if (rc != 0) throw new Exception("QueryDisplayConfig rc=" + rc);
      var rows = new List<Row>();
      for (int i = 0; i < np; i++) {
        IntPtr b = new IntPtr(paths.ToInt64() + i * PATH);
        var r = new Row();
        r.Raw = new byte[PATH]; Marshal.Copy(b, r.Raw, 0, PATH);
        r.TgtLow = (uint)Marshal.ReadInt32(b, 20); r.TgtHigh = Marshal.ReadInt32(b, 24); r.TgtId = (uint)Marshal.ReadInt32(b, 28);
        r.Tech = (uint)Marshal.ReadInt32(b, 36);
        r.Available = Marshal.ReadInt32(b, 60) != 0;
        r.Active = (Marshal.ReadInt32(b, 68) & 1) != 0;
        r.Name = TargetName(r.TgtLow, r.TgtHigh, r.TgtId);
        rows.Add(r);
      }
      return rows;
    } finally { Marshal.FreeHGlobal(paths); Marshal.FreeHGlobal(modes); }
  }

  // Same thing Win+P "Second screen only" does.
  public static int SetExternalTopology(bool validateOnly) {
    return SetDisplayConfig(0, IntPtr.Zero, 0, IntPtr.Zero, TOPO_EXTERNAL | (validateOnly ? VALIDATE : APPLY));
  }

  // Fallback: hand Windows exactly one path (source 0 -> this target) and let it choose the modes.
  public static int ApplyExplicit(Row r, bool validateOnly) {
    byte[] raw = (byte[])r.Raw.Clone();
    BitConverter.GetBytes(0xFFFFFFFFu).CopyTo(raw, 12);   // source mode index: invalid, system picks
    BitConverter.GetBytes(0xFFFFFFFFu).CopyTo(raw, 32);   // target mode index: invalid, system picks
    BitConverter.GetBytes(1u).CopyTo(raw, 68);            // path flags: active
    IntPtr buf = Marshal.AllocHGlobal(PATH);
    try {
      Marshal.Copy(raw, 0, buf, PATH);
      return SetDisplayConfig(1, buf, 0, IntPtr.Zero, TOPO_SUPPLIED | ALLOW_ORDER | (validateOnly ? VALIDATE : APPLY));
    } finally { Marshal.FreeHGlobal(buf); }
  }

  // Re-applies the current configuration and makes the driver re-enumerate modes (the Settings "Detect" idea).
  public static int ForceEnumeration() {
    return SetDisplayConfig(0, IntPtr.Zero, 0, IntPtr.Zero, USE_DB_CURRENT | APPLY | FORCE_ENUM);
  }

  // Win+Ctrl+Shift+B: Windows' own "reset the graphics driver" hotkey; re-probes every connector.
  public static void ResetGraphicsHotkey() {
    byte[] keys = { 0x5B, 0x11, 0x10, 0x42 };
    foreach (byte k in keys) keybd_event(k, 0, 0, UIntPtr.Zero);
    System.Threading.Thread.Sleep(100);
    for (int i = keys.Length - 1; i >= 0; i--) keybd_event(keys[i], 0, 2, UIntPtr.Zero);
  }
}
'@

function Get-ExternalTargets {
  [Ccd]::Query(1) | Where-Object { $_.Available -and -not $_.Internal } | Group-Object Key | ForEach-Object { $_.Group[0] }
}
function Test-ExternalOnly {
  $a = @([Ccd]::Query(2))
  ($a.Count -ge 1) -and (@($a | Where-Object { $_.Internal }).Count -eq 0)
}
function Get-ActiveSummary {
  $a = @([Ccd]::Query(2))
  if ($a.Count -eq 0) { return 'none' }
  ($a | ForEach-Object { '{0}{1}' -f $(if ($_.Name) { $_.Name } else { 'target ' + $_.TgtId }), $(if ($_.Internal) { '(internal)' } else { '' }) }) -join ', '
}

# $say receives (state, title, detail); state is 'busy', 'ok' or 'fail'. Returns $true when external-only is active.
function Invoke-Switch([scriptblock]$say) {
  Write-Log ('--- start{0}; active now: {1}' -f $(if ($DryRun) { ' (dry run)' } else { '' }), (Get-ActiveSummary))
  for ($n = 1; $n -le $Attempts; $n++) {
    & $say 'busy' 'Looking for an external display...' "Attempt $n of $Attempts"
    $ext = @(Get-ExternalTargets)
    Write-Log ('attempt {0}: external targets available = [{1}]; active = {2}' -f $n, (($ext | ForEach-Object { '{0}#{1} tech{2}' -f $_.Name, $_.TgtId, $_.Tech }) -join ', '), (Get-ActiveSummary))

    if ($ext.Count -eq 0) {
      $rc = [Ccd]::ForceEnumeration()
      Write-Log "  none found; re-enumerate modes rc=$rc"
      if ($n -ge 3) {
        & $say 'busy' 'No display found - resetting graphics...' "Attempt $n of $Attempts (the screen may flash)"
        Write-Log '  sending Win+Ctrl+Shift+B'
        [Ccd]::ResetGraphicsHotkey()
        Start-Sleep 4
      }
      Start-Sleep 2
      continue
    }

    $label = if ($ext[0].Name) { $ext[0].Name } else { 'external display' }
    if ($DryRun) {
      $rc = [Ccd]::SetExternalTopology($true); $rc2 = [Ccd]::ApplyExplicit($ext[0], $true)
      Write-Log "  dry run: validate external topology rc=$rc, validate explicit path rc=$rc2"
      & $say 'ok' "Dry run: $label found" "Windows validated: topology rc=$rc, explicit rc=$rc2"
      return $true
    }

    if (Test-ExternalOnly) { Write-Log '  already external only'; & $say 'ok' "Already on $label" 'Nothing to change.'; return $true }

    & $say 'busy' "Switching to $label..." "Attempt $n of $Attempts"
    # Explicit single-path request first: the Win+P-style "external only" call returned rc=31 (general failure)
    # on this machine even when the display was fine.
    $rc = [Ccd]::ApplyExplicit($ext[0], $false)
    Start-Sleep 2
    $ok = Test-ExternalOnly
    Write-Log "  explicit path rc=$rc; external-only active = $ok; active = $(Get-ActiveSummary)"
    if (-not $ok) {
      $rc = [Ccd]::SetExternalTopology($false)
      Start-Sleep 2
      $ok = Test-ExternalOnly
      Write-Log "  SetDisplayConfig(external) rc=$rc; external-only active = $ok; active = $(Get-ActiveSummary)"
    }
    if ($ok) { & $say 'ok' "Switched to $label" 'The laptop screen is off.'; return $true }
    Start-Sleep 2
  }
  Write-Log 'gave up: no external display could be activated'
  & $say 'fail' 'Could not switch to an external display' 'Check the cable and that the monitor is on and set to this input.'
  return $false
}

if ($NoUi) {
  $ok = Invoke-Switch { param($state, $title, $detail) "[$state] $title - $detail" | Write-Host }
  exit $(if ($ok) { 0 } else { 1 })
}

Add-Type -AssemblyName System.Windows.Forms, System.Drawing
[System.Windows.Forms.Application]::EnableVisualStyles()
$mutex = New-Object System.Threading.Mutex($false, 'Local\SwitchToExternalDisplayPopup')
if (-not $mutex.WaitOne(0)) { exit }   # already running

$colors = @{
  busy = [System.Drawing.Color]::FromArgb(70, 130, 200)
  ok   = [System.Drawing.Color]::FromArgb(46, 160, 80)
  fail = [System.Drawing.Color]::FromArgb(200, 70, 70)
}
$script:dot = $colors.busy
$script:closing = $false

$form = New-Object System.Windows.Forms.Form
$form.Text = 'Switch Display'
$form.ClientSize = New-Object System.Drawing.Size(380, 130)
$form.FormBorderStyle = 'FixedDialog'
$form.MaximizeBox = $false
$form.MinimizeBox = $false
$form.TopMost = $true
$form.KeyPreview = $true
$form.StartPosition = 'Manual'
$wa = [System.Windows.Forms.Screen]::PrimaryScreen.WorkingArea
$form.Location = New-Object System.Drawing.Point(($wa.Right - $form.Width - 16), ($wa.Bottom - $form.Height - 16))
try { $form.Icon = New-Object System.Drawing.Icon((Join-Path $PSScriptRoot 'display.ico')) } catch [System.ArgumentException], [System.IO.FileNotFoundException] { }

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
$title.Size = New-Object System.Drawing.Size(300, 30)
$title.Font = New-Object System.Drawing.Font('Segoe UI Semibold', 13)

$hint = New-Object System.Windows.Forms.Label
$hint.Location = New-Object System.Drawing.Point(66, 52)
$hint.Size = New-Object System.Drawing.Size(300, 40)
$hint.Font = New-Object System.Drawing.Font('Segoe UI', 9)
$hint.ForeColor = [System.Drawing.Color]::DimGray

$again = New-Object System.Windows.Forms.Button
$again.Text = 'Try again'
$again.Location = New-Object System.Drawing.Point(278, 94)
$again.Size = New-Object System.Drawing.Size(90, 28)
$again.Enabled = $false

$form.Controls.AddRange(@($dotPanel, $title, $hint, $again))
$form.Add_KeyDown({ param($s, $e) if ($e.KeyCode -eq 'Escape') { $form.Close() } })

$say = {
  param($state, $t, $d)
  $script:dot = $colors[$state]
  $title.Text = $t
  $hint.Text = $d
  $again.Enabled = ($state -ne 'busy')
  $dotPanel.Invalidate(); $form.Refresh()
  [System.Windows.Forms.Application]::DoEvents()
}

function Start-Run {
  $again.Enabled = $false
  $ok = Invoke-Switch $say
  if ($ok -and -not $script:closing) {
    $end = (Get-Date).AddSeconds(4)
    while ((Get-Date) -lt $end -and -not $script:closing) { Start-Sleep -Milliseconds 100; [System.Windows.Forms.Application]::DoEvents() }
    if (-not $script:closing) { $form.Close() }
  }
}
$again.Add_Click({ Start-Run })
$form.Add_FormClosing({ $script:closing = $true })
$form.Add_FormClosed({ $mutex.ReleaseMutex() })
$form.Add_Shown({ Start-Run })
[void]$form.ShowDialog()
