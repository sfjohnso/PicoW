' Analog LCD Clock on the WaveShare Pico-Eval-Board ("P-E-B")
' May 12, 2023  S.F. Johnson  Newbury Park, California
' Uses features of the Waveshare P-E-B including
'    Display Brightness based on LDR Light Dependent Resistor on GP26
'    Tick sound using On-board Speaker. . . . . . . . . . . . on GP14
'    Color-changing LED using On-board NeoPixel . . . . . . . on GP4
' Depends on LCD in "Rotated" Portrait mode:
'    OPTION LCDPANEL ILI9488W, RPORTRAIT,GP8,GP15,GP9,GP13
' Depends on being initialized with the local WiFi, or Date$ and Time$
' being set manually (which will not survive a reset cycle)

Const Bright% = &H0F        ' Control the brightness of the NeoPixel

Const Back   = RGB(BLACK)   ' Color of Background
Const Fore   = RGB(WHITE)   ' Color of Foreground / Tic Marks
Const Textc  = RGB(LightGray) ' Color of text on LCD display
Const Symbol = RGB(Gray)    ' Color of connection and meridian symbols
Const HCtr   = MM.HRes/2    ' Horizontal center of LCD display & clock
Const VCtr   = MM.VRes/2    ' Vertical center of LCD display and clock
Const Top    = 0
Const Left   = 0
Const Right  = MM.HRes-1
Const Bottom = MM.VRes-1
Const x      = HCtr         ' Shorthand for horizontal center
Const y      = VCtr         ' Shorthand for vertical center

Const Smajor = 3            ' Width of Seconds divisible by 5
Const Sminor = 3            ' Width of Seconds not divisible by 5
Const Smajc  = Fore         ' Color of Major tic marks
Const Sminc  = Back         ' Color of Minor tic marks
Const Souter = Int(Min(x-1, y-1)) ' Outer Dia. of Clock Face
Const Sc     = RGB(RED)     ' Color of Second Hand
Const Sinner = Souter-28     ' Inner Dia. of Tics divisible by 5
Const Smid   = Souter-14     ' Inner Dia. of Tics not divisible by 5

Const Mw     = 3            ' Width of Minute Hand
Const Mc     = RGB(CYAN)    ' Color of Minute Hand
Const Minner = 0            ' Inner Dia. of Minute Hand
Const Mouter = Sinner-5     ' Outer Dia. of Minute Hand

Const Hw     = 5            ' Width of Hour Hand
Const Hc     = RGB(CYAN)    ' Color of Hour Hand
Const Hinner = 0            ' Inner DIa. of Hour Hand
Const Houter = Mouter-40    ' Outer Dia. of Hour Hand

Const NTPSource = "192.168.178.1"

Option BASE 0

Colour Back, Fore           ' Set default colors
Font 1, 3                   ' Set default font

Dim C% As INTEGER           ' Create value for NeoPixel

Dim TimeString$, DateString$, Meridian$, Connect$ As STRING

SetPin GP4, DOUT            ' NeoPixel connected to GPIO 4 - bitbang it
SetPin GP14, DOUT           ' Beeper / Buzzer connected to GP14. PWM it
SetPin GP26, AIN            ' Light-Dependent Resistor LDR on GP26

CLS Back                    ' Clear the LCD screen
UpdateLDR
DrawClockFace
TimeConnect

Hrs$ = Mid$(Time$,1,2) : Min$ = Mid$(Time$,4,2) : Sec$ = Mid$(Time$,7,2)
OldHrs$  = Hrs$ : OldMin$  = Min$ : OldSec$  = Sec$

UpdateHands : UpdateText

SYNC 1000000


Do
  SYNC
  Hrs$ = Mid$(Time$,1,2) : Min$ = Mid$(Time$,4,2) : Sec$ = Mid$(Time$,7,2)
  UpdateClick : UpdateSecond : UpdateLDR
  If OldMin$ <> Min$ Then UpdateText : UpdateHands
  If OldHour$ <> Hrs$ Then TimeConnect
  OldHour$ = Hrs$ : OldMin$ = Min$ : OldSec$ = Sec$
Loop

CLS
Option HEARTBEAT ON

Sub TimeConnect
  On Error Skip
  WEB ntp -7, "time.nist.gov"
  If MM.Errno = 0 Then
    Connect$ = Chr$(&HA8)
  Else
    Connect$ = Chr$(&H98)
    Print MM.Errno, MM.ErrMsg$
  EndIf
  Text 0,0, Connect$, "LT", 4, 3, Symbol, Back ' Connection Symbol at upper left
End Sub

Sub UpdateText
  Local String DateString$
  H12 = Val(Hrs$) Mod 12 : H24 = Val(Hrs$)  ' Prep for AM/PM test
  If H12=H24 Then Meridian$ = "AM" Else Meridian$ = "PM"
  TimeString$  = " "+Str$(H12) +":"+Mid$(Time$,4,2)+" " ' + Meridian$
  Text x, Top, TimeString$, "CT", 6,1, Textc, Back
  Text Right, Top, Meridian$, "RT", 4, 3, Symbol, Back ' AM or PM at upper right
  DateString$ = " "+Day$(NOW)+" "+Right$(Date$,4)+"-"+Mid$(Date$,4,2)+"-"+Left$(Date$,2) + " "
  Text x, Bottom, DateString$, "CB", 3, 1, Textc, Back
End Sub

Sub UpdateLDR
  Local Float LDRVal
  Local Integer BLight
  Local String LDRStr$, BRTStr$
  LDRVal = Pin(GP26)
  BLight = Int(Min(50+15*LDRVal, 100))
  Backlight BLight
End Sub

Sub UpdateHands ' Adjust position for hours, minutes
  Circle x, y, Mouter+1, 1, 1, Back, Back ' Clear the inner face

  H = 30*(Val(Hrs$) Mod 12) + Val(Min$)/2 ' Hour hand interpolates
  a1 = H - Hw : a2 = H + Hw
  Arc x, y, Hinner, Houter, a1, a2, Hc

  M = Int(MinsToDeg(Val(Min$)))           ' Minute hand doesn't interpolate
  a1 = M - Mw : a2 = M + Mw
  Arc x, y, Minner, Mouter, a1, a2, Mc

  Circle x, y, 6, ,,HC, HC                 ' Dot at the center
End Sub

Sub UpdateClick
'  C%=RGB(Int(Bright%*Rnd),Int(Bright%*Rnd),Int(Bright%*Rnd))
'  Bitbang ws2812 o, GP4, 1, C% ' Set the NeoPixel to a random color
  Pin(GP14) = 1 - Pin(GP14)    ' Toggle the buzzer to make a "Tick" sound
  Pause 15
  Pin(GP14) = 1 - Pin(GP14)    ' Toggle the buzzer to make a "Tick" sound
End Sub

Sub UpdateSecond
  ' Erase old second position
  S = Int(SecsToDeg(Val(OldSec$)))
  If S Mod 30 = 0 Then
    a1 = S-Smajor : a2 = S+Smajor
    If S = 0 Then
      Arc x, y, Sinner, Souter, a1, a2, RGB(Yellow)
    Else
      Arc x, y, Sinner, Souter, a1, a2, Smajc
    EndIf
  Else
    a1 = S-Sminor : a2 = S+Sminor
    Arc x, y, Smid, Souter, a1, a2, Sminc
  EndIf

  ' Draw new second position
  S = Int(SecsToDeg(Val(Sec$)))
  If S Mod 30 = 0 Then
    a1 = S - Smajor : a2 = S + Smajor
    If S = 0 Then
      Arc x, y, Sinner, Souter, a1, a2, RGB(Yellow)
    Else
      Arc x, y, Sinner, Souter, a1, a2, Smajc
    EndIf
    Arc x, y, Sinner, Souter, a1, a2, Sc
  Else
    a1 = S - Sminor : a2 = S + Sminor
    Arc x, y, Smid, Souter, a1, a2, Sc
  EndIf
End Sub

Function HrsToDeg(hrs%) As Float
  HrsToDeg = 30*(hrs% Mod 12)
End Function

Function MinsToDeg(mins%) As Float
  MinsToDeg = 6*mins%
End Function

Function SecsToDeg(secs%) As Float
  SecsToDeg = 6*secs%
End Function

Sub DrawClockFace
  For m = 0 To 354 Step 6   ' 360 degrees / 60 minutes >= 6
    If m Mod 30 = 0 Then    ' Major tic mark
      a1 = m - Smajor : a2 = m + Smajor
      If m = 0 Then
        Arc x, y, Sinner, Souter, a1, a2, RGB(Yellow)
      Else
        Arc x, y, Sinner, Souter, a1, a2, Smajc
      EndIf
    Else                    ' Minor tic mark
      a1 = m - Sminor : a2 = m + Sminor
      Arc x, y, Smid, Souter, a1, a2, Sminc
    EndIf
  Next m
End Sub
