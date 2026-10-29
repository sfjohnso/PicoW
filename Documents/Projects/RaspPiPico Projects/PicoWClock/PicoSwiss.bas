' PicoSwiss - Swiss Federal Railways style clock on the Waveshare Pico-Eval-Board
' May 2026  Steve Johnson
' Inspired by the SBB Swiss Railway clock design by Hans Hilfiker, 1944
' Uses features of the Waveshare Pico-Eval-Board:
'    Display brightness via LDR on GP26
'    Tick sound via on-board speaker on GP14
' Requires LCD initialised in Rotated Portrait mode at BASIC prompt:
'    OPTION LCDPANEL ILI9488W, RPORTRAIT, GP8, GP15, GP9, GP13
' Requires WiFi credentials stored, or Date$ and Time$ set manually.

' ---------------------------- Set up the BASIC environment -------------------

Option EXPLICIT
Option Angle Degrees
Option BASE 0

' ---------------------------- Colors -----------------------------------------

Const Back    = RGB(BLACK)           ' Screen surround: black
Const FaceC   = RGB(BLACK)           ' Clock face: black

' Day mode color constants
Const DayRingC  = RGB(200, 200, 200) ' Outer bezel ring: light grey
Const DayTickC  = RGB(GRAY)          ' Tick marks
Const DayHandC  = RGB(CYAN)          ' Hour and minute hands
Const DaySecC   = RGB(RED)           ' Second hand and lollipop dot
Const DayTextC  = RGB(WHITE)         ' Text
Const DaySymbol = RGB(220, 220, 220) ' NTP, meridian, TZ symbols

' Night mode color constants: muted reds for dark adaptation
Const NightRingC  = RGB( 75, 0, 0)
Const NightTickC  = RGB(100, 0, 0)
Const NightHandC  = RGB(160, 0, 0)
Const NightSecC   = RGB(130, 0, 0)
Const NightTextC  = RGB(140, 0, 0)
Const NightSymbol = RGB( 80, 0, 0)

' Active color variables (switched between day and night modes)
Dim RingC  As Integer
Dim TickC  As Integer
Dim HandC  As Integer
Dim SecC   As Integer
Dim Textc  As Integer
Dim Symbol As Integer

' ---------------------------- DST offsets ------------------------------------

Const StdOff% = -8                   ' Pacific Standard Time (UTC-8)
Const DayOff% = -7                   ' Pacific Daylight Time (UTC-7)

' ---------------------------- Display geometry --------------------------------

Const HCtr   = MM.HRes / 2           ' Horizontal center (= 240)
Const VCtr   = MM.VRes / 2           ' Vertical center   (= 160)
Const cx     = HCtr                  ' Clock pivot x
Const cy     = 206                   ' Clock pivot y = TopH + G + Souter  = 50 + 16 + 140
Const Right  = MM.HRes - 1           ' Right edge  (= 319)
Const Bottom = MM.VRes - 1           ' Bottom edge (= 479)

' ---------------------------- Clock face geometry ----------------------------
'
'  Screen is 320 wide x 480 tall.  Layout top-to-bottom:
'
'  TopH   ( 50) -- time strip   y=0-49    (Font 6 scale 1 time)
'  G      ( 16) -- even gap
'  Souter (140) -- clock face   y=66-345  (280 px = 2*140 diameter)
'  G      ( 16) -- even gap
'  WxH    ( 40) -- condition    y=346-385 (Font 1 scale 2, CT at y=362)
'  G      ( 16) -- even gap
'  TempH  ( 66) -- temperature  y=386-451 (Font 6 scale 1, CT at y=402)
'  G      ( 16) -- even gap
'  BotH   ( 28) -- date         y=452-479 (Font 1 scale 1, CB at y=479)
'
'  cy (206) = TopH + G + Souter = 50 + 16 + 140
'
'  Souter (140) ---- grey ring outer edge
'  FaceR  (128) ---- white face / ring inner edge
'  TkOuter(127) ---- outer edge of all tick marks
'  TkHrIn ( 96) ---- inner edge of hour marks       (31 px long)
'  TkIntIn(113) ---- inner edge of minute marks     (14 px long)
'  HndMOut(108) ---- minute hand tip
'  HndHOut( 71) ---- hour hand tip
'  SecDotD( 84) ---- second-hand lollipop centre
'  SecOut (111) ---- second-hand arm tip

Const Souter  = 140
Const RingW   =  12
Const FaceR   = Souter - RingW       ' = 128

' ---- Tick marks -------------------------------------------------------------

Const TkOuter = 127
Const TkHrIn  =  96                  ' Hour marks:   23 px long
Const TkIntIn = 113                  ' Minute marks: 10 px long (drawn as 1px Line)
Const TkHrW   =   2                  ' Hour tick half-width in degrees

' ---- Hands ------------------------------------------------------------------

Const HndHOut =  71                  ' Hour hand length
Const HndHW   =   9                  ' Hour hand half-width in pixels
Const HndMOut = 108                  ' Minute hand length
Const HndMW   =   7                  ' Minute hand half-width in pixels
Const HndCtrR =   8                  ' Centre cap circle radius

' ---- Second hand ------------------------------------------------------------

Const SecOut  = 111                  ' Arm tip radius
Const SecDotD =  84                  ' Lollipop centre distance from pivot
Const SecDotR =  13                  ' Lollipop circle radius
Const SecTail =  28                  ' Tail length behind pivot

' ---- Text and weather strip heights -----------------------------------------

Const TopH    =  50                  ' Top strip height (px): fits Font 6 scale 1
Const G       =  16                  ' Even gap between every display element
Const WxH     =  40                  ' Condition zone  (px): y=346-385  (CT text at 362  = G+24)
Const TempH   =  66                  ' Temperature zone (px): y=386-451  (CT text at 402  = G+50)
Const BotH    =  28                  ' Bottom strip height (px): y=452-479 (CB text at 479 = G+12)

' ---------------------------- Declare variables ------------------------------

Dim TimeString$, DateString$, Meridian$, Connect$, TZ$ As STRING
Dim Hrs$, Min$, Sec$, OldHrs$, OldMin$, OldSec$        As STRING
Dim WxStr$                                              As STRING
Dim Toggle, Ambient, TempI                              As Integer
Dim GMTOff%                                             As Integer
Dim Quarter                                             As Integer  ' 0-3 sub-second counter
Dim SecAng!                                             As Float    ' Last-drawn second angle
Dim NightMode                                           As Integer  ' -1=init, 0=day, 1=night

' Polygon vertex buffers (global to avoid potential Local-array limitations)
Dim HandPX(3) As Integer
Dim HandPY(3) As Integer

' HTTP response buffer (LONGSTRING; 256 integers = 2 KB)
Dim buf%(255) As Integer

' ---------------------------- Initialization ---------------------------------

Toggle    = 0
Ambient   = 0
NightMode = -1
GMTOff%   = StdOff%
TZ$       = "PST"                    ' Safe default; Time_Connect will update
Connect$  = Chr$(&H98)               ' Default to stick figure until NTP succeeds
Quarter   = 0
SecAng!   = 0.0
WxStr$    = ""
TempI     = 0
Set_Day_Colors
Colour TickC, FaceC
Font 1, 2

SetPin GP14, DOUT
SetPin GP26, AIN

CLS Back
Update_LDR
Draw_Clock_Face
Time_Connect
Get_Weather

Hrs$ = Mid$(Time$,1,2) : Min$ = Mid$(Time$,4,2) : Sec$ = Mid$(Time$,7,2)
OldHrs$ = Hrs$ : OldMin$ = Min$ : OldSec$ = Sec$

Draw_Hands
Update_Text
Update_Weather
Update_Second

SYNC 250000                          ' Main loop runs 4 times per second

' ---------------------------- Main Loop --------------------------------------

Do
  SYNC

  Hrs$ = Mid$(Time$,1,2) : Min$ = Mid$(Time$,4,2) : Sec$ = Mid$(Time$,7,2)

  If Sec$ <> OldSec$ Then           ' Actions taken once per second
    Quarter = 0
    Update_LDR
    Update_Tock
    If OldMin$ <> Min$ Then
      If Min$ = "30" Then Get_Weather
      Update_Hands
      Update_Text
      Update_Weather
    EndIf
    If OldHrs$ <> Hrs$ Then Time_Connect
    OldHrs$ = Hrs$ : OldMin$ = Min$ : OldSec$ = Sec$
  EndIf

  If Quarter = 2 Then Update_Tick   ' Tick sound at 500ms mark

  Update_Second                     ' Advance second hand 1.5 degrees per quarter

  If Quarter < 3 Then Quarter = Quarter + 1

Loop

Display_Terminating_Error

End

' ---------------------------- Subs and Functions -----------------------------

Sub Set_Day_Colors
  RingC  = DayRingC  : TickC  = DayTickC  : HandC  = DayHandC
  SecC   = DaySecC   : Textc  = DayTextC  : Symbol = DaySymbol
  NightMode = 0
End Sub

Sub Set_Night_Colors
  RingC  = NightRingC  : TickC  = NightTickC  : HandC  = NightHandC
  SecC   = NightSecC   : Textc  = NightTextC  : Symbol = NightSymbol
  NightMode = 1
End Sub

Sub Switch_Night_Mode(night%)
  If night% Then Set_Night_Colors Else Set_Day_Colors
  Colour TickC, FaceC
  CLS Back
  Draw_Clock_Face
  Draw_Hands
  Update_Text
  Update_Weather
  Update_Second
End Sub

Sub Time_Connect
  Local Integer got_time
  GMTOff% = DST_Offset()
  If GMTOff% = DayOff% Then TZ$ = "PDT" Else TZ$ = "PST"
  On Error Skip
  WEB ntp GMTOff%, "time.nist.gov"     ' First call: gets the date right
  got_time = (MM.Errno = 0)
  If got_time Then
    GMTOff% = DST_Offset()             ' Recompute DST now that date is correct
    If GMTOff% = DayOff% Then TZ$ = "PDT" Else TZ$ = "PST"
    On Error Skip
    WEB ntp GMTOff%, "time.nist.gov"   ' Second call: gets the time right
    got_time = (MM.Errno = 0)
  EndIf
  If got_time Then
    Connect$ = Chr$(&HA8)
  Else
    Connect$ = Chr$(&H98)
    Print MM.Errno, MM.ErrMsg$
  EndIf
  Text 0, 0, Connect$, "LT", 4, 2, Symbol, Back
End Sub

Sub Get_Weather
  Local String tx$
  Local Integer idx, wcode
  Local Float tempF
  tx$ = "GET /v1/forecast?latitude=34.16962&longitude=-118.94857"
  tx$ = tx$ + "&current=temperature_2m,weather_code"
  tx$ = tx$ + "&temperature_unit=fahrenheit HTTP/1.0" + Chr$(13) + Chr$(10)
  tx$ = tx$ + "Host: api.open-meteo.com" + Chr$(13) + Chr$(10) + Chr$(13) + Chr$(10)
  On Error Skip
  WEB OPEN TCP CLIENT "api.open-meteo.com", 80
  If MM.Errno <> 0 Then Exit Sub
  On Error Skip
  WEB TCP CLIENT REQUEST tx$, buf%(), 5000
  WEB CLOSE TCP CLIENT
  If MM.Errno <> 0 Then Exit Sub
  idx = LInStr(buf%(), Chr$(34) + "current" + Chr$(34) + ":")
  If idx = 0 Then Exit Sub
  Local String tail$
  tail$ = LGetStr$(buf%(), idx, 200)
  idx = InStr(tail$, "temperature_2m" + Chr$(34) + ":")
  If idx = 0 Then Exit Sub
  idx = idx + Len("temperature_2m" + Chr$(34) + ":")
  tempF = Val(Mid$(tail$, idx, 8))
  idx = InStr(idx, tail$, "weather_code" + Chr$(34) + ":")
  TempI = Int(tempF + 0.5)
  If idx = 0 Then
    WxStr$ = ""
    Exit Sub
  EndIf
  idx = idx + Len("weather_code" + Chr$(34) + ":")
  wcode = Val(Mid$(tail$, idx, 5))
  WxStr$ = Wx_Code$(wcode)
End Sub

Sub Update_Weather
  Box 0, cy+Souter,      MM.HRes, WxH,   1, Back, Back
  Box 0, cy+Souter+WxH,  MM.HRes, TempH, 1, Back, Back
  If WxStr$ <> "" Then
    Text cx, cy+Souter+G,       WxStr$,                       "CT", 1, 2, Textc, Back
  EndIf
  If TempI > 0 Then
    Text cx, cy+Souter+WxH+G,   Str$(TempI) + Chr$(&H60),    "CT", 6, 1, Textc, Back
  EndIf
End Sub

Sub Display_Terminating_Error
  Local String T_String$
  T_String$ = "End with " + Str$(MM.Errno) + " " + MM.ErrMsg$
  CLS Back
  Text cx, cy, T_String$, "CM", 1, 2, Textc, Back
End Sub

Sub Update_Text
  ' Clears the entire top strip and bottom strip, then redraws ALL text in one pass.
  ' This avoids any ordering issue where a later Box clear erases earlier text.
  ' NTP symbol (Connect$) is redrawn here so it survives the full-strip clear.
  Local Integer H24, H12

  H12 = Val(Hrs$) Mod 12 : H24 = Val(Hrs$)
  If H12 = H24 Then Meridian$ = "AM" Else Meridian$ = "PM"
  If H12 = 0 Then H12 = 12

  ' --- Clear both text strips in one operation each ---
  Box 0, 0,          MM.HRes, TopH, 1, Back, Back   ' Top strip  y=0-49
  Box 0, Bottom-BotH+1, MM.HRes, BotH, 1, Back, Back  ' Bottom strip y=452-479

  ' --- Top strip: NTP+AM/PM (left), HH:MM (centre), temperature (right) ---
  Text 0,  0,  Connect$,  "LT", 4, 2, Symbol, Back   ' NTP / person icon
  Text 36, 0,  Meridian$, "LT", 1, 2, Symbol, Back   ' AM or PM
  Text 36, 26, TZ$,       "LT", 1, 1, Symbol, Back   ' PST or PDT

  TimeString$ = Str$(H12) + ":" + Mid$(Time$,4,2)
  Text cx, 0, TimeString$, "CT", 6, 1, Textc, Back   ' Font 6 scale 1

  ' --- Bottom strip: date centred ---
  DateString$ = Left$(Day$(NOW),3) + " " + Right$(Date$,4) + "-" + Mid$(Date$,4,2) + "-" + Left$(Date$,2)
  Text cx, Bottom, DateString$, "CB", 1, 1, Textc, Back
End Sub

Sub Update_LDR
  Local Float LDRVal
  Local Integer BLight
  LDRVal = Pin(GP26)
  BLight = Max(Int(Min(15 + 15*LDRVal, 200)), 17) ' floor = brightness at LDR 0.15 night threshold
  Ambient = 0
  Backlight BLight
  If NightMode = -1 Then
    If LDRVal < 0.15 Then Set_Night_Colors Else Set_Day_Colors
  ElseIf NightMode = 0 And LDRVal < 0.15 Then
    Switch_Night_Mode 1
  ElseIf NightMode = 1 And LDRVal > 0.20 Then
    Switch_Night_Mode 0
  EndIf
End Sub

Sub Draw_Clock_Face
  ' Hour marks: filled arc wedges.  Minute marks: single-pixel lines.
  Local Integer m, a1, a2, mx, my, ox, oy
  Circle cx, cy, Souter, 1, 1, RingC, RingC
  Circle cx, cy, FaceR,  1, 1, FaceC, FaceC
  For m = 0 To 354 Step 6
    If m Mod 30 = 0 Then
      a1 = m - TkHrW  : a2 = m + TkHrW
      Arc cx, cy, TkHrIn, TkOuter, a1, a2, TickC
    Else
      mx = cx + Int(TkIntIn * Sin(m)) : my = cy - Int(TkIntIn * Cos(m))
      ox = cx + Int(TkOuter * Sin(m)) : oy = cy - Int(TkOuter * Cos(m))
      Line mx, my, ox, oy, 1, TickC
    EndIf
  Next m
End Sub

Sub Redraw_All_Ticks
  Local Integer m, a1, a2, mx, my, ox, oy
  For m = 0 To 354 Step 6
    If m Mod 30 = 0 Then
      a1 = m - TkHrW  : a2 = m + TkHrW
      Arc cx, cy, TkHrIn, TkOuter, a1, a2, TickC
    Else
      mx = cx + Int(TkIntIn * Sin(m)) : my = cy - Int(TkIntIn * Cos(m))
      ox = cx + Int(TkOuter * Sin(m)) : oy = cy - Int(TkOuter * Cos(m))
      Line mx, my, ox, oy, 1, TickC
    EndIf
  Next m
End Sub

Sub Redraw_Tick(sec_pos%)
  ' Redraws the tick mark at second position sec_pos% (0-59).
  Local Integer deg, a1, a2, mx, my, ox, oy
  deg = sec_pos% * 6
  If deg Mod 30 = 0 Then
    a1 = deg - TkHrW  : a2 = deg + TkHrW
    Arc cx, cy, TkHrIn, TkOuter, a1, a2, TickC
  Else
    mx = cx + Int(TkIntIn * Sin(deg)) : my = cy - Int(TkIntIn * Cos(deg))
    ox = cx + Int(TkOuter * Sin(deg)) : oy = cy - Int(TkOuter * Cos(deg))
    Line mx, my, ox, oy, 1, TickC
  EndIf
End Sub

Sub Draw_Rect_Hand(Ang!, Hw%, Rout%, Col%)
  ' Rectangular hand via Polygon.
  ' Ang!: angle in degrees (0=12 o-clock, clockwise).
  ' Hw%:  half-width in pixels.  Rout%: tip radius.  Col%: fill colour.
  Local Float sa, ca
  sa = Sin(Ang!) : ca = Cos(Ang!)
  HandPX(0) = cx + Int( Hw% * ca)
  HandPY(0) = cy - Int(-Hw% * sa)
  HandPX(1) = cx + Int(Rout% * sa + Hw% * ca)
  HandPY(1) = cy - Int(Rout% * ca - Hw% * sa)
  HandPX(2) = cx + Int(Rout% * sa - Hw% * ca)
  HandPY(2) = cy - Int(Rout% * ca + Hw% * sa)
  HandPX(3) = cx + Int(-Hw% * ca)
  HandPY(3) = cy - Int( Hw% * sa)
  Polygon 4, HandPX(), HandPY(), Col%, Col%
End Sub

Sub Draw_Hands
  Local Float H, M
  H = 30*(Val(Hrs$) Mod 12) + Val(Min$)/2
  M = Val(Min$) * 6
  Draw_Rect_Hand H, HndHW, HndHOut, HandC
  Draw_Rect_Hand M, HndMW, HndMOut, HandC
  Circle cx, cy, HndCtrR, 1, 1, HandC, HandC
End Sub

Sub Update_Hands
  ' Erase old hand positions, restore all ticks, draw new hands.
  Local Float OldH, OldM
  OldH = 30*(Val(OldHrs$) Mod 12) + Val(OldMin$)/2
  OldM = Val(OldMin$) * 6
  Draw_Rect_Hand OldH, HndHW, HndHOut, FaceC
  Draw_Rect_Hand OldM, HndMW, HndMOut, FaceC
  Redraw_All_Ticks
  Draw_Hands
End Sub

Sub Update_Second
  ' Advance second hand 1.5 degrees (one quarter step).
  ' Uses SecAng! to track sub-second fractional position.
  Local Float OldAng, NewAng
  Local Integer Otx, Oty, Ohx, Ohy, Odx, Ody
  Local Integer Ntx, Nty, Nhx, Nhy, Ndx, Ndy

  OldAng = SecAng!
  NewAng = Val(Sec$) * 6.0 + Quarter * 1.5

  Otx = cx - Int(SecTail * Sin(OldAng)) : Oty = cy + Int(SecTail * Cos(OldAng))
  Ohx = cx + Int(SecOut  * Sin(OldAng)) : Ohy = cy - Int(SecOut  * Cos(OldAng))
  Odx = cx + Int(SecDotD * Sin(OldAng)) : Ody = cy - Int(SecDotD * Cos(OldAng))

  Ntx = cx - Int(SecTail * Sin(NewAng)) : Nty = cy + Int(SecTail * Cos(NewAng))
  Nhx = cx + Int(SecOut  * Sin(NewAng)) : Nhy = cy - Int(SecOut  * Cos(NewAng))
  Ndx = cx + Int(SecDotD * Sin(NewAng)) : Ndy = cy - Int(SecDotD * Cos(NewAng))

  ' --- Erase old second hand (line slightly wider than drawn) ---
  Line Otx, Oty, Ohx, Ohy, 4, FaceC
  Circle Odx, Ody, SecDotR+2, 1, 1, FaceC, FaceC
  Redraw_Tick Int(OldAng / 6.0)
  Draw_Hands

  ' --- Draw new second hand ---
  Line Ntx, Nty, Nhx, Nhy, 3, SecC
  Circle Ndx, Ndy, SecDotR, 1, 1, SecC, SecC
  Circle cx, cy, HndCtrR, 1, 1, HandC, HandC

  SecAng! = NewAng
End Sub

Sub Update_Tick
  If Ambient = 1 Then Pin(GP14) = 1 - Pin(GP14)
End Sub

Sub Update_Tock
  If Ambient = 1 Then
    Pin(GP14) = 1 - Pin(GP14)
    Pause 10
    Pin(GP14) = 1 - Pin(GP14)
  EndIf
End Sub

Function Wx_Code$(c%) As String
  Select Case c%
    Case 0        : Wx_Code$ = "Clear"
    Case 1        : Wx_Code$ = "Mostly Clear"
    Case 2        : Wx_Code$ = "Partly Cloudy"
    Case 3        : Wx_Code$ = "Overcast"
    Case 45, 48   : Wx_Code$ = "Foggy"
    Case 51, 53   : Wx_Code$ = "Drizzle"
    Case 55       : Wx_Code$ = "Heavy Drizzle"
    Case 61, 63   : Wx_Code$ = "Rain"
    Case 65       : Wx_Code$ = "Heavy Rain"
    Case 71, 73   : Wx_Code$ = "Snow"
    Case 75       : Wx_Code$ = "Heavy Snow"
    Case 77       : Wx_Code$ = "Snow Grains"
    Case 80, 81   : Wx_Code$ = "Showers"
    Case 82       : Wx_Code$ = "Heavy Showers"
    Case 85, 86   : Wx_Code$ = "Snow Showers"
    Case 95       : Wx_Code$ = "Thunderstorm"
    Case 96, 99   : Wx_Code$ = "Heavy T-Storm"
    Case Else     : Wx_Code$ = "Unknown"
  End Select
End Function

' ---------------------------- DST Functions ----------------------------------

Function DST_Offset() As Integer
  Local Integer yr, mo, dy, hr, mar_day, nov_day, in_dst
  yr = Val(Right$(Date$, 4))
  mo = Val(Mid$(Date$, 4, 2))
  dy = Val(Left$(Date$, 2))
  hr = Val(Left$(Time$, 2))
  mar_day = DST_Start(yr)
  nov_day = DST_End(yr)
  in_dst = 0
  If mo > 3 And mo < 11 Then
    in_dst = 1
  ElseIf mo = 3 And dy > mar_day Then
    in_dst = 1
  ElseIf mo = 3 And dy = mar_day And hr >= 2 Then
    in_dst = 1
  ElseIf mo = 11 And dy < nov_day Then
    in_dst = 1
  ElseIf mo = 11 And dy = nov_day And hr < 2 Then
    in_dst = 1
  EndIf
  If in_dst Then DST_Offset = DayOff% Else DST_Offset = StdOff%
End Function

Function DST_Start(yr%) As Integer
  Local Integer d, first_sun
  d = DOW(yr%, 3, 1)
  first_sun = 1 + (7 - d) Mod 7
  DST_Start = first_sun + 7
End Function

Function DST_End(yr%) As Integer
  Local Integer d
  d = DOW(yr%, 11, 1)
  DST_End = 1 + (7 - d) Mod 7
End Function

Function DOW(yr%, mo%, dy%) As Integer
  ' Tomohiko Sakamoto algorithm. Returns 0=Sunday, 1=Monday ... 6=Saturday.
  Local Integer t(11), y
  t(0)=0 : t(1)=3 : t(2)=2 : t(3)=5 : t(4)=0 : t(5)=3
  t(6)=5 : t(7)=1 : t(8)=4 : t(9)=6 : t(10)=2 : t(11)=4
  y = yr%
  If mo% < 3 Then y = y - 1
  DOW = (y + y\4 - y\100 + y\400 + t(mo%-1) + dy%) Mod 7
End Function
