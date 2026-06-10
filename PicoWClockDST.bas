' PicoWClock - an analog LCD Clock on the WaveShare Pico-Eval-Board ("P-E-B")
' August 2023  Steve Johnson
' Uses features of the Waveshare P-E-B including
'    Display Brightness based on LDR Light Dependent Resistor on GP26
'    Tick sound using On-board Speaker. . . . . . . . . . . . on GP14
' Depends on LCD in "Rotated" Portrait mode:
'    OPTION LCDPANEL ILI9488W, RPORTRAIT,GP8,GP15,GP9,GP13
' Depends on being initialized with the local WiFi, or Date$ and Time$
'    being set manually (which will not survive a reset cycle)

' ---------------------------- Set up the BASIC environment -------------------

Option EXPLICIT
Option Angle Degrees
Option BASE 0

' ---------------------------- Declare constants ------------------------------

Const StdOff%  = -8             ' Pacific Standard Time (UTC-8)
Const DayOff%  = -7             ' Pacific Daylight Time (UTC-7)

Const Back     = RGB(BLACK)     ' Color of Background
Const Fore     = RGB(PINK)      ' Color of Foreground / Tic Marks
Const Textc    = RGB(BROWN)     ' Color of text on LCD display
Const Twelvec  = RGB(RED)       ' Color of Twelve o'clock marker
Const Symbol   = RGB(BROWN)     ' Color of connection and meridian symbols

' ---- Weather settings (Open-Meteo over plain HTTP; no TLS, no API key) ------
Const WXHOST$  = "api.open-meteo.com"   ' weather service, plain HTTP on port 80
Const WXLAT$   = "34.16962"             ' latitude  (Newbury Park, CA)
Const WXLON$   = "-118.94857"           ' longitude

Const WxDim    = RGB(GRAY)      ' shown when a weather fetch fails

Const HCtr     = MM.HRes/2      ' Horizontal center of LCD display & clock
Const VCtr     = MM.VRes/2      ' Vertical center of LCD display and clock

Const Top      = 0              ' Origin is at top-left corner of screen
Const Left     = 0
Const Right    = MM.HRes-1      ' Coordinate of right-hand edge
Const Bottom   = MM.VRes-1      ' Coordinate of bottom edge

Const x        = HCtr           ' Shorthand for horizontal center
Const y        = VCtr           ' Shorthand for vertical center

Const Smajor   = 3              ' Width of Seconds divisible by 5
Const Sminor   = 3              ' Width of Seconds not divisible by 5
Const Smajc    = Fore           ' Color of Major tic marks
Const Sminc    = Back           ' Color of Minor tic marks
Const Souter   = Int(Min(x-1, y-1)) ' Outer Dia. of Clock Face
Const Sc       = RGB(RED)       ' Color of Second Hand
Const Sinner   = Souter-28      ' Inner Dia. of Tics divisible by 5
Const Smid     = Souter-14      ' Inner Dia. of Tics not divisible by 5

Const Mw       = 3              ' Width of Minute Hand
Const Mc       = RGB(RED)       ' Color of Minute Hand
Const Minner   = 0              ' Inner Dia. of Minute Hand
Const Mouter   = Sinner-5       ' Outer Dia. of Minute Hand

Const Hw       = 5              ' Width of Hour Hand
Const Hc       = RGB(RED)       ' Color of Hour Hand
Const Hinner   = 0              ' Inner DIa. of Hour Hand
Const Houter   = Mouter-40      ' Outer Dia. of Hour Hand

' ---------------------------- Declare variables ------------------------------

Dim TimeString$, DateString$, Meridian$, Connect$ As STRING
Dim Hrs$, Min$, Sec$, OldHrs$, OldMin$, OldSec$   As STRING
Dim Toggle, Ambient As Integer
Dim GMTOff%   As Integer        ' Active UTC offset, set by DST_Offset()

Dim buf%(1023)                  ' LONGSTRING receive buffer (~8 KB) for weather
Dim CRLF$ As String             ' set in initialization
Dim WxTempF As Float            ' current temperature, deg F (from Open-Meteo)
Dim WxCode  As Integer          ' WMO weather code (from Open-Meteo)
Dim WxLine$ As String           ' formatted "80 deg F Clear" line for the display

' ---------------------------- Initialization ---------------------------------

Toggle = 0                      ' Alternate 0 and 1 for tick/tock
Ambient = 0                     ' 1 -> Do the tick/tock sound 0 -> Don't tick/tock
GMTOff% = StdOff%               ' Safe default; Time_Connect will compute actual DST
CRLF$   = Chr$(13) + Chr$(10)   ' line ending for the HTTP request
Colour Back, Fore               ' Set default colors
Font 1, 3                       ' Set default font

SetPin GP14, DOUT               ' Beeper / Buzzer connected to GP14. PWM it
SetPin GP26, AIN                ' Light-Dependent Resistor LDR on GP26

CLS Back                        ' Clear the LCD screen
Update_LDR                      ' Adjust screen brightness for ambient light
Draw_Clock_Face
Time_Connect

Hrs$ = Mid$(Time$,1,2) : Min$ = Mid$(Time$,4,2) : Sec$ = Mid$(Time$,7,2)
OldHrs$  = Hrs$ : OldMin$  = Min$ : OldSec$  = Sec$

Update_Hands : Update_Text
Update_Weather                 ' Initial weather fetch at startup

SYNC 500000                    ' Run main loop twice a second, toggling

' ---------------------------- Main Loop --------------------------------------

Do
  SYNC                          ' Run main loop twice a second

  If Toggle = 0 Then    ' Only do this part every second
    Update_LDR
    Hrs$ = Mid$(Time$,1,2) : Min$ = Mid$(Time$,4,2) : Sec$ = Mid$(Time$,7,2)
    Update_Tock
    Update_Second

    If OldMin$ <> Min$ Then
      Update_Text : Update_Hands
      If Min$ = "30" Then Update_Weather  ' Refresh weather on the half hour
    EndIf
    If OldHrs$ <> Hrs$ Then Time_Connect  ' Reconnect to NTP once an hour

    OldHrs$ = Hrs$ : OldMin$ = Min$ : OldSec$ = Sec$
  Else
    Update_Tick
  EndIf

  Toggle = 1-Toggle
Loop

Display_Terminating_Error

End

' ---------------------------- Subs and Functions -----------------------------

Sub Time_Connect
  GMTOff% = DST_Offset()       ' Recompute DST before each NTP sync
  On Error Skip
  WEB ntp GMTOff%, "time.nist.gov"
  If MM.Errno = 0 Then
    Connect$ = Chr$(&HA8) ' Show a "computer" symbol indicating NTP time
  Else
    Connect$ = Chr$(&H98) ' Show a stick figure indicating manually-entered time
    Print MM.Errno, MM.ErrMsg$
  EndIf
  Text 0,0, Connect$, "LT", 4, 3, Symbol, Back ' Connection Symbol at upper left
End Sub

' ---------------------------- Weather Subs / Functions -----------------------
' Pull current temperature + condition from Open-Meteo over plain HTTP and show
' it in the strip just above the date.  Open-Meteo needs no TLS and no API key,
' and returns JSON; we parse the numbers straight out of the LONGSTRING receive
' buffer (the JSON body is > 255 chars, MMBasic's string limit).

Sub Update_Weather
  Local Integer tcol
  Local String tStr$

  If Get_Weather() = 1 Then
    tStr$ = Str$(WxTempF, 0, 0)               ' temperature rounded to whole degrees
    WxLine$ = tStr$ + Chr$(&H60) + "F " + Wx_Code$(WxCode)  ' e.g. "80" deg "F Clear"
    If Len(WxLine$) > 19 Then WxLine$ = Left$(WxLine$, 19)
    tcol = Textc                              ' same colour as the day / date
  Else
    WxLine$ = "wx --"                         ' fetch failed
    tcol = WxDim
  EndIf

  Box Left, 410, MM.HRes, 30, 1, Back, Back   ' erase the weather strip
  Text x, 412, WxLine$, "CT", 3, 1, tcol, Back
End Sub

Function Get_Weather()
  Local req$, path$, n, p, q
  Local String DQ$

  Get_Weather = 0
  WxTempF = 0 : WxCode = -1
  DQ$ = Chr$(34)                               ' a literal double-quote character

  ' Open-Meteo: current temperature (deg F) and WMO weather code, HTTP/1.0
  path$ =        "/v1/forecast?latitude=" + WXLAT$ + "&longitude=" + WXLON$
  path$ = path$ + "&current=temperature_2m,weather_code&temperature_unit=fahrenheit"

  req$ =        "GET " + path$ + " HTTP/1.0" + CRLF$
  req$ = req$ + "Host: " + WXHOST$ + CRLF$
  req$ = req$ + "Connection: close" + CRLF$ + CRLF$

  On Error Skip
  WEB OPEN TCP CLIENT WXHOST$, 80
  If MM.Errno <> 0 Then Exit Function

  On Error Skip
  WEB TCP CLIENT REQUEST req$, buf%(), 8000

  On Error Skip
  WEB CLOSE TCP CLIENT

  n = LLen(buf%())
  If n = 0 Then Exit Function

  ' Parse straight from the receive buffer (body exceeds the 255-char string
  ' limit).  The live values live in the "current" block; the earlier
  ' "current_units" block holds only unit strings, so anchor past "current":.
  p = LInStr(buf%(), DQ$ + "current" + DQ$ + ":")
  If p = 0 Then Exit Function

  q = LInStr(buf%(), "temperature_2m" + DQ$ + ":", p)
  If q = 0 Then Exit Function
  q = q + Len("temperature_2m" + DQ$ + ":")
  WxTempF = Val(LGetStr$(buf%(), q, 12))

  q = LInStr(buf%(), "weather_code" + DQ$ + ":", p)
  If q = 0 Then Exit Function
  q = q + Len("weather_code" + DQ$ + ":")
  WxCode = Val(LGetStr$(buf%(), q, 6))

  Get_Weather = 1
End Function

Function Wx_Code$(c As Integer) As String
  ' Map an Open-Meteo WMO weather code to a short condition word.
  Select Case c
    Case 0          : Wx_Code$ = "Clear"
    Case 1          : Wx_Code$ = "Mostly Clear"
    Case 2          : Wx_Code$ = "Partly Cloudy"
    Case 3          : Wx_Code$ = "Overcast"
    Case 45, 48     : Wx_Code$ = "Fog"
    Case 51, 53, 55 : Wx_Code$ = "Drizzle"
    Case 56, 57     : Wx_Code$ = "Frz Drizzle"
    Case 61, 63, 65 : Wx_Code$ = "Rain"
    Case 66, 67     : Wx_Code$ = "Frz Rain"
    Case 71, 73, 75 : Wx_Code$ = "Snow"
    Case 77         : Wx_Code$ = "Snow Grains"
    Case 80, 81, 82 : Wx_Code$ = "Showers"
    Case 85, 86     : Wx_Code$ = "Snow Showers"
    Case 95, 96, 99 : Wx_Code$ = "Thunderstorm"
    Case Else       : Wx_Code$ = "Code " + Str$(c)
  End Select
End Function

Sub Display_Terminating_Error
  Local String T_String$
  T_String$ = "End with " + Str$(MM.Errno) + " " + MM.ErrMsg$
  CLS Back
  Text x, y, T_String$, "CM", 1, 1, Textc, Back
End Sub

Sub Update_Text
  Local String DateString$, TZ$
  Local Integer H24, H12

  H12 = Val(Hrs$) Mod 12 : H24 = Val(Hrs$)  ' Prep for AM/PM test
  If H12=H24 Then Meridian$ = "AM" Else Meridian$ = "PM"
  If H12=0 Then H12=12

  TimeString$  = " "+Str$(H12) +":"+Mid$(Time$,4,2)+" " ' + Meridian$
  Text x, Top, TimeString$, "CT", 6,1, Textc, Back
  Text Right, Top, Meridian$, "RT", 3, 1, Symbol, Back ' AM/PM, top-aligned with time
  If GMTOff% = DayOff% Then TZ$ = "PDT" Else TZ$ = "PST"
  Text Right, 50, TZ$, "RB", 3, 1, Symbol, Back        ' DST label, bottom-aligned with time (font 6 = 50px tall)

  DateString$ = " "+Day$(NOW)+" "+Right$(Date$,4)+"-"+Mid$(Date$,4,2)+"-"+Left$(Date$,2) + " "
  Text x, Bottom, DateString$, "CB", 3, 1, Textc, Back
End Sub

Sub Update_LDR
  Local Float LDRVal
  Local Integer BLight

  BLight = Int(Min(15+15*Pin(GP26), 200))
'  If BLight < 10 Then Ambient = 0 Else Ambient = 1
  Ambient = 0
  Backlight BLight
End Sub

Sub Update_Hands ' Adjust position for hours, minutes
  Local FLOAT H, M
  Local INTEGER a1, a2

  Circle x, y, Mouter+1, 1, 1, Back, Back   ' Clear the inner face

  H = 30*(Val(Hrs$) Mod 12) + Val(Min$)/2   ' Hour hand interpolates
  a1 = H - Hw : a2 = H + Hw                 ' Arc angles in degrees
  Arc x, y, Hinner, Houter, a1, a2, Hc      ' Draws from Hinner outward to Houter

  M = Int(Mins_To_Deg(Val(Min$)))           ' Minute hand doesn't interpolate
  a1 = M - Mw : a2 = M + Mw                 ' Arc angles in degrees
  Arc x, y, Minner, Mouter, a1, a2, Mc      ' Draws from Minner outward to Mouter

  Circle x, y, 6, ,,HC, HC                  ' Dot at the center
End Sub

Sub Update_Tick
  If Ambient = 1 Then Pin(GP14) = 1 - Pin(GP14) ' Toggle the buzzer to make a "Tick" sound
End Sub

Sub Update_Tock
  If Ambient = 1 Then
    Pin(GP14) = 1 - Pin(GP14) ' Toggle the buzzer to make a "Tock" sound
    Pause 10                  ' Wait 15 milliseconds
    Pin(GP14) = 1 - Pin(GP14) ' Toggle the buzzer to make a "Tock" sound
  EndIf
End Sub

Sub Update_Second
  Local INTEGER a1, a2, S

  ' Erase old second position
  S = Int(Secs_To_Deg(Val(OldSec$)))
  If S Mod 30 = 0 Then
    a1 = S-Smajor : a2 = S+Smajor
    If S = 0 Then
      Arc x, y, Sinner, Souter, a1, a2, Twelvec
    Else
      Arc x, y, Sinner, Souter, a1, a2, Smajc
    EndIf
  Else
    a1 = S-Sminor : a2 = S+Sminor
    Arc x, y, Smid, Souter, a1, a2, Sminc
  EndIf

  ' Draw new second position
  S = Int(Secs_To_Deg(Val(Sec$)))
  If S Mod 30 = 0 Then
    a1 = S - Smajor : a2 = S + Smajor
    If S = 0 Then
      Arc x, y, Sinner, Souter, a1, a2, Twelvec
    Else
      Arc x, y, Sinner, Souter, a1, a2, Smajc
    EndIf
    Arc x, y, Sinner, Souter, a1, a2, Sc
  Else
    a1 = S - Sminor : a2 = S + Sminor
    Arc x, y, Smid, Souter, a1, a2, Sc
  EndIf
End Sub

Function Mins_To_Deg(mins%) As Float
  Mins_To_Deg = 6*mins%
End Function

Function Secs_To_Deg(secs%) As Float
  Secs_To_Deg = 6*secs%
End Function

Sub Draw_Clock_Face
  Local INTEGER a1, a2, m

  For m = 0 To 354 Step 6   ' 360 degrees / 60 minutes >= 6
    If m Mod 30 = 0 Then    ' Major tic mark
      a1 = m - Smajor : a2 = m + Smajor
      If m = 0 Then
        Arc x, y, Sinner, Souter, a1, a2, Twelvec ' See CONST definitions at top
      Else
        Arc x, y, Sinner, Souter, a1, a2, Smajc
      EndIf
    Else                    ' Minor tic mark
      a1 = m - Sminor : a2 = m + Sminor
      Arc x, y, Smid, Souter, a1, a2, Sminc
    EndIf
  Next m
End Sub

' ---------------------------- DST Functions ----------------------------------

Function DST_Offset() As Integer
  ' Returns the correct UTC offset for US Pacific time based on DST status.
  ' DST: 2nd Sunday in March 02:00 AM through 1st Sunday in November 02:00 AM.
  Local Integer yr, mo, dy, hr, mar_day, nov_day, in_dst

  yr = Val(Right$(Date$, 4))
  mo = Val(Mid$(Date$, 4, 2))
  dy = Val(Left$(Date$, 2))
  hr = Val(Left$(Time$, 2))

  mar_day = DST_Start(yr)         ' Day-of-month: 2nd Sunday in March
  nov_day = DST_End(yr)           ' Day-of-month: 1st Sunday in November

  in_dst = 0
  If mo > 3 And mo < 11 Then
    in_dst = 1                          ' April through October: always DST
  ElseIf mo = 3 And dy > mar_day Then
    in_dst = 1                          ' March, past the DST start day
  ElseIf mo = 3 And dy = mar_day And hr >= 2 Then
    in_dst = 1                          ' DST start day at or after 2 AM
  ElseIf mo = 11 And dy < nov_day Then
    in_dst = 1                          ' November, before the DST end day
  ElseIf mo = 11 And dy = nov_day And hr < 2 Then
    in_dst = 1                          ' DST end day before 2 AM
  EndIf

  If in_dst Then DST_Offset = DayOff% Else DST_Offset = StdOff%
End Function

Function DST_Start(yr%) As Integer
  ' Returns the day-of-month of the 2nd Sunday in March for the given year.
  Local Integer d, first_sun
  d = DOW(yr%, 3, 1)            ' Day-of-week for March 1
  first_sun = 1 + (7 - d) Mod 7
  DST_Start = first_sun + 7
End Function

Function DST_End(yr%) As Integer
  ' Returns the day-of-month of the 1st Sunday in November for the given year.
  Local Integer d
  d = DOW(yr%, 11, 1)           ' Day-of-week for November 1
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
                                          