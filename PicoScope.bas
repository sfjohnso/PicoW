' PicoScope - a simple oscilloscope on the RP2040-LCD-0.96
' Steve Johnson  August, 2023
'
' Test   Signal output on GP18 / Pin 24
' Analog Signal input  on GP26 / Pin 31
'
' Scope Controls (via serial input from terminal emulator):
'  Voltage: 0, 1, 2               to change vertical scale
'  Time:    [F]aster, [S]lower    to change horizontal scale
'  Trigger: [U]p, [D]own, [N]one  to change trigger criteria
'
' Many ideas were shared on the Back Shed Forum for this program.
'   Sampling, Test Signal, Triggering, Framebuffers from stanleyella
'   Math & Memory Copy from matherp
'   Ganssle's switch debounce routine from CaptainBoing and Steve Johnson
'   Graticules, Array Drawing, variable scales & triggering from Steve Johnson

Option EXPLICIT

' Specifically For Waveshare RP2040-LCD-0.96

Const Hres       = MM.HRes        ' Horizontal screen resolution
Const Vres       = MM.VRes        ' Vertical   screen resolution

Const Major      = Vres/4
Const Minor      = Major/4

Const PWM_Pin    = 24             ' GP18 for the PWM Test Signal
Const ADC_Pin    = 31             ' GP26 for the analog input
Const V_sw       =  1             ' GP0  for the Voltage Range  push-button switch
Const T_sw       =  2             ' GP1  for the Trigger Select push-button switch
Const A_sw       =  4             ' GP2  for the Time Scale     push-button switch

Const PWM_Freq   =   5000         ' Frequency for Test Signal on Pin 24
Const PWM_Duty   =   49.0         ' Base duty cycle for test signal PWM in %
Const PWM_Jitter =    2.0         ' Jitter in % for test signal PWM
Const Debounce   =  &H3FF         ' Debounce delay for front panel switches

' Generic to all displays

Const HMajor     = Hres/Major     ' How many Horiz Major Tic marks
Const VMajor     = Vres/Major     ' How many Vert  Major Tic marks
Const HMinor     = Hres/Minor     ' How many Horiz Minor Tic marks
Const VMinor     = Vres/Minor     ' How many Vert  Minor Tic marks

Const Background = RGB(32,64,32)  ' Color for Display background, a la Tektronix scopes
Const GMajor     = RGB(black)     ' Color for Major tic marks
Const GMinor     = RGB(black)     ' Color for Minor tic marks
Const Signal     = RGB(yellow)    ' Color for displayed signal
Const TxtColor   = RGB(cyan)      ' Color for displayed text

Const Trig_None  = 0
Const Trig_Down  = 1              ' Enumerate possible trigger conditions
Const Trig_Up    = 2

Const ADC_Max    = 7              ' Maximum index into ADC Time Scale array (0-7)
Const V_Max      = 3              ' Maximum index into Voltage  Scale array (0-3)
Const Trig_Max   = 2

' Declare storage

Dim INTEGER c, n, x, y, trigger, Vselect, Trig_Type, ADC_select
Dim INTEGER v_btn, t_btn, a_btn   ' Counters to do switch debouncing

Dim INTEGER Horizontal(Hres)      ' Horizontal coordinate buffer for fast graticule and buffer draw
Dim FLOAT   sample(2*Hres)        ' Extra size to hopefully pick up a trigger event
Dim FLOAT   buffer(Hres)          ' Display buffer for screen output
Dim INTEGER addr.sample, addr.buffer

Dim INTEGER HMajorX1(HMajor+1), HMajory1(HMajor+1), HMajory2(HMajor+1) ' For Horizontal Major Axes
Dim INTEGER VMajorX1(VMajor+1), VMajorx2(VMajor+1), VMajory1(VMajor+1) ' For Vertical   Major Axes
Dim INTEGER HMinorX1(HMinor+1), HMinory1(HMinor+1), HMinory2(HMinor+1) ' For Horizontal Minor Axes
Dim INTEGER VMinorX1(VMinor+1), VMinorx2(VMinor+1), VMinory1(VMinor+1) ' For Vertical   Minor Axes

Dim Float   V.Scale(10), V.Offset(10), V.MajVolts(10)
Dim Float   H.Freq(10),  H.Seconds(10)

Dim String  H.Units$(10), keypress$

' ========================== Initialization =================================
Initialize_Arrays
Initialize_Hardware
Display_Instructions
Draw_Graticules

' ========================== Processing Loop ================================
Do
  Randomize_Test_Signal
  Timer = 0
  Do
    Handle_Keypresses
    Handle_Switches
    trigger = -1
    Get_Samples
    trigger = find_trigger(Trig_Type)
    If Timer > 2500 Then Display_No_Trigger ' Waiting too long for a trigger.
  Loop Until trigger >= 0
  If Timer > 2500 Then Draw_Graticules      ' OK, we've got a trigger. Clear message.
  Scale_Samples
  Update_Display
'  Handle_Keypresses
'  Handle_Switches
Loop

' ========================== Subs and Functions ============================

Sub Handle_Keypresses
  keypress$ = Inkey$
  Select Case keypress$
    Case "0"      : Vselect = 0
    Case "1"      : Vselect = 1
    Case "2"      : Vselect = 2
    Case "3"      : Vselect = 3
    Case "u", "U" : Trig_Type  = Trig_Up
    Case "d", "D" : Trig_Type  = Trig_Down
    Case "n", "N" : Trig_Type  = Trig_None
    Case "f", "F" : ADC_select = Min(ADC_select+1, ADC_Max) : Set_ADC_Timing
    Case "s", "S" : ADC_select = Max(ADC_select-1, 0) : Set_ADC_Timing
    Case "" : Exit Sub               ' No key pressed. No change.
    Case Else : Display_Instructions ' Unknown key pressed.
  End Select
  Draw_Graticules
End Sub

Sub Handle_Switches ' using Ganssel's debounce routine in Fruit of the Shed
  Local INTEGER V, T, A
  v_btn = v_btn<<1 Xor Pin(V_sw) And Debounce
  t_btn = t_btn<<1 Xor Pin(T_sw) And Debounce
  a_btn = a_btn<<1 Xor Pin(A_sw) And Debounce

  If v_btn = 0 Then
    Vselect = (Vselect+1) Mod (V_Max+1)
    v_btn = Debounce
  EndIf

  If t_btn = 0 Then
    Select Case Trig_Type
      Case Trig_Up:   Trig_Type = Trig_Down
      Case Trig_Down: Trig_Type = Trig_None
      Case Trig_None: Trig_Type = Trig_Up
      Case Else :     Trig_Type = Trig_Up
    End Select
    t_btn = Debounce
  EndIf

  If a_btn = 0 Then
    ADC_select = (ADC_select+1) Mod (ADC_Max+1) : Set_ADC_Timing
    a_btn = Debounce
  EndIf

  Draw_Graticules

End Sub

Function find_trigger(a)
  Local median = Math(MEAN sample())
  Select Case a
    Case Trig_Down
      For c = 0 To Hres-1
        If sample(c) > median+0.1 And sample(c+1) < median+0.2 Then find_trigger = c : Exit Function
      Next
      find_trigger = -1 : Exit Function
    Case Trig_Up
      For c = 0 To Hres-1
        If sample(c+1) > median+0.1 And sample(c) < median+0.2 Then find_trigger = c : Exit Function
      Next
      find_trigger = -1 : Exit Function
    Case Else
      find_trigger = 0 : Exit Function
  End Select
End Function

Sub Display_Instructions
  FRAMEBUFFER WRITE L
  CLS Background
  Text   6,  Vres/2, "PicoScope", CMV, 7, 1, Signal, Background
  Text   18,  3,     "Keyboard:", LT, 7, 1, TxtColor, Background
  Text   25, 15,     "Volt: 0, 1, 2, 3", LT, 7, 1, TxtColor, Background
  Text   25, 27,     "Time: Faster, Slower", LT, 7, 1, TxtColor, Background
  Text   25, 39,     "Trig: Up, Dn, None", LT, 7, 1, TxtColor, Background
  Text   25, 51,     "Pin 24: Test Signal", LT, 7, 1, TxtColor, Background
  FRAMEBUFFER COPY L, F
  FRAMEBUFFER COPY F,N
  Pause 4000
  Text   20, 79,     "Waiting for Trigger.....", LB, 7, 1, Signal, Background
  FRAMEBUFFER COPY L, F
  FRAMEBUFFER COPY F,N
End Sub

Sub Display_No_Trigger
  FRAMEBUFFER WRITE L
  Text Hres/2, Vres/2, "Waiting for Trigger.....", CM, 7, 1, Signal, Background
  FRAMEBUFFER COPY L, F
  FRAMEBUFFER COPY F,N
End Sub

Sub Initialize_Arrays
  addr.sample = Peek(varaddr sample())
  addr.buffer = Peek(varaddr buffer())

  Math Set 0, HMajory1() : Math Set Vres, HMajory2()  ' Set Arrays for Time  Major Axis
  Math Set 0, VMajorX1() : Math Set Hres, VMajorx2()  ' Set Arrays for Volts Major Axis

  Math Set 3*Vres/4+Minor/2, HMinory1()
  Math Set 3*Vres/4-Minor/2, HMinory2() ' Set Arrays for Time  Minor Axis
  Math Set Hres/2+Minor/2,   VMinorx1()
  Math Set Hres/2-Minor/2,   VMinorx2() ' Set Arrays for Volts Minor Axis

  For x = 0 To Hres-1 : Horizontal(x) = x     : Next ' used in array graphics
  For x = 0 To HMajor : HMajorx1(x) = x*Major : Next ' for Time  Major Axis graticule
  For y = 0 To VMajor : VMajory1(y) = y*Major : Next ' for Volts Major Axis graticule
  For x = 0 To HMinor : HMinorx1(x) = x*Minor : Next ' for Time  Minor Axis graticule
  For y = 0 To VMinor : VMinory1(y) = y*Minor : Next ' for Volts Minor Axis graticule

  ' RP2040-LCD-0.96 - specific values here.
  '
  ' For Voltage, Zero is at pixel 60, 3/4 down the display
  V.Scale(0)= -4.0 : V.Offset(0)=60 : V.MajVolts(0) = 5.000 ' 5.000 Volts per Major Division
  V.Scale(1)= -8.0 : V.Offset(1)=60 : V.MajVolts(1) = 2.500 ' 2.500 Volts per Major Division
  V.Scale(2)=-16.0 : V.Offset(2)=60 : V.MajVolts(2) = 1.250 ' 1.250 Volts per Major Division
  V.Scale(3)=-32.0 : V.Offset(3)=60 : V.MajVolts(3) = 0.675 ' 0.675 Volts per Major Division

  ' For Time,

  H.Freq(0) =   2000 : H.Units(0)="mSec" : H.Seconds(0) = 10.0
  H.Freq(1) =   4000 : H.Units(1)="mSec" : H.Seconds(1) = 5.0
  H.Freq(2) =  10000 : H.Units(2)="mSec" : H.Seconds(2) = 2.0
  H.Freq(3) =  20000 : H.Units(3)="mSec" : H.Seconds(3) = 1.0
  H.Freq(4) =  40000 : H.Units(4)="uSec" : H.Seconds(4) = 500.0
  H.Freq(5) = 100000 : H.Units(5)="uSec" : H.Seconds(5) = 200.0
  H.Freq(6) = 200000 : H.Units(6)="uSec" : H.Seconds(6) = 100.0
  H.Freq(7) = 400000 : H.Units(7)="uSec" : H.Seconds(7) = 50.0

  ADC_select = 3              ' Se;ect from a number of time scales
  Vselect    = 1              ' Select from a number of Scale factors and offsets for volts
  Trig_Type  = Trig_Down      ' Select from a number of trigger types

End Sub

Sub Initialize_Hardware
  SetPin V_SW, DIN, PULLUP        ' Switch to cycle through Voltage scales
  SetPin T_SW, DIN, PULLUP        ' Switch to cycle through Trigger types
  SetPin A_SW, DIN, PULLUP        ' Switch to cycle through ADC Frequency (Time) scales
  SetPin GP18, PWM1A              ' Set up pin 24 for PWM test signal output
  PWM 1, PWM_Freq, PWM_Duty       ' Square wave on Pin 24
  SetPin (31), AIn                ' ADC input on Pin 31
  ADC open H.Freq(ADC_Select), 1  ' Sample at specified frequency

  FRAMEBUFFER CREATE F
  FRAMEBUFFER LAYER  L
End Sub

Sub Set_ADC_Timing
  ADC FREQUENCY H.Freq(ADC_Select)
End Sub

Sub Draw_Graticules
  Local STRING T$, V$, A$
  FRAMEBUFFER WRITE L
  CLS Background
  Line HMinorx1(), HMinory1(), HMinorx1(), HMinory2(), 1, GMinor ' Draw minor horiz graticules
  Line VMinorx1(), VMinory1(), VMinorx2(), VMinory1(), 1, GMinor ' Draw minor vert  graticules
  Line HMajorx1(), HMajory1(), HMajorx1(), HMajory2(), 1, GMajor ' Draw major horiz graticules
  Line VMajorx1(), VMajory1(), VMajorx2(), VMajory1(), 1, GMajor ' Draw major vert  graticules

  A$ = Str$(H.Seconds(ADC_Select),3,0) + " " + H.Units(ADC_Select)
  Text    0, Vres, A$, LB, 7, 1, TxtColor, Background     ' Draw Time Scale at Lower Left
  V$ = Str$(V.MajVolts(Vselect), 2, 2)+" V"
  Text Hres, Vres, V$, RB, 7, 1, TxtColor, Background     ' Draw Volts Scale at Lower Right
  Select Case Trig_Type
    Case Trig_Up:   T$ = "/"
    Case Trig_Down: T$ = "\"
    Case Trig_None: T$ = "-"
    Case Else :     T$ = " "
  End Select
  Text Hres/2, Vres, T$, CB, 7, 1, TxtColor, Background    ' Draw Trigger at Lower Middle
End Sub

Sub Randomize_Test_Signal
  PWM 1, PWM_Freq, PWM_Duty+PWM_Jitter*Rnd  ' Randomize duty cycle to ensure displayed signal is refreshing
End Sub

Sub Get_Samples               ' Load ADC inputs into sample() array
  sample(2*Hres-1) = -1       ' Samples will never be < 0
  ADC start sample() : Do While sample(2*Hres-1) < 0 : Loop
End Sub

Sub Scale_Samples
  Memory copy FLOAT addr.sample + trigger*8, addr.buffer, Hres
  Math Scale buffer!(), V.Scale(Vselect),  buffer!()  ' Scale samples to fit vertically
  Math Add   buffer!(), V.Offset(Vselect), buffer!()  ' Offset to bottom of screen (positive is up)
End Sub


Sub Update_Display
  FRAMEBUFFER COPY L, F
  FRAMEBUFFER WRITE F : Pixel Horizontal(), buffer!(), Signal
  FRAMEBUFFER COPY F,N
End Sub