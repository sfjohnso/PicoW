' TwoTrace - a 2-channel oscilloscope on the RP2040-LCD-0.96
' Steve Johnson  September, 2023
'
' Analog Signal input 1    on GP26 / Pin 31 - trace 1 / trigger
' Analog Signal input 2    on GP27 / Pin 32 - trace 2 / trigger
' Test   Signal output     on GP18 / Pin 24 - 1 KHz square wave
'
' Scope controls via momentary contact switches to ground
'   Vertical Scale switch on GP13 / Pin 17
'   Trigger Select switch on GP14 / Pin 19
'   Time Scale     switch on GP15 / Pin 20
'
' Scope Controls (via serial input from terminal emulator):
'   Voltage: [0], [1], [2]         to change vertical scale
'   Time:    [F]aster, [S]lower    to change horizontal scale
'   Trigger: [U]p, [D]own, [N]one  to change trigger criteria
'
' Sampling, Test Signal, Triggering, Framebuffers from stanleyella
' Math & Memory Copy from matherp
' Ganssle's switch debounce routine from CaptainBoing and Steve Johnson
' Graticules, Array Drawing, variable scales & triggering from Steve Johnson
' Interrupt Service Routines from Steve Johnson

Option EXPLICIT
Option Base 0

' Specifically For Waveshare RP2040-LCD-0.96

Const Hres       = MM.HRes        ' Horizontal screen resolution
Const Vres       = MM.VRes        ' Vertical   screen resolution

Const Major      = Vres/4         ' Major tic mark pixel spacing
Const Minor      = Major/4        ' Minor tic mark pixel spacing

Const PWM_Pin    = MM.Info(PINNO GP18) ' Pin 24 - PWM Test signal output pin
Const ADC1_Pin   = MM.Info(PINNO GP26) ' Pin 31 - Analog to Digital Converter 1
Const ADC2_Pin   = MM.Info(PINNO GP27) ' Pin 32 - Analog to Digital Converter 2

Const V_sw       = MM.Info(PINNO GP13) ' Pin 17 - Vertical Scale pushbutton switch
Const T_sw       = MM.Info(PINNO GP14) ' Pin 19 - Trigger Select pushbutton switch
Const A_sw       = MM.Info(PINNO GP15) ' Pin 20 - Time Scale     pushbutton switch

Const PWM_Freq   = 1000           ' Frequency for Test Signal on Pin 24
Const PWM_Duty   =   50           ' Base duty cycle for test signal PWM in %
Const PWM_Jitter =    0           ' Jitter in % for test signal PWM
Const Debounce   =  200           ' Debounce delay in mSec for front panel switches

' Generic to all displays - calculations done off screen dimensions

Const HMajor     = Hres/Major     ' How many Horiz Major Tic marks
Const VMajor     = Vres/Major     ' How many Vert  Major Tic marks
Const HMinor     = Hres/Minor     ' How many Horiz Minor Tic marks
Const VMinor     = Vres/Minor     ' How many Vert  Minor Tic marks

Const Background = RGB(32,64,32)  ' Color for Display background, a la Tektronix scopes
Const GMajor     = RGB(black)     ' Color for Major tic marks
Const GMinor     = RGB(black)     ' Color for Minor tic marks
Const T1_color   = RGB(yellow)    ' Color for displayed Trace1
Const T2_color   = RGB(cyan)      ' Color for displayed Trace2
Const TxtColor   = RGB(white)     ' Color for displayed text
Const ShadColor  = RGB(black)     ' Color for text shadows

Const Trig_None_1  = 0
Const Trig_Down_1  = 1              ' Enumerate possible trigger conditions for GP26
Const Trig_Up_1    = 2
Const Trig_None_2  = 3
Const Trig_Down_2  = 4              ' Enumerate possible trigger conditions for GP27
Const Trig_Up_2    = 5


Const ADC_Max    = 7              ' Maximum index into ADC Time Scale array (0-7)
Const V_Max      = 3              ' Maximum index into Voltage  Scale array (0-3)
Const Trig_Max   = 5

' Declare storage

Dim INTEGER c, n, x, y, trigger, trigger_timeout, Vselect, Trig_Type, ADC_select
Dim INTEGER v_time, v_diff, v_state, v_old, v_press ' Volts   scale button debouncing and status
Dim INTEGER t_time, t_diff, t_state, t_old, t_press ' Trigger type  button debouncing and status
Dim INTEGER a_time, a_diff, a_state, a_old, a_press ' ADC frequency button debouncing and status

Dim INTEGER Horizontal(Hres)      ' Horizontal coordinate buffer for fast graticule and buffer draw
Dim FLOAT   trace1(2*Hres)        ' Extra size to hopefully pick up a trigger event
Dim FLOAT   trace2(2*Hres)        ' Extra size to hopefully pick up a trigger event
Dim FLOAT   buffer1(Hres)         ' Display buffer for screen output
Dim FLOAT   buffer2(Hres)         ' Display buffer for screen output
Dim INTEGER addr.trace1, addr.trace2, addr.buffer1, addr.buffer2

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

Timer = 0

' ========================== Processing Loop ================================

Do
  trigger_timeout = Timer
  Do
    Handle_Keypresses
    Handle_Switches
    trigger = -1
    Get_Samples
    trigger = find_trigger(Trig_Type)
    If Timer-trigger_timeout > 2500 Then Display_No_Trigger ' Waiting too long for a trigger.
  Loop Until trigger >= 0
  If Timer-trigger_timeout > 2500 Then Draw_Graticules      ' OK, we've got a trigger. Clear message.
  Scale_Samples
  Update_Display
Loop

' ========================== Interrupt Service Routines ====================

Sub V_ISR ' Interrupt Service Routine for Voltage SPST Button
  If v_press = 0 Then ' Ignore further  button presses until handled in main loop
    If (Timer - v_time) > debounce Then v_press = 1 : v_time = Timer
  EndIf
End Sub

Sub T_ISR ' Interrupt Service Routine for Trigger SPST Button
  If t_press = 0 Then ' Ignore further  button presses until handled in main loop
    If (Timer - t_time) > debounce Then t_press = 1 : t_time = Timer
  EndIf
End Sub

Sub A_ISR ' Interrupt Service Routine for ADC Frequency SPST Button
  If a_press = 0 Then ' Ignore further  button presses until handled in main loop
    If (Timer - a_time) > debounce Then a_press = 1 : a_time = Timer
  EndIf
End Sub

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

Sub Handle_Switches ' switch press detection done in Interrupt Service Routines
  If v_press = 1 Then
    v_press = 0
    Print "Voltage: "; Str$(V.MajVolts(Vselect),3,3),"->",
    Vselect = (Vselect+1) Mod (V_Max+1)
    Draw_Graticules
    Print Str$(V.MajVolts(Vselect),3,3);" V/Div"
  EndIf

  If t_press = 1 Then
    t_press = 0
    Select Case Trig_Type
      Case Trig_Up_1:   Print "Trigger:  "; " Trig_Up_1",   "->", " Trig_Down_1"
      Case Trig_Down_1: Print "Trigger:  "; " Trig_Down_1", "->", " Trig_None_1"
      Case Trig_None_1: Print "Trigger:  "; " Trig_None_1", "->", " Trig_Up_2"
      Case Trig_Up_2:   Print "Trigger:  "; " Trig_Up_2",   "->", " Trig_Down_2"
      Case Trig_Down_2: Print "Trigger:  "; " Trig_Down_2", "->", " Trig_None_2"
      Case Trig_None_2: Print "Trigger:  "; " Trig_None_2", "->", " Trig_Up_1"
      Case Else :       Print "Trigger:  "; " *Unknown*",   "->", " Trig_Up_1"
    End Select
    Trig_Type = (Trig_Type+1) Mod (Trig_Max+1)
    Draw_Graticules
  EndIf

  If a_press = 1 Then
    a_press = 0
    Print "Timebase: "; Str$(H.seconds(ADC_select),3,3);" ";H.units(ADC_select), "->",
    ADC_select = (ADC_select+1) Mod (ADC_Max+1) : Set_ADC_Timing
    Print Str$(H.seconds(ADC_select),3,3);" ";H.units(ADC_select); "/Div"
    Draw_Graticules
  EndIf

End Sub

Function find_trigger(a)
  Local median = Math(MEAN trace1())

  Select Case a

    Case Trig_Down_1
      median = Math(MEAN trace1())
      For c = 0 To Hres-1
        If trace1(c) > median+0.1 And trace1(c+1) < median+0.2 Then find_trigger = c : Exit Function
      Next
      find_trigger = -1

    Case Trig_Up_1
      median = Math(MEAN trace1())
      For c = 0 To Hres-1
        If trace1(c+1) > median+0.1 And trace1(c) < median+0.2 Then find_trigger = c : Exit Function
      Next
      find_trigger = -1

    Case Trig_Down_2
      median = Math(MEAN trace2())
      For c = 0 To Hres-1
        If trace2(c) > median+0.1 And trace2(c+1) < median+0.2 Then find_trigger = c : Exit Function
      Next
      find_trigger = -1

    Case Trig_Up_2
      median = Math(MEAN trace2())
      For c = 0 To Hres-1
        If trace2(c+1) > median+0.1 And trace2(c) < median+0.2 Then find_trigger = c : Exit Function
      Next
      find_trigger = -1

    Case Else
      find_trigger = 0

  End Select
End Function

Sub Display_Instructions
  FRAMEBUFFER WRITE L
  CLS Background
  Text   6, Vres/2, "Pico", CMV, 1, 1, T1_Color, Background
  Text   Hres-6, Vres/2, "Scope", CMV, 1, 1, T2_Color, Background
  Text   18,  3,     "Keyboard:",            LT, 7, 1, TxtColor, Background
  Text   25, 15,     "Volt: 0, 1, 2, 3",     LT, 7, 1, TxtColor, Background
  Text   25, 27,     "Time: Faster, Slower", LT, 7, 1, TxtColor, Background
  Text   25, 39,     "Trig: Up, Dn, None",   LT, 7, 1, TxtColor, Background
  Text   18, 51,     "Pin 24: Test Signal",  LT, 7, 1, TxtColor, Background
  Text   18, 63,     "Pins 31,32: Inputs",   LT, 7, 1, TxtColor, Background
  FRAMEBUFFER COPY L, F
  FRAMEBUFFER COPY F,N
  Pause 4000

  Select Case Trig_Type
    Case Trig_Up_1, Trig_Down_1, Trig_None_1: Text 20, 79, "Waiting for Trigger, Ch 1", LB, 7, 1, T1_Color, Background
    Case Trig_Up_2, Trig_Down_2, Trig_None_2: Text 20, 79, "Waiting for Trigger, Ch 2", LB, 7, 1, T2_Color, Background
  End Select

  FRAMEBUFFER COPY L, F
  FRAMEBUFFER COPY F,N
End Sub

Sub Display_No_Trigger
  FRAMEBUFFER WRITE L
  Select Case Trig_Type
    Case Trig_Up_1, Trig_Down_1, Trig_None_1: Text Hres/2, Vres/2, "Waiting for Trigger, Ch 1", CM, 7, 1, T1_Color, Background
    Case Trig_Up_2, Trig_Down_2, Trig_None_2: Text Hres/2, Vres/2, "Waiting for Trigger, Ch 2", CM, 7, 1, T2_Color, Background
  End Select
  FRAMEBUFFER COPY L, F
  FRAMEBUFFER COPY F,N
End Sub

Sub Initialize_Arrays
  addr.trace1  = Peek(varaddr trace1())
  addr.trace2  = Peek(varaddr trace2())
  addr.buffer1 = Peek(varaddr buffer1())
  addr.buffer2 = Peek(varaddr buffer2())

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

  H.Freq(0) =   1000 : H.Units(0)="mSec" : H.Seconds(0) = 20.0
  H.Freq(1) =   2000 : H.Units(1)="mSec" : H.Seconds(1) = 10.0
  H.Freq(2) =   4000 : H.Units(2)="mSec" : H.Seconds(2) = 5.0
  H.Freq(3) =  10000 : H.Units(3)="mSec" : H.Seconds(3) = 2.0
  H.Freq(4) =  20000 : H.Units(4)="mSec" : H.Seconds(4) = 1.0
  H.Freq(5) =  40000 : H.Units(5)="uSec" : H.Seconds(5) = 500.0
  H.Freq(6) = 100000 : H.Units(6)="uSec" : H.Seconds(6) = 200.0
  H.Freq(7) = 200000 : H.Units(7)="uSec" : H.Seconds(7) = 100.0
' H.Freq(8) = 400000 : H.Units(8)="uSec" : H.Seconds(8) = 50.0 - too fast for 2 channels

  ADC_select = 7              ' Select from a number of time scales
  Vselect    = 2              ' Select from a number of Scale factors and offsets for volts
  Trig_Type  = Trig_Down_1    ' Select from a number of trigger types

End Sub

Sub Initialize_Hardware
  SetPin V_SW, INTL, V_ISR, PULLUP ' Switch to cycle through Voltage scales
  SetPin T_SW, INTL, T_ISR, PULLUP ' Switch to cycle through Trigger types
  SetPin A_SW, INTL, A_ISR, PULLUP ' Switch to cycle through ADC Frequency (Time) scales
  SetPin GP18, PWM1A               ' Set up pin 24 for PWM test signal output
  PWM 1, PWM_Freq, PWM_Duty        ' Square wave on Pin 24
  SetPin ADC1_Pin, AIn             ' ADC input on Pin 31 "Channel 1"
  SetPin ADC2_Pin, AIn             ' ADC input on Pin 32 "Channel 2"
  ADC open H.Freq(ADC_Select), 2   ' Sample at specified frequency

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
    Case Trig_Up_1:   Text Hres/2, Vres, "/", CB, 7, 1, T1_Color, Background
    Case Trig_Down_1: Text Hres/2, Vres, "\", CB, 7, 1, T1_Color, Background
    Case Trig_None_1: Text Hres/2, Vres, "-", CB, 7, 1, T1_Color, Background
    Case Trig_Up_2:   Text Hres/2, Vres, "/", CB, 7, 1, T2_Color, Background
    Case Trig_Down_2: Text Hres/2, Vres, "\", CB, 7, 1, T2_Color, Background
    Case Trig_None_2: Text Hres/2, Vres, "-", CB, 7, 1, T2_Color, Background
    Case Else :       Text Hres/2, Vres, " ", CB, 7, 1, T1_Color, Background
  End Select
End Sub

Sub Randomize_Test_Signal
  PWM 1, PWM_Freq, PWM_Duty+PWM_Jitter*Rnd  ' Randomize duty cycle to ensure displayed signal is refreshing
End Sub

Sub Get_Samples               ' Load ADC inputs into sample() array
  trace1(2*Hres-1) = -1       ' Samples will never be < 0
  ADC start trace1(), trace2()
  Do While trace1(2*Hres-1) < 0 : Loop
End Sub

Sub Scale_Samples
  Memory copy FLOAT addr.trace1 + trigger*8, addr.buffer1, Hres
  Memory copy FLOAT addr.trace2 + trigger*8, addr.buffer2, Hres
  Math Scale buffer1!(), V.Scale(Vselect),  buffer1!()  ' Scale samples to fit vertically
  Math Scale buffer2!(), V.Scale(Vselect),  buffer2!()  ' Scale samples to fit vertically
  Math Add   buffer1!(), V.Offset(Vselect), buffer1!()  ' Offset to bottom of screen (positive is up)
  Math Add   buffer2!(), V.Offset(Vselect), buffer2!()  ' Offset to bottom of screen (positive is up)
End Sub


Sub Update_Display
  FRAMEBUFFER COPY L, F
  FRAMEBUFFER WRITE F : Pixel Horizontal(), buffer1!(), T1_color
  FRAMEBUFFER WRITE F : Pixel Horizontal(), buffer2!(), T2_color
  FRAMEBUFFER COPY F,N
End Sub
