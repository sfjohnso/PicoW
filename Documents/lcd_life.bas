REM A program to play the Game of Life on the WaveShare P-E-B
REM by John CONWAY. Unfortunately, Covid19 took John`s life
REM on April 11, 2020. His legacy lives on in the countless
REM people inspired by his invention of the cellular automaton
REM called the Game of Life and his other works.
REM
REM Written by vegipete, April 22, 2020 for use with GFXTerm
REM Modified by Steve Johnson, May 2023 for use with P-E-B
REM Modified by Steve Johnson, May 2023 for use with MMB4W
REM Assumes P-E-B is in "Rotated" Portrait mode
REM Uses integers, inverted cell logic and Pico Math primitives (faster)

Option BASE 0
Option DEFAULT INTEGER
Dim INTEGER MaxDim = 40

Dim INTEGER i, j, count, w(MaxDim+1,MaxDim+1)

DIM INTEGER x      = MM.HRes/2    : REM  Horizontal Center of LCD
DIM INTEGER y      = MM.VRes/2    : REM  Vertical Center of LCD
DIM INTEGER top    = 0
DIM INTEGER bot    = MM.VRes-1
DIM INTEGER left   = 0
DIM INTEGER right  = MM.HRes-1
DIM INTEGER ltop   = 30           : REM   Top edge of Universe display on P-E-B
DIM INTEGER lleft  = 30           : REM   Left edge of Universe display on P-E-B
DIM INTEGER lspac  = 7            : REM   Cell spacing on Universe display on P-E-B
DIM INTEGER width  = lspac-1      : REM   Width and Height of Cells on P-E-B
DIM INTEGER height = lspac-1

DIM INTEGER live   = RGB(Cyan)
DIM INTEGER dead   = RGB(Gray)    : REM   Color to show killed cells
DIM INTEGER empty  = RGB(Gray)

CLS

Text x, top, "Conways LIFE", "CT", 4,2

Math SET 0, w() : REM clear the world to start

RandomizeLife

LCDLife                : REM  Display the Universe

Changed = 1

Do while Changed AND inkey$=""
  i = i+1 : Changed = 0
  CONWAYLife           : REM  Calculate the next generation
  LCDLife              : REM  Display the Universe
Loop

REM  If no changes, then static. Finish by displaying # of generations
CLS
Text x, y, "Conway's LIFE "+Str$(i), "CT", 4,2

End

Sub CONWAYLife : REM Calculate a new generation of life
  Local integer i, j, count
  Math SCALE w(), 2, w() : REM shift left & clear new generation
REM  Wrap corners & edges to simplify neighbor count evaluation
  w(0,0)=w(MaxDim,MaxDim) : w(MaxDim+1,MaxDim+1)=w(1, 1) 
  w(0,MaxDim+1)=w(MaxDim,1) : w(MaxDim+1, 0)=w(1,MaxDim)
  For i = 1 To MaxDim  : REM wrap the edges
    w(0,i)=w(MaxDim,i) : w(MaxDim+1,i)=w(1,i) 
    w(i,0)=w(i,MaxDim) : w(i,MaxDim+1)=w(i,1)
  Next i
REM calculate new generation
  For i = 1 To MaxDim
    For j = 1 To MaxDim
      count = Neighbors(i,j)
      If count=3 Then Inc w(i,j)
      If count=2 Then
        If w(i,j) And 2) = 2 Then Inc w(i,j)
      EndIf
    Next j
  Next i
End Sub

Function Neighbors(x,y) : REM return number of neighbors around given point
  Neighbors = (w(x-1,y-1) And 2) + (w(x+0,y-1) And 2) + (w(x+1,y-1) And 2) + (w(x-1,y+0) And 2) 
  Neighbors = (Neighbors + (w(x+1,y+0) And 2) + (w(x-1,y+1) And 2) + (w(x+0,y+1) And 2) + (w(x+1,y+1) And 2))>>1
End Function

Sub LCDLife : REM display the current world on LCD. Indicate if changes still occurring
  Local integer i,j, xcoord, ycoord
  xcoord = lleft         : REM  Left side of Universe display on LCD screen
  For i = 1 To MaxDim
    Inc xcoord, lspac
    ycoord = ltop        : REM  top edge of Universe display on LCD screen
    For j = 1 To MaxDim
      Inc ycoord, lspac
      Select Case (w(i,j) And 3)
        Case 1: Box xcoord, ycoord, width, height, 1, live, live  : Changed = 1
        Case 2: Box xcoord, ycoord, width, height, 1, dead, empty : Changed = 1
      End Select
    Next j
  Next i
End Sub

Sub SeedLife

  REM seed the world
  w( 9,10) = 1 : w(10,10) = 1 : w(11,10) = 1
  w( 9,11) = 1 : w(11,11) = 1
  w( 9,12) = 1 : w(11,12) = 1
  w(10,13) = 1
  w( 7,14) = 1 : w( 9,14) = 1 : w(10,14) = 3: w(11,14) = 1
  w( 8,15) = 1 : w(10,15) = 1 : w(12,15) = 1
  w(10,16) = 1 : w(13,16) = 1
  w( 9,17) = 1 : w(11,17) = 1
  w( 9,18) = 1 : w(11,18) = 1
  w(19,31) = 1 : w(11,31) = 1
  w(19,32) = 1 : w(11,32) = 1
  w(20,33) = 1
  w(17,34) = 1 : w( 9,34) = 1 : w(10,34) = 1: w(11,34) = 1
  w(18,35) = 1 : w(10,35) = 1 : w(12,35) = 1
  w(20,36) = 1 : w(13,36) = 1
  w(19,37) = 1 : w(11,37) = 1
  w(19,38) = 1 : w(11,38) = 1
End Sub

Sub RandomizeLife
  Local Integer count, i, j
  For count = 1 to MaxDim*MaxDim/6
    i = MaxDim * RND : j = MaxDim * RND : w(i,j) = 1
  Next count
End Sub

