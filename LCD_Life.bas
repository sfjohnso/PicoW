' A program to play the Game of Life on the WaveShare P-E-B
' by John CONWAY. Unfortunately, Covid19 took John`s life
' on April 11, 2020. His legacy lives on in the countless
' people inspired by his invention of the cellular automaton
' called the Game of Life and his other works.
'
' Written by vegipete, April 22, 2020 for use with GFXTerm
' Modified by Steve Johnson, May 2023 for use with P-E-B
' Modified by Steve Johnson, May 2023 for use with MMB4W
' Assumes P-E-B is in "Rotated" Portrait mode
' Uses integers, inverted cell logic and MMBasic Math primitives (faster)
' It's possible to run through the loops once instead of twice, but the
' impact of seeing the screen update all at once is preferred, I think.

Option BASE 0
Option DEFAULT INTEGER

Const size    = 40          ' Size of Universe
Const lspac   = 7           ' Dimensions of Cell

Dim INTEGER i, j, count, w(size+1,size+1)

Const x       = MM.HRes/2  ' Horizontal Center
Const y       = MM.VRes/2  ' Vertical Center
Const top     = 0
Const bot     = MM.VRes-1
Const left    = 0
Const right   = MM.HRes-1
Const ltop    = y-size*lspac/2 ' Top edge of Universe display
Const lleft   = x-size*lspac/2 ' Left edge of Universe display
Const btop    = ltop           ' Border box top
Const bleft   = lleft          ' Border box left
Const bwidth  = (size+2)*lspac ' Border box width
Const bheight = (size+2)*lspac ' Border box height
Const width   = lspac-1        ' Width and Height of Cells
Const height  = lspac-1

Const live    = RGB(Cyan)
Const dead    = RGB(Gray)      ' Color to show killed cells
Const empty   = RGB(63,63,63)
Const back    = RGB(Black)
Const border  = RGB(Gold)

CLS

Text x, top, "Conway's LIFE", "CT", 4,2
Math SET 0, w()                        ' Clear the world to start
Box bleft, btop, bwidth, bheight, 1, border, back

RandLife
LCDLife                                ' Display the Universe

Changed = 1

Do While Changed And (Inkey$="")
  i = i+1 : Changed = 0
  CONWAYLife                           ' Calculate the next generation
  LCDLife                              ' Display the Universe
Loop

' Finish by displaying # of generations
CLS : Text x, y, "Conway's LIFE: "+Str$(i), "CT", 4,1
End

Sub CONWAYLife                         ' Rem Calculate a new generation of life
  Local integer i, j, xcoord, ycoord
  Math SCALE w(), 2, w()               ' shift left & clear new generation

  ' Simplify neighbor count evaluation

  w(0,0)=w(size,size) : w(size+1,size+1)=w(1, 1) ' wrap corners
  w(0,size+1)=w(size,1) : w(size+1, 0)=w(1,size)
  For i = 1 To size                              ' wrap the edges
    w(0,i)=w(size,i) : w(size+1,i)=w(1,i) : w(i,0)=w(i,size) : w(i,size+1)=w(i,1)
  Next i

  ' calculate new generation

'  xcoord = lleft        ' Left side of Universe display on LCD screen
  For i = 1 To size
'    Inc xcoord, lspac
'    ycoord = ltop       ' top edge of Universe display on LCD screen
    For j = 1 To size
'      Inc ycoord, lspac
      Select Case Neighbors(i,j)
        Case 3
          Inc w(i,j)
        Case 2
          If w(i,j) And 2) = 2 Then Inc w(i,j)
      End Select
'      Select Case (w(i,j) And 3)
'        Case 1 : Box xcoord, ycoord, width, height, 1, live, live : Changed = 1
'        Case 2 : Box xcoord, ycoord, width, height, 1, dead, dead : Changed = 1
'      End Select
    Next j
  Next i
End Sub

Function Neighbors(x,y) ' return number of neighbors around given point
  count = (w(x-1,y-1) And 2) + (w(x,y-1) And 2) + (w(x+1,y-1) And 2) + (w(x-1,y) And 2)
  count = (count + (w(x+1,y) And 2) + (w(x-1,y+1) And 2) + (w(x,y+1) And 2) + (w(x+1,y+1) And 2))>>1
'  Print x, y, count
  Neighbors = count
End Function

Sub LCDLife             ' display the current world. Indicate if changes still occurring
  Local integer i,j, xcoord, ycoord
  xcoord = lleft        ' Left side of Universe display on LCD screen
  For i = 1 To size
    Inc xcoord, lspac
    ycoord = ltop       ' top edge of Universe display on LCD screen
    For j = 1 To size
      Inc ycoord, lspac
      Select Case (w(i,j) And 3)
        Case 1 : Box xcoord, ycoord, width, height, 1, live, live : Changed = 1
        Case 2 : Box xcoord, ycoord, width, height, 1, dead, dead : Changed = 1
      End Select
    Next j
  Next i
End Sub

Sub SeedLife ' Seed the world with a known pattern

  Rem seed the world
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

Sub RandLife ' Seed the world with a random pattern
  Local Integer count, i, j
  For count = 1 To size*size/7
    w(size*Rnd,size*Rnd) = 1
  Next count
End Sub
