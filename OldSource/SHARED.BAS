Option Compare Text
Option Explicit

Global Const NoDmg = 0              ' No Damage -- Effect Only
Global Const Phys = 1               ' Physical Hit
Global Const Heal = 2               ' Healing Power
Global Const Morph = 3              ' Secret Morph
Global Const Cover = 4              ' Protection Move
Global Const StopMove = 5           ' Stop an enemy from attacking you
Global Const Critical = 6           ' Critical Hit / Mutual Death
Global Const Sacrifice = 7          ' Sacrifice Move
Global Const MindMeld = 9           ' Luna, Artemis, Diana Mind Melds
Global Const Invin = 10             ' Temporary Invincibility
Global Const HPTheft = 11           ' HP Theft
Global Const MPTheft = 12           ' MP Theft
Global Const Life = 17              ' Restore Life
Global Const Demi = 19              ' Cuts enemy's HP in half
Global Const MoonE = 20             ' Moon Power
Global Const Shadow = 21            ' Shadow
Global Const Water = 22             ' Water
Global Const Reveal = 23            ' Scan
Global Const Fire = 24              ' Fire nullifies Freeze.
Global Const Lit = 25               ' Lightning
Global Const Heart = 26             ' Heart
Global Const Earth = 27             ' Earth
Global Const RandE = 28             ' Random Elemental
Global Const Mult = 29              ' Multiple Elementals
Global Const Lum = 30               ' Luminous Energy


Function EName (ByVal E As Integer) As String
  Select Case E
    Case NoDmg: EName = "Nothing!"
    Case Phys: EName = "Physical Attacks"
    Case Heal: EName = "Heals"
    Case Morph: EName = "Morph"
    Case Cover: EName = "Cover"
    Case StopMove: EName = "StopMove"
    Case Critical: EName = "Critical"
    Case Sacrifice: EName = "Sacrifice"
    Case MindMeld: EName = "Mind Melds"
    Case Invin: EName = "Invincibility"
    Case HPTheft: EName = "HP Theft"
    Case MPTheft: EName = "MP Theft"
    Case Life: EName = "Life"
    Case Demi: EName = "Demi"
    Case MoonE: EName = "Moon Energy"
    Case Shadow: EName = "Shadow"
    Case Water: EName = "Water"
    Case Reveal: EName = "Reveal"
    Case Fire: EName = "Fire"
    Case Lit: EName = "Lightning"
    Case Heart: EName = "Heart Power"
    Case Earth: EName = "Earth Power"
    Case RandE: EName = "Random Element Attacks"
    Case Mult: EName = "Multi-Element Attacks"
    Case Lum: EName = "Luminous Energy"
  End Select
End Function

Function HTrim$ (ByVal S$)
Dim X$, Y%
  X$ = ""
  For Y = 1 To Len(S$)
    If Asc(Mid$(S$, Y, 1)) >= 32 And Asc(Mid$(S$, Y, 1)) < 127 Then X$ = X$ + Mid$(S$, Y, 1)
  Next Y
  HTrim = X$
End Function

Function KInStr (ByVal S1$, ByVal S2$) As Integer
' Is S1 a subset of S2?
Dim X1, X2 As Integer
  KInStr = False
  X2 = KLen(S1)
  If (X2 < 5) Then
    Exit Function
  End If
  For X1 = 1 To KLen(S2) - X2 + 1
    If LCase(Mid$(S2, X1, X2)) = LCase(S1) Then
      KInStr = True
    End If
  Next X1
End Function

Function KLen (ByVal S As String) As Integer
Dim X, X1 As Integer
  X1 = 0
  For X = 1 To Len(S)
    If (Asc(Mid$(S, X, 1)) <> 0) Then
      X1 = X1 + 1
    Else
      Exit For
    End If
  Next X
  KLen = X1
End Function

Function KTrim$ (ByVal S$)
Dim X$, Y%
  X$ = ""
  For Y = 1 To Len(S$)
    If Asc(Mid$(S$, Y, 1)) > 32 And Asc(Mid$(S$, Y, 1)) < 127 Then X$ = X$ + Mid$(S$, Y, 1)
  Next Y
  KTrim = X$
End Function

Sub LoadChibotChar (ByVal Filename$, ByVal X1%)
On Error GoTo NoWay
  Open Filename$ For Binary As #14
    Get #14, , Senshi(X1)
  Close #14
  Exit Sub
NoWay:
  MsgBox Error$(Err) + " loading Chibot Character " + Filename$ + ".", , "Chibot"
  Exit Sub
End Sub

Function Rand (ByVal A%, ByVal b%) As Integer
' Returns a Random Number in the range (A..B) inclusive
Dim X As Integer
  X = Int(Rnd * (b - A + 1)) + A
  Rand = X
End Function

Function TrimStr (ByVal S As Variant) As String
' A fast way of doing Trim(Str$("Whatever"))
  TrimStr = Trim(Str$(S))
End Function

