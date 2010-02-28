Attribute VB_Name = "MainMod"
Option Explicit

Declare Function Init Lib "in_snes.dll" ()
Declare Function Quit Lib "in_snes.dll" ()
Declare Function About Lib "in_snes.dll" ()

Sub Main()
  Init
  About
  Quit
End Sub
