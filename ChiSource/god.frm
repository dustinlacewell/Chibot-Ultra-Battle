VERSION 2.00
Begin Form fGod 
   BackColor       =   &H00000000&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "ChUB 2000: GodMode"
   ClientHeight    =   3720
   ClientLeft      =   1260
   ClientTop       =   1515
   ClientWidth     =   6420
   Height          =   4125
   Icon            =   GOD.FRX:0000
   Left            =   1200
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   3720
   ScaleWidth      =   6420
   Top             =   1170
   Width           =   6540
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Command 36"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H000080FF&
      Height          =   255
      Index           =   35
      Left            =   4440
      TabIndex        =   46
      Top             =   2400
      Width           =   1935
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Command 35"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H000080FF&
      Height          =   255
      Index           =   34
      Left            =   4440
      TabIndex        =   45
      Top             =   2160
      Width           =   1935
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Command 34"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H000080FF&
      Height          =   255
      Index           =   33
      Left            =   4440
      TabIndex        =   44
      Top             =   1920
      Width           =   1935
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Command 33"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H000080FF&
      Height          =   255
      Index           =   32
      Left            =   4440
      TabIndex        =   43
      Top             =   1680
      Width           =   1935
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Command 32"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H000080FF&
      Height          =   255
      Index           =   31
      Left            =   4440
      TabIndex        =   42
      Top             =   1440
      Width           =   1935
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Destroy their Weapon"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   30
      Left            =   4440
      TabIndex        =   41
      Top             =   1200
      Width           =   1935
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Destroy Dropped Wpns."
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   29
      Left            =   4440
      TabIndex        =   40
      Top             =   960
      Width           =   1935
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Add Dropped Weapon"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   28
      Left            =   4440
      TabIndex        =   39
      Top             =   720
      Width           =   1935
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Frostbite"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   27
      Left            =   4440
      TabIndex        =   38
      Top             =   480
      Width           =   855
   End
   Begin CommandButton cLocal 
      Caption         =   "Local"
      Height          =   255
      Left            =   3720
      TabIndex        =   37
      Top             =   3000
      Width           =   735
   End
   Begin TextBox tChat 
      Height          =   285
      Left            =   480
      TabIndex        =   32
      Top             =   3360
      Width           =   5895
   End
   Begin CommandButton cSend 
      Caption         =   "SEND!"
      FontBold        =   -1  'True
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   13.5
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   495
      Left            =   2400
      TabIndex        =   31
      Top             =   2760
      Width           =   1215
   End
   Begin TextBox tTarget 
      Height          =   285
      Left            =   1080
      TabIndex        =   30
      Top             =   3000
      Width           =   1215
   End
   Begin TextBox tHost 
      Height          =   285
      Left            =   1080
      TabIndex        =   29
      Top             =   2760
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Rot-13"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF80FF&
      Height          =   255
      Index           =   26
      Left            =   3120
      TabIndex        =   28
      Top             =   2400
      Width           =   855
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Level 6 Supers"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   24
      Left            =   3120
      TabIndex        =   26
      Top             =   1920
      Width           =   1335
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Destroy Item"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   23
      Left            =   3120
      TabIndex        =   25
      Top             =   1680
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Drop an Item"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   22
      Left            =   3120
      TabIndex        =   24
      Top             =   1440
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Eject their CD ^_^"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H0080FF80&
      Height          =   255
      Index           =   17
      Left            =   1560
      TabIndex        =   19
      Top             =   2400
      Width           =   1575
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Cuss Words!!"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   6
      Left            =   120
      TabIndex        =   8
      Top             =   1920
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Full SP"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   21
      Left            =   3120
      TabIndex        =   23
      Top             =   1200
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Zero SP"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   20
      Left            =   3120
      TabIndex        =   22
      Top             =   960
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Kill Everyone"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   18
      Left            =   3120
      TabIndex        =   20
      Top             =   480
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Banish to Void"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   25
      Left            =   3120
      TabIndex        =   27
      Top             =   2160
      Width           =   1335
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Restore HP"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   19
      Left            =   3120
      TabIndex        =   21
      Top             =   720
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Hyper Someone"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   16
      Left            =   1560
      TabIndex        =   18
      Top             =   2160
      Width           =   1455
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Barrier Someone"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   15
      Left            =   1560
      TabIndex        =   17
      Top             =   1920
      Width           =   1455
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Life Someone"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   14
      Left            =   1560
      TabIndex        =   16
      Top             =   1680
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Kill Someone"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   13
      Left            =   1560
      TabIndex        =   15
      Top             =   1440
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Activate Kamek"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   12
      Left            =   1560
      TabIndex        =   14
      Top             =   1200
      Width           =   1455
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Unpause"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   11
      Left            =   1560
      TabIndex        =   13
      Top             =   960
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Pause Battle"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   10
      Left            =   1560
      TabIndex        =   12
      Top             =   720
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "ShowReveal"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Index           =   9
      Left            =   1560
      TabIndex        =   11
      Top             =   480
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Push a Button"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H0080FFFF&
      Height          =   255
      Index           =   7
      Left            =   120
      TabIndex        =   9
      Top             =   2160
      Width           =   1335
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Obliterate"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H000000FF&
      Height          =   255
      Index           =   8
      Left            =   120
      TabIndex        =   10
      Top             =   2400
      Width           =   975
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Super Nova"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   5
      Left            =   120
      TabIndex        =   7
      Top             =   1680
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Speed Test"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   4
      Left            =   120
      TabIndex        =   6
      Top             =   1440
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Crash Windows"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   3
      Left            =   120
      TabIndex        =   5
      Top             =   1200
      Width           =   1455
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Linna Destroy"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   2
      Left            =   120
      TabIndex        =   4
      Top             =   960
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Ooga Scroll"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   1
      Left            =   120
      TabIndex        =   3
      Top             =   720
      Width           =   1215
   End
   Begin OptionButton oCmd 
      BackColor       =   &H00000000&
      Caption         =   "Shut Off Bot"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   0
      Left            =   120
      TabIndex        =   2
      Top             =   480
      Width           =   1215
   End
   Begin TextBox tYourSN 
      Height          =   285
      Left            =   840
      TabIndex        =   1
      Top             =   120
      Width           =   1215
   End
   Begin Label Label5 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00000000&
      BackStyle       =   0  'Transparent
      Caption         =   "Chat"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   36
      Top             =   3360
      Width           =   375
   End
   Begin Label Label4 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00000000&
      BackStyle       =   0  'Transparent
      Caption         =   "Target"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   240
      TabIndex        =   35
      Top             =   3000
      Width           =   735
   End
   Begin Label Label3 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00000000&
      BackStyle       =   0  'Transparent
      Caption         =   "Host's SN"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   34
      Top             =   2760
      Width           =   855
   End
   Begin Label Label2 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Blue: Only during Battle"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Left            =   2160
      TabIndex        =   33
      Top             =   120
      Width           =   1695
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00000000&
      BackStyle       =   0  'Transparent
      Caption         =   "Your SN"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   0
      Top             =   120
      Width           =   735
   End
End
Option Explicit

Sub cLocal_Click ()
Dim T%, C#, S$, X%, Pin%
  Pin = Rand(1000, 9999)
  T = SNKeyCode(tYourSN.Text, Pin)
  For X = 0 To 29
    If oCmd(X).Value = True Then GoTo Okay1
  Next X
  MsgBox "No command selected", 16, "ChUB GodMode"
  Exit Sub
Okay1: X = X + 1
  C# = CommandNum(T, X)
  S$ = "~" + Trim(Str$(Pin)) + "!" + Trim(Str$(T)) + ":" + Trim(Str$(C)) + "|" + tHost.Text + "`" + tTarget.Text
  'MsgBox "Command was " + S$
  tChat.Text = S$
End Sub

Sub cSend_Click ()
Dim T%, C#, S$, X%, Pin%
  Pin = Rand(1000, 9999)
  T = SNKeyCode(tYourSN.Text, Pin)
  For X = 0 To 29
    If oCmd(X).Value = True Then GoTo Okay
  Next X
  MsgBox "No command selected", 16, "ChUB GodMode"
  Exit Sub
Okay: X = X + 1
  C# = CommandNum(T, X)
  S$ = "~" + Trim(Str$(Pin)) + "!" + Trim(Str$(T)) + ":" + Trim(Str$(C)) + "|" + tHost.Text + "`" + tTarget.Text
  'MsgBox "Command was " + S$
  ScrollSend1 (S$)
End Sub

Sub Form_Load ()
 Randomize Timer
  tYourSN = AOLGetUser()
End Sub

Function Rand (ByVal A%, ByVal B%) As Integer
' Returns a Random Number in the range (A..B) inclusive
Dim X As Integer
  X = Int(Rnd * (B - A + 1)) + A
  Rand = X
End Function

Sub tChat_KeyPress (KeyAscii As Integer)
  If KeyAscii = 13 Then
    ScrollSend1 (tChat.Text)
    tChat.Text = ""
  End If
End Sub

