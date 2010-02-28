VERSION 5.00
Begin VB.Form fGod 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "ChUB 2000 SE: GodMode"
   ClientHeight    =   3690
   ClientLeft      =   1260
   ClientTop       =   1515
   ClientWidth     =   6465
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H80000008&
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3690
   ScaleWidth      =   6465
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox tPIN 
      Alignment       =   2  'Center
      Height          =   285
      Left            =   3960
      MaxLength       =   4
      TabIndex        =   49
      TabStop         =   0   'False
      Text            =   "6666"
      Top             =   120
      Width           =   615
   End
   Begin VB.TextBox tKey 
      Height          =   285
      Left            =   5760
      TabIndex        =   48
      Text            =   "0"
      Top             =   120
      Width           =   615
   End
   Begin VB.CommandButton cAdvertise 
      Caption         =   "Advertise"
      Height          =   255
      Left            =   4560
      TabIndex        =   47
      Top             =   3000
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Command 36"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000D&
      Height          =   255
      Index           =   35
      Left            =   4440
      TabIndex        =   46
      Top             =   2400
      Width           =   1935
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Command 35"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000D&
      Height          =   255
      Index           =   34
      Left            =   4440
      TabIndex        =   45
      Top             =   2160
      Width           =   1935
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Command 34"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000D&
      Height          =   255
      Index           =   33
      Left            =   4440
      TabIndex        =   44
      Top             =   1920
      Width           =   1935
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Eternal HostBoot"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000D&
      Height          =   255
      Index           =   32
      Left            =   4440
      TabIndex        =   43
      Top             =   1680
      Width           =   1935
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Eternal HostSave"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000D&
      Height          =   255
      Index           =   31
      Left            =   4440
      TabIndex        =   42
      Top             =   1440
      Width           =   1935
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Destroy their Weapon"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   30
      Left            =   4440
      TabIndex        =   41
      Top             =   1200
      Width           =   1935
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Destroy Dropped Wpns."
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   29
      Left            =   4440
      TabIndex        =   40
      Top             =   960
      Width           =   2175
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Add Dropped Weapon"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   28
      Left            =   4440
      TabIndex        =   39
      Top             =   720
      Width           =   1935
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Frostbite"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   27
      Left            =   4440
      TabIndex        =   38
      Top             =   480
      Width           =   975
   End
   Begin VB.CommandButton cLocal 
      Caption         =   "Local"
      Height          =   255
      Left            =   3720
      TabIndex        =   37
      Top             =   3000
      Width           =   735
   End
   Begin VB.TextBox tChat 
      Height          =   285
      Left            =   480
      TabIndex        =   32
      Top             =   3360
      Width           =   5895
   End
   Begin VB.CommandButton cSend 
      Caption         =   "SEND!"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   13.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   2400
      TabIndex        =   31
      Top             =   2760
      Width           =   1215
   End
   Begin VB.TextBox tTarget 
      Height          =   285
      Left            =   1080
      TabIndex        =   30
      Top             =   3000
      Width           =   1215
   End
   Begin VB.TextBox tHost 
      Height          =   285
      Left            =   1080
      TabIndex        =   29
      Top             =   2760
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Rot-13"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   26
      Left            =   3120
      TabIndex        =   28
      Top             =   2400
      Width           =   855
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Level 6 Supers"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   24
      Left            =   3120
      TabIndex        =   26
      Top             =   1920
      Width           =   1455
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Destroy Item"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   23
      Left            =   3120
      TabIndex        =   25
      Top             =   1680
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Drop an Item"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   22
      Left            =   3120
      TabIndex        =   24
      Top             =   1440
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Eject their CD ^_^"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   17
      Left            =   1560
      TabIndex        =   19
      Top             =   2400
      Width           =   1695
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Cuss Words!!"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   6
      Left            =   120
      TabIndex        =   8
      Top             =   1920
      Width           =   1455
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Full SP"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   21
      Left            =   3120
      TabIndex        =   23
      Top             =   1200
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Zero SP"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   20
      Left            =   3120
      TabIndex        =   22
      Top             =   960
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Kill Everyone"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   18
      Left            =   3120
      TabIndex        =   20
      Top             =   480
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Banish to Void"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   25
      Left            =   3120
      TabIndex        =   27
      Top             =   2160
      Width           =   1335
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Restore HP"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   19
      Left            =   3120
      TabIndex        =   21
      Top             =   720
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Hyper Someone"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   16
      Left            =   1560
      TabIndex        =   18
      Top             =   2160
      Width           =   1455
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Barrier Someone"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   15
      Left            =   1560
      TabIndex        =   17
      Top             =   1920
      Width           =   1575
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Life Someone"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   14
      Left            =   1560
      TabIndex        =   16
      Top             =   1680
      Width           =   1455
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Kill Someone"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   13
      Left            =   1560
      TabIndex        =   15
      Top             =   1440
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Activate Kamek"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   12
      Left            =   1560
      TabIndex        =   14
      Top             =   1200
      Width           =   1455
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Unpause"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   11
      Left            =   1560
      TabIndex        =   13
      Top             =   960
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Pause Battle"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   10
      Left            =   1560
      TabIndex        =   12
      Top             =   720
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "ShowReveal"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Index           =   9
      Left            =   1560
      TabIndex        =   11
      Top             =   480
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Push a Button"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   7
      Left            =   120
      TabIndex        =   9
      Top             =   2160
      Width           =   1335
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Obliterate"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   8
      Left            =   120
      TabIndex        =   10
      Top             =   2400
      Width           =   975
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Super Nova"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   5
      Left            =   120
      TabIndex        =   7
      Top             =   1680
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Speed Test"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   4
      Left            =   120
      TabIndex        =   6
      Top             =   1440
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Crash Windows"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   3
      Left            =   120
      TabIndex        =   5
      Top             =   1200
      Width           =   1455
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Linna Destroy"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   2
      Left            =   120
      TabIndex        =   4
      Top             =   960
      Width           =   1455
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Ooga Scroll"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   1
      Left            =   120
      TabIndex        =   3
      Top             =   720
      Width           =   1215
   End
   Begin VB.OptionButton oCmd 
      Caption         =   "Shut Off Bot"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   0
      Left            =   120
      TabIndex        =   2
      Top             =   480
      Width           =   1215
   End
   Begin VB.TextBox tYourSN 
      Height          =   285
      Left            =   840
      TabIndex        =   1
      Top             =   120
      Width           =   1215
   End
   Begin VB.Label Label6 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "God Key"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   4920
      TabIndex        =   50
      Top             =   120
      Width           =   735
   End
   Begin VB.Label Label5 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Chat"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   0
      TabIndex        =   36
      Top             =   3360
      Width           =   375
   End
   Begin VB.Label Label4 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Target"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   240
      TabIndex        =   35
      Top             =   3000
      Width           =   735
   End
   Begin VB.Label Label3 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Host's SN"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   120
      TabIndex        =   34
      Top             =   2760
      Width           =   855
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Blue: Only during Battle"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   255
      Left            =   2160
      TabIndex        =   33
      Top             =   120
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Your SN"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   0
      TabIndex        =   0
      Top             =   120
      Width           =   735
   End
End
Attribute VB_Name = "fGod"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cAdvertise_Click()
  ScrollSend1 ("<---- is using ChUB 2000 GodMode! Look out!")
End Sub

Private Sub cLocal_Click()
Dim T%, C#, S$, X%, PIN%
  PIN = Val(tPIN.Text)
  T = SNKeyCode(tYourSN.Text, PIN)
  For X = 0 To 29
    If oCmd(X).Value = True Then GoTo Okay1
  Next X
  MsgBox "No command selected", 16, "ChUB GodMode"
  Exit Sub
Okay1: X = X + 1
  C# = CommandNum(T, X)
  S$ = "~" + Trim(Str$(PIN)) + "!" + Trim(Str$(T)) + ":" + Trim(Str$(C)) + "|" + tHost.Text + "`" + tTarget.Text
  'MsgBox "Command was " + S$
  tChat.Text = S$
  PIN = Rand(1000, 9999)
  tPIN.Text = Trim(Str$((PIN)))
End Sub

Private Sub cSend_Click()
Dim T%, C#, S$, X%, PIN%
  PIN = Val(tPIN.Text)
  T = SNKeyCode(tYourSN.Text, PIN)
  For X = 0 To 35
    If oCmd(X).Value = True Then GoTo Okay
  Next X
  MsgBox "No command selected", 16, "ChUB GodMode"
  Exit Sub
Okay: X = X + 1
  C# = CommandNum(T, X)
  S$ = "~" + Trim(Str$(PIN)) + "!" + Trim(Str$(T)) + ":" + Trim(Str$(C)) + "|" + tHost.Text + "`" + tTarget.Text
  'MsgBox "Command was " + S$
  ScrollSend1 (S$)
  PIN = Rand(1000, 9999)
  'tPIN.Text = Trim(Str$((PIN)))
End Sub

Private Sub Form_Load()
 Randomize Timer
  tYourSN = AOLGetUser()
End Sub

Private Function Rand(ByVal a%, ByVal B%) As Integer
' Returns a Random Number in the range (A..B) inclusive
Dim X As Integer
  X = Int(Rnd * (B - a + 1)) + a
  Rand = X
End Function

Private Sub tChat_KeyPress(KeyAscii As Integer)
  If KeyAscii = 13 Then
    ScrollSend1 (tChat.Text)
    tChat.Text = ""
  End If
End Sub

