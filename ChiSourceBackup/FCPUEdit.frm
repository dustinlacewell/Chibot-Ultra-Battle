VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.1#0"; "COMCTL32.OCX"
Begin VB.Form fCPUEdit 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "CPU Edit"
   ClientHeight    =   1980
   ClientLeft      =   1125
   ClientTop       =   5160
   ClientWidth     =   3525
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00C0C0C0&
   Icon            =   "FCPUEdit.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   1980
   ScaleWidth      =   3525
   Begin ComctlLib.Slider hsGoodwill 
      Height          =   360
      Left            =   720
      TabIndex        =   9
      Top             =   360
      Width           =   1815
      _ExtentX        =   3201
      _ExtentY        =   635
      _Version        =   327680
      MouseIcon       =   "FCPUEdit.frx":030A
      LargeChange     =   10
      Max             =   100
      TickFrequency   =   10
   End
   Begin VB.CheckBox cIsCPU 
      Caption         =   "CPU-Controlled"
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
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   1695
   End
   Begin ComctlLib.Slider hsArrogance 
      Height          =   360
      Left            =   720
      TabIndex        =   10
      Top             =   1440
      Width           =   1815
      _ExtentX        =   3201
      _ExtentY        =   635
      _Version        =   327680
      MouseIcon       =   "FCPUEdit.frx":0326
      LargeChange     =   10
      Max             =   100
      TickFrequency   =   10
   End
   Begin ComctlLib.Slider hsWrath 
      Height          =   360
      Left            =   720
      TabIndex        =   11
      Top             =   1080
      Width           =   1815
      _ExtentX        =   3201
      _ExtentY        =   635
      _Version        =   327680
      MouseIcon       =   "FCPUEdit.frx":0342
      LargeChange     =   10
      Max             =   100
      TickFrequency   =   10
   End
   Begin ComctlLib.Slider hsGreed 
      Height          =   360
      Left            =   720
      TabIndex        =   12
      Top             =   720
      Width           =   1815
      _ExtentX        =   3201
      _ExtentY        =   635
      _Version        =   327680
      MouseIcon       =   "FCPUEdit.frx":035E
      LargeChange     =   10
      Max             =   100
      TickFrequency   =   10
   End
   Begin VB.Label LArrogance 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "100"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   120
      TabIndex        =   8
      Top             =   1440
      Width           =   495
   End
   Begin VB.Label LWrath 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "100"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   120
      TabIndex        =   7
      Top             =   1080
      Width           =   495
   End
   Begin VB.Label LGreed 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "100"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   120
      TabIndex        =   6
      Top             =   720
      Width           =   495
   End
   Begin VB.Label LGoodwill 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "100"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   120
      TabIndex        =   5
      Top             =   360
      Width           =   495
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Arrogance"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   255
      Index           =   3
      Left            =   2640
      TabIndex        =   4
      Top             =   1440
      Width           =   855
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Wrath"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   255
      Index           =   2
      Left            =   2640
      TabIndex        =   3
      Top             =   1080
      Width           =   855
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Greed"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   255
      Index           =   1
      Left            =   2640
      TabIndex        =   2
      Top             =   720
      Width           =   855
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Goodwill"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   255
      Index           =   0
      Left            =   2640
      TabIndex        =   1
      Top             =   360
      Width           =   855
   End
End
Attribute VB_Name = "fCPUEdit"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Const MyName = "fCpuOld"

Private Sub cIsCPU_Click()
  P(Temp3).CPU = cIsCPU.Value
  If (P(Temp3).CPU = 1) Then
    P(Temp3).ScrNam = Senshi(P(Temp3).CharID).FullName
  Else
    P(Temp3).ScrNam = "SN Goes Here"
  End If
  Me.Caption = "CPU Edit (" + P(Temp3).ScrNam + ")"
  UpdateWin
End Sub

Private Sub cSmart_Click()
  'p(Temp3).Smart = cSmart
End Sub

Private Sub cIsCPU_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhCPU_IsCPU)
End Sub

Private Sub Form_Load()
Dim X As Integer
Dim S1 As String
Dim Px As PlayerType
  LoadPosition Me, MyName
  SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3             ' Always on top
  Me.Show
  Me.Caption = "CPU Edit (" + P(Temp3).ScrNam + ")"
  cIsCPU = P(Temp3).CPU
  hsGoodwill = P(Temp3).Goodwill
  hsGreed = P(Temp3).Greed
  hsWrath = P(Temp3).Wrath
  hsArrogance = P(Temp3).Arrogance
  If Config.NewUser Then Yoshi (yhCPUEdit)
End Sub

Private Sub Form_Unload(Cancel As Integer)
  SavePosition Me, MyName

End Sub

Private Sub hsArrogance_Change()
  P(Temp3).Arrogance = hsArrogance
  LArrogance.Caption = TrimStr(hsArrogance)
End Sub

Private Sub hsArrogance_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhCPU_Arrogance)
End Sub

Private Sub hsArrogance_Scroll()
  hsArrogance_Change
End Sub

Private Sub hsGoodwill_Change()
  P(Temp3).Goodwill = hsGoodwill
  LGoodwill.Caption = TrimStr(hsGoodwill)
End Sub

Private Sub hsGoodwill_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhCPU_Goodwill)
End Sub

Private Sub hsGoodwill_Scroll()
  hsGoodwill_Change
End Sub

Private Sub hsGreed_Change()
  P(Temp3).Greed = hsGreed
  LGreed.Caption = TrimStr(hsGreed)
End Sub

Private Sub hsGreed_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhCPU_Greed)
End Sub

Private Sub hsGreed_Scroll()
  hsGreed_Change
End Sub

Private Sub hsWrath_Change()
  P(Temp3).Wrath = hsWrath
  LWrath.Caption = TrimStr(hsWrath)
End Sub

Private Sub hsWrath_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhCPU_Wrath)
End Sub

Private Sub hsWrath_Scroll()
  hsWrath_Change
End Sub

