VERSION 5.00
Begin VB.Form fArena 
   Appearance      =   0  'Flat
   BackColor       =   &H00400040&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Pick Arena"
   ClientHeight    =   3960
   ClientLeft      =   45
   ClientTop       =   2760
   ClientWidth     =   4365
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00FFFFFF&
   Icon            =   "FARENA.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3960
   ScaleWidth      =   4365
   Begin VB.CommandButton cDone 
      Appearance      =   0  'Flat
      Caption         =   "&This One"
      Default         =   -1  'True
      Height          =   495
      Left            =   1320
      TabIndex        =   3
      Top             =   3120
      Width           =   1695
   End
   Begin VB.CommandButton cNext 
      Appearance      =   0  'Flat
      Caption         =   "&Next"
      Height          =   495
      Left            =   3480
      TabIndex        =   2
      Top             =   3120
      Width           =   735
   End
   Begin VB.CommandButton cPrev 
      Appearance      =   0  'Flat
      Caption         =   "&Prev"
      Height          =   495
      Left            =   120
      TabIndex        =   1
      Top             =   3120
      Width           =   735
   End
   Begin VB.Label LName 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      BackStyle       =   0  'Transparent
      Caption         =   "Hoser"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   18
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   -1  'True
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF00FF&
      Height          =   495
      Left            =   0
      TabIndex        =   0
      Top             =   120
      Width           =   4335
   End
   Begin VB.Label StatLine 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Arena Selection"
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
      Left            =   0
      TabIndex        =   14
      Top             =   3720
      Width           =   4485
   End
   Begin VB.Label LDesc 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C000C0&
      BackStyle       =   0  'Transparent
      Caption         =   "Blah blah blah blah blah blah blah blah blah blah"
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   9
      Left            =   120
      TabIndex        =   13
      Top             =   2760
      Width           =   4150
   End
   Begin VB.Label LDesc 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C000C0&
      BackStyle       =   0  'Transparent
      Caption         =   "Blah blah blah blah blah blah blah blah blah blah"
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   8
      Left            =   120
      TabIndex        =   12
      Top             =   2520
      Width           =   4150
   End
   Begin VB.Label LDesc 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C000C0&
      BackStyle       =   0  'Transparent
      Caption         =   "Blah blah blah blah blah blah blah blah blah blah"
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   7
      Left            =   120
      TabIndex        =   11
      Top             =   2280
      Width           =   4150
   End
   Begin VB.Label LDesc 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C000C0&
      BackStyle       =   0  'Transparent
      Caption         =   "Blah blah blah blah blah blah blah blah blah blah"
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   6
      Left            =   120
      TabIndex        =   10
      Top             =   2040
      Width           =   4150
   End
   Begin VB.Label LDesc 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C000C0&
      BackStyle       =   0  'Transparent
      Caption         =   "Blah blah blah blah blah blah blah blah blah blah"
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   5
      Left            =   120
      TabIndex        =   9
      Top             =   1800
      Width           =   4150
   End
   Begin VB.Label LDesc 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C000C0&
      BackStyle       =   0  'Transparent
      Caption         =   "Blah blah blah blah blah blah blah blah blah blah"
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   4
      Left            =   120
      TabIndex        =   8
      Top             =   1560
      Width           =   4150
   End
   Begin VB.Label LDesc 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C000C0&
      BackStyle       =   0  'Transparent
      Caption         =   "Blah blah blah blah blah blah blah blah blah blah"
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   3
      Left            =   120
      TabIndex        =   7
      Top             =   1320
      Width           =   4150
   End
   Begin VB.Label LDesc 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C000C0&
      BackStyle       =   0  'Transparent
      Caption         =   "Blah blah blah blah blah blah blah blah blah blah"
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   2
      Left            =   120
      TabIndex        =   6
      Top             =   1080
      Width           =   4150
   End
   Begin VB.Label LDesc 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C000C0&
      BackStyle       =   0  'Transparent
      Caption         =   "Blah blah blah blah blah blah blah blah blah blah"
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   1
      Left            =   120
      TabIndex        =   5
      Top             =   840
      Width           =   4150
   End
   Begin VB.Label LDesc 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C000C0&
      BackStyle       =   0  'Transparent
      Caption         =   "Blah blah blah blah blah blah blah blah blah blah"
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   0
      Left            =   120
      TabIndex        =   4
      Top             =   600
      Width           =   4150
   End
End
Attribute VB_Name = "fArena"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

' FARENA.FRM
' Enables the selection of an Arena
' Mostly self-explanatory.

Option Explicit

Const MyName = "fArena"

Private Sub cDone_Click()
  Unload Me
End Sub

Private Sub cDone_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Pick This Arena"
End Sub

Private Sub cNext_Click()
  CurArena = CurArena + 1
  If (CurArena = MaxArena) Then
    cNext.Enabled = False
  End If
  If (CurArena > 1) Then
    cPrev.Enabled = True
  End If
  UpdateArenaWind
End Sub

Private Sub cNext_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Next Arena"
End Sub

Private Sub cPrev_Click()
  CurArena = CurArena - 1
  If (CurArena = 1) Then
    cPrev.Enabled = False
  End If
  If (CurArena < MaxArena) Then
    cNext.Enabled = True
  End If
  UpdateArenaWind
End Sub

Private Sub cPrev_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Previous Arena"
End Sub

Private Sub Form_Load()
  LoadPosition Me, MyName
  SetWindowPos fArena.hWnd, -1, 0, 0, 0, 0, 3
  UpdateArenaWind
End Sub

Private Sub Form_Unload(Cancel As Integer)
  SavePosition Me, MyName
End Sub

Private Sub LDesc_MouseMove(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Arena Description."
End Sub

Private Sub StatLine_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "I am made of Phued, no?"
End Sub

Private Sub UpdateArenaWind()
Dim X As Integer
  If (CurArena > MaxArena) Then CurArena = MaxArena
  If (CurArena < 1) Then CurArena = 1
  For X = 1 To 10
    LDesc(X - 1).Caption = Arena(CurArena).Desc(X)
  Next X
  LName.Caption = Arena(CurArena).Name
  fOption!LArenaName.Caption = Arena(CurArena).Name
End Sub

