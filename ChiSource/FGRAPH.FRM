VERSION 5.00
Begin VB.Form fGraph 
   Appearance      =   0  'Flat
   BackColor       =   &H00000000&
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "HP/SP Graphs"
   ClientHeight    =   1950
   ClientLeft      =   3090
   ClientTop       =   2400
   ClientWidth     =   5670
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
   Icon            =   "FGRAPH.frx":0000
   LinkTopic       =   "Form2"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   130
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   378
   Begin VB.Timer tGra 
      Interval        =   1000
      Left            =   0
      Top             =   2400
   End
   Begin VB.PictureBox pSP 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H80000008&
      Height          =   735
      Left            =   1200
      ScaleHeight     =   300
      ScaleMode       =   0  'User
      ScaleWidth      =   500
      TabIndex        =   5
      Top             =   1200
      Width           =   4455
   End
   Begin VB.PictureBox pMP 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H80000008&
      Height          =   735
      Left            =   1200
      ScaleHeight     =   300
      ScaleMode       =   0  'User
      ScaleWidth      =   500
      TabIndex        =   4
      Top             =   1920
      Visible         =   0   'False
      Width           =   4455
   End
   Begin VB.PictureBox pHP 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H80000008&
      Height          =   735
      Left            =   1200
      ScaleHeight     =   400
      ScaleMode       =   0  'User
      ScaleWidth      =   500
      TabIndex        =   1
      Top             =   480
      Width           =   4455
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "SP"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   18
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   375
      Left            =   0
      TabIndex        =   2
      Top             =   1320
      Width           =   1095
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "MP"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   18
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   375
      Left            =   0
      TabIndex        =   6
      Top             =   2040
      Visible         =   0   'False
      Width           =   1095
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "HP"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   18
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   375
      Left            =   0
      TabIndex        =   3
      Top             =   600
      Width           =   1095
   End
   Begin VB.Label TL 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "???"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   18
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   495
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   5655
   End
End
Attribute VB_Name = "fGraph"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Const Wid = 75

Dim HP%(Wid)
'Dim MP%(Wid)
Dim SP%(Wid)

Const MyName = "fGraph"

Private Sub Form_Load()
  LoadPosition Me, MyName
  TL = P(Graph).ScrNam
  pHP.ScaleWidth = Wid
  'PMP.ScaleWidth = Wid
  pSP.ScaleWidth = Wid
  pHP.ScaleHeight = MaxHP
  'PMP.ScaleHeight = MaxMP
  pSP.ScaleHeight = 300
  SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3             ' Always on top
  Me.Show
  If Config.NewUser Then Yoshi yhGraph
End Sub

Private Sub Form_Unload(Cancel As Integer)
  SavePosition Me, MyName

End Sub

Private Sub tGra_Timer()
Dim X1%, X2%
Dim C As Long
  If P(Graph).HP < 0 And TL <> P(Graph).ScrNam + ": DECEASED" Then TL = P(Graph).ScrNam + ": DECEASED"
  If Paused Then Exit Sub
  For X1 = 1 To Wid
    pHP.PSet (X1, pHP.ScaleHeight - HP(X1)), pHP.BackColor
    'PMP.PSet (X1, PMP.ScaleHeight - MP(X1)), PMP.BackColor
    pSP.PSet (X1, pSP.ScaleHeight - SP(X1)), pSP.BackColor
    If X1 < Wid Then
      HP(X1) = HP(X1 + 1)
      'MP(X1) = MP(X1 + 1)
      SP(X1) = SP(X1 + 1)
    End If
  Next X1
  DoEvents
  HP(Wid) = P(Graph).HP
  If HP(Wid) > pHP.ScaleHeight Then pHP.ScaleHeight = HP(Wid)
  'MP(Wid) = p(Graph).MP
  'If MP(Wid) > PMP.ScaleHeight Then PMP.ScaleHeight = MP(Wid)
  SP(Wid) = P(Graph).Super
  If SP(Wid) > pSP.ScaleHeight Then pSP.ScaleHeight = SP(Wid)
  'PHP.Cls
  'PMP.Cls
  'PSP.Cls
  For X1 = 1 To Wid
    Select Case HP(X1)
      Case 0 To 100: C = &HFF
      Case 101 To 250: C = QBColor(14)
      Case Else: C = QBColor(15)
    End Select
    pHP.PSet (X1, pHP.ScaleHeight - HP(X1)), C
    C = QBColor(15)
    'PMP.PSet (X1, PMP.ScaleHeight - MP(X1)), C
    pSP.PSet (X1, pSP.ScaleHeight - SP(X1)), C
  Next X1
  DoEvents
End Sub

