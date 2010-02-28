VERSION 5.00
Begin VB.Form fCharEdit 
   Appearance      =   0  'Flat
   BackColor       =   &H00FF0000&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Edit Players."
   ClientHeight    =   1290
   ClientLeft      =   1995
   ClientTop       =   1665
   ClientWidth     =   5820
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
   Icon            =   "FCHAREDI.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   86
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   388
   Begin VB.VScrollBar hsPNdx 
      Height          =   1095
      Left            =   5520
      Max             =   10
      Min             =   1
      TabIndex        =   16
      Top             =   120
      Value           =   1
      Width           =   255
   End
   Begin VB.CommandButton cQwikCPU 
      Appearance      =   0  'Flat
      BackColor       =   &H00882826&
      Caption         =   "QuickCPU"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   1080
      TabIndex        =   15
      ToolTipText     =   "Quickly make this char a CPU of whatever character is shown"
      Top             =   600
      Width           =   1095
   End
   Begin VB.ComboBox cSenshiID 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BeginProperty Font 
         Name            =   "Arial Narrow"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   345
      Index           =   0
      Left            =   2640
      Style           =   2  'Dropdown List
      TabIndex        =   5
      ToolTipText     =   "Pick the person's character"
      Top             =   240
      Width           =   2775
   End
   Begin VB.CommandButton Command1 
      Appearance      =   0  'Flat
      Caption         =   "random char"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   2640
      TabIndex        =   8
      ToolTipText     =   "Randomly selects a character"
      Top             =   600
      Width           =   1215
   End
   Begin VB.TextBox tScrNam 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BeginProperty Font 
         Name            =   "Arial Narrow"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   360
      Index           =   0
      Left            =   120
      TabIndex        =   0
      Text            =   "Chibot Hoser Nough"
      ToolTipText     =   "The player's nick"
      Top             =   240
      Width           =   1455
   End
   Begin VB.CommandButton cCPU 
      Appearance      =   0  'Flat
      BackColor       =   &H00882826&
      Caption         =   "CPU"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   1560
      TabIndex        =   6
      Top             =   240
      Width           =   615
   End
   Begin VB.TextBox tNam 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   480
      TabIndex        =   11
      Text            =   "Good Guys"
      ToolTipText     =   "Team Name"
      Top             =   840
      Width           =   4935
   End
   Begin VB.TextBox TID 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   120
      MaxLength       =   1
      TabIndex        =   14
      Text            =   "S"
      ToolTipText     =   "Team to change name"
      Top             =   840
      Width           =   255
   End
   Begin VB.CommandButton cAllCPUs 
      Appearance      =   0  'Flat
      Caption         =   "???"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   1680
      TabIndex        =   13
      ToolTipText     =   "Makes all characters randomly-selected CPUs"
      Top             =   0
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.CommandButton cEZTeams 
      Appearance      =   0  'Flat
      Caption         =   "EZTeams"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   2640
      TabIndex        =   10
      Top             =   0
      Width           =   975
   End
   Begin VB.CommandButton Command2 
      Appearance      =   0  'Flat
      Caption         =   "random CPU"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   4200
      TabIndex        =   9
      ToolTipText     =   "Randomly selects a character and makes it a CPU"
      Top             =   600
      Width           =   1215
   End
   Begin VB.CommandButton mDone 
      Appearance      =   0  'Flat
      Caption         =   "E&xit"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   4680
      TabIndex        =   7
      Top             =   0
      Width           =   735
   End
   Begin VB.TextBox tTeamID 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H00000000&
      Height          =   285
      Index           =   0
      Left            =   2280
      MaxLength       =   1
      TabIndex        =   1
      ToolTipText     =   "Place this player on a team"
      Top             =   240
      Width           =   255
   End
   Begin VB.Label StatLine 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Player Editor"
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
      TabIndex        =   12
      Top             =   1560
      Width           =   5535
   End
   Begin VB.Label LScrNam 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00FF0000&
      BackStyle       =   0  'Transparent
      Caption         =   "Nick"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   4
      Top             =   0
      Width           =   1455
   End
   Begin VB.Label LTeamID 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00FF0000&
      BackStyle       =   0  'Transparent
      Caption         =   "Team"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2160
      TabIndex        =   3
      Top             =   0
      Width           =   495
   End
   Begin VB.Label LSenshiID 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00FF0000&
      BackStyle       =   0  'Transparent
      Caption         =   "Character"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3480
      TabIndex        =   2
      Top             =   0
      Width           =   1215
   End
End
Attribute VB_Name = "fCharEdit"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

' FCHAREDI.FRM
' Provides a means of easily editing Characters
' NOTE!!! If you want to add more MaxPlayers, then you must add more
' edit boxes here and status boxes in FGAMESTA.FRM. It's just cut-and-paste
' really...

Dim temp As CharType

Dim PNdx As Integer

Const MyName = "fCheOld"

Option Explicit

Private Sub cAllCPUs_Click()
  x50CPU
End Sub

Private Sub cAllCPUs_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhChar_AllCPUs)
End Sub

Private Sub cCPU_Click()
  Temp3 = PNdx
  Unload fCPUEdit
  Load fCPUEdit
End Sub

Private Sub cCPU_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Make 'em a CPU!"
End Sub

Private Sub cCPU_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi yhChar_CPU
End Sub

Private Sub cEZTeams_Click()
Dim NumT As String
Dim x As Integer
Dim Td As String
  NumT = kDlgBoxInput("Enter highest team letter (up to Z)", "EZ-Teams", "B")
  If (Asc(NumT) < Asc("A")) Or (Asc(NumT) > Asc("Z")) Then Exit Sub
  x = 1
  Td = "A"
  Do
    P(x).TeamID = Td
    x = x + 1
    Td = Chr$(Asc(Td) + 1)
    If (Td > NumT) Then Td = "A"
  Loop Until (x > MaxPlayers)
  If (Td <> "A") Then
    kDlgBox "Warning! Teams are unbalanced by number.", 48, "EZ-Teams"
  Else
    kDlgBox "Teams evenly distributed!", 64, "EZ-Teams"
  End If
  UpdateWin
End Sub

Private Sub cEZTeams_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Sets up teams quickly and easily"
End Sub

Private Sub cEZTeams_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi yhChar_EZTeams
End Sub

Private Sub Command1_Click()
  P(PNdx).CharID = Rand(1, HighSenshi)
  UpdateWin
End Sub

Private Sub Command1_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Give 'em a random character"
End Sub

Private Sub Command1_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi yhChar_RandChar
End Sub

Private Sub Command2_Click()
  P(PNdx).CharID = Rand(1, HighSenshi)
  P(PNdx).CPU = 1
  P(PNdx).ScrNam = Senshi(P(PNdx).CharID).FullName
  P(PNdx).TeamID = Chr$(PNdx + 64)
  P(PNdx).Arrogance = 1
  P(PNdx).Wrath = Rand(0, 100)
  P(PNdx).Goodwill = 100
  P(PNdx).Greed = Rand(25, 100)
  UpdateWin
End Sub

Private Sub Command2_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Make a random CPU"
End Sub

Private Sub Command2_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Shift And Button = 2 Then Yoshi yhChar_RandCPU
End Sub

Private Sub cQwikCPU_Click()
  P(PNdx).CPU = 1
  P(PNdx).ScrNam = Senshi(P(PNdx).CharID).FullName
  P(PNdx).TeamID = Chr$(PNdx + 64)
  P(PNdx).Arrogance = 1
  P(PNdx).Wrath = Rand(0, 100)
  P(PNdx).Goodwill = 100
  P(PNdx).Greed = Rand(25, 100)
  UpdateWin
End Sub

Private Sub cQwikCPU_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi yhChar_QwikCPU
End Sub

Private Sub cSenshiID_Change(index As Integer)
'  cSenshiID_Click
End Sub

Private Sub cSenshiID_Click(index As Integer)
Dim x As Integer
  x = cSenshiID(index).ListIndex
  If (x <= HighSenshi) Or (Godmode) Then
    P(PNdx).CharID = x
  End If
  InitMoves (PNdx)
  UpdateWin
End Sub

Private Sub Form_Load()
Dim x, x2 As Integer
  LoadPosition Me, MyName
  'For X = 1 To Maxplayers
    For x2 = 0 To HighSenshi ' Set up the Combo Box
      cSenshiID(0).AddItem Senshi(x2).FullName
    Next x2
  'Next X
  SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3
  'cFFA.AddItem "Teams"
  'cFFA.AddItem "Capture the Flag"
  'cFFA.AddItem "ProgYma"
  'cFFA.AddItem "Classic FFA"
  'cFFA.AddItem "Frag FFA"
  'cFFA.AddItem "Respawn FFA"
  'cFFA.AddItem "Fatality FFA"
  PNdx = hsPNdx
  UpdateWin
  
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 Then cAllCPUs.Visible = True
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Player Editor"
End Sub

Private Sub Form_Unload(Cancel As Integer)
  SavePosition Me, MyName
  ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
End Sub

Private Sub hsPNdx_Change()
  PNdx = hsPNdx
  'tN = TrimStr(hsPNdx)
  UpdateWin
End Sub

Private Sub mDone_Click()
  Me.Hide
  ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
End Sub

Private Sub mDone_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Outta here!"
End Sub

Private Sub StatLine_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "What are you doing?"
End Sub

Private Sub tFragLimit_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Change frag limit"
End Sub

Private Sub TID_Change()
On Error GoTo Blah
  tNam.Text = tName(Asc(TID))
Blah: Exit Sub
End Sub

Private Sub TID_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi yhChar_TeamName
End Sub

Private Sub tNam_Change()
  tName(Asc(TID)) = tNam
End Sub

Private Sub tScrNam_Change(index As Integer)
  P(PNdx).ScrNam = tScrNam(index).Text
End Sub

Private Sub tScrNam_MouseDown(index As Integer, Button As Integer, Shift As Integer, x As Single, Y As Single)
  If (Godmode) And (Button = 2) Then
    If kDlgBoxfn("Make " + P(index + 1).ScrNam + " GODLY?", 36, "GOD Mode") = 6 And (P(index + 1).God = False) Then
      P(index + 1).God = True
      Send (P(index + 1).ScrNam + " has a magical Ribbon...")
    ElseIf (P(index + 1).God = True) Then
      P(index + 1).God = False
      Send ("Sally comes and takes " + P(index + 1).ScrNam + "'s Ribbon away!")
    End If
  End If
End Sub

Private Sub tScrNam_MouseMove(index As Integer, Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Edit screen name"
End Sub

Private Sub tScrNam_MouseUp(index As Integer, Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 1 And Shift Then Yoshi yhChar_ScrNam
End Sub

Private Sub tTeamID_Change(index As Integer)
  tTeamID(index).Text = UCase(tTeamID(index).Text)
  P(PNdx).TeamID = tTeamID(index).Text
  TID = tTeamID(index).Text
End Sub

Private Sub tTeamID_MouseMove(index As Integer, Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Change their team"
End Sub

Private Sub x50CPU()
Dim x As Integer
Dim Y%, F%
  If MaxPlayers > HighSenshi Then MaxPlayers = HighSenshi
  For x = 1 To MaxPlayers
    Do
      P(x).CharID = Rand(1, HighSenshi)
      F = 0
      For Y = 1 To x - 1
        If P(Y).CharID = P(x).CharID Then
          F = 1
          Exit For
        End If
      Next Y
    Loop Until (F = 0)
    P(x).CPU = 1
    P(x).ScrNam = Senshi(P(x).CharID).FullName
    P(x).Wrath = 0
    P(x).Goodwill = 100
    P(x).Greed = 25
    P(x).Arrogance = 0
    P(x).TeamID = Chr$(x + 64)
  Next x
  UpdateWin
End Sub

Private Sub tTeamID_MouseUp(index As Integer, Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi yhChar_TeamID
End Sub
