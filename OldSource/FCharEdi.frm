VERSION 2.00
Begin Form fCharEdit 
   BackColor       =   &H00FF0000&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Edit Players."
   ClientHeight    =   1815
   ClientLeft      =   1995
   ClientTop       =   1665
   ClientWidth     =   5535
   ForeColor       =   &H00FFFFFF&
   Height          =   2220
   Icon            =   FCHAREDI.FRX:0000
   Left            =   1935
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   1815
   ScaleWidth      =   5535
   Top             =   1320
   Width           =   5655
   Begin CommandButton cQwikCPU 
      BackColor       =   &H00882826&
      Caption         =   "QuickCPU"
      Height          =   195
      Left            =   1200
      TabIndex        =   17
      Top             =   600
      Width           =   975
   End
   Begin ComboBox cSenshiID 
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H00000000&
      Height          =   315
      Index           =   0
      Left            =   2640
      Style           =   2  'Dropdown List
      TabIndex        =   5
      Top             =   240
      Width           =   2775
   End
   Begin CommandButton Command1 
      Caption         =   "random char"
      Height          =   195
      Left            =   2640
      TabIndex        =   9
      Top             =   600
      Width           =   1215
   End
   Begin TextBox tN 
      Height          =   285
      Left            =   4920
      TabIndex        =   13
      Text            =   "1"
      Top             =   840
      Width           =   495
   End
   Begin HScrollBar hsPNdx 
      Height          =   255
      Left            =   120
      Max             =   10
      Min             =   1
      TabIndex        =   8
      Top             =   840
      Value           =   1
      Width           =   4815
   End
   Begin TextBox tScrNam 
      BackColor       =   &H00FFFFFF&
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Courier New"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00000000&
      Height          =   360
      Index           =   0
      Left            =   120
      TabIndex        =   0
      Top             =   240
      Width           =   1455
   End
   Begin CommandButton cCPU 
      BackColor       =   &H00882826&
      Caption         =   "CPU"
      Height          =   375
      Left            =   1680
      TabIndex        =   6
      Top             =   240
      Width           =   495
   End
   Begin TextBox tNam 
      Height          =   285
      Left            =   480
      TabIndex        =   12
      Text            =   "Good Guys"
      Top             =   1200
      Width           =   4935
   End
   Begin TextBox TID 
      Height          =   285
      Left            =   120
      MaxLength       =   1
      TabIndex        =   16
      Text            =   "S"
      Top             =   1200
      Width           =   255
   End
   Begin CommandButton cAllCPUs 
      Caption         =   "???"
      Height          =   255
      Left            =   1680
      TabIndex        =   15
      Top             =   0
      Visible         =   0   'False
      Width           =   495
   End
   Begin CommandButton cEZTeams 
      Caption         =   "EZTeams"
      Height          =   255
      Left            =   2640
      TabIndex        =   11
      Top             =   0
      Width           =   855
   End
   Begin CommandButton Command2 
      Caption         =   "random CPU"
      Height          =   195
      Left            =   4320
      TabIndex        =   10
      Top             =   600
      Width           =   1095
   End
   Begin CommandButton mDone 
      Caption         =   "E&xit"
      Height          =   255
      Left            =   4800
      TabIndex        =   7
      Top             =   0
      Width           =   735
   End
   Begin TextBox tTeamID 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H00000000&
      Height          =   285
      Index           =   0
      Left            =   2280
      MaxLength       =   1
      TabIndex        =   1
      Top             =   240
      Width           =   255
   End
   Begin Label StatLine 
      Alignment       =   2  'Center
      BackColor       =   &H00C0C0C0&
      Caption         =   "Player Editor"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   0
      TabIndex        =   14
      Top             =   1560
      Width           =   5535
   End
   Begin Label LScrNam 
      Alignment       =   2  'Center
      BackColor       =   &H00FF0000&
      Caption         =   "Screen Name"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   4
      Top             =   0
      Width           =   1455
   End
   Begin Label LTeamID 
      Alignment       =   2  'Center
      BackColor       =   &H00FF0000&
      Caption         =   "Team"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2160
      TabIndex        =   3
      Top             =   0
      Width           =   495
   End
   Begin Label LSenshiID 
      Alignment       =   2  'Center
      BackColor       =   &H00FF0000&
      Caption         =   "Character"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3480
      TabIndex        =   2
      Top             =   0
      Width           =   1215
   End
End

' FCHAREDI.FRM
' Provides a means of easily editing Characters
' NOTE!!! If you want to add more MaxPlayers, then you must add more
' edit boxes here and status boxes in FGAMESTA.FRM. It's just cut-and-paste
' really...

Dim Temp As CharType

Dim PNdx As Integer

Const MyName = "fCheOld"

Option Explicit

Sub cAllCPUs_Click ()
  x50CPU
End Sub

Sub cCPU_Click ()
  Temp3 = PNdx
  Unload fCPUEdit
  Load fCPUEdit
End Sub

Sub cCPU_MouseMove (Button As Integer, Shift As Integer, x As Single, y As Single)
  StatLine = "Make 'em a CPU!"
End Sub

Sub cEZTeams_Click ()
Dim NumT As String
Dim x As Integer
Dim Td As String
  NumT = kDlgBoxInput("Enter highest team letter (up to Z)", "EZ-Teams", "B")
  If (Asc(NumT) < Asc("A")) Or (Asc(NumT) > Asc("Z")) Then Exit Sub
  x = 1
  Td = "A"
  Do
    p(x).TeamID = TrimStr(Td)
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

Sub cEZTeams_MouseMove (Button As Integer, Shift As Integer, x As Single, y As Single)
  StatLine = "Sets up teams quickly and easily"
End Sub

Sub Command1_Click ()
  p(PNdx).CharID = Rand(1, HighSenshi)
  UpdateWin
End Sub

Sub Command1_MouseMove (Button As Integer, Shift As Integer, x As Single, y As Single)
  StatLine = "Give 'em a random character"
End Sub

Sub Command2_Click ()
  p(PNdx).CharID = Rand(1, HighSenshi)
  p(PNdx).CPU = 1
  p(PNdx).ScrNam = Senshi(p(PNdx).CharID).FullName
  p(PNdx).TeamID = Chr$(PNdx + 64)
  UpdateWin
End Sub

Sub Command2_MouseMove (Button As Integer, Shift As Integer, x As Single, y As Single)
  StatLine = "Make a random CPU"
End Sub

Sub cQwikCPU_Click ()
  p(PNdx).CPU = 1
  p(PNdx).ScrNam = Senshi(p(PNdx).CharID).FullName
  p(PNdx).TeamID = Chr$(PNdx + 64)
  UpdateWin
End Sub

Sub cSenshiID_Change (Index As Integer)
'  cSenshiID_Click
End Sub

Sub cSenshiID_Click (Index As Integer)
Dim x As Integer
  x = cSenshiID(Index).ListIndex
  If (x <= HighSenshi) Or (GodMode) Then
    p(PNdx).CharID = x
  End If
  InitMoves (PNdx)
  UpdateWin
End Sub

Sub Form_Load ()
Dim x, X2 As Integer
  LoadPosition Me, MyName
  'For X = 1 To Maxplayers
    For X2 = 0 To NumSenshi ' Set up the Combo Box
      cSenshiID(0).AddItem Senshi(X2).FullName
    Next X2
  'Next X
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3
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

Sub Form_MouseDown (Button As Integer, Shift As Integer, x As Single, y As Single)
  If Button = 2 Then cAllCPUs.Visible = True
End Sub

Sub Form_MouseMove (Button As Integer, Shift As Integer, x As Single, y As Single)
  StatLine = "Player Editor"
End Sub

Sub Form_Unload (Cancel As Integer)
  SavePosition Me, MyName

End Sub

Sub hsPNdx_Change ()
  PNdx = hsPNdx
  tN = TrimStr(hsPNdx)
  UpdateWin
End Sub

Sub mDone_Click ()
  Me.Hide
End Sub

Sub mDone_MouseMove (Button As Integer, Shift As Integer, x As Single, y As Single)
  StatLine = "Outta here!"
End Sub

Sub StatLine_MouseMove (Button As Integer, Shift As Integer, x As Single, y As Single)
  StatLine = "What are you doing?"
End Sub

Sub tFragLimit_MouseMove (Button As Integer, Shift As Integer, x As Single, y As Single)
  StatLine = "Change frag limit"
End Sub

Sub TID_Change ()
On Error GoTo Blah
  TNam.Text = TName(Asc(TID))
Blah: Exit Sub
End Sub

Sub tN_Change ()
  If Val(tN) <= hsPNdx.Max And Val(tN) >= hsPNdx.Min Then hsPNdx = Val(tN)
End Sub

Sub tN_MouseMove (Button As Integer, Shift As Integer, x As Single, y As Single)
  StatLine = "Change player"
End Sub

Sub tNam_Change ()
  TName(Asc(TID)) = TNam
End Sub

Sub tScrNam_Change (Index As Integer)
  p(PNdx).ScrNam = tScrNam(Index).Text
End Sub

Sub tScrNam_MouseDown (Index As Integer, Button As Integer, Shift As Integer, x As Single, y As Single)
  If (GodMode) And (Button = 2) Then
    If kDlgBoxfn("Make " + p(Index + 1).ScrNam + " GODLY?", 36, "GOD Mode") = 6 And (p(Index + 1).God = False) Then
      p(Index + 1).God = True
      Send (p(Index + 1).ScrNam + " has a magical Ribbon...")
    ElseIf (p(Index + 1).God = True) Then
      p(Index + 1).God = False
      Send ("Sally comes and takes " + p(Index + 1).ScrNam + "'s Ribbon away!")
    End If
  End If
End Sub

Sub tScrNam_MouseMove (Index As Integer, Button As Integer, Shift As Integer, x As Single, y As Single)
  StatLine = "Edit screen name"
End Sub

Sub tTeamID_Change (Index As Integer)
  tTeamID(Index).Text = UCase(tTeamID(Index).Text)
  p(PNdx).TeamID = tTeamID(Index).Text
  TID = tTeamID(Index).Text
End Sub

Sub tTeamID_MouseMove (Index As Integer, Button As Integer, Shift As Integer, x As Single, y As Single)
  StatLine = "Change their team"
End Sub

Sub x50CPU ()
Dim x As Integer
Dim y%, F%
  If MaxPlayers > NumSenshi Then MaxPlayers = NumSenshi
  For x = 1 To MaxPlayers
    Do
      p(x).CharID = Rand(1, NumSenshi)
      F = 0
      For y = 1 To x - 1
        If p(y).CharID = p(x).CharID Then
          F = 1
          Exit For
        End If
      Next y
    Loop Until (F = 0)
    p(x).CPU = 1
    p(x).ScrNam = Senshi(p(x).CharID).FullName
    p(x).Wrath = 0
    p(x).Goodwill = 100
    p(x).Greed = 25
    p(x).Arrogance = 0
    p(x).TeamID = Chr$(x + 64)
  Next x
  UpdateWin
End Sub

