VERSION 5.00
Begin VB.Form fWav 
   Appearance      =   0  'Flat
   BackColor       =   &H00000000&
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Millenium Wave Player"
   ClientHeight    =   2805
   ClientLeft      =   3585
   ClientTop       =   675
   ClientWidth     =   2910
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
   Icon            =   "FWAV.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2805
   ScaleWidth      =   2910
   Begin VB.TextBox What 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3240
      TabIndex        =   7
      Text            =   "what"
      Top             =   360
      Width           =   1215
   End
   Begin VB.CheckBox Lup 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      Caption         =   "Loop."
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   240
      TabIndex        =   6
      Top             =   2520
      Width           =   855
   End
   Begin VB.Timer tiMidi 
      Interval        =   50
      Left            =   2400
      Top             =   2880
   End
   Begin VB.DirListBox DirBox 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1830
      Left            =   0
      TabIndex        =   4
      Top             =   360
      Width           =   1455
   End
   Begin VB.FileListBox File 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   2175
      Left            =   1440
      Pattern         =   "*.wav"
      TabIndex        =   3
      Top             =   0
      Width           =   1455
   End
   Begin VB.CommandButton cbPlay 
      Appearance      =   0  'Flat
      Caption         =   "Play"
      Height          =   255
      Left            =   1200
      TabIndex        =   2
      Top             =   2520
      Width           =   855
   End
   Begin VB.CommandButton cbPause 
      Appearance      =   0  'Flat
      Caption         =   "Pause"
      Height          =   255
      Left            =   2040
      TabIndex        =   1
      Top             =   2520
      Width           =   855
   End
   Begin VB.DriveListBox DriveBox 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   315
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   855
   End
   Begin VB.Label LTitle 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Wave Name"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   5
      Top             =   2280
      Width           =   2895
   End
End
Attribute VB_Name = "fWav"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim Filn$

Dim Errored%

Dim ChanNum%

Const MyName = "fWav"

Private Sub cbPause_Click()
Dim Ret%
  If cbPause.Caption = "Pause" Then
    Ret = mciSendString("pause Phude", 0&, 0, 0)
    WavState = 2
    cbPause.Caption = "Resume"
  Else
    Ret = mciSendString("play Phude", 0&, 0, 0)
    WavState = 1
    cbPause.Caption = "Pause"
  End If
End Sub

Private Sub cbPlay_Click()
Dim Ret As Integer
Dim Fil As String
Dim Whoops$
  If ChanNum < 1 Or ChanNum > 8 Then ChanNum = 1
  If cbPlay.Caption = "Stop" Then
    cbPlay.Caption = "Play"
    WavState = 0    ' Stopped
    Ret = mciSendString("stop Phude" + TrimStr(ChanNum), 0&, 0, 0)
    Ret = mciSendString("close Phude" + TrimStr(ChanNum), 0&, 0, 0)
    cbPause.Caption = "Pause"
  Else
    cbPlay.Caption = "Stop"
    If What.Text <> "" Then
      Fil = What.Text
      What.Text = ""
    Else
      Fil = UCase(File.Path + "\" + File.List(File.ListIndex))
    End If
    Filn$ = Fil
    Ret = mciSendString("open """ + Fil + """ type waveaudio alias Phude" + TrimStr(ChanNum), 0&, 0, 0)
    'MsgBox TrimStr(Ret)
    If Ret <> 0 And Errored = 0 Then GoTo FuckedUpWav
    Ret = mciSendString("play Phude" + TrimStr(ChanNum), 0&, 0, 0)
    ChanNum = ChanNum + 1
    'MsgBox TrimStr(Ret)
    If Ret = 0 Then
      WavState = 1    ' playing
    Else
      cbPlay.Caption = "Play?"
      If Errored = 0 Then
FuckedUpWav:
        Whoops$ = Space$(255)
        Ret = mciGetErrorString(Ret, Whoops$, 255)
        MsgBox Trim(Whoops$), 48, "Unable to play " + LCase(Fil)
        Errored = 1
      End If
    End If
  End If
End Sub

Private Sub DirBox_Change()
  File.Path = DirBox.Path
End Sub

Private Sub DriveBox_Change()
  DirBox.Path = DriveBox.List(DriveBox.ListIndex)
  File.Path = DirBox.List(DirBox.Path)
End Sub

Private Sub File_Click()
Dim Msg$
  Msg = File.List(File.ListIndex)
  LTitle = Msg
  cbPlay.Caption = "Stop"
  What.Text = UCase(File.Path + "\" + File.List(File.ListIndex))
End Sub

Private Sub File_DblClick()
Dim Ret%
  If ChanNum < 1 Or ChanNum > 8 Then ChanNum = 1
  If File.List(File.ListIndex) <> Filn$ Then
    Ret = mciSendString("stop Phude" + TrimStr(ChanNum), 0&, 0, 0)
    Ret = mciSendString("close Phude" + TrimStr(ChanNum), 0&, 0, 0)
    WavState = 0
    cbPlay.Caption = "Play"
  End If
  cbPlay_Click
  cbPlay.Caption = "Stop"
End Sub

Private Sub Form_Load()
Dim Ret%, P1$, P2$, P3$, P4$, PO%
  LoadPosition Me, MyName
  Me.Hide
  If ChanNum < 1 Or ChanNum > 8 Then ChanNum = 1
  For PO% = 1 To 8
    Ret = mciSendString("stop Phude" + TrimStr(PO%), 0&, 0, 0)
    Ret = mciSendString("close Phude" + TrimStr(PO%), 0&, 0, 0)
  Next PO%
  P1$ = "Millenium"
  P2$ = "HosePath"
  P3$ = App.Path
  P4$ = Space$(255)
  'PO% = GetPrivateProfileString(P1$, P2$, P3$, P4$, 255, "ChUB2000.ini")
  P4$ = GetSetting("ChUB Resurrection", "Settings", P2$, P3$)
  On Error GoTo Noperz
  P4$ = Trim(P4$)
  DriveBox.Drive = Left$(P4$, 1)
  DirBox.Path = P4$
Noperz:
  Exit Sub
End Sub

Private Sub Form_Unload(Cancel As Integer)
  SavePosition Me, MyName
Dim Ret%, P1$, P2$, P4$, PO%
  For PO% = 1 To 8
    Ret = mciSendString("stop Phude" + TrimStr(PO%), 0&, 0, 0)
    Ret = mciSendString("close Phude" + TrimStr(PO%), 0&, 0, 0)
  Next PO%
  P1$ = "Millenium"
  P2$ = "HosePath"
  P4$ = DirBox.Path
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB Resurrection", "Settings", P2$, P4$
End Sub

Private Sub tiMidi_Timer()
Dim Stat$, Ret As Integer
Dim mLen%, Msg As String * 255
  If WavState > 0 Then
    Stat$ = "status Phude mode"
    Msg$ = String$(255, 0)
    Ret = mciSendString(Stat$, Msg$, 255, 0)
    If InStr(Msg$, "stopped") Or (Msg$ = String$(255, 0)) Or (InStr(Msg$, "paused") And WavState = 1) Then
      Select Case Lup.Value
        Case 1:
          Ret = mciSendString("seek Phude to start", 0&, 0, 0)
          Ret = mciSendString("play Phude", 0&, 0, 0)
        Case Else:
          Ret = mciSendString("stop Phude", 0&, 0, 0)
          Ret = mciSendString("close Phude", 0&, 0, 0)
          cbPlay.Caption = "Play"
          cbPause.Caption = "Pause"
          WavState = 0
      End Select
    End If
  End If
  If WavState = 0 Then
    If SndNdx <> SndPtr Then
      What.Text = SndRay(SndNdx)
      SndNdx = SndNdx + 1
      If (SndNdx > SndMax) Then SndNdx = 1
    End If
  End If
End Sub

Private Sub What_Change()
  If What.Text <> UCase(File.Path + "\" + File.List(File.ListIndex)) And WavState = 0 Then cbPlay_Click
End Sub

