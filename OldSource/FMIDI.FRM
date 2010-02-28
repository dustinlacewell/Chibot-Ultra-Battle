VERSION 2.00
Begin Form fMidi 
   BackColor       =   &H00812577&
   BorderStyle     =   3  'Fixed Double
   Caption         =   "Millenium Midi Player"
   ClientHeight    =   2805
   ClientLeft      =   6630
   ClientTop       =   675
   ClientWidth     =   2910
   Height          =   3210
   Icon            =   FMIDI.FRX:0000
   Left            =   6570
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   2805
   ScaleWidth      =   2910
   Top             =   330
   Width           =   3030
   Begin CommandButton cbPause 
      Caption         =   "Pause"
      Height          =   255
      Left            =   2040
      TabIndex        =   3
      Top             =   2520
      Width           =   855
   End
   Begin CommandButton cbPlay 
      Caption         =   "Play"
      Height          =   255
      Left            =   1200
      TabIndex        =   2
      Top             =   2520
      Width           =   855
   End
   Begin CommandButton cHide 
      Caption         =   "Hide"
      Height          =   255
      Left            =   2040
      TabIndex        =   8
      Top             =   2280
      Width           =   855
   End
   Begin TextBox What 
      Height          =   285
      Left            =   3240
      TabIndex        =   7
      Text            =   "what"
      Top             =   600
      Width           =   1215
   End
   Begin CheckBox Lup 
      BackColor       =   &H00812577&
      Caption         =   "Loop."
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   240
      TabIndex        =   6
      Top             =   2520
      Value           =   1  'Checked
      Width           =   855
   End
   Begin DriveListBox DriveBox 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   315
      Left            =   0
      TabIndex        =   5
      Top             =   0
      Width           =   855
   End
   Begin Timer tiMidi 
      Interval        =   500
      Left            =   2400
      Top             =   2880
   End
   Begin FileListBox File 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   2175
      Left            =   1440
      Pattern         =   "*.mid"
      TabIndex        =   0
      Top             =   0
      Width           =   1455
   End
   Begin DirListBox DirBox 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   1830
      Left            =   0
      TabIndex        =   1
      Top             =   360
      Width           =   1455
   End
   Begin Label LTitle 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Midi Name"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   4
      Top             =   2280
      Width           =   2055
   End
End
Option Explicit

Dim MidState As Integer
Dim Filn$

Const MyName = "fMidi"

Sub cbPause_Click ()
Dim Ret%
  If cbPause.Caption = "Pause" Then
    Ret = mciSendString("pause ChUBMidi", 0&, 0, 0)
    MidState = 2
    cbPause.Caption = "Resume"
  Else
    Ret = mciSendString("play ChUBMidi", 0&, 0, 0)
    MidState = 1
    cbPause.Caption = "Pause"
  End If
End Sub

Sub cbPlay_Click ()
Dim Ret As Integer
Dim Fil As String
  If cbPlay.Caption = "Stop" Then
    cbPlay.Caption = "Play"
    MidState = 0    ' Stopped
    Ret = mciSendString("stop ChUBMidi", 0&, 0, 0)
    Ret = mciSendString("close ChUBMidi", 0&, 0, 0)
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
    Ret = mciSendString("open " + Fil + " type sequencer alias ChUBMidi", 0&, 0, 0)
    Ret = mciSendString("play ChUBMidi", 0&, 0, 0)
    MidState = 1    ' playing
  End If
End Sub

Sub cHide_Click ()
  Me.Hide
End Sub

Sub DirBox_Change ()
  File.Path = DirBox.Path
End Sub

Sub DriveBox_Change ()
  DirBox.Path = DriveBox.List(DriveBox.ListIndex)
  File.Path = DirBox.List(DirBox.Path)
End Sub

Sub File_Click ()
Dim Msg$
  LTitle = File.List(File.ListIndex)
  cbPlay.Caption = "Stop"
  What.Text = UCase(File.Path + "\" + File.List(File.ListIndex))
End Sub

Sub File_DblClick ()
Dim Ret%
  If File.List(File.ListIndex) <> Filn$ Then
    Ret = mciSendString("stop ChUBMidi", 0&, 0, 0)
    Ret = mciSendString("close ChUBMidi", 0&, 0, 0)
    MidState = 0
    cbPlay.Caption = "Play"
  End If
  cbPlay_Click
End Sub

Sub Form_Load ()
Dim Ret%, P1$, P2$, P3$, P4$, PO%
  LoadPosition Me, MyName
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3
  Ret = mciSendString("stop ChUBMidi", 0&, 0, 0)
  Ret = mciSendString("close ChUBMidi", 0&, 0, 0)
  P1$ = "Millenium"
  P2$ = "MidPath"
  P3$ = App.Path
  P4$ = Space$(255)
  PO% = GetPrivateProfileString(P1$, P2$, P3$, P4$, 255, "ChUB2000.ini")
  On Error GoTo Nope
  P4$ = Trim(P4$)
  DriveBox.Drive = Left$(P4$, 1)
  DirBox.Path = P4$
Nope:
  Exit Sub
End Sub

Sub Form_Unload (Cancel As Integer)
  SavePosition Me, MyName
Dim Ret%, P1$, P2$, P4$, PO%
  Ret = mciSendString("stop ChUBMidi", 0&, 0, 0)
  Ret = mciSendString("close ChUBMidi", 0&, 0, 0)
  P1$ = "Millenium"
  P2$ = "MidPath"
  P4$ = DirBox.Path
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
End Sub

Sub tiMidi_Timer ()
Dim Stat$, Ret As Integer
Dim mLen%, Msg As String * 255
  If MidState > 0 Then
    Stat$ = "status ChUBMidi mode"
    Msg$ = String$(255, 0)
    Ret = mciSendString(Stat$, Msg$, 255, 0)
    If InStr(Msg$, "stopped") Or (InStr(Msg$, "paused") And MidState = 1) Then
      Select Case Lup.Value
        Case 1:
          Ret = mciSendString("seek ChUBMidi to start", 0&, 0, 0)
          Ret = mciSendString("play ChUBMidi", 0&, 0, 0)
          cbPlay.Caption = "Stop"
        Case Else:
          Ret = mciSendString("stop ChUBMidi", 0&, 0, 0)
          Ret = mciSendString("close ChUBMidi", 0&, 0, 0)
          cbPlay.Caption = "Play"
          cbPause.Caption = "Pause"
          If Lup.Value = 0 Then
            File.ListIndex = Rand(0, File.ListCount)
            File_DblClick
          End If
      End Select
    End If
  End If
End Sub

Sub What_Change ()
  If What.Text <> UCase(File.Path + "\" + File.List(File.ListIndex)) And What.Text <> "" Then
    cbPlay_Click
    cbPlay.Caption = "Stop"
  End If
End Sub

