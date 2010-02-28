VERSION 5.00
Begin VB.Form fShootYoshi 
   BackColor       =   &H00858585&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "SHOOT YOSHI!!!"
   ClientHeight    =   4275
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   7560
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   Moveable        =   0   'False
   ScaleHeight     =   4275
   ScaleWidth      =   7560
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer tYoshi 
      Interval        =   1
      Left            =   7080
      Top             =   0
   End
   Begin VB.Image pYoshi 
      Height          =   480
      Left            =   3600
      MousePointer    =   2  'Cross
      Picture         =   "fShootYoshi.frx":0000
      Top             =   1920
      Width           =   390
   End
   Begin VB.Label Ready 
      BackStyle       =   0  'Transparent
      Caption         =   "Weapon Ready"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   615
      Left            =   0
      TabIndex        =   2
      Top             =   3720
      Width           =   3375
   End
   Begin VB.Label Shots 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Shots Remaining: 50"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   375
      Left            =   5280
      TabIndex        =   1
      Top             =   3960
      Width           =   2295
   End
   Begin VB.Label Score 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "00"
      BeginProperty Font 
         Name            =   "System"
         Size            =   19.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   495
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   7575
   End
   Begin VB.Image LYoshi2 
      Height          =   480
      Left            =   1440
      Picture         =   "fShootYoshi.frx":0A42
      Top             =   0
      Visible         =   0   'False
      Width           =   390
   End
   Begin VB.Image LYoshi1 
      Height          =   480
      Left            =   960
      Picture         =   "fShootYoshi.frx":1484
      Top             =   0
      Visible         =   0   'False
      Width           =   390
   End
   Begin VB.Image RYoshi2 
      Height          =   480
      Left            =   480
      Picture         =   "fShootYoshi.frx":1EC6
      Top             =   0
      Visible         =   0   'False
      Width           =   390
   End
   Begin VB.Image RYoshi1 
      Height          =   480
      Left            =   0
      Picture         =   "fShootYoshi.frx":2908
      Top             =   0
      Visible         =   0   'False
      Width           =   390
   End
End
Attribute VB_Name = "fShootYoshi"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim Yr%, Xr%, P%, S%, Tr%
Const MaxShots = 50
Const KillYoshi = 30

Private Sub Form_Load()
  PlaySnd ("Yoshi!")
  SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3
  pYoshi.Top = (fShootYoshi.Height / 2) - (pYoshi.Height / 2)
  S% = MaxShots
  Me.Show
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Tr = 0 Then
    PlaySnd ("YBum")
    S% = S% - 1
    Shots.Caption = "Shots Remaining: " + TrimStr(S%)
    Tr = 20
  End If
End Sub

Private Sub pYoshi_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Tr = 0 Then
    Select Case Rand(1, 3)
      Case 1: PlaySnd ("Yhurt")
      Case 2: PlaySnd ("Laser")
      Case 3: PlaySnd ("Ydie")
    End Select
    Score.Caption = TrimStr(Val(Score.Caption) + 1)
    S% = S% - 1
    Shots.Caption = "Shots Remaining: " + TrimStr(S%)
    Xr = -Sgn(Xr) * (Abs(Xr) + 10)
    Yr = Rand(-50, 50)
    Tr = 10
  End If
End Sub


Private Sub Ready_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
  Form_MouseDown Button, Shift, X, Y
End Sub

Private Sub Score_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
  Form_MouseDown Button, Shift, X, Y
End Sub

Private Sub Shots_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
  Form_MouseDown Button, Shift, X, Y
End Sub

Private Sub tYoshi_Timer()
  If (Xr = 0 And Yr = 0) Then
    Xr = -30
  End If
  If Tr <> 0 And Ready.Caption <> "Not Ready" Then
    Ready.Caption = "Not Ready"
    Ready.ForeColor = &HFF&
  End If
  If Tr = 0 And Ready.Caption <> "Weapon Ready" Then
    Ready.Caption = "Weapon Ready"
    Ready.ForeColor = &HFFFFFF
  End If
  If Tr > 0 Then Tr = Tr - 1
  pYoshi.Left = pYoshi.Left + Xr
  pYoshi.Top = pYoshi.Top + Yr
  'If Rand(1, 100) = 5 Then PlaySnd ("Ybum")
  If P = 0 Then
    P = 1
    If Xr < 0 Then
      pYoshi.Picture = LYoshi2.Picture
    Else
      pYoshi.Picture = RYoshi2.Picture
    End If
  Else
    P = 0
    If Xr < 0 Then
      pYoshi.Picture = LYoshi1.Picture
    Else
      pYoshi.Picture = RYoshi1.Picture
    End If
  End If
  If Val(Score) > KillYoshi Then
    Yoshi yhGiveUp
    Exit Sub
    Unload fShootYoshi
  End If
  If (S% <= 0) Then
    kDlgBox "You ran out of shots!... You'll get 'im next time...", 64, "Yoshi"
    Unload Me
  End If
  If (pYoshi.Left + pYoshi.Width < 0) Or (pYoshi.Left > fShootYoshi.Width) Or (pYoshi.Top + pYoshi.Height < 0) Or (pYoshi.Top - pYoshi.Height > fShootYoshi.Height) Then
    kDlgBox "Oh no, he got away!", 64, "Yoshi"
    kDlgBox "Wait, here comes another!", 64, "Yoshi"
    Yr = 0
    Select Case Rand(0, 1)
      Case 0:
        pYoshi.Left = fShootYoshi.Width
        pYoshi.Top = (fShootYoshi.Height / 2) - (pYoshi.Height / 2)
        Xr = -50
      Case 1:
        pYoshi.Left = 0 - pYoshi.Width
        pYoshi.Top = (fShootYoshi.Height / 2) - (pYoshi.Height / 2)
        Xr = 50
    End Select
  End If
End Sub
Sub PlaySnd(ByVal F$)
Dim Ret%
  Ret = mciSendString("close YoshiSound", 0&, 0, 0)
  Ret = mciSendString("open " + App.Path + "\sounds\" + F$ + ".wav type waveaudio alias YoshiSound", 0&, 0, 0)
  Ret = mciSendString("play YoshiSound", 0&, 0, 0)
End Sub

