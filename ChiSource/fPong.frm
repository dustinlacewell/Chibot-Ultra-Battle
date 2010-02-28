VERSION 5.00
Begin VB.Form fPong 
   Appearance      =   0  'Flat
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "It's like, totally retro dude!"
   ClientHeight    =   4935
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   7515
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4935
   ScaleWidth      =   7515
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer pongCPU 
      Interval        =   100
      Left            =   6480
      Top             =   3480
   End
   Begin VB.Timer pongTimer 
      Interval        =   50
      Left            =   6960
      Top             =   3480
   End
   Begin VB.Label GoalMsg 
      Alignment       =   2  'Center
      BackColor       =   &H00000000&
      Caption         =   "Goal - Hose"
      BeginProperty Font 
         Name            =   "System"
         Size            =   19.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   495
      Left            =   2520
      TabIndex        =   5
      Top             =   2040
      Visible         =   0   'False
      Width           =   2415
   End
   Begin VB.Label Ball 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3600
      TabIndex        =   2
      Top             =   2160
      Width           =   255
   End
   Begin VB.Label paddleCPU 
      Appearance      =   0  'Flat
      BackColor       =   &H000000FF&
      ForeColor       =   &H80000008&
      Height          =   1095
      Left            =   7080
      TabIndex        =   1
      Top             =   120
      Width           =   255
   End
   Begin VB.Label paddleHuman 
      Appearance      =   0  'Flat
      BackColor       =   &H00FF0000&
      ForeColor       =   &H80000008&
      Height          =   1095
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   255
   End
   Begin VB.Label scorePlayer 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      Caption         =   "0"
      BeginProperty Font 
         Name            =   "System"
         Size            =   19.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   495
      Left            =   2520
      TabIndex        =   3
      Top             =   0
      Width           =   495
   End
   Begin VB.Label scoreCPU 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      Caption         =   "0"
      BeginProperty Font 
         Name            =   "System"
         Size            =   19.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   495
      Left            =   4440
      TabIndex        =   4
      Top             =   0
      Width           =   495
   End
End
Attribute VB_Name = "fPong"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub PlayDaSound(ByVal Fil$)
Dim Ret%
  Ret = mciSendString("close PongSound", 0&, 0, 0)
  Ret = mciSendString("open " + Fil + " type waveaudio alias PongSound", 0&, 0, 0)
  Ret = mciSendString("play PongSound", 0&, 0, 0)
End Sub

Private Sub Form_Click()
  GoalMsg.Visible = False
  pongTimer.Enabled = True
  pongCPU.Enabled = True
End Sub

Private Sub Form_Load()
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3
  scorePlayer.Caption = "0"
  scoreCPU.Caption = "0"
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  paddleHuman.Top = Int(Y - (paddleHuman.Height / 2))
End Sub

Private Sub GoalMsg_Click()
  GoalMsg.Visible = False
  pongTimer.Enabled = True
  pongCPU.Enabled = True
End Sub

Private Sub pongCPU_Timer()
Dim yc%
  yc = Ball.Top - (0.5 * Ball.Height)
  If paddleCPU.Top > yc Then
    paddleCPU.Top = paddleCPU.Top - 250
  Else
    paddleCPU.Top = paddleCPU.Top + 250
  End If
  If paddleCPU.Top < 0 Then paddleCPU.Top = 0
  If paddleCPU.Top + paddleCPU.Height > fPong.Height Then paddleCPU.Top = fPong.Height - paddleCPU.Height
  'If (yc + paddleCPU.Height <= fPong.Height) And (yc + paddleCPU.Height >= 0) Then
  '  If paddleCPU.Top > yc Then
  '    paddleCPU.Top = paddleCPU.Top - 10
  '  Else
  '    paddleCPU.Top = yc
  '  End If
  'End If
End Sub

Private Sub pongTimer_Timer()
Static xVel%
Static yVel%
  If (xVel = 0 And yVel = 0) Then
    xVel = -100
    yVel = -100
  End If
  If (Ball.Left + xVel < paddleHuman.Left + paddleHuman.Width) And (Ball.Top + Ball.Height + yVel > paddleHuman.Top) And (Ball.Top + yVel < paddleHuman.Top + paddleHuman.Height) And (Ball.Left + Ball.Width + xVel > paddleHuman.Left) Then
    PlayDaSound LCase(App.Path + "\boing.wav")
    xVel = Int(-xVel * Rand(8, 15) / 10)
    If xVel > 500 Then xVel = 500
    yVel = Int(yVel * Rand(8, 15) / 10)
    If yVel > 500 Then yVel = 500
  End If
  If (Ball.Left + xVel + Ball.Width > paddleCPU.Left) And (Ball.Top + yVel < paddleCPU.Top + paddleCPU.Height) And (Ball.Top + yVel + Ball.Height > paddleCPU.Top) Then
    PlayDaSound LCase(App.Path + "\boing.wav")
    xVel = Int(-xVel * Rand(8, 15) / 10)
    If xVel > 500 Then xVel = 500
    yVel = Int(yVel * Rand(8, 15) / 10)
    If yVel > 500 Then yVel = 500
  End If
  If (Ball.Top + yVel < 0) Or (Ball.Top + Ball.Height + yVel > fPong.Height) Then
    yVel = -yVel
  End If
  Ball.Left = Ball.Left + xVel
  Ball.Top = Ball.Top + yVel
  If (Ball.Left < 0) Then
    PlayDaSound LCase(App.Path + "\goal.wav")
    scoreCPU.Caption = TrimStr(Val(scoreCPU.Caption) + 1)
    Ball.Left = 3600
    Ball.Top = 2160
    xVel = -100
    yVel = -100
    pongTimer.Enabled = False
    pongCPU.Enabled = False
    GoalMsg.Caption = "Goal - Red"
    GoalMsg.ForeColor = RGB(255, 0, 0)
    GoalMsg.Visible = True
  End If
  If (Ball.Left + Ball.Width > fPong.Width) Then
    PlayDaSound LCase(App.Path + "\goal.wav")
    scorePlayer.Caption = TrimStr(Val(scorePlayer.Caption) + 1)
    Ball.Left = 3600
    Ball.Top = 2160
    xVel = 100
    yVel = 100
    pongTimer.Enabled = False
    pongCPU.Enabled = False
    GoalMsg.Caption = "Goal - Blue"
    GoalMsg.ForeColor = RGB(0, 0, 255)
    GoalMsg.Visible = True
  End If
  If (Val(scorePlayer.Caption) >= 10) Then
    kDlgBox "You are the winner!", 0, "ChUB Pong"
    Unload Me
  End If
  If (Val(scoreCPU.Caption) >= 10) Then
    kDlgBox "I win! Let's play again sometime!", 0, "ChUB Pong"
    Unload Me
  End If
End Sub
