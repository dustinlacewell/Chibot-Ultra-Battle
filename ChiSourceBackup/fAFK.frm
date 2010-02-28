VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.1#0"; "COMCTL32.OCX"
Begin VB.Form fAFK 
   Caption         =   "AFK Bot"
   ClientHeight    =   3405
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   5790
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   3405
   ScaleWidth      =   5790
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cClear 
      Caption         =   "Clear"
      Height          =   315
      Left            =   120
      TabIndex        =   7
      Top             =   3000
      Width           =   1215
   End
   Begin VB.ListBox Msgs 
      Height          =   1425
      ItemData        =   "fAFK.frx":0000
      Left            =   120
      List            =   "fAFK.frx":0002
      TabIndex        =   6
      Top             =   1560
      Width           =   5535
   End
   Begin VB.Timer tOutput 
      Interval        =   1000
      Left            =   1560
      Top             =   1080
   End
   Begin VB.Timer tInput 
      Enabled         =   0   'False
      Interval        =   1000
      Left            =   2040
      Top             =   1080
   End
   Begin VB.CommandButton cStart 
      Caption         =   "Start"
      Height          =   375
      Left            =   4440
      TabIndex        =   5
      Top             =   1080
      Width           =   1215
   End
   Begin VB.CheckBox cMsg 
      Caption         =   "Take Messages"
      Height          =   375
      Left            =   120
      TabIndex        =   4
      Top             =   1080
      Width           =   1575
   End
   Begin ComctlLib.Slider sFreq 
      Height          =   375
      Left            =   1440
      TabIndex        =   1
      ToolTipText     =   "Frequency"
      Top             =   600
      Width           =   3495
      _ExtentX        =   6165
      _ExtentY        =   661
      _Version        =   327682
      LargeChange     =   60
      SmallChange     =   30
      Max             =   300
      SelStart        =   30
      TickFrequency   =   30
      Value           =   30
   End
   Begin VB.TextBox tReason 
      Height          =   285
      Left            =   120
      TabIndex        =   0
      Text            =   "Reason"
      Top             =   120
      Width           =   5535
   End
   Begin VB.Label Label1 
      Caption         =   "Frequency (sec) 0=no notification"
      Height          =   375
      Left            =   120
      TabIndex        =   3
      Top             =   600
      Width           =   1215
   End
   Begin VB.Label Duration 
      Caption         =   "30"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   5160
      TabIndex        =   2
      Top             =   600
      Width           =   495
   End
End
Attribute VB_Name = "fAFK"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cClear_Click()
  Msgs.Clear
End Sub

Private Sub cClear_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi yhAFK_Clear
End Sub

Private Sub cMsg_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi yhAFK_Msg
End Sub

Private Sub cStart_Click()
  If cStart.Caption = "Start" Then
    Send ("C2afK: " + YourSN + " is AFK: " + tReason.Text)
    If cMsg.Value = 1 Then Send ("Type /" + YourSN + " <msg> to leave a message")
    cStart.Caption = "Stop"
  Else
    Send ("C2afK: " + YourSN + " is no longer AFK")
    cStart.Caption = "Start"
  End If
End Sub

Private Sub cStart_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi yhAFK_Start
End Sub

Private Sub Form_Unload(Cancel As Integer)
  'ScrollSend DATASET.CharsNotCleared
  'fPreBattl.Show
  'fPreBattl.tiPreBatt.Enabled = True
End Sub

Private Sub Msgs_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi yhAFK_Msgs
End Sub

Private Sub sFreq_Change()
  Duration.Caption = TrimStr(sFreq.Value)
  'tOutput.Value = 1000 * sFreq.Value
End Sub

Private Sub sFreq_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi yhAFK_Freq
End Sub

Private Sub sFreq_Scroll()
  sFreq_Change
End Sub

Private Sub tInput_Timer()
Dim SN$, Ct$, Chat$, M$
  If (GetNdx <> GetSave) Then
    Chat = GetRay(GetNdx)
    GetNdx = GetNdx + 1
    If (GetNdx > MaxRay) Then GetNdx = 1
    'If Left$(GetCt(Chat), 1) <> "/" Then GoTo DoAgain1
  Else
    Exit Sub
  End If
  SN = GetSN(Chat)
  Ct = Trim(GetCt(Chat))
  If Skip(Ct, Len(YourSN)) = UCase(YourSN) And cStart.Caption = "Stop" And cMsg.Value = 1 Then
    M$ = Format(Time$, "hh:mm:ss AMPM") + " " + SN + ": " + Right$(Ct, Len(Ct) - Len(YourSN) - 2)
    Send ("C2afK: " + SN + ", your message was received at " + Format(Time$, "hh:mm:ss AMPM"))
    Msgs.AddItem (M$)
  End If
End Sub

Private Sub tOutput_Timer()
Static Passes%
  Passes = Passes + 1
  If sFreq.Value = 0 Or cStart.Caption = "Start" Then Passes = -1
  If Passes >= sFreq.Value And sFreq.Value <> 0 And cStart.Caption = "Stop" Then
    Send ("C2afK: " + YourSN + " is AFK: " + tReason.Text)
    If cMsg.Value = 1 Then Send ("Type /" + YourSN + " <msg> to leave a message")
    Passes = 0
  End If
End Sub

Private Sub tReason_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 1 And Shift Then Yoshi yhAFK_Reason
End Sub
