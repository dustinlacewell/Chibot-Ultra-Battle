VERSION 5.00
Begin VB.Form fChatLine 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "ChUB 2000 Chat Line"
   ClientHeight    =   330
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4725
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   ScaleHeight     =   330
   ScaleWidth      =   4725
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton cSend 
      Caption         =   "Send"
      Default         =   -1  'True
      Height          =   255
      Left            =   3960
      TabIndex        =   1
      Top             =   360
      Width           =   615
   End
   Begin VB.TextBox tChat 
      Height          =   285
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   4695
   End
End
Attribute VB_Name = "fChatLine"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Const MyName = "fChatLine"
Dim Cus$

Private Sub cSend_Click()
Dim S As String
Dim Q$
  Cus$ = GetSetting("ChUB Resurrection", "Yoshi!", "Cuss", "Yes")
  S = tChat.Text
  tChat.Text = ""
  If ((Mid$(S, 1, 1) = ".") Or Mid$(S, 1, 1) = "~") Then
    If Left$(S, 1) = "." Then S = "/" + Right$(S, Len(S) - 1)
    Q$ = YourSN
    GetRay(GetSave) = Q$ + ":  " + Trim(S)
    If (GetSave + 1 > MaxRay) Then
      GetSave = 1
    Else
      GetSave = GetSave + 1
    End If
  Else
    If Left$(S, 1) <> "/" Then
      ScrollSend (ccBold + S)
    Else
      ScrollSend S
    End If
  End If
  If Cus$ <> "NoMore" Then
    If YouCussed(S) Then Yoshi yhPotty
  End If
End Sub

Private Sub Form_Load()
  LoadPosition Me, MyName
  Me.Height = 705
  Me.Width = 4815
  SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
Dim X%
  If GlobalTerm = 0 Then
    X = kDlgBoxfn("If you close the Chat Line, you will be unable to bring it back up, and you will find it difficult to chat without it. Proceed?", 36, "Chat Line")
    If X <> 6 Then Cancel = True
  End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
  SavePosition Me, MyName
End Sub

Private Sub tChat_KeyPress(KeyAscii As Integer)
  'If KeyAscii = 18 Then tChat.Text = tChat.Text + ccReverse
  'If KeyAscii = 11 Then tChat.Text = tChat.Text + ccColor
  'If KeyAscii = 2 Then tChat.Text = tChat.Text + ccBold
  'If KeyAscii = 21 Then tChat.Text = tChat.Text + ccUnderline
  'tChat.SelStart = Len(tChat.Text)
  'Stop
End Sub

Private Sub tChat_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Shift Then Yoshi (yhChat_Chat)
End Sub
