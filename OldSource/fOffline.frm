VERSION 2.00
Begin Form fOffline 
   BackColor       =   &H00404040&
   Caption         =   "ChUB 2000 Offline Output"
   ClientHeight    =   6180
   ClientLeft      =   75
   ClientTop       =   1140
   ClientWidth     =   7170
   ForeColor       =   &H00FFFFFF&
   Height          =   6585
   Icon            =   FOFFLINE.FRX:0000
   Left            =   15
   LinkTopic       =   "Form1"
   ScaleHeight     =   6180
   ScaleWidth      =   7170
   Top             =   795
   Width           =   7290
   Begin TextBox tDebug 
      BackColor       =   &H00000000&
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   1815
      Left            =   0
      MultiLine       =   -1  'True
      ScrollBars      =   3  'Both
      TabIndex        =   0
      Top             =   0
      Width           =   2655
   End
End

' fOffline.FRM
' The Offline Play/Debug Mode

Option Explicit

Const MyName = "fOffline"

Sub Form_Load ()
  LoadPosition Me, MyName
End Sub

Sub Form_QueryUnload (Cancel As Integer, UnloadMode As Integer)
  If kDlgBoxfn("This will end your game. Proceed?", 36, "Exit?") <> 6 Then
    Cancel = 1
  End If
End Sub

Sub Form_Resize ()
  On Error Resume Next
  tDebug.Width = Me.Width - 125
  tDebug.Height = Me.Height - 400
  'SavePosition Me, MyName
End Sub

Sub Form_Unload (Cancel As Integer)
  SavePosition Me, MyName
  Unload fMidi
  Unload fWav
  End
End Sub

Sub tDebug_Change ()
Dim S As String
Dim X As Integer
  'S = tDebug.Text
  'If LineCount(S) >= 26 Then
  '  Delay (1)
  '  For X = Len(S) To 1 Step -1
  '    If (Mid$(S, X, 1) = Chr$(10)) Then Exit For
  '  Next X
  '  tDebug.Text = Mid$(S, X + 1, Len(S) - X + 1)
  'End If
End Sub

