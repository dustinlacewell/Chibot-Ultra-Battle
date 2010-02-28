VERSION 5.00
Begin VB.Form fOffline 
   Appearance      =   0  'Flat
   BackColor       =   &H00404040&
   Caption         =   "ChUB 2000 Offline Output"
   ClientHeight    =   6180
   ClientLeft      =   75
   ClientTop       =   1140
   ClientWidth     =   7170
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
   Icon            =   "FOFFLINE.frx":0000
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6180
   ScaleWidth      =   7170
   Begin VB.TextBox tDebug 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
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
Attribute VB_Name = "fOffline"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

' fOffline.FRM
' The Offline Play/Debug Mode

Option Explicit

Const MyName = "fOffline"

Private Sub Form_Load()
  LoadPosition Me, MyName
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
  If GlobalTerm = 0 Then
    If kDlgBoxfn("This will end your game. Proceed?", 36, "Exit?") <> 6 Then
      Cancel = 1
    Else
      GlobalTerm = True
      Unload fChUBMain
    End If
  End If
End Sub

Private Sub Form_Resize()
  On Error Resume Next
  tDebug.Width = Me.Width - 125
  tDebug.Height = Me.Height - 400
  'SavePosition Me, MyName
End Sub

Private Sub Form_Unload(Cancel As Integer)
  SavePosition Me, MyName
  Unload fMidi
  Unload fWav
  End
End Sub

Private Sub tDebug_Change()
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

