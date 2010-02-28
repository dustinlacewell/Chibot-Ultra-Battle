VERSION 2.00
Begin Form fDialog 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   3  'Fixed Double
   Caption         =   "???"
   ClientHeight    =   1395
   ClientLeft      =   3705
   ClientTop       =   735
   ClientWidth     =   3660
   ControlBox      =   0   'False
   Height          =   1800
   Left            =   3645
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   93
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   244
   Top             =   390
   Width           =   3780
   Begin TextBox tInputBox 
      Height          =   285
      Left            =   0
      TabIndex        =   7
      Top             =   1440
      Visible         =   0   'False
      Width           =   3615
   End
   Begin CommandButton cbX 
      Caption         =   "Button 6"
      Height          =   255
      Index           =   6
      Left            =   1200
      TabIndex        =   0
      Top             =   1080
      Visible         =   0   'False
      Width           =   1215
   End
   Begin CommandButton cbX 
      Caption         =   "Button 5"
      Height          =   255
      Index           =   5
      Left            =   1800
      TabIndex        =   6
      Top             =   1080
      Visible         =   0   'False
      Width           =   1215
   End
   Begin CommandButton cbX 
      Caption         =   "Button 4"
      Height          =   255
      Index           =   4
      Left            =   600
      TabIndex        =   5
      Top             =   1080
      Visible         =   0   'False
      Width           =   1215
   End
   Begin CommandButton cbX 
      Caption         =   "Button 3"
      Height          =   255
      Index           =   3
      Left            =   2400
      TabIndex        =   4
      Top             =   1080
      Visible         =   0   'False
      Width           =   1215
   End
   Begin CommandButton cbX 
      Caption         =   "Button 2"
      Height          =   255
      Index           =   2
      Left            =   1200
      TabIndex        =   3
      Top             =   1080
      Visible         =   0   'False
      Width           =   1215
   End
   Begin CommandButton cbX 
      Caption         =   "Button 1"
      Height          =   255
      Index           =   1
      Left            =   0
      TabIndex        =   2
      Top             =   1080
      Visible         =   0   'False
      Width           =   1215
   End
   Begin Image iInfo 
      Height          =   480
      Left            =   0
      Picture         =   FDIALOG.FRX:0000
      Top             =   240
      Visible         =   0   'False
      Width           =   480
   End
   Begin Image iQuestion 
      Height          =   480
      Left            =   0
      Picture         =   FDIALOG.FRX:0CC2
      Top             =   240
      Visible         =   0   'False
      Width           =   480
   End
   Begin Image iStop 
      Height          =   480
      Left            =   0
      Picture         =   FDIALOG.FRX:1584
      Top             =   240
      Visible         =   0   'False
      Width           =   480
   End
   Begin Image iWarning 
      Height          =   480
      Left            =   0
      Picture         =   FDIALOG.FRX:1886
      Top             =   240
      Visible         =   0   'False
      Width           =   480
   End
   Begin Label LText 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      BorderStyle     =   1  'Fixed Single
      Caption         =   "Blah?"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   975
      Left            =   600
      TabIndex        =   1
      Top             =   0
      Width           =   3015
   End
End
Option Explicit

Sub cbX_Click (Index As Integer)
  Select Case cbX(Index).Caption
    Case "&OK": dlgResponse = 1
    Case "&Cancel": dlgResponse = 2
    Case "&Abort": dlgResponse = 3
    Case "&Retry": dlgResponse = 4
    Case "&Ignore": dlgResponse = 5
    Case "&Yes": dlgResponse = 6
    Case "&No": dlgResponse = 7
    Case Else: dlgResponse = 0
  End Select
  Unload Me
End Sub

Sub cbX_KeyPress (Index As Integer, KeyAscii As Integer)
Dim X%, X1%
  For X = 1 To 6
    For X1 = 1 To Len(cbX(X).Caption)
      If Mid$(cbX(X).Caption, X1, 1) = "&" And X1 <> Len(cbX(X).Caption) Then
        If UCase(Chr$(KeyAscii)) = UCase(Mid$(cbX(X).Caption, X1 + 1, 1)) Then
          cbX_Click (X)
        End If
      End If
    Next X1
  Next X
End Sub

Sub Form_KeyPress (KeyAscii As Integer)
  cbX_KeyPress 0, KeyAscii
End Sub

Sub Form_Load ()
Dim XP%, YP%
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3             ' Always on top
  XP = Int(Screen.Width / 2) - Int(Me.Width / 2)
  YP = Int(Screen.Height / 2) - Int(Me.Height / 2)
  Me.Left = XP
  Me.Top = YP
End Sub

Sub Form_Unload (Cancel As Integer)
  If dlgResponse = 1 And tInputBox.Visible Then dlgResponse = tInputBox.Text
End Sub

