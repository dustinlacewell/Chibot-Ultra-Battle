VERSION 2.00
Begin Form fTwit 
   BackColor       =   &H00882826&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "The Legions of the Twitted"
   ClientHeight    =   2910
   ClientLeft      =   1890
   ClientTop       =   2595
   ClientWidth     =   5445
   Height          =   3315
   Left            =   1830
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   2910
   ScaleWidth      =   5445
   Top             =   2250
   Width           =   5565
   Begin CommandButton cSaveBackTwit 
      Caption         =   "Sav>>"
      Height          =   375
      Left            =   2760
      TabIndex        =   8
      Top             =   2160
      Width           =   615
   End
   Begin CommandButton cSaveTwit 
      Caption         =   "<<Sav"
      Height          =   375
      Left            =   2040
      TabIndex        =   7
      Top             =   2160
      Width           =   615
   End
   Begin CommandButton cDelBackTwit 
      Caption         =   "Del>>"
      Height          =   375
      Left            =   2760
      TabIndex        =   6
      Top             =   1680
      Width           =   615
   End
   Begin CommandButton cDelTwit 
      Caption         =   "<<Del"
      Height          =   375
      Left            =   2040
      TabIndex        =   5
      Top             =   1680
      Width           =   615
   End
   Begin CommandButton cAddBackTwit 
      Caption         =   "Add>>"
      Height          =   375
      Left            =   2760
      TabIndex        =   4
      Top             =   720
      Width           =   615
   End
   Begin CommandButton cAddTwit 
      Caption         =   "<<Add"
      Height          =   375
      Left            =   2040
      TabIndex        =   3
      Top             =   720
      Width           =   615
   End
   Begin TextBox tScrNam 
      Height          =   285
      Left            =   2040
      TabIndex        =   2
      Text            =   "Screen Name"
      Top             =   360
      Width           =   1335
   End
   Begin ListBox LBackTwit 
      Height          =   2175
      Left            =   3480
      Sorted          =   -1  'True
      TabIndex        =   1
      Top             =   360
      Width           =   1695
   End
   Begin ListBox LTwit 
      Height          =   2175
      Left            =   240
      Sorted          =   -1  'True
      TabIndex        =   0
      Top             =   360
      Width           =   1695
   End
   Begin Label Label3 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "BACKTWIT"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   12
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3480
      TabIndex        =   11
      Top             =   0
      Width           =   1695
   End
   Begin Label Label2 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "TWIT"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   12
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   240
      TabIndex        =   10
      Top             =   0
      Width           =   1695
   End
   Begin Label Label1 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Note: Twit list does not automatically save to disk. Click Sav to save."
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   360
      TabIndex        =   9
      Top             =   2640
      Width           =   4935
   End
End
Option Explicit

Const MyName = "fTwit"

Sub cAddBackTwit_Click ()
  LBackTwit.AddItem tScrNam.Text
  UpdateBackTwit
End Sub

Sub cAddTwit_Click ()
  LTwit.AddItem tScrNam.Text
  UpdateTwit
End Sub

Sub cDelBackTwit_Click ()
On Error Resume Next
  tScrNam.Text = LBackTwit.List(LBackTwit.ListIndex)
  LBackTwit.RemoveItem (LBackTwit.ListIndex)
  UpdateBackTwit
End Sub

Sub cDelTwit_Click ()
On Error Resume Next
  tScrNam.Text = LTwit.List(LTwit.ListIndex)
  LTwit.RemoveItem (LTwit.ListIndex)
  UpdateTwit
End Sub

Sub cSaveBackTwit_Click ()
Dim X%
  Open "BACKTWIT.TXT" For Output As #175
  For X = 1 To MaxBackTwit
    Write #175, BackTwit(X)
  Next X
  Close #175
  kDlgBox "BACKTWIT.TXT saved.", 64, "BACKTWIT.TXT saved"
End Sub

Sub cSaveTwit_Click ()
Dim X%
  Open "TWIT.TXT" For Output As #174
  For X = 1 To MaxTwit
    Write #174, Twit(X)
  Next X
  Close #174
  kDlgBox "TWIT.TXT saved.", 64, "TWIT.TXT saved"
End Sub

Sub Form_Load ()
Dim X%
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3
  LoadPosition Me, MyName
  On Error Resume Next
  For X = 1 To MaxTwit
    LTwit.AddItem Twit(X)
  Next X
  For X = 1 To MaxBackTwit
    LBackTwit.AddItem BackTwit(X)
  Next X
  LTwit.ListIndex = 0
  LBackTwit.ListIndex = 0
End Sub

Sub Form_Unload (Cancel As Integer)
  SavePosition Me, MyName
End Sub

Sub UpdateBackTwit ()
Dim X%
  ReDim BackTwit(LBackTwit.ListCount)
  MaxBackTwit = LBackTwit.ListCount
  For X = 0 To MaxBackTwit - 1
    BackTwit(X + 1) = LBackTwit.List(X)
  Next X
End Sub

Sub UpdateTwit ()
Dim X%
  ReDim Twit(LTwit.ListCount)
  MaxTwit = LTwit.ListCount
  For X = 0 To MaxTwit - 1
    Twit(X + 1) = LTwit.List(X)
  Next X
End Sub

