VERSION 5.00
Begin VB.Form fTwit 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "The Legions of the Twitted"
   ClientHeight    =   2760
   ClientLeft      =   1890
   ClientTop       =   2595
   ClientWidth     =   5445
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
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2760
   ScaleWidth      =   5445
   Begin VB.CommandButton cSaveBackTwit 
      Caption         =   "Sav>>"
      Height          =   375
      Left            =   2760
      TabIndex        =   8
      Top             =   2040
      Width           =   735
   End
   Begin VB.CommandButton cSaveTwit 
      Caption         =   "<<Sav"
      Height          =   375
      Left            =   2040
      TabIndex        =   7
      Top             =   2040
      Width           =   735
   End
   Begin VB.CommandButton cDelBackTwit 
      Caption         =   "Del>>"
      Height          =   375
      Left            =   2760
      TabIndex        =   6
      Top             =   1560
      Width           =   735
   End
   Begin VB.CommandButton cDelTwit 
      Caption         =   "<<Del"
      Height          =   375
      Left            =   2040
      TabIndex        =   5
      Top             =   1560
      Width           =   735
   End
   Begin VB.CommandButton cAddBackTwit 
      Caption         =   "Add>>"
      Height          =   375
      Left            =   2760
      TabIndex        =   4
      Top             =   720
      Width           =   735
   End
   Begin VB.CommandButton cAddTwit 
      Caption         =   "<<Add"
      Height          =   375
      Left            =   2040
      TabIndex        =   3
      Top             =   720
      Width           =   735
   End
   Begin VB.TextBox tScrNam 
      Height          =   285
      Left            =   2040
      TabIndex        =   2
      Text            =   "Screen Name"
      Top             =   360
      Width           =   1455
   End
   Begin VB.ListBox LBackTwit 
      Height          =   2010
      Left            =   3600
      Sorted          =   -1  'True
      TabIndex        =   1
      Top             =   360
      Width           =   1695
   End
   Begin VB.ListBox LTwit 
      Height          =   2010
      Left            =   240
      Sorted          =   -1  'True
      TabIndex        =   0
      Top             =   360
      Width           =   1695
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "BACKTWIT"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   3600
      TabIndex        =   11
      Top             =   0
      Width           =   1695
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "TWIT"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   240
      TabIndex        =   10
      Top             =   0
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Note: Twit list does not automatically save to disk. Click Sav to save."
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   360
      TabIndex        =   9
      Top             =   2520
      Width           =   4935
   End
End
Attribute VB_Name = "fTwit"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Const MyName = "fTwit"

Private Sub cAddBackTwit_Click()
  LBackTwit.AddItem tScrNam.Text
  UpdateBackTwit
End Sub

Private Sub cAddTwit_Click()
  LTwit.AddItem tScrNam.Text
  UpdateTwit
End Sub

Private Sub cDelBackTwit_Click()
On Error Resume Next
  tScrNam.Text = LBackTwit.List(LBackTwit.ListIndex)
  LBackTwit.RemoveItem (LBackTwit.ListIndex)
  UpdateBackTwit
End Sub

Private Sub cDelTwit_Click()
On Error Resume Next
  tScrNam.Text = LTwit.List(LTwit.ListIndex)
  LTwit.RemoveItem (LTwit.ListIndex)
  UpdateTwit
End Sub

Private Sub cSaveBackTwit_Click()
Dim X%
  Open "BACKTWIT.TXT" For Output As #175
  For X = 1 To MaxBackTwit
    Write #175, BackTwit(X)
  Next X
  Close #175
  kDlgBox "BACKTWIT.TXT saved.", 64, "BACKTWIT.TXT saved"
End Sub

Private Sub cSaveTwit_Click()
Dim X%
  Open "TWIT.TXT" For Output As #174
  For X = 1 To MaxTwit
    Write #174, Twit(X)
  Next X
  Close #174
  kDlgBox "TWIT.TXT saved.", 64, "TWIT.TXT saved"
End Sub

Private Sub Form_Load()
Dim X%
  SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3
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
  Me.Show
  If Config.NewUser Then Yoshi (yhTwit)
End Sub

Private Sub Form_Unload(Cancel As Integer)
  SavePosition Me, MyName
End Sub

Private Sub UpdateBackTwit()
Dim X%
  ReDim BackTwit(LBackTwit.ListCount)
  MaxBackTwit = LBackTwit.ListCount
  For X = 0 To MaxBackTwit - 1
    BackTwit(X + 1) = LBackTwit.List(X)
  Next X
End Sub

Private Sub UpdateTwit()
Dim X%
  ReDim Twit(LTwit.ListCount)
  MaxTwit = LTwit.ListCount
  For X = 0 To MaxTwit - 1
    Twit(X + 1) = LTwit.List(X)
  Next X
End Sub

