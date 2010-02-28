VERSION 5.00
Begin VB.Form fIniLoad 
   Appearance      =   0  'Flat
   BackColor       =   &H00882826&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Dataset Selection"
   ClientHeight    =   1470
   ClientLeft      =   4455
   ClientTop       =   3480
   ClientWidth     =   2760
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
   Icon            =   "FINILOAD.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   98
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   184
   Begin VB.CommandButton cLoadSort 
      Appearance      =   0  'Flat
      Caption         =   "Load and Sort"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   1320
      TabIndex        =   4
      ToolTipText     =   "Loads the dataset and sorts the characters by name"
      Top             =   1200
      Width           =   1335
   End
   Begin VB.CommandButton cExit 
      Appearance      =   0  'Flat
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   120
      TabIndex        =   3
      ToolTipText     =   "Wuss out and don't load a dataset"
      Top             =   1080
      Width           =   1095
   End
   Begin VB.CommandButton cLoad 
      Appearance      =   0  'Flat
      Caption         =   "Load"
      Default         =   -1  'True
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   1320
      TabIndex        =   2
      ToolTipText     =   "Loads the dataset."
      Top             =   960
      Width           =   1335
   End
   Begin VB.FileListBox fIniPick 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   810
      Left            =   0
      Pattern         =   "*.ini"
      TabIndex        =   0
      ToolTipText     =   "Choose a dataset to load."
      Top             =   0
      Width           =   1215
   End
   Begin VB.Label LName 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   855
      Left            =   1320
      TabIndex        =   1
      Top             =   0
      Width           =   1335
   End
End
Attribute VB_Name = "fIniLoad"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cExit_Click()
  'ScrollSend1 ("Chibot Ultra Battle 2000 has been cancelled. <b>THE BOT IS OFF!</b>")
  'End
  Unload Me
End Sub

Private Sub cLoad_Click()
  If DATASET.LoadStr <> "" Then
    ClearChars
    UpdateWin
  End If
  If fIniPick.FileName <> "" Then
    Send ("Loading dataset " + LName.Caption + "...")
    If YourSN = "KamEkSaLLy" Then Send ("I am just LOADING it! You STILL can't pick a character yet, so don't try to!")
    cLoad.Enabled = False
    cLoadSort.Enabled = False
    InitFromDisk (fIniPick.FileName)
    Me.Hide
    cLoad.Enabled = True
    cLoadSort.Enabled = True
    ToChUBBot ("CHUBDS " + LName.Caption)
  Else
    kDlgBox "Pick an .INI File!", 16, "ChUB Resurrection Dataset Loader"
  End If
End Sub

Private Sub cLoadSort_Click()
  If DATASET.LoadStr <> "" Then
    ClearChars
    UpdateWin
  End If
  If fIniPick.FileName <> "" Then
    Send ("Loading dataset " + LName.Caption + "...")
    If YourSN = "KamEkSaLLy" Then Send ("I am just LOADING it! You STILL can't pick a character yet, so don't try to!")
    cLoad.Enabled = False
    cLoadSort.Enabled = False
    InitFromDisk (fIniPick.FileName)
    ScrollSend1 ("Sorting dataset...")
    Load fSortStatus
    Me.Hide
    cLoad.Enabled = True
    cLoadSort.Enabled = True
    ToChUBBot ("CHUBDS " + LName.Caption)
  Else
    kDlgBox "Pick an .INI File!", 16, "ChUB Resurrection Dataset Loader"
  End If
End Sub

Private Sub fIniPick_Click()
Dim S$
  Open fIniPick.FileName For Input As #83
  Input #83, S$
  Close #83
  LName.Caption = S$
End Sub

Private Sub Form_Load()
  LoadPosition Me, Me.name
  SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3
  Me.Show
  If Config.NewUser Then Yoshi yhIniLoad
End Sub

