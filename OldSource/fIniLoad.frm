VERSION 2.00
Begin Form fIniLoad 
   BackColor       =   &H00882826&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Dataset Selection"
   ClientHeight    =   1710
   ClientLeft      =   4455
   ClientTop       =   3480
   ClientWidth     =   2760
   ControlBox      =   0   'False
   Height          =   2115
   Icon            =   FINILOAD.FRX:0000
   Left            =   4395
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1710
   ScaleWidth      =   2760
   Top             =   3135
   Width           =   2880
   Begin CommandButton cLoadSort 
      Caption         =   "Load and Sort"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   1320
      TabIndex        =   4
      Top             =   1440
      Width           =   1335
   End
   Begin CommandButton cExit 
      Cancel          =   -1  'True
      Caption         =   "Exit ChUB"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   120
      TabIndex        =   3
      Top             =   1320
      Width           =   1095
   End
   Begin CommandButton cLoad 
      Caption         =   "Load"
      Default         =   -1  'True
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   1320
      TabIndex        =   2
      Top             =   1200
      Width           =   1335
   End
   Begin FileListBox fIniPick 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   1200
      Left            =   0
      Pattern         =   "*.ini"
      TabIndex        =   0
      Top             =   0
      Width           =   1215
   End
   Begin Label LName 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   1095
      Left            =   1320
      TabIndex        =   1
      Top             =   0
      Width           =   1335
   End
End
Option Explicit

Sub cExit_Click ()
  ScrollSend1 ("Chibot Ultra Battle 2000 has been cancelled. <b>THE BOT IS OFF!</b>")
  End
End Sub

Sub cLoad_Click ()
  If fIniPick.FileName <> "" Then
    Send ("Loading dataset " + LName.Caption + "...")
    If YourSN = "KamEkSaLLy" Then Send ("I am just LOADING it! You STILL can't pick a character yet, so don't try to!")
    cLoad.Enabled = False
    cLoadSort.Enabled = False
    InitFromDisk (fIniPick.FileName)
    Me.Hide
    cLoad.Enabled = True
    cLoadSort.Enabled = True
  Else
    kDlgBox "Pick an .INI File!", 16, "ChUB 2000 Dataset Loader"
  End If
End Sub

Sub cLoadSort_Click ()
  If fIniPick.FileName <> "" Then
    Send ("Loading dataset " + LName.Caption + "...")
    If YourSN = "KamEkSaLLy" Then Send ("I am just LOADING it! You STILL can't pick a character yet, so don't try to!")
    cLoad.Enabled = False
    cLoadSort.Enabled = False
    InitFromDisk (fIniPick.FileName)
    Load fSortStatus
    Me.Hide
    cLoad.Enabled = True
    cLoadSort.Enabled = True
  Else
    kDlgBox "Pick an .INI File!", 16, "ChUB 2000 Dataset Loader"
  End If
End Sub

Sub fIniPick_Click ()
Dim S$
  Open fIniPick.FileName For Input As #83
  Input #83, S$
  Close #83
  LName.Caption = S$
End Sub

Sub Form_Load ()
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3
End Sub

