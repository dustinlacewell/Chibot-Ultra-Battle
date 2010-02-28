VERSION 2.00
Begin Form fWhat 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "Whatsit"
   ClientHeight    =   2325
   ClientLeft      =   3945
   ClientTop       =   345
   ClientWidth     =   3735
   ForeColor       =   &H00000000&
   Height          =   2730
   Left            =   3885
   LinkTopic       =   "fWhat"
   ScaleHeight     =   2325
   ScaleWidth      =   3735
   Top             =   0
   Visible         =   0   'False
   Width           =   3855
   Begin ListBox Files 
      BackColor       =   &H00000000&
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Small Fonts"
      FontSize        =   6
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   1980
      Left            =   1560
      Sorted          =   -1  'True
      TabIndex        =   1
      Top             =   0
      Width           =   2175
   End
   Begin FileListBox Huh 
      BackColor       =   &H00000000&
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Small Fonts"
      FontSize        =   6
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   1980
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   1575
   End
   Begin Label Label1 
      Alignment       =   2  'Center
      BackColor       =   &H00000000&
      Caption         =   "Loading ChUB 2000..."
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFC0C0&
      Height          =   255
      Left            =   0
      TabIndex        =   2
      Top             =   2040
      Width           =   3735
   End
End
Option Explicit

Sub Form_Load ()
  Me.Show
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3
End Sub

