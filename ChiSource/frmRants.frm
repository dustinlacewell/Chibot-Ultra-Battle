VERSION 5.00
Begin VB.Form frmRants 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Rants and Ravings"
   ClientHeight    =   2550
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   3510
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   ScaleHeight     =   2550
   ScaleWidth      =   3510
   Begin VB.CommandButton cmdRant 
      Caption         =   "Rant!"
      Default         =   -1  'True
      Height          =   255
      Left            =   120
      TabIndex        =   8
      Top             =   2160
      Width           =   3255
   End
   Begin VB.TextBox tRant 
      Height          =   285
      Left            =   120
      TabIndex        =   6
      Text            =   "No Llamas"
      Top             =   1800
      Width           =   3255
   End
   Begin VB.CommandButton smcSTFU 
      Caption         =   "STFU"
      Height          =   375
      Left            =   1800
      TabIndex        =   5
      Top             =   1080
      Width           =   1575
   End
   Begin VB.CommandButton cmdSlowDown 
      Caption         =   "SLOW DOWN"
      Height          =   375
      Left            =   120
      TabIndex        =   4
      Top             =   1080
      Width           =   1575
   End
   Begin VB.CommandButton cmdNoBattle 
      Caption         =   "BATTLE'S OVER"
      Height          =   375
      Left            =   1800
      TabIndex        =   3
      Top             =   600
      Width           =   1575
   End
   Begin VB.CommandButton cmdNoSelect 
      Caption         =   "SELECTION'S OFF"
      Height          =   375
      Left            =   1800
      TabIndex        =   2
      Top             =   120
      Width           =   1575
   End
   Begin VB.CommandButton cmdPaused 
      Caption         =   "IT'S PAUSED"
      Height          =   375
      Left            =   120
      TabIndex        =   1
      Top             =   600
      Width           =   1575
   End
   Begin VB.CommandButton cmdNoScrolling 
      Caption         =   "NO SCROLLING"
      Height          =   375
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   1575
   End
   Begin VB.Label lblOtherRant 
      Caption         =   "Other Rant/Raving..."
      Height          =   255
      Left            =   120
      TabIndex        =   7
      Top             =   1560
      Width           =   3255
   End
End
Attribute VB_Name = "frmRants"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cmdNoBattle_Click()
  Send ccBold + ccUnderline + ccColor + "04~~~--->>> BATTLE'S OVER!!! <<<---~~~"
End Sub

Private Sub cmdNoScrolling_Click()
  Send ccBold + ccUnderline + ccColor + "04~~~--->>> NO FLOODING!!! <<<---~~~"
End Sub

Private Sub cmdNoSelect_Click()
  Send ccBold + ccUnderline + ccColor + "04~~~--->>> SELECTION'S OFF!!! <<<---~~~"
End Sub

Private Sub cmdPaused_Click()
  Send ccBold + ccUnderline + ccColor + "04~~~--->>> IT'S PAUSED!!! <<<---~~~"
End Sub

Private Sub cmdRant_Click()
  Send ccBold + ccUnderline + ccColor + "04~~~--->>> " + UCase(tRant.Text) + " <<<---~~~"
End Sub

Private Sub cmdSlowDown_Click()
  Send ccBold + ccUnderline + ccColor + "04~~~--->>> SLOW DOWN!!! <<<---~~~"
End Sub

Private Sub Form_Load()
  Me.Show
  If Config.NewUser Then Yoshi (yhRant)
End Sub

Private Sub smcSTFU_Click()
  Send ccBold + ccUnderline + ccColor + "04~~~--->>> STFU! STFU! <<<---~~~"
End Sub
