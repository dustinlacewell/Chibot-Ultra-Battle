VERSION 5.00
Begin VB.Form DCalc 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Damage Calculator"
   ClientHeight    =   3795
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4695
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3795
   ScaleWidth      =   4695
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton cHeal 
      Caption         =   "Heal"
      Height          =   495
      Left            =   3360
      TabIndex        =   29
      Top             =   3120
      Width           =   1215
   End
   Begin VB.CommandButton cCalc 
      Caption         =   "Calculate"
      Height          =   495
      Left            =   120
      TabIndex        =   27
      Top             =   3120
      Width           =   1215
   End
   Begin VB.TextBox tMaxHP 
      Height          =   285
      Left            =   720
      TabIndex        =   26
      Text            =   "600"
      Top             =   2280
      Width           =   615
   End
   Begin VB.TextBox tDMult 
      Height          =   285
      Left            =   2880
      TabIndex        =   24
      Text            =   "100"
      Top             =   2520
      Width           =   615
   End
   Begin VB.TextBox tSuper 
      Height          =   285
      Left            =   600
      TabIndex        =   22
      Text            =   "0"
      Top             =   1800
      Width           =   615
   End
   Begin VB.TextBox tEMult 
      Height          =   285
      Left            =   2880
      TabIndex        =   20
      Text            =   "1"
      Top             =   2160
      Width           =   615
   End
   Begin VB.TextBox tAMult 
      Height          =   285
      Left            =   2880
      TabIndex        =   19
      Text            =   "1"
      Top             =   1800
      Width           =   615
   End
   Begin VB.CheckBox cDeath 
      Caption         =   "Sudden Death"
      Height          =   255
      Left            =   120
      TabIndex        =   16
      Top             =   1440
      Width           =   1335
   End
   Begin VB.CheckBox cFire 
      Caption         =   "On Fire"
      Height          =   255
      Left            =   2760
      TabIndex        =   15
      Top             =   1200
      Width           =   1215
   End
   Begin VB.CheckBox cStrong 
      Caption         =   "Strong Against"
      Height          =   255
      Left            =   1320
      TabIndex        =   14
      Top             =   1200
      Width           =   1455
   End
   Begin VB.CheckBox cWeak 
      Caption         =   "Weak To"
      Height          =   255
      Left            =   120
      TabIndex        =   13
      Top             =   1200
      Width           =   1215
   End
   Begin VB.CheckBox cCurse 
      Caption         =   "Curse"
      Height          =   255
      Left            =   3480
      TabIndex        =   12
      Top             =   480
      Width           =   735
   End
   Begin VB.CheckBox cWonder 
      Caption         =   "Wonder"
      Height          =   255
      Left            =   2520
      TabIndex        =   11
      Top             =   480
      Width           =   975
   End
   Begin VB.TextBox tScroller 
      Height          =   285
      Left            =   2280
      TabIndex        =   10
      Text            =   "0"
      Top             =   840
      Width           =   615
   End
   Begin VB.CheckBox cBless 
      Caption         =   "Bless"
      Height          =   255
      Left            =   1680
      TabIndex        =   8
      Top             =   480
      Width           =   735
   End
   Begin VB.TextBox tBarrier 
      Height          =   285
      Left            =   2280
      TabIndex        =   7
      Text            =   "0"
      Top             =   120
      Width           =   615
   End
   Begin VB.TextBox tMovStr 
      Height          =   285
      Left            =   840
      TabIndex        =   5
      Text            =   "50"
      Top             =   840
      Width           =   615
   End
   Begin VB.TextBox tDef 
      Height          =   285
      Left            =   840
      TabIndex        =   3
      Text            =   "55"
      Top             =   480
      Width           =   615
   End
   Begin VB.TextBox tStr 
      Height          =   285
      Left            =   840
      TabIndex        =   1
      Text            =   "55"
      Top             =   120
      Width           =   615
   End
   Begin VB.Label LDmg 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   1440
      TabIndex        =   28
      Top             =   3120
      Width           =   1815
   End
   Begin VB.Label Label10 
      Caption         =   "MaxHP"
      Height          =   255
      Left            =   120
      TabIndex        =   25
      Top             =   2280
      Width           =   615
   End
   Begin VB.Label Label9 
      Caption         =   "Damage Multiplier"
      Height          =   255
      Left            =   1560
      TabIndex        =   23
      Top             =   2520
      Width           =   1335
   End
   Begin VB.Label Label8 
      Caption         =   "Super Level"
      Height          =   495
      Left            =   120
      TabIndex        =   21
      Top             =   1800
      Width           =   495
   End
   Begin VB.Label Label7 
      Caption         =   "Element Multiplier"
      Height          =   255
      Left            =   1560
      TabIndex        =   18
      Top             =   2160
      Width           =   1335
   End
   Begin VB.Label Label6 
      Caption         =   "Arena Multiplier"
      Height          =   255
      Left            =   1560
      TabIndex        =   17
      Top             =   1800
      Width           =   1215
   End
   Begin VB.Label Label5 
      Caption         =   "Scroller"
      Height          =   255
      Left            =   1680
      TabIndex        =   9
      Top             =   840
      Width           =   615
   End
   Begin VB.Label Label4 
      Caption         =   "Barrier"
      Height          =   255
      Left            =   1680
      TabIndex        =   6
      Top             =   120
      Width           =   615
   End
   Begin VB.Label Label3 
      Caption         =   "MovePwr"
      Height          =   255
      Left            =   120
      TabIndex        =   4
      Top             =   840
      Width           =   735
   End
   Begin VB.Label Label2 
      Caption         =   "Defense"
      Height          =   255
      Left            =   120
      TabIndex        =   2
      Top             =   480
      Width           =   615
   End
   Begin VB.Label Label1 
      Caption         =   "Strength"
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   615
   End
End
Attribute VB_Name = "DCalc"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cCalc_Click()
Dim L%, H%
  DamageCount L%, H%
  If L% = H% Then
    LDmg = Trim(Str$(L%))
  Else
    LDmg = Trim(Str$(L%)) + "-" + Trim(Str$(H%))
  End If
End Sub

Private Sub cHeal_Click()
Dim L%, H%
  HealCount L%, H%
  If L% = H% Then
    LDmg = Trim(Str$(L%))
  Else
    LDmg = Trim(Str$(L%)) + "-" + Trim(Str$(H%))
  End If
End Sub
