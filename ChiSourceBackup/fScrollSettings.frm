VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.2#0"; "COMCTL32.OCX"
Begin VB.Form fScrollSettings 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Advanced Settings"
   ClientHeight    =   1200
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   3975
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   ScaleHeight     =   1200
   ScaleWidth      =   3975
   Begin ComctlLib.Slider sScRa 
      Height          =   480
      Left            =   1680
      TabIndex        =   3
      Top             =   600
      Width           =   1575
      _ExtentX        =   2778
      _ExtentY        =   847
      _Version        =   327682
      LargeChange     =   1
      Min             =   1
      Max             =   4
      SelStart        =   1
      Value           =   1
   End
   Begin ComctlLib.Slider sOutput 
      Height          =   480
      Left            =   1680
      TabIndex        =   0
      Top             =   120
      Width           =   1575
      _ExtentX        =   2778
      _ExtentY        =   847
      _Version        =   327682
      LargeChange     =   100
      SmallChange     =   10
      Min             =   1
      Max             =   4000
      SelStart        =   1
      TickFrequency   =   500
      Value           =   1
   End
   Begin VB.Label LScRa 
      Caption         =   "1"
      Height          =   255
      Left            =   3240
      TabIndex        =   5
      Top             =   600
      Width           =   255
   End
   Begin VB.Label Label2 
      Alignment       =   1  'Right Justify
      Caption         =   "Lines Scrolled Per Delay"
      Height          =   495
      Left            =   120
      TabIndex        =   4
      Top             =   600
      Width           =   1455
   End
   Begin VB.Label OutScroll 
      Caption         =   "4000"
      Height          =   255
      Left            =   3240
      TabIndex        =   2
      Top             =   120
      Width           =   495
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Caption         =   "Output Scroll Delay"
      Height          =   255
      Left            =   120
      TabIndex        =   1
      Top             =   120
      Width           =   1455
   End
End
Attribute VB_Name = "fScrollSettings"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()

End Sub

Private Sub sOutput_Change()
Dim P1$, P2$, P4$, PO%
  fChUBMain!tiOutput.interval = sOutput.Value
  OutScroll.Caption = TrimStr(sOutput.Value)
  P2$ = "ScrDelay"
  P4$ = TrimStr(hsScroll.Value)
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
End Sub

Private Sub sOutput_Scroll()
  sOutput_Change
End Sub

Private Sub sScRa_Change()
  ScRa = sScRa.Value
  LScRa.Caption = TrimStr(ScRa)
  SaveSetting "ChUB 2000 SE", "Settings", "ScRa", LScRa.Caption
End Sub

Private Sub sScRa_Scroll()
  sScRa_Change
End Sub
