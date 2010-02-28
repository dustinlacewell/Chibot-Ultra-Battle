VERSION 2.00
Begin Form fCPUEdit 
   BackColor       =   &H00882826&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "CPU Edit"
   ClientHeight    =   1545
   ClientLeft      =   1125
   ClientTop       =   5160
   ClientWidth     =   3525
   Height          =   1950
   Icon            =   FCPUEDIT.FRX:0000
   Left            =   1065
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   1545
   ScaleWidth      =   3525
   Top             =   4815
   Width           =   3645
   Begin HScrollBar hsDarkness 
      Height          =   255
      LargeChange     =   10
      Left            =   720
      Max             =   100
      TabIndex        =   11
      Top             =   1200
      Visible         =   0   'False
      Width           =   1815
   End
   Begin HScrollBar hsArrogance 
      Height          =   255
      LargeChange     =   10
      Left            =   720
      Max             =   100
      TabIndex        =   10
      Top             =   960
      Width           =   1815
   End
   Begin HScrollBar hsWrath 
      Height          =   255
      LargeChange     =   10
      Left            =   720
      Max             =   100
      TabIndex        =   9
      Top             =   720
      Width           =   1815
   End
   Begin HScrollBar hsGreed 
      Height          =   255
      LargeChange     =   10
      Left            =   720
      Max             =   100
      TabIndex        =   8
      Top             =   480
      Width           =   1815
   End
   Begin HScrollBar hsGoodwill 
      Height          =   255
      LargeChange     =   10
      Left            =   720
      Max             =   100
      TabIndex        =   1
      Top             =   240
      Width           =   1815
   End
   Begin CheckBox cSmart 
      BackColor       =   &H00882826&
      Caption         =   "Smart CPU"
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1800
      TabIndex        =   7
      Top             =   0
      Width           =   1215
   End
   Begin CheckBox cIsCPU 
      BackColor       =   &H00882826&
      Caption         =   "CPU-Controlled"
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   1575
   End
   Begin Label LDarkness 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "100"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   16
      Top             =   1200
      Visible         =   0   'False
      Width           =   495
   End
   Begin Label LArrogance 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "100"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   15
      Top             =   960
      Width           =   495
   End
   Begin Label LWrath 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "100"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   14
      Top             =   720
      Width           =   495
   End
   Begin Label LGreed 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "100"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   13
      Top             =   480
      Width           =   495
   End
   Begin Label LGoodwill 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "100"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   12
      Top             =   240
      Width           =   495
   End
   Begin Label Label1 
      BackStyle       =   0  'Transparent
      Caption         =   "Darkness"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   4
      Left            =   2640
      TabIndex        =   6
      Top             =   1200
      Visible         =   0   'False
      Width           =   855
   End
   Begin Label Label1 
      BackStyle       =   0  'Transparent
      Caption         =   "Arrogance"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   3
      Left            =   2640
      TabIndex        =   5
      Top             =   960
      Width           =   855
   End
   Begin Label Label1 
      BackStyle       =   0  'Transparent
      Caption         =   "Wrath"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   2
      Left            =   2640
      TabIndex        =   4
      Top             =   720
      Width           =   855
   End
   Begin Label Label1 
      BackStyle       =   0  'Transparent
      Caption         =   "Greed"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   1
      Left            =   2640
      TabIndex        =   3
      Top             =   480
      Width           =   855
   End
   Begin Label Label1 
      BackStyle       =   0  'Transparent
      Caption         =   "Goodwill"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   0
      Left            =   2640
      TabIndex        =   2
      Top             =   240
      Width           =   855
   End
End
Option Explicit

Const MyName = "fCpuOld"

Sub cIsCPU_Click ()
  p(Temp3).CPU = cIsCpu.Value
  If (p(Temp3).CPU = 1) Then
    p(Temp3).ScrNam = Senshi(p(Temp3).CharID).FullName
  Else
    p(Temp3).ScrNam = "SN Goes Here"
  End If
  Me.Caption = "CPU Edit (" + p(Temp3).ScrNam + ")"
  UpdateWin
End Sub

Sub cSmart_Click ()
  'p(Temp3).Smart = cSmart
End Sub

Sub Form_Load ()
Dim X As Integer
Dim S1 As String
Dim px As PlayerType
  LoadPosition Me, MyName
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3             ' Always on top
  Me.Show
  Me.Caption = "CPU Edit (" + p(Temp3).ScrNam + ")"
  cIsCpu = p(Temp3).CPU
  'cSmart = Abs(p(Temp3).Smart)
  hsGoodwill = p(Temp3).Goodwill
  hsGreed = p(Temp3).Greed
  hsWrath = p(Temp3).Wrath
  hsArrogance = p(Temp3).Arrogance
  'hsDarkness = p(Temp3).Darkness
  'px = p(Temp3)
  'For X = 0 To 100
  '  S1 = TrimStr(X)
  '  DoEvents
  '  If px.Goodwill >= X Then
  '    hsGoodwill = X
  '  End If
  '  If px.Greed >= X Then
  '    hsGreed = X
  '  End If
  '  If px.Wrath >= X Then
  '    hsWrath = X
  '  End If
  '  If px.Arrogance >= X Then
  '    hsArrogance = X
  '  End If
  '  If px.Darkness >= X Then
  '    hsDarkness = X
  '  End If
  'Next X
End Sub

Sub Form_Unload (Cancel As Integer)
  SavePosition Me, MyName

End Sub

Sub hsArrogance_Change ()
  p(Temp3).Arrogance = hsArrogance
  LArrogance.Caption = TrimStr(hsArrogance)
End Sub

Sub hsArrogance_Scroll ()
  hsArrogance_Change
End Sub

Sub hsDarkness_Change ()
  'p(Temp3).Darkness = hsDarkness
  LDarkness.Caption = TrimStr(hsDarkness)
End Sub

Sub hsDarkness_Scroll ()
  hsDarkness_Change
End Sub

Sub hsGoodwill_Change ()
  p(Temp3).Goodwill = hsGoodwill
  LGoodwill.Caption = TrimStr(hsGoodwill)
End Sub

Sub hsGoodwill_Scroll ()
  hsGoodwill_Change
End Sub

Sub hsGreed_Change ()
  p(Temp3).Greed = hsGreed
  LGreed.Caption = TrimStr(hsGreed)
End Sub

Sub hsGreed_Scroll ()
  hsGreed_Change
End Sub

Sub hsWrath_Change ()
  p(Temp3).Wrath = hsWrath
  LWrath.Caption = TrimStr(hsWrath)
End Sub

Sub hsWrath_Scroll ()
  hsWrath_Change
End Sub

