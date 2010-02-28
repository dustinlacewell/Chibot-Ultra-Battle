VERSION 2.00
Begin Form fGraph 
   BackColor       =   &H00000000&
   BorderStyle     =   3  'Fixed Double
   Caption         =   "HP/SP Graphs"
   ClientHeight    =   1950
   ClientLeft      =   3090
   ClientTop       =   2400
   ClientWidth     =   5670
   ForeColor       =   &H00FFFFFF&
   Height          =   2355
   Icon            =   FGRAPH.FRX:0000
   Left            =   3030
   LinkTopic       =   "Form2"
   ScaleHeight     =   1950
   ScaleWidth      =   5670
   Top             =   2055
   Width           =   5790
   Begin Timer tGra 
      Interval        =   1000
      Left            =   0
      Top             =   2400
   End
   Begin PictureBox pSP 
      BackColor       =   &H00404040&
      Height          =   735
      Left            =   1200
      ScaleHeight     =   300
      ScaleMode       =   0  'User
      ScaleWidth      =   500
      TabIndex        =   5
      Top             =   1200
      Width           =   4455
   End
   Begin PictureBox pMP 
      BackColor       =   &H00404040&
      Height          =   735
      Left            =   1200
      ScaleHeight     =   300
      ScaleMode       =   0  'User
      ScaleWidth      =   500
      TabIndex        =   4
      Top             =   1920
      Visible         =   0   'False
      Width           =   4455
   End
   Begin PictureBox pHP 
      BackColor       =   &H00404040&
      Height          =   735
      Left            =   1200
      ScaleHeight     =   400
      ScaleMode       =   0  'User
      ScaleWidth      =   500
      TabIndex        =   1
      Top             =   480
      Width           =   4455
   End
   Begin Label Label3 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "SP"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   18
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   375
      Left            =   0
      TabIndex        =   2
      Top             =   1320
      Width           =   1095
   End
   Begin Label Label2 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "MP"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   18
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   375
      Left            =   0
      TabIndex        =   6
      Top             =   2040
      Visible         =   0   'False
      Width           =   1095
   End
   Begin Label Label1 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "HP"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   18
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   375
      Left            =   0
      TabIndex        =   3
      Top             =   600
      Width           =   1095
   End
   Begin Label TL 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "???"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   18
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   495
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   5655
   End
End
Option Explicit
Const Wid = 75

Dim HP%(Wid)
'Dim MP%(Wid)
Dim SP%(Wid)

Const MyName = "fGraph"

Sub Form_Load ()
  LoadPosition Me, MyName
  TL = p(Graph).ScrNam
  PHP.ScaleWidth = Wid
  'PMP.ScaleWidth = Wid
  PSP.ScaleWidth = Wid
  PHP.ScaleHeight = MaxHP
  'PMP.ScaleHeight = MaxMP
  PSP.ScaleHeight = 300
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3             ' Always on top
End Sub

Sub Form_Unload (Cancel As Integer)
  SavePosition Me, MyName

End Sub

Sub tGra_Timer ()
Dim X1%, X2%
Dim C As Long
  If p(Graph).HP < 0 And TL <> p(Graph).ScrNam + ": DECEASED" Then TL = p(Graph).ScrNam + ": DECEASED"
  If Paused Then Exit Sub
  For X1 = 1 To Wid
    PHP.PSet (X1, PHP.ScaleHeight - HP(X1)), PHP.BackColor
    'PMP.PSet (X1, PMP.ScaleHeight - MP(X1)), PMP.BackColor
    PSP.PSet (X1, PSP.ScaleHeight - SP(X1)), PSP.BackColor
    If X1 < Wid Then
      HP(X1) = HP(X1 + 1)
      'MP(X1) = MP(X1 + 1)
      SP(X1) = SP(X1 + 1)
    End If
  Next X1
  DoEvents
  HP(Wid) = p(Graph).HP
  If HP(Wid) > PHP.ScaleHeight Then PHP.ScaleHeight = HP(Wid)
  'MP(Wid) = p(Graph).MP
  'If MP(Wid) > PMP.ScaleHeight Then PMP.ScaleHeight = MP(Wid)
  SP(Wid) = p(Graph).Super
  If SP(Wid) > PSP.ScaleHeight Then PSP.ScaleHeight = SP(Wid)
  'PHP.Cls
  'PMP.Cls
  'PSP.Cls
  For X1 = 1 To Wid
    Select Case HP(X1)
      Case 0 To 100: C = &HFF
      Case 101 To 250: C = QBColor(14)
      Case Else: C = QBColor(15)
    End Select
    PHP.PSet (X1, PHP.ScaleHeight - HP(X1)), C
    C = QBColor(15)
    'PMP.PSet (X1, PMP.ScaleHeight - MP(X1)), C
    PSP.PSet (X1, PSP.ScaleHeight - SP(X1)), C
  Next X1
  DoEvents
End Sub

