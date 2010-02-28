VERSION 2.00
Begin Form fStats 
   BackColor       =   &H00000040&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Game Statistics"
   ClientHeight    =   1560
   ClientLeft      =   1050
   ClientTop       =   1740
   ClientWidth     =   5940
   Height          =   1965
   Icon            =   FSTATS.FRX:0000
   Left            =   990
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   1560
   ScaleWidth      =   5940
   Top             =   1395
   Width           =   6060
   Begin CommandButton cShow 
      Caption         =   "Show"
      Height          =   255
      Left            =   3120
      TabIndex        =   22
      Top             =   0
      Width           =   1215
   End
   Begin Timer Timer1 
      Interval        =   1000
      Left            =   5040
      Top             =   1680
   End
   Begin Label LASP 
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3960
      TabIndex        =   0
      Top             =   1200
      Width           =   1215
   End
   Begin Label LAMP 
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3960
      TabIndex        =   21
      Top             =   960
      Width           =   1215
   End
   Begin Label LAHP 
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3960
      TabIndex        =   20
      Top             =   720
      Width           =   1215
   End
   Begin Label LAFatal 
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3960
      TabIndex        =   19
      Top             =   480
      Width           =   1215
   End
   Begin Label LAKills 
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3960
      TabIndex        =   18
      Top             =   240
      Width           =   1215
   End
   Begin Label LSP 
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1320
      TabIndex        =   17
      Top             =   1200
      Width           =   1215
   End
   Begin Label LMP 
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1320
      TabIndex        =   16
      Top             =   960
      Width           =   1215
   End
   Begin Label LHP 
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1320
      TabIndex        =   15
      Top             =   720
      Width           =   1215
   End
   Begin Label LFatal 
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1320
      TabIndex        =   14
      Top             =   480
      Width           =   1215
   End
   Begin Label LKills 
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1320
      TabIndex        =   13
      Top             =   240
      Width           =   1215
   End
   Begin Label LTime 
      BackStyle       =   0  'Transparent
      Caption         =   "00:00"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1320
      TabIndex        =   12
      Top             =   0
      Width           =   1215
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Time Elapsed:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   11
      Top             =   0
      Width           =   1215
   End
   Begin Label Label11 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "SP/Sec:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2640
      TabIndex        =   10
      Top             =   1200
      Width           =   1215
   End
   Begin Label Label10 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Total SP Used:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   9
      Top             =   1200
      Width           =   1215
   End
   Begin Label Label9 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "MP/Sec:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2640
      TabIndex        =   8
      Top             =   960
      Width           =   1215
   End
   Begin Label Label8 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "HP/Sec:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2640
      TabIndex        =   7
      Top             =   720
      Width           =   1215
   End
   Begin Label Label7 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Total Fatalities"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   6
      Top             =   480
      Width           =   1215
   End
   Begin Label Label6 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Total Deaths:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   5
      Top             =   240
      Width           =   1215
   End
   Begin Label Label5 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Total MP Used:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   4
      Top             =   960
      Width           =   1215
   End
   Begin Label Label4 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Total HP Damage:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   3
      Top             =   720
      Width           =   1215
   End
   Begin Label Label3 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Fatalities/Min:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2640
      TabIndex        =   2
      Top             =   480
      Width           =   1215
   End
   Begin Label Label2 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Deaths/Min:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2640
      TabIndex        =   1
      Top             =   240
      Width           =   1215
   End
End
Option Explicit

Sub cShow_Click ()
  Send ("Time Elapsed: " + LTime)
  Send ("Total Deaths: " + LKills + " [" + LAKills + "/min]")
  Send ("Total Fatalities: " + LFatal + " [" + LAFatal + "/min]")
  Send ("Total HP Damage: " + LHP + " [" + LAHP + "/sec]")
  'Send ("Total MP Used: " + LMP + " [" + LAMP + "/sec]")
  Send ("Total SP Used: " + LSP + " [" + LASP + "/sec]")
End Sub

Sub Form_Load ()
  LoadPosition Me, "fStats"
End Sub

Sub Timer1_Timer ()
Dim Min%, Sec%, T$, Mi!
Dim Dm!, Fm!, Hs!, Ms!, Ss!
  Min = Int(XTimer / 60)
  Sec = XTimer - (Min * 60)
  If (Min < 10) Then
    T$ = "0" + TrimStr(Min)
  Else
    T$ = TrimStr(Min)
  End If
  If (Sec < 10) Then
    T$ = T$ + "0" + TrimStr(Sec)
  Else
    T$ = T$ + TrimStr(Sec)
  End If
  'T$ = Str$(Min) + Str$(Sec)
  LTime = Format$(T$, "0#:0#")
  LKills = TrimStr(TKills)
  LFatal = TrimStr(TFatal)
  LHP = TrimStr(THP)
  'LMP = TrimStr(TMP)
  LSP = TrimStr(TSP)
  Mi! = XTimer / 60
  On Error Resume Next
  Dm! = TKills / Mi!
  LAKills = Format$(Dm!, "##0.0##")
  Fm! = TFatal / Mi!
  LAFatal = Format$(Fm!, "##0.0##")
  Hs! = THP / XTimer
  LAHP = Format$(Hs!, "##,##0.0##")
  'Ms! = TMP / XTimer
  'LAMP = Format$(Ms!, "##,##0.0##")
  Ss! = TSP / XTimer
  LASP = Format$(Ss!, "##,##0.0##")
End Sub

