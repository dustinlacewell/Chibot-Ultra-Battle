VERSION 5.00
Begin VB.Form fStats 
   Appearance      =   0  'Flat
   BackColor       =   &H00808080&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Game Statistics"
   ClientHeight    =   2295
   ClientLeft      =   1050
   ClientTop       =   1740
   ClientWidth     =   5940
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
   Icon            =   "FSTATS.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   153
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   396
   Begin VB.CommandButton cShow 
      Appearance      =   0  'Flat
      Caption         =   "Show"
      Height          =   255
      Left            =   3600
      TabIndex        =   22
      Top             =   0
      Width           =   1215
   End
   Begin VB.Timer Timer1 
      Interval        =   1000
      Left            =   5040
      Top             =   1680
   End
   Begin VB.Label LTimeLeft 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Infinity"
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
      Left            =   1560
      TabIndex        =   26
      Top             =   1920
      Width           =   1215
   End
   Begin VB.Label LGetItem 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "None"
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
      Left            =   1560
      TabIndex        =   25
      Top             =   1680
      Width           =   1215
   End
   Begin VB.Label Label13 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Time Remaining:"
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
      Left            =   240
      TabIndex        =   24
      Top             =   1920
      Width           =   1215
   End
   Begin VB.Label Label12 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Item Available:"
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
      Left            =   240
      TabIndex        =   23
      Top             =   1680
      Width           =   1215
   End
   Begin VB.Label LASP 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
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
      Left            =   4440
      TabIndex        =   0
      Top             =   1200
      Width           =   1215
   End
   Begin VB.Label LAMP 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
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
      Left            =   4440
      TabIndex        =   21
      Top             =   960
      Width           =   1215
   End
   Begin VB.Label LAHP 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
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
      Left            =   4440
      TabIndex        =   20
      Top             =   720
      Width           =   1215
   End
   Begin VB.Label LAFatal 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
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
      Left            =   4440
      TabIndex        =   19
      Top             =   480
      Width           =   1215
   End
   Begin VB.Label LAKills 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
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
      Left            =   4440
      TabIndex        =   18
      Top             =   240
      Width           =   1215
   End
   Begin VB.Label LSP 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
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
      Left            =   1440
      TabIndex        =   17
      Top             =   1200
      Width           =   1215
   End
   Begin VB.Label LMP 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
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
      Left            =   1440
      TabIndex        =   16
      Top             =   960
      Width           =   1215
   End
   Begin VB.Label LHP 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
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
      Left            =   1440
      TabIndex        =   15
      Top             =   720
      Width           =   1215
   End
   Begin VB.Label LFatal 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
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
      Left            =   1440
      TabIndex        =   14
      Top             =   480
      Width           =   1215
   End
   Begin VB.Label LKills 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
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
      Left            =   1440
      TabIndex        =   13
      Top             =   240
      Width           =   1215
   End
   Begin VB.Label LTime 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "00:00"
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
      Left            =   1440
      TabIndex        =   12
      Top             =   0
      Width           =   1215
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Time Elapsed:"
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
      Left            =   120
      TabIndex        =   11
      Top             =   0
      Width           =   1215
   End
   Begin VB.Label Label11 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "SP/Sec:"
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
      Left            =   3120
      TabIndex        =   10
      Top             =   1200
      Width           =   1215
   End
   Begin VB.Label Label10 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Total SP Used:"
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
      Left            =   120
      TabIndex        =   9
      Top             =   1200
      Width           =   1215
   End
   Begin VB.Label Label9 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "MP/Sec:"
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
      Left            =   3120
      TabIndex        =   8
      Top             =   960
      Width           =   1215
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "HP/Sec:"
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
      Left            =   3120
      TabIndex        =   7
      Top             =   720
      Width           =   1215
   End
   Begin VB.Label Label7 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Total Fatalities"
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
      Left            =   120
      TabIndex        =   6
      Top             =   480
      Width           =   1215
   End
   Begin VB.Label Label6 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Total Deaths:"
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
      Left            =   120
      TabIndex        =   5
      Top             =   240
      Width           =   1215
   End
   Begin VB.Label Label5 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Total MP Used:"
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
      Left            =   120
      TabIndex        =   4
      Top             =   960
      Width           =   1215
   End
   Begin VB.Label Label4 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Total HP Damage:"
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
      Left            =   0
      TabIndex        =   3
      Top             =   720
      Width           =   1335
   End
   Begin VB.Label Label3 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Fatalities/Min:"
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
      Left            =   3120
      TabIndex        =   2
      Top             =   480
      Width           =   1215
   End
   Begin VB.Label Label2 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Deaths/Min:"
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
      Left            =   3120
      TabIndex        =   1
      Top             =   240
      Width           =   1215
   End
End
Attribute VB_Name = "fStats"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cShow_Click()
  Send ("Time Elapsed: " + LTime)
  Send ("Total Deaths: " + LKills + " [" + LAKills + "/min]")
  Send ("Total Fatalities: " + LFatal + " [" + LAFatal + "/min]")
  Send ("Total HP Damage: " + LHP + " [" + LAHP + "/sec]")
  'Send ("Total MP Used: " + LMP + " [" + LAMP + "/sec]")
  Send ("Total SP Used: " + LSP + " [" + LASP + "/sec]")
End Sub

Private Sub Form_Load()
  LoadPosition Me, "fStats"
End Sub

Private Sub LGetItem_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
Dim W%, S$
  If Battle Then
    If Button = 2 Then
      W = Rand(1, MaxWeapons)
      AddDroppedWeapon W, Weapons(W).NumUses
      Send ("The """ + Weapons(W).name + """ is lying on the floor to be picked up. (/wget-" + Weapons(W).PickMe + ")")
    Else
      If (Item2Get = 0) Then
        If kDlgBoxfn("Drop an item onto the battlefield?", 36, "Item Maker") = 6 Then
          NewItem
        End If
      Else
        If kDlgBoxfn("Zap the current item to dust?", 36, "Item Zapper") = 6 Then
          S = YourSN + " throws a lightning bolt which zaps the " + Items(Item2Get).name + " into oblivion!"
          Send (S)
          Item2Get = 0
        End If
      End If
    End If
  End If
End Sub

Private Sub LTimeLeft_Click()
Dim X As Integer
Dim S$
Dim TP As Integer
  X = Val(kDlgBoxInput("Enter new time left (in seconds), -1 for no time limit:", "Time Limit", "-1"))
  If (X <> 0) Then
    If (X = -1) Then
      TLimit = 0
      Send ("=== Time Limit Removed ===")
    Else
      TP = XTimer - TimeLimit
      TLimit = TP + X
      Send ("=== You now have " + TrimStr(Int(X / 60)) + " minutes left ===")
    End If
  End If
End Sub

Private Sub Timer1_Timer()
Dim Min%, Sec%, T$, Mi!, Tix%, Tix1%
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
  LHP = TrimStr(tHP)
  'LMP = TrimStr(TMP)
  LSP = TrimStr(tSP)
  Mi! = XTimer / 60
  On Error Resume Next
  Dm! = TKills / Mi!
  LAKills = Format$(Dm!, "##0.0##")
  Fm! = TFatal / Mi!
  LAFatal = Format$(Fm!, "##0.0##")
  Hs! = tHP / XTimer
  LAHP = Format$(Hs!, "##,##0.0##")
  'Ms! = TMP / XTimer
  'LAMP = Format$(Ms!, "##,##0.0##")
  Ss! = tSP / XTimer
  LASP = Format$(Ss!, "##,##0.0##")
  If (Item2Get = 0) Then
    LGetItem = "None"
  Else
    LGetItem = Items(Item2Get).name
  End If
  If (TLimit > 0) Then
    Tix = (XTimer - TimeLimit)
    Tix1 = (TLimit - Tix)
    Select Case Tix1
      Case Is > 60: LTimeLeft.Caption = TrimStr(Fix((TLimit - Tix) / 60)) + " minutes"
      Case 0 To 59: LTimeLeft.Caption = TrimStr(TLimit - Tix) + " seconds"
    End Select
  End If
End Sub

Private Sub NewItem()
Dim S As String
Dim S1 As String
Dim W%
  If Rand(1, 2) = 2 Or MaxWeapons = 0 Or Config.WeaponEnable = 0 Then
    S1 = Items(Item2Get).Telefrag
    Send (S1)
    Item2Get = Rand(1, MaxItem)
    S = Items(Item2Get).Spawn + " (.get)"
    Send (S)
  Else
    Do
      W = Rand(1, MaxWeapons)
      DoEvents
    Loop Until Left$(Weapons(W).PickMe, 1) <> "~"
    AddDroppedWeapon W, Weapons(W).NumUses
    Send ("The """ + Weapons(W).name + """ is lying on the floor to be picked up. (/wget-" + Weapons(W).PickMe + ")")
  End If
End Sub

