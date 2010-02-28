VERSION 5.00
Begin VB.Form fOption 
   Appearance      =   0  'Flat
   BackColor       =   &H00400000&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Battle Settings"
   ClientHeight    =   3345
   ClientLeft      =   855
   ClientTop       =   1500
   ClientWidth     =   5040
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00000000&
   Icon            =   "foption.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3345
   ScaleWidth      =   5040
   Begin VB.CommandButton cMinimize 
      Caption         =   "_"
      Height          =   255
      Left            =   4800
      TabIndex        =   36
      Top             =   0
      Width           =   255
   End
   Begin VB.CheckBox cOldSchool 
      Appearance      =   0  'Flat
      BackColor       =   &H00400000&
      Caption         =   "OldSchool Kombat"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   35
      Top             =   720
      Width           =   1695
   End
   Begin VB.CheckBox cMulti 
      Appearance      =   0  'Flat
      BackColor       =   &H00400000&
      Caption         =   "Multitarget Moves"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   34
      Top             =   1680
      Width           =   1815
   End
   Begin VB.TextBox tFragLimit 
      Height          =   285
      Left            =   3120
      TabIndex        =   33
      Text            =   "0"
      Top             =   1800
      Width           =   615
   End
   Begin VB.CheckBox cFours 
      Appearance      =   0  'Flat
      BackColor       =   &H00400000&
      Caption         =   "Extended Supers"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   13
      Top             =   1920
      Width           =   1695
   End
   Begin VB.CheckBox cFlag 
      Appearance      =   0  'Flat
      BackColor       =   &H00400000&
      Caption         =   "Capture the Flag"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   14
      Top             =   960
      Width           =   1575
   End
   Begin VB.CheckBox cRespawn 
      Appearance      =   0  'Flat
      BackColor       =   &H00400000&
      Caption         =   "Respawn"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   31
      Top             =   1200
      Width           =   1575
   End
   Begin VB.CheckBox cDefect 
      Appearance      =   0  'Flat
      BackColor       =   &H00400000&
      Caption         =   "Defects"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   5
      Top             =   1440
      Width           =   1575
   End
   Begin VB.CheckBox cSameChar 
      Appearance      =   0  'Flat
      BackColor       =   &H00400000&
      Caption         =   "Pick Same Char."
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   1
      Top             =   480
      Width           =   1575
   End
   Begin VB.CheckBox Check1 
      Appearance      =   0  'Flat
      BackColor       =   &H00400000&
      Caption         =   "Zombie Mode"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   18
      Top             =   240
      Width           =   1575
   End
   Begin VB.TextBox tSP 
      Height          =   285
      Left            =   4560
      TabIndex        =   30
      Text            =   "1000"
      Top             =   1320
      Width           =   495
   End
   Begin VB.TextBox tHP 
      Height          =   285
      Left            =   4560
      TabIndex        =   29
      Text            =   "600"
      Top             =   1080
      Width           =   495
   End
   Begin VB.TextBox tDmg 
      Height          =   315
      Left            =   3120
      TabIndex        =   24
      Text            =   "100"
      Top             =   1080
      Width           =   495
   End
   Begin VB.TextBox tFlaCon 
      Height          =   285
      Left            =   3120
      TabIndex        =   22
      Text            =   "60"
      Top             =   1440
      Width           =   615
   End
   Begin VB.CommandButton cLog 
      Appearance      =   0  'Flat
      Caption         =   "Log File"
      Height          =   255
      Left            =   3840
      TabIndex        =   20
      Top             =   3480
      Visible         =   0   'False
      Width           =   1215
   End
   Begin VB.ComboBox cUnit 
      Appearance      =   0  'Flat
      Height          =   315
      Left            =   3720
      Style           =   2  'Dropdown List
      TabIndex        =   9
      Top             =   720
      Width           =   1215
   End
   Begin VB.TextBox tbLimit 
      Height          =   315
      Left            =   3120
      TabIndex        =   8
      Text            =   "0"
      Top             =   720
      Width           =   615
   End
   Begin VB.ComboBox tFontName 
      Appearance      =   0  'Flat
      Height          =   315
      Left            =   2040
      Sorted          =   -1  'True
      TabIndex        =   19
      Text            =   "Arial"
      Top             =   2160
      Width           =   2055
   End
   Begin VB.CommandButton bShow 
      Appearance      =   0  'Flat
      Caption         =   "Show &Options"
      Height          =   255
      Left            =   0
      TabIndex        =   3
      Top             =   2760
      Width           =   1815
   End
   Begin VB.CommandButton cArena 
      Appearance      =   0  'Flat
      Caption         =   "Pick Arena..."
      Height          =   255
      Left            =   0
      TabIndex        =   2
      Top             =   2520
      Width           =   1815
   End
   Begin VB.CheckBox cNoJoin 
      Appearance      =   0  'Flat
      BackColor       =   &H00400000&
      Caption         =   "Disable Joining"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   1575
   End
   Begin VB.TextBox tReason 
      Height          =   285
      Left            =   2520
      TabIndex        =   16
      Text            =   "none"
      Top             =   0
      Width           =   1695
   End
   Begin VB.CommandButton cTest 
      Appearance      =   0  'Flat
      Caption         =   "Test"
      Height          =   495
      Left            =   3360
      TabIndex        =   15
      Top             =   2520
      Width           =   735
   End
   Begin VB.TextBox tFontColor 
      Height          =   285
      Left            =   2040
      TabIndex        =   12
      Top             =   2520
      Width           =   1215
   End
   Begin VB.TextBox tGetRate 
      Height          =   285
      Left            =   3120
      TabIndex        =   11
      Text            =   "0"
      Top             =   360
      Width           =   615
   End
   Begin VB.CommandButton bDone 
      Appearance      =   0  'Flat
      Caption         =   "&Done"
      Default         =   -1  'True
      Height          =   495
      Left            =   4200
      TabIndex        =   4
      Top             =   2520
      Width           =   735
   End
   Begin VB.Label StatLine 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Status Bar."
      BeginProperty Font 
         Name            =   "Arial Narrow"
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
      TabIndex        =   27
      Top             =   3120
      Width           =   5055
   End
   Begin VB.Label Label3 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Frags To Win:"
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
      Height          =   255
      Left            =   1920
      TabIndex        =   32
      Top             =   1800
      Width           =   1095
   End
   Begin VB.Label LArenaName 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00400000&
      Caption         =   "Boring"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   6
      Top             =   2280
      Width           =   1815
   End
   Begin VB.Label Label11 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Max SP"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3840
      TabIndex        =   28
      Top             =   1320
      Width           =   615
   End
   Begin VB.Label Label9 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Max HP"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3840
      TabIndex        =   26
      Top             =   1080
      Width           =   615
   End
   Begin VB.Label Label8 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "%"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   12
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3600
      TabIndex        =   25
      Top             =   1080
      Width           =   255
   End
   Begin VB.Label Label7 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Dmg. Multiplier"
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
      Height          =   255
      Left            =   1920
      TabIndex        =   23
      Top             =   1080
      Width           =   1095
   End
   Begin VB.Label Label6 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Flag Time Limit (in seconds):"
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
      Height          =   375
      Left            =   1920
      TabIndex        =   21
      Top             =   1320
      Width           =   1095
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Time Limit:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1800
      TabIndex        =   7
      Top             =   720
      Width           =   1215
   End
   Begin VB.Label Label5 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Reason:"
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
      Height          =   255
      Left            =   1800
      TabIndex        =   17
      Top             =   0
      Width           =   615
   End
   Begin VB.Label Label2 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Item Rate:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   375
      Left            =   1800
      TabIndex        =   10
      Top             =   360
      Width           =   1215
   End
End
Attribute VB_Name = "fOption"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

' FOPTION.FRM
' Happy-Fun-Options Menu!!!

Const MyName = "fOption"

Option Explicit

Private Sub bDone_Click()
Dim P1$, P2$, P4$, PO%
  Me.Hide
  P1$ = "Millenium"
  P2$ = "Join"
  If Config.NoJoin = 0 Then
    P4$ = "1"
  Else
    P4$ = "0"
  End If
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "SameChar"
  If Config.SameChar <> 0 Then
    P4$ = "1"
  Else
    P4$ = "0"
  End If
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  'P2$ = "InfMP"
  'If Config.InfMP <> 0 Then
  '  P4$ = "1"
  'Else
  '  P4$ = "0"
  'End If
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "Flag"
  If Config.Flag <> 0 Then
    P4$ = "1"
  Else
    P4$ = "0"
  End If
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "Fours"
  If Config.Fours <> 0 Then
    P4$ = "1"
  Else
    P4$ = "0"
  End If
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "Multitarget"
  If Config.Multi <> 0 Then
    P4$ = "1"
  Else
    P4$ = "0"
  End If
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "Respawn"
  If Config.Respawn <> 0 Then
    P4$ = "1"
  Else
    P4$ = "0"
  End If
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "Defect"
  If Config.Defect <> 0 Then
    P4$ = "1"
  Else
    P4$ = "0"
  End If
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "Zombie"
  If ZombieM = 1 Then
    P4$ = "1"
  ElseIf ZombieM = 0 Then
    P4$ = "0"
  End If
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "OldSchool"
  If Config.OldSchool = 1 Then
    P4$ = "1"
  ElseIf Config.OldSchool = 0 Then
    P4$ = "0"
  End If
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "FontName"
  P4$ = Config.FontName
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "FontColor"
  P4$ = Config.FontColor
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "WhyNoJoin"
  P4$ = Config.Reason
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "FlaCon"
  P4$ = tFlaCon.Text
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "DMult"
  P4$ = tDmg.Text
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "MaxHP"
  P4$ = tHP.Text
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  'P2$ = "MaxMP"
  'P4$ = tMP.Text
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "MaxSP"
  P4$ = tSP.Text
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "GetRate"
  P4$ = tGetRate.Text
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  P2$ = "FragLimit"
  P4$ = tFragLimit.Text
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
  BalancingAct
End Sub

Private Sub bDone_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Save changes and exit"
End Sub

Private Sub bShow_Click()
Dim XX%, XX5%
Dim Msg As String
  Send ("Chibot Phude Settings:")
  Send ("Version: [" + VerID + "]")
  If Config.NoJoin Then
    Send ("Join: [Nobody else may join]")
  Else
    Send ("Join: [Anyone may join until the game is full]")
  End If
  If Config.SameChar Then
    Send ("Pick Same Character: [Dupes of same character allowed]")
  Else
    Send ("Pick Same Character: [No two people may be the same character]")
  End If
  If Config.OldSchool <> 0 Then Msg = "Battle: OldSchool " Else Msg = "Battle: Enhanced "
  If Config.Flag = 0 Then
    If Config.Respawn <> 0 Then Msg = Msg + "Respawn "
    If Config.Defect <> 0 Then Msg = Msg + "with Defects "
    If FragLimit < 0 Then
      Msg = Msg + " [" + TrimStr(Abs(FragLimit)) + " Fatalities to win]"
    ElseIf FragLimit > 0 Then
      Msg = Msg + " [" + TrimStr(FragLimit) + " Frags to Win]"
    Else
      Msg = Msg + " [No Frag Limit]"
    End If
  Else
    Msg = Msg + "Capture the Flag"
  End If
  Send (Msg)
  'If Config.InfMP Then
  '  Send ("Infinite MP: [All moves are free!]")
  'Else
  '  Send ("Infinite MP: [NO]")
  'End If
  Send ("Max HP: [" + TrimStr(MaxHP) + "] Max SP: [" + TrimStr(MaxSP) + "]")
  Send ("Damage Multiplier: [" + TrimStr(DMult) + "%]")
  If (Config.Fours = 0) Then
    Send ("Level 4 and 5 Supers: [DISABLED]")
  Else
    Send ("Level 4 and 5 Supers: [ENABLED]")
  End If
  If (ZombieM = 0) Then
    Send ("First to Die: [Dies]")
  Else
    Send ("First to Die: [Becomes a Zombie]")
  End If
  If (GetRate = 0) Then
    Send ("Item Spawn Rate: [Items Disabled]")
  Else
    Send ("Item Spawn Rate: [" + TrimStr(GetRate) + "]")
  End If
  If TLimit = 0 Then
    Send ("Time Limit: [None]")
  Else
    Msg = "Time Limit: ["
    XX = TLimit
    If (TLimit > 60) Then
      If (TLimit > 60 * 60) Then
        XX5% = Int(XX / 60 / 60)
        Msg = Msg + TrimStr(XX5%) + " hours "
        XX = XX - (XX5% * 60 * 60)
      End If
      XX5% = Int(XX / 60)
      Msg = Msg + TrimStr(XX5%) + " minutes "
      XX = XX - (XX5% * 60)
    End If
    Msg = Msg + TrimStr(XX) + " seconds]"
    Send (Msg)
  End If
  ShowArena
End Sub

Private Sub bShow_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shows options to chatroom"
End Sub

Private Sub cArena_Click()
  fArena.Show
End Sub

Private Sub cArena_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
Static T%
  If Button = 2 Then T = T + 1
  If T = 10 Then
    Send ("Langolier arena unlocked.")
    MaxArena = MaxArena + 1
    ReDim Preserve Arena(MaxArena)
    Arena(MaxArena) = Langolier
    Lang = MaxArena
  End If
End Sub

Private Sub cArena_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Change Arena"
End Sub

Private Sub cDefect_Click()
  Config.Defect = cDefect.Value
End Sub

Private Sub cDefect_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Allows team changes"
End Sub

Private Sub cFlag_Click()
  Config.Flag = cFlag.Value
  If (Config.Flag = 1) Then
    Config.Respawn = 1
    cRespawn.Value = 1
    cRespawn.Enabled = False
    Config.Defect = 0
    cDefect.Value = 0
    cDefect.Enabled = False
  Else
    cRespawn.Enabled = True
    cDefect.Enabled = True
  End If
End Sub

Private Sub cFlag_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Enables Capture the Flag battle"
End Sub

Private Sub cFours_Click()
  Config.Fours = cFours.Value
End Sub

Private Sub cFours_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Enables Level 4 and 5 Supers"
End Sub

Private Sub Check1_Click()
  ZombieM = Check1
End Sub

Private Sub Check1_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "First person to die becomes Zombie"
End Sub

Private Sub cLog_Click()
  If Trim(Config.WLog) = "" Then
    Config.WLog = kDlgBoxInput("Enter log filename:", "Battle Log", "chibat.txt")
    LogFileOpen (Config.WLog)
  Else
    LogFileClose
    kDlgBox "Log file closed.", 64, "ChUB 2000"
    Config.WLog = ""
  End If
End Sub

Private Sub cMinimize_Click()
  Me.WindowState = 1
End Sub

Private Sub cMulti_Click()
  Config.Multi = cMulti.Value
End Sub

Private Sub cMulti_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Allows moves that hit more than one player"
End Sub

Private Sub cNoJoin_Click()
  Config.NoJoin = cNoJoin.Value
End Sub

Private Sub cNoJoin_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Allows joining during battle"
End Sub

Private Sub cOldSchool_Click()
  Config.OldSchool = cOldSchool.Value
  Select Case Config.OldSchool
    Case 0: Send ("OldSchool Kombat mode disabled!")
    Case 1: Send ("OldSchool Kombat mode enabled! (No Runes or Weapons)")
  End Select
End Sub

Private Sub cOldSchool_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "OldSchool Kombat. No Runes or Weapons."
End Sub

Private Sub cRespawn_Click()
  Config.Respawn = cRespawn.Value
End Sub

Private Sub cRespawn_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Come back to life after 30 game seconds"
End Sub

Private Sub cSameChar_Click()
  Config.SameChar = cSameChar.Value
End Sub

Private Sub cSameChar_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Allows more than one person to pick the same char"
End Sub

Private Sub cTest_Click()
  ScrollSend ("This is font " + Trim(Config.FontName) + " and color " + Trim(Config.FontColor) + ".")
End Sub

Private Sub cTest_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Test current font"
End Sub

Private Sub cUnit_Change()
  tbLimit_Change
End Sub

Private Sub Form_Load()
Dim X%
  On Error Resume Next
  LoadPosition Me, MyName
  SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3             ' Always on top
  LArenaName.Caption = Arena(CurArena).Name
  cNoJoin.Value = Config.NoJoin
  cSameChar.Value = Config.SameChar
  cDefect.Value = Config.Defect
  'Config.InfMP = 1
  Select Case ZombieM
    Case 1, 2: Check1 = 1
    Case 0: Check1 = 0
  End Select
  tbLimit.Text = TrimStr(TLimit)
  cUnit.AddItem "seconds"
  cUnit.AddItem "minutes"
  cUnit.AddItem "hours???"
  cUnit.ListIndex = 0
  tGetRate.Text = TrimStr(GetRate)
  tReason.Text = Config.Reason
  tFontColor = Config.FontColor
  'Cmd.Color = Val("&H" + ToHTML(Config.FontColor))
  'Cmd.FontName = Config.FontName
  For X% = 1 To Screen.FontCount
    tFontName.AddItem Screen.Fonts(X)
  Next X%
  tFontName.Text = Config.FontName
  Config.FontColor = tFontColor
  tFlaCon.Text = TrimStr(Config.FlaCon)
  tHP.Text = TrimStr(MaxHP)
  tSP.Text = TrimStr(MaxSP)
  cFlag = Config.Flag
  cRespawn = Config.Respawn
  cFours = Config.Fours
  tFragLimit.Text = TrimStr(FragLimit)
  tDmg.Text = TrimStr(DMult)
  cMulti = Config.Multi
  cOldSchool = Config.OldSchool
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Battle Settings"
End Sub

Private Sub Form_Unload(Cancel As Integer)
  SavePosition Me, MyName

End Sub

Private Sub LArenaName_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Current Arena"
End Sub

Private Sub tbLimit_Change()
  Select Case cUnit.ListIndex
    Case 0: TLimit = Val(tbLimit)
    Case 1: TLimit = Val(tbLimit) * 60
    Case 2: TLimit = Val(tbLimit) * 60 * 60
  End Select
End Sub

Private Sub tbLimit_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Change time limit for match"
End Sub

Private Sub tDmg_Change()
  DMult = Val(tDmg)
End Sub

Private Sub tDmg_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Changes amount of damage done. 200=Double 50=Half"
End Sub

Private Sub tFlaCon_Change()
  Config.FlaCon = Val(tFlaCon.Text)
End Sub

Private Sub tFlaCon_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Keep enemy flag for this long in Capture the Flag to win"
End Sub

Private Sub tFontColor_Change()
  Config.FontColor = tFontColor
  'Me.BackColor = cmd.Color
  'Label1.ForeColor = 16777215 Xor Me.BackColor
  'Label2.ForeColor = 16777215 Xor Me.BackColor
  'Label3.ForeColor = 16777215 Xor Me.BackColor
  'Label4.ForeColor = 16777215 Xor Me.BackColor
  'Label5.ForeColor = 16777215 Xor Me.BackColor
  'Check1.ForeColor = 16777215 Xor Me.BackColor
  'Check1.BackColor = Me.BackColor
  'LArenaName.ForeColor = 16777215 Xor Me.BackColor
  'LArenaName.BackColor = Me.BackColor
  On Error Resume Next
  'If YourSN = "Player" Then
  '  fOffline!tDebug.FontName = Config.FontName
  '  fOffline!tDebug.ForeColor = Val("&H" + ToHTML(Config.FontColor))
  '  fOffline!tDebug.BackColor = 16777215 Xor fOffline!tDebug.ForeColor
  'End If
NoWay5:
End Sub

Private Sub tFontColor_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Change font color"
End Sub

Private Sub tFontName_Change()
  Config.FontName = tFontName
  On Error GoTo Nope1
  tFontName.FontName = tFontName
  'If YourSN = "Player" Then
  '  fOffline!tDebug.FontName = Config.FontName
  '  fOffline!tDebug.ForeColor = Val("&H" + ToHTML(Config.FontColor))
  '  fOffline!tDebug.BackColor = 16777215 Xor fOffline!tDebug.ForeColor
  'End If
Nope1: Exit Sub
End Sub

Private Sub tFontName_Click()
  Config.FontName = tFontName
  On Error GoTo Nope
  tFontName.FontName = tFontName
  'If YourSN = "Player" Then
  '  fOffline!tDebug.FontName = Config.FontName
  '  fOffline!tDebug.ForeColor = Val("&H" + ToHTML(Config.FontColor))
  '  fOffline!tDebug.BackColor = 16777215 Xor Me.BackColor
  'End If
Nope: Exit Sub
End Sub

Private Sub tFragLimit_Change()
  FragLimit = Val(tFragLimit)
End Sub

Private Sub tFragLimit_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Frag limit: Positive value = Kills, Negative value = Fatalities"
End Sub

Private Sub tGetRate_Change()
  GetRate = Val(tGetRate)
End Sub

Private Sub tGetRate_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Item respawn rate. Good value: 100"
End Sub

Private Sub tHP_Change()
Dim X%
  MaxHP = Val(tHP)
  'For X = 1 To MaxPlayers
  '  P(X).MaxHP = MaxHP
  'Next X
  BalancingAct
End Sub

Private Sub tHP_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Changes maximum HP allowed. Also affects global move strength and danger HP level"
End Sub

Private Sub tReason_Change()
  Config.Reason = tReason
End Sub

Private Sub tReason_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Change reason displayed for not allowing joining"
End Sub

Private Sub tSP_Change()
  MaxSP = Val(tSP)
End Sub

Private Sub tSP_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Maximum amount of Super points allowed"
End Sub

