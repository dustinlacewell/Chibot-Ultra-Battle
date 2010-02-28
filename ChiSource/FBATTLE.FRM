VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.2#0"; "COMCTL32.OCX"
Begin VB.Form fBattle 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Phude Battle?"
   ClientHeight    =   5280
   ClientLeft      =   4620
   ClientTop       =   3930
   ClientWidth     =   6555
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00FFFFFF&
   Icon            =   "FBATTLE.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5280
   ScaleWidth      =   6555
   Begin ComctlLib.Toolbar Toolbar1 
      Align           =   1  'Align Top
      Height          =   645
      Left            =   0
      TabIndex        =   22
      Top             =   0
      Width           =   6555
      _ExtentX        =   11562
      _ExtentY        =   1138
      ButtonWidth     =   1164
      ButtonHeight    =   979
      AllowCustomize  =   0   'False
      Appearance      =   1
      _Version        =   327682
      BeginProperty Buttons {0713E452-850A-101B-AFC0-4210102A8DA7} 
         NumButtons      =   8
         BeginProperty Button1 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Exit"
            Key             =   ""
            Description     =   "Outta here!"
            Object.Tag             =   ""
         EndProperty
         BeginProperty Button2 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Begin"
            Key             =   ""
            Description     =   "Begins the battle."
            Object.Tag             =   ""
         EndProperty
         BeginProperty Button3 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Pause"
            Key             =   ""
            Description     =   "Pauses/unpauses the battle."
            Object.Tag             =   ""
         EndProperty
         BeginProperty Button4 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Moves"
            Key             =   ""
            Description     =   "Displays Moves window"
            Object.Tag             =   ""
         EndProperty
         BeginProperty Button5 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Chars"
            Key             =   ""
            Description     =   "Scrolls character list"
            Object.Tag             =   ""
         EndProperty
         BeginProperty Button6 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Teams"
            Key             =   ""
            Description     =   "Scrolls team list"
            Object.Tag             =   ""
         EndProperty
         BeginProperty Button7 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "NeoStat"
            Key             =   ""
            Description     =   "Shows status window"
            Object.Tag             =   ""
         EndProperty
         BeginProperty Button8 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Stats"
            Key             =   ""
            Description     =   "Displays game stats"
            Object.Tag             =   ""
         EndProperty
      EndProperty
   End
   Begin VB.CommandButton cGodMode 
      Caption         =   "CLICK FOR GOD MODE"
      Height          =   255
      Left            =   840
      MousePointer    =   12  'No Drop
      TabIndex        =   0
      Top             =   1560
      Width           =   2175
   End
   Begin VB.CommandButton cMystery 
      Caption         =   "????"
      Height          =   255
      Left            =   0
      TabIndex        =   2
      Top             =   1560
      Width           =   735
   End
   Begin VB.CommandButton cTwit 
      Caption         =   "Twit"
      Height          =   255
      Left            =   0
      TabIndex        =   21
      Top             =   1320
      Width           =   735
   End
   Begin VB.Timer tiFullAuto 
      Interval        =   1
      Left            =   2520
      Top             =   3480
   End
   Begin VB.CommandButton cPaused 
      Caption         =   "It's Paused!"
      Height          =   255
      Left            =   3120
      TabIndex        =   1
      Top             =   1560
      Width           =   1335
   End
   Begin VB.CommandButton Command5 
      Caption         =   "Slow Down!"
      Height          =   255
      Left            =   3120
      TabIndex        =   10
      Top             =   1320
      Width           =   1335
   End
   Begin VB.CommandButton Command4 
      Caption         =   "No Scrolling!"
      Height          =   255
      Left            =   3120
      TabIndex        =   9
      Top             =   1080
      Width           =   1335
   End
   Begin VB.TextBox tLag 
      Alignment       =   2  'Center
      BackColor       =   &H00808080&
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
      Height          =   285
      Left            =   3960
      TabIndex        =   14
      Text            =   "1000"
      Top             =   2040
      Width           =   495
   End
   Begin VB.HScrollBar hsLag 
      Height          =   255
      LargeChange     =   100
      Left            =   120
      Max             =   10000
      Min             =   100
      SmallChange     =   10
      TabIndex        =   12
      Top             =   2040
      Value           =   1000
      Width           =   3735
   End
   Begin VB.CommandButton cbMidi 
      Caption         =   "Midi"
      Height          =   255
      Left            =   0
      TabIndex        =   17
      Top             =   1080
      Width           =   735
   End
   Begin VB.HScrollBar hsScroll 
      Height          =   240
      LargeChange     =   100
      Left            =   840
      Max             =   4000
      Min             =   1
      SmallChange     =   10
      TabIndex        =   3
      Top             =   1080
      Value           =   1
      Width           =   2175
   End
   Begin VB.TextBox tChatLine 
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   315
      Left            =   120
      TabIndex        =   8
      Top             =   2400
      Width           =   4335
   End
   Begin VB.Timer CTimer 
      Interval        =   1000
      Left            =   600
      Top             =   3480
   End
   Begin VB.OptionButton oLin 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      Caption         =   "4 Lines"
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   4
      Left            =   3000
      TabIndex        =   7
      Top             =   3120
      Width           =   855
   End
   Begin VB.OptionButton oLin 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      Caption         =   "3 Lines"
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   3
      Left            =   2040
      TabIndex        =   6
      Top             =   3120
      Width           =   975
   End
   Begin VB.OptionButton oLin 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      Caption         =   "2 Lines"
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   2
      Left            =   1080
      TabIndex        =   5
      Top             =   3120
      Width           =   975
   End
   Begin VB.OptionButton oLin 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      Caption         =   "1 Line"
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   1
      Left            =   240
      TabIndex        =   4
      Top             =   3120
      Width           =   855
   End
   Begin VB.Timer tiMoveDmg 
      Interval        =   100
      Left            =   2040
      Top             =   3480
   End
   Begin VB.Timer tiCPU 
      Interval        =   750
      Left            =   1560
      Top             =   3480
   End
   Begin VB.Timer tiMisc 
      Interval        =   2000
      Left            =   1080
      Top             =   3480
   End
   Begin VB.CommandButton bSend 
      Appearance      =   0  'Flat
      Caption         =   "Send"
      Default         =   -1  'True
      Height          =   375
      Left            =   3600
      TabIndex        =   11
      Top             =   3480
      Width           =   615
   End
   Begin VB.Timer tiDoMove 
      Interval        =   100
      Left            =   120
      Top             =   3480
   End
   Begin VB.Label LFour 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "3"
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
      Left            =   2040
      TabIndex        =   13
      Top             =   600
      Width           =   255
   End
   Begin VB.Label LLag 
      Alignment       =   2  'Center
      Caption         =   "Lagometer. Lower Value = Faster Speed"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   120
      TabIndex        =   15
      Top             =   1800
      Width           =   4335
   End
   Begin VB.Label StatLine 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "???"
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
      TabIndex        =   16
      Top             =   2760
      Width           =   4515
   End
   Begin VB.Line Line1 
      X1              =   0
      X2              =   4440
      Y1              =   960
      Y2              =   960
   End
   Begin VB.Label LItem 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "darnedifiknow!"
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
      Left            =   2160
      TabIndex        =   18
      Top             =   600
      Width           =   2055
   End
   Begin VB.Label Label4 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "neverneverland"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   -1  'True
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   0
      TabIndex        =   19
      Top             =   600
      Width           =   2175
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Scroll Delay: ????"
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
      Left            =   840
      TabIndex        =   20
      Top             =   1320
      Width           =   2175
   End
   Begin VB.Menu mBattle 
      Caption         =   "&Battle"
      Begin VB.Menu mBeginBattle 
         Caption         =   "&Begin Battle"
         Shortcut        =   ^B
      End
      Begin VB.Menu mPauseBattle 
         Caption         =   "&Pause Battle"
         Shortcut        =   ^P
      End
      Begin VB.Menu mExitBattle 
         Caption         =   "E&xit Battle"
         Shortcut        =   ^X
      End
      Begin VB.Menu cMaxP 
         Caption         =   "&Add More Players"
      End
   End
   Begin VB.Menu mShow 
      Caption         =   "&Show"
      Begin VB.Menu mShowMoves 
         Caption         =   "&Moves"
      End
      Begin VB.Menu mShowChars 
         Caption         =   "&Chars"
      End
      Begin VB.Menu mShowTeams 
         Caption         =   "&Teams"
         Shortcut        =   ^T
      End
      Begin VB.Menu cStat2 
         Caption         =   "&Stats Window"
         Shortcut        =   ^N
      End
      Begin VB.Menu cbStats 
         Caption         =   "&Game Stats"
      End
   End
   Begin VB.Menu mToggles 
      Caption         =   "&Toggles"
      Begin VB.Menu cJoin 
         Caption         =   "&Joining"
         Checked         =   -1  'True
      End
      Begin VB.Menu cStatus 
         Caption         =   "/&status"
         Checked         =   -1  'True
      End
      Begin VB.Menu cMoves 
         Caption         =   "/&moves"
         Checked         =   -1  'True
      End
      Begin VB.Menu cCPU 
         Caption         =   "/&cpu"
      End
      Begin VB.Menu cAttacking 
         Caption         =   "/&attacking"
         Checked         =   -1  'True
      End
      Begin VB.Menu cVersion 
         Caption         =   "/&version"
         Checked         =   -1  'True
      End
      Begin VB.Menu cInfo 
         Caption         =   "/&help"
         Checked         =   -1  'True
      End
      Begin VB.Menu cTeam 
         Caption         =   "&Team Functions (defect, etc.)"
         Checked         =   -1  'True
      End
   End
   Begin VB.Menu cIMs 
      Caption         =   "&IM's"
      Begin VB.Menu cIMOn 
         Caption         =   "&ON"
      End
      Begin VB.Menu cIMOff 
         Caption         =   "O&FF"
      End
   End
End
Attribute VB_Name = "fBattle"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit

Const GoodStatus = 60
Const BadStatus = 20

Const HostageLength = 30

Dim Tm As Long
Dim AllSlot As Boolean
Dim Six%

Const MyName = "fBattle"

Private Sub bSend_Click()
Dim S As String
Dim Q$
  S = tChatLine.Text
  tChatLine.Text = ""
  'If (DebugWindow) Then
  '  fOffline!LSendStr.Caption = YourSN + ": " + S
  'Else
    If (Mid$(S, 1, 1) = "/") Or (Mid$(S, 1, 1) = "~") Then
      If (GetSave > MaxRay) Then GetSave = 1
      Q$ = YourSN
      GetRay(GetSave) = Q$ + ":  " + Trim(S)
      If (GetSave + 1 > MaxRay) Then
        GetSave = 1
      Else
        GetSave = GetSave + 1
      End If
    Else
      ScrollSend ("<i>" + S)
    End If
  'End If
End Sub

Private Sub cAttacking_Click()
  cAttacking.Checked = Not cAttacking.Checked
End Sub

Private Sub cbMidi_Click()
  fMidi.Show
End Sub

Private Sub cbMidi_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If (Button = 2) Then fWav.Show
End Sub

Private Sub cbMidi_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Play some music!"
End Sub

Private Sub cbStats_Click()
  fStats.Show
End Sub

Private Sub cbStats_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Show game statistics"
End Sub

Private Sub cCPU_Click()
  cCPU.Checked = Not cCPU.Checked
End Sub

Private Sub cGodMode_GotFocus()
  cGodMode.Visible = False
End Sub

Private Sub cGodMode_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  cGodMode_GotFocus
End Sub

Private Sub cIMOff_Click()
  IMControl ("OFF")
End Sub

Private Sub cIMOff_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Turn IM's Off"
End Sub

Private Sub cIMOn_Click()
  IMControl ("ON")
End Sub

Private Sub cIMOn_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Turn IM's On"
End Sub

Private Sub cInfo_Click()
  cInfo.Checked = Not cInfo.Checked
End Sub

Private Sub cJoin_Click()
  cJoin.Checked = Not cJoin.Checked
End Sub

Private Sub cMaxP_Click()
Dim X As Integer
Dim y2 As Integer
  X = Val(kDlgBoxInput("Enter the number of players to increase MaxPlayers by:", "MaxPlayer Increase", "1"))
  If (X > 0) And (MaxPlayers + X < fPreBattl!vsMaxPlayers.Max) Then
    y2 = MaxPlayers
    MaxPlayers = MaxPlayers + X
    ReDim Preserve P(MaxPlayers)
    ReDim Preserve Vote(MaxPlayers)
    Send ("MaxPlayers increased by " + TrimStr(X) + ".")
    For X = y2 To MaxPlayers
      P(X).MaxHP = MaxHP
    Next X
    fPreBattl!vsMaxPlayers = MaxPlayers
  End If
End Sub

Private Sub cMaxP_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Allow more people to play."
End Sub

Private Sub cMoves_Click()
  cMoves.Checked = Not cMoves.Checked
End Sub

Private Sub cMystery_Click()
Dim X As Integer
Dim Z As Integer
Dim X1%, X2%, X6%, x7%
Dim L1#, L2#
  X = Rand(1, 24)
  Select Case X
    Case 1: AOLTitle ("America On LSD")
    Case 2: AOLTitle ("Team Rocket Ownz U")
    Case 3: AOLTitle ("why am i changing the aol title bar?")
    Case 4: AOLTitle ("Are we having fun yet?")
    Case 5:
      cMystery.Caption = ""
      For Z = 1 To 17
        cMystery.Caption = cMystery.Caption + Chr$(Rand(1, 255))
      Next Z
    Case 6: Send ("Congratulations, your Host has pushed the Mystery Button!")
    Case 7: AOLTitle ("AOHell")
    Case 8: kDlgBox "We told you not to push it. Now we'll sic Weezing on you.", 48, "Team Rocket"
    Case 9: kDlgBox "Ow! That hurts!", 48, "Mystery Button"
    Case 10: Command5.Caption = "No Guts"
    Case 11: kDlgBox "I wouldn't do that if I were you.", 48, "Mystery Button"
    Case 12: AOLTitle ("Steve Case is a dildo!")
    Case 13: AOLTitle ("Get //\\// or get out.")
    Case 14: Send ("<u>I am made of Phued, no?")
    Case 15: Send ("Look at that -- your host wants to know what the Mystery Button does.")
    Case 16: Send ("I lied -- you CAN'T be Kefka.")
    Case 17: Send ("Mystery Button say, tell your host to stop pushing Mystery Button!")
    Case 18: Command5.Caption = "Slow Down"
    Case 19: kDlgBox "We told you not to push it. Now we'll sic Arbok on you.", 48, "Team Rocket"
    Case 20:
      X1% = Val(kDlgBoxInput("Enter a number.", "It Does Math, Too!", "2"))
      If X1% = 0 Then X1% = 2
      X2% = Val(kDlgBoxInput("Enter another number.", "It Does Math, Too!", "2"))
      If X2% = 0 Then X2% = 2
      Send ("Mystery Button do math. " + TrimStr(X1) + "+" + TrimStr(X2) + "=" + TrimStr(Int((X1 + X2 + 1) * 3 / 2)) + ". Huh huh. Mystery Button gonna graduate.")
    Case 21:
      L1# = Rand(150, 10000)
      L2# = L1# ^ 2
      Send ("Do you know what the square root of " + TrimStr(L2#) + " is? " + TrimStr(L1#) + ".")
    Case 22:
      Send ("Forget Jessie and James! Around here, we do things the Kamek way!")
      Send ("To destroy the world with devastation...")
      Send ("To annihilate all peoples from within their nation...")
      Send ("To make humans cower with a wave of my hand!")
      Send ("To spread my reign of terror across the land.")
      Send ("My name is Kamek! Believe me, this is no joke!")
      Send ("Surrender NOW, or abandon all hope.")
      Send (" -- Team Kamek slogan Copyright 1999 by Kamek --")
    Case 23:
      Do
        X6 = 0
        X6 = kDlgBoxfn("", Rand(32, 37), "")
        X6 = kDlgBoxfn("", Rand(32, 37), "")
        X6 = kDlgBoxfn("", Rand(32, 37), "")
        X6 = kDlgBoxfn("", Rand(32, 37), "")
        X6 = kDlgBoxfn("", Rand(32, 37), "")
        x7 = kDlgBoxfn("Are you pissed off yet?", 36, "")
      Loop Until (x7 = 6)
      Send ("Yeesh! Your host is SUCH a grouch! If I were you I wouldn't bother him or her today.")
    Case 24:
      Send ("To protect the world from devastation...")
      Send ("To unite all peoples within our nation...")
      Send ("To denounce the evils of truth and love!")
      Send ("To extend our reach to the stars above!")
      Send ("Jesse!  James!")
      Send ("Team Rocket! Blast off at the speed of light!")
      Send ("Surrender now or prepare to fight!")
      Send ("TM and Copyright 1999 by Team Rocket. Provided to you")
      Send ("courtesy of the Mystery Button, owned by Team Rocket.")
  End Select
End Sub

Private Sub cMystery_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Property of Team Rocket. Do not touch!"
End Sub

Private Sub Command4_Click()
  Send ("<font color=""#FF0000""><b><i><u>>>>---!!!~~~ NO SCROLLING ~~~!!!---<<<")
End Sub

Private Sub Command4_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
Dim S$
  If (Button = 2) Then
    S$ = kDlgBoxInput("No what?", "No...?", "scrolling")
    S$ = UCase(S$)
    Send ("<font color=""#FF0000""><b><i><u>>>>---!!!~~~ NO " + S$ + " ~~~!!!---<<<")
  End If
End Sub

Private Sub Command4_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "I'll tell them to stop scrolling!"
End Sub

Private Sub Command5_Click()
  If (Command5.Caption = "No Guts") Then
    Send ("<font color=""#FF0000""><b><i><u>No guts, no glory.")
  Else
    Send ("<font color=""#FF0000""><b><i><u>@@@### SLOW DOWN ###@@@")
  End If
End Sub

Private Sub Command5_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If (Command5.Caption = "No Guts") Then
    StatLine = "No guts, no glory."
  Else
    StatLine = "I'll tell them to slow down!!!"
  End If
End Sub

Private Sub cPaused_Click()
  Send ("<font color=""#FF0000""><b><i><u>!!!@@@ IT'S PAUSED @@@!!!")
End Sub

Private Sub cPaused_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Reminds the people that the BATTLE IS PAUSED YOU MORONS!!!"
End Sub

Private Sub cStat2_Click()
  fNeoStat.Show
End Sub

Private Sub cStat2_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shows the cool new Status Window"
End Sub

Private Sub cStatus_Click()
  cStatus.Checked = Not cStatus.Checked
End Sub

Private Sub cTeam_Click()
  cTeam.Checked = Not cTeam.Checked
End Sub

Private Sub CTimer_Timer()
Dim X2 As Integer
  If (Battle And Not Paused) Then
    XTimer = XTimer + 1
    'For x2 = 1 To MaxPlayers
    '  If p(x2).Status(sStop) <> 0 Or p(x2).Status(sMIA) <> 0 Then
    '    p(x2).MoveStart = p(x2).MoveStart + 1
    '  End If
    'Next x2
  End If
End Sub

Private Sub cTwit_Click()
  fTwit.Show
End Sub

Private Function DetermineTeam(X As Integer) As String
Dim a As String
  'If FFABattle >= bFFA Then
    If (X <= 10) Then
      a = Chr$(47 + X)
    Else
      a = Chr$(65 + X - 10)
    End If
  'Else
  '  A = "S"
  'End If
  DetermineTeam = a
End Function

Private Sub EndBattle()
Dim Ok As Integer
Dim X%
  If (Not Auto) Then Ok = (kDlgBoxfn("Really end the battle?", 36, "End Battle") = 6)
  If (Auto Or Ok) Then
    Playwav ("lose")
    EndIt DATASET.GameAborted
    For X = 1 To 255
      TCaptain(X) = 0
      tName(X) = ""
    Next X
    For X = 1 To MaxPlayers
      If P(X).CPU <> 0 Then P(X).Rune = 0
    Next X
  End If
End Sub

Private Sub EndIt(ByVal Reason$)
Dim X4%
  Unload fMidi
  Battle = False
  Paused = False
  mBeginBattle.Enabled = True
  mBeginBattle.Caption = "&Begin"
  mPauseBattle.Enabled = False
  Send (DATASET.BattleEnd)
  Send (Reason$)
  DispFrags
  For X4 = 1 To MaxPlayers
    If P(X4).Status(sMorph) Then
      P(X4).CharID = P(X4).OldCharID
    End If
  Next X4
End Sub

Private Sub FlagTeams()
Dim X%, X2%
Dim Td As String
  For X = 1 To MaxPlayers
    P(X).TeamID = ""
  Next X
  For X = 1 To Int(MaxPlayers / 2)
    Do
      X2 = Rand(1, MaxPlayers)
    Loop Until P(X2).TeamID = ""
    P(X2).TeamID = "R"
  Next X
  For X = 1 To MaxPlayers - Int(MaxPlayers / 2)
    Do
      X2 = Rand(1, MaxPlayers)
    Loop Until P(X2).TeamID = ""
    P(X2).TeamID = "B"
  Next X
  If (NumPlaying() < 4) Then Exit Sub
  tName(Asc("B")) = "<font color=""#0000FF"">Blue Team</font>"
  tName(Asc("R")) = "<font color=""#FF0000"">Red Team</font>"
  cTeam.Checked = False
  cJoin.Checked = False
  TLimit = 0
  'y(1).Enabled = False
  'y(2).Enabled = False
  'y(3).Enabled = False
  'y(4).Enabled = False
  'y(5).Enabled = False
  Do
    RedFlag = Rand(1, MaxPlayers)
    DoEvents
  Loop Until (P(RedFlag).CharID) And P(RedFlag).TeamID = "R"
  FoundRedFlag = 0
  Do
    BlueFlag = Rand(1, MaxPlayers)
    DoEvents
  Loop Until (P(BlueFlag).CharID) And P(BlueFlag).TeamID = "B"
  FoundBlueFlag = 0
End Sub

Private Sub cVersion_Click()
  cVersion.Checked = Not cVersion.Checked
End Sub

Private Sub Form_Load()
Dim X As Integer
Dim S As String
Dim P2$, P4$, PO%
  LoadPosition Me, MyName
  Unload fMidi
  Me.Enabled = True
  Me.Caption = DATASET.LoadStr + " Battle"
  SetWindowPos fBattle.hWnd, -1, 0, 0, 0, 0, 3
  hsScroll.Value = fIntro!tiOutput.interval
  Label2.Caption = "Scroll Delay: " + TrimStr(hsScroll.Value)
  oLin(ScRa).Value = True
  P2$ = "Lag-O-Meter"
  P4$ = String$(6, 32)
  'PO% = GetPrivateProfileString("Millenium", P2$, "2000", P4$, 81, "ChUB2000.ini")
  P4$ = GetSetting("ChUB 2000 SE", "Settings", P2$, "2000")
  hsLag = Val(P4$)
  Temp1 = 0
  Temp4 = 0
  Battle = False
  Paused = False
  tiMisc_Timer
  'tChatLine.SetFocus
  'S = "Mystery Button"
  'cMystery.Caption = ""
  'For X = 1 To Len(S)
  '  cMystery.Caption = cMystery.Caption + Mid$(S, X, 1)
  '  Delay (0)
  'Next X
  XTimer = 1
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = DATASET.LoadStr + " Battle"
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
  If Battle Then
    EndIt DATASET.GameAborted
    mBeginBattle.Caption = "&Begin"
  End If
  If (Not Battle) Then
    Cancel = False
  Else
    Cancel = True
  End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
  SavePosition Me, MyName
  'Unload fyControl
  Unload fNeoStat
  Unload fMidi
  Unload fStats
  GameStage = 0
  Me.Enabled = False
  fPreBattl!tiPreBatt.Enabled = True
  fPreBattl!hsScroll.Value = Me!hsScroll.Value
  fPreBattl!oLin(ScRa).Enabled = True
  'If kDlgBox("Clear characters?", 36, "Exit Battle") = 6 Then
  If FullAuto.Enabled Then
    ClearChars
    ScrollSend DATASET.ClearChars
  Else
    ScrollSend DATASET.CharsNotCleared
  End If
  ScrollSend (DATASET.BeginSelect)
  fPreBattl.Show
  Playwav ("interlud")
End Sub

Private Sub hsLag_Change()
Dim P2$, P4$, PO%
  CTimer.interval = hsLag
  tiMisc.interval = hsLag
  tLag.Text = TrimStr(hsLag)
  P2$ = "Lag-O-Meter"
  P4$ = TrimStr(hsLag.Value)
  'PO% = WritePrivateProfileString("Millenium", P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
End Sub

Private Sub hsScroll_Change()
  fIntro!tiOutput.interval = hsScroll
  Label2.Caption = "Scroll Delay: " + TrimStr(hsScroll.Value)
End Sub

Private Sub hsScroll_Scroll()
  hsScroll_Change
End Sub

Private Sub Label2_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Changes the scroll delay"
End Sub

Private Sub Label3_Click()

End Sub

Private Sub Label4_Click()
Dim X As Integer
Dim S$
Dim TP As Integer
  If CurArena = Lang Then
    kDlgBox "You cannot change the time remaining in the Langolier arena.", 0, "ChUB 2000"
  Else
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
  End If
End Sub

Private Sub Label4_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "How much time do we have left?..."
End Sub

Private Sub LItem_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
Dim W%, S$
  If Battle Then
    If Button = 2 Then
      W = Rand(1, MaxWeapons)
      AddDroppedWeapon W, Weapons(W).NumUses
      Send ("The """ + Weapons(W).Name + """ is lying on the floor to be picked up. (/wget-" + Weapons(W).PickMe + ")")
    Else
      If (Item2Get = 0) Then
        If kDlgBoxfn("Drop an item onto the battlefield?", 36, "Item Maker") = 6 Then
          NewItem
        End If
      Else
        If kDlgBoxfn("Zap the current item to dust?", 36, "Item Zapper") = 6 Then
          S = YourSN + " throws a lightning bolt which zaps the " + Items(Item2Get).Name + " into oblivion!"
          Send (S)
          Item2Get = 0
        End If
      End If
    End If
  End If
End Sub

Private Sub LItem_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Click here to be evil and drop/destroy an item."
End Sub

Private Sub mBeginBattle_Click()
Dim S4 As String
Dim X As Integer, T%
Dim Ok As Integer
Dim Msg$, P1$, P2$, P4$, PO%
  If mBeginBattle.Caption = "&Begin Battle" Then
    S4 = ""
    If Config.Flag <> 0 Then FlagTeams
    For X = 1 To MaxPlayers
      P(X).MaxHP = MaxHP
      If P(X).CharID = 7 And P(X).ScrNam = "XRunes" And P(X).TeamID = "Q" And tName(Asc("Q")) = "morerunes" Then
        If MainDeclares.XRunes = 0 Then
          Send ("Extra Runes Enabled!")
          MainDeclares.XRunes = True
        Else
          Send ("Extra Runes are already enabled.")
        End If
        Exit Sub
      End If
    Next X
    For X = 1 To MaxPlayers
      If (P(X).CharID <> 0) Then
        If (S4 = "") Then
          S4 = P(X).TeamID
        End If
        If (P(X).TeamID <> S4) Then
          S4 = "!!!!!"
          Exit For
        End If
      End If
    Next X
    If (S4 <> "!!!!!") Then
      ScrollSend ("Can't start battle -- Team " + S4 + " would win automatically")
      If (Not Auto) Then kDlgBox "Cannot start battle -- Team " + S4 + " would win with no opposition!", 16, "Invalid Teams"
    Else
      For X = 1 To 255
        TCaptain(X) = 0
      Next X
      For X = 1 To MaxPlayers
        P(X).Ready = 0
        P(X).AttackedMe = 0
        P(X).SuperNum = 0
        'p(x).InactiveTime = Timer
        If P(X).TeamID <> "" Then
          If TCaptain(Asc(P(X).TeamID)) = 0 Then TCaptain(Asc(P(X).TeamID)) = X
        End If
        P(X).WpnUsesLeft = Weapons(P(X).Weapon).NumUses
        If Config.OldSchool Then
          P(X).Rune = 0
          P(X).Weapon = 0
        End If
      Next X
      GameNum = GameNum + 1
      P1$ = "Millenium"
      P2$ = "GameNum"
      P4$ = TrimStr(GameNum)
      'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
      SaveSetting "ChUB 2000 SE", "Settings", P2$, P4$
      mPauseBattle.Enabled = True
      mPauseBattle.Caption = "Un&P"
      ClearStats
      ClearFrags
      ReDim Dropped(0)
      ReDim DropUses(0)
      MaxDropped = 0
      BalancingAct
      For X = 1 To MaxPlayers
        InitMoves (X)
      Next X
      ScrollSend ("ChUB 2000 version " + VerID + " (Battle #" + TrimStr(GameNum) + ")")
      ScrollSend ("Current Dataset: " + DATASET.LoadStr)
      If FragLimit = 0 And Config.Respawn <> 0 And Config.Flag = 0 Then ScrollSend ("<font color=#FF0000>WARNING: Frag Limit not set for Respawn FFA! Game will continue infinitely")
      Delay (0.5)
      Item2Get = 0
      TKills = 0
      TFatal = 0
      tHP = 0
      tMP = 0
      tSP = 0
      Paused = True
      XTimer = 1
      TimeLimit = XTimer
      Battle = True
      mBeginBattle.Caption = "&End"
      Temp4 = -100
      T% = 0
      ScrollChars
      SneakAttack
    End If
  Else
    EndBattle
  End If
End Sub

Private Sub mBeginBattle_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If mBeginBattle.Caption = "&Begin Battle" Then
    StatLine = "Start your engines!"
  Else
    StatLine = "Abort Game"
  End If
End Sub

Private Sub mExitBattle_Click()
  Unload Me
End Sub

Private Sub mExitBattle_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Leaving already?"
End Sub

Private Sub mPauseBattle_Click()
Dim X%
  If (Not Paused) Then
    Paused = True
    mPauseBattle.Caption = "Un&pause Battle"
    Send (DATASET.BattlePause)
    Playwav ("down")
    mShowChars.Enabled = True
    mShowTeams.Enabled = True
  ElseIf (AOLCheck() = 333 Or AOLCheck() = 444 Or YourSN = "Player") Then
    mPauseBattle.Caption = "&Pause Battle"
    mShowChars.Enabled = False
    mShowTeams.Enabled = False
    'For X = 1 To MaxPlayers
    '  p(X).InactiveTime = Timer
    '  p(X).InactiveWarn = 0
    'Next X
    'Stop
    For X = 1 To MaxPlayers
      If P(X).CharID = 5 And P(X).ScrNam = "PING" And P(X).TeamID = "P" And tName(Asc("P")) = "PONG" Then
        ScrollSend1 ("Your host is taking a retro trip with the most awesome game ever created...")
        ScrollSend1 ("That's right, the one and only PONG!!!!!")
        fPong.Show
        Exit Sub
      End If
      If P(X).CharID = 6 And P(X).ScrNam = "Super6" And P(X).TeamID = "6" And tName(Asc("6")) = "666" Then
        P(X).CharID = 0
        Send ("Level 6 Supers Activated by " + YourSN + "!")
        Six = True
      End If
      If P(X).CharID = 14 And P(X).ScrNam = "Setzer" Then
        P(X).CharID = 0
        Send ("/slot command enabled by " + YourSN)
        AllSlot = True
      End If
    Next X
    If (TimeLimit = XTimer) Then
      Send (DATASET.BattleBegin)
      Playwav ("beat2")
    Else
      Playwav ("up")
      Send (DATASET.BattleUnPause)
    End If
    Paused = False
  Else
    kDlgBox "Not logged on -- please log back on before resuming!", 16, "Not Logged On"
  End If
  tChatLine.SetFocus
End Sub

Private Sub mPauseBattle_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Pause or Unpause"
End Sub

Private Sub mShowChars_Click()
  If (Paused) Then ScrollChars
End Sub

Private Sub mShowChars_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shows list of characters playing."
End Sub

Private Sub mShowMoves_Click()
  fMoves.Show
End Sub

Private Sub mShowMoves_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shows moves"
End Sub

Private Sub mShowTeams_Click()
  If (Paused) Then ListByTeams
End Sub

Private Sub mShowTeams_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shows Teams"
End Sub

Private Sub NewItem()
Dim S As String
Dim S1 As String
Dim W%
  If Rand(1, 2) = 2 Or MaxWeapons = 0 Or Config.OldSchool Then
    S1 = Items(Item2Get).Telefrag
    Send (S1)
    Item2Get = Rand(1, MaxItem)
    S = Items(Item2Get).Spawn
    Send (S)
  Else
    Do
      W = Rand(1, MaxWeapons)
      DoEvents
    Loop Until Left$(Weapons(W).PickMe, 1) <> "~"
    AddDroppedWeapon W, Weapons(W).NumUses
    Send ("The """ + Weapons(W).Name + """ is lying on the floor to be picked up. (/wget-" + Weapons(W).PickMe + ")")
  End If
End Sub

Private Function NotHostage%(X As Integer)
Dim X2 As Integer
  NotHostage = True
End Function

Private Sub oLin_Click(Index As Integer)
  ScRa = Index
End Sub

Private Sub SelectJoin(ByVal X%, ByVal X3%, ByVal SN$)
Dim X2 As Integer
Dim t4 As Integer, P1$, P2$, P4$, PO%
  clearStatus (X)
  P1$ = SN
  P2$ = "Wins"
  P4$ = String$(6, 32)
  'PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
  P4$ = GetSetting("ChUB 2000 SE", "Settings", P2$, "0")
  P(X).Wins = Val(P4$)
  P1$ = SN
  P2$ = "Losses"
  P4$ = String$(6, 32)
  'PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
  P4$ = GetSetting("ChUB 2000 SE", "Settings", P2$, "0")
  P(X).Losses = Val(P4$)
  P(X).CPU = 0
  P(X).ScrNam = SN
  P(X).MaxHP = MaxHP
  P(X).HP = P(X).MaxHP
  P(X).CharID = X3
  P(X).TeamID = DetermineTeam(X)
  P(X).PhysStr = Senshi(X3).PhysStr
  P(X).MagStr = Senshi(X3).MagStr
  P(X).PhysDef = Senshi(X3).PhysDef
  P(X).MagDef = Senshi(X3).MagDef
  For t4 = 1 To MaxMoves - 1
    P(X).Moves(t4) = Moves(Senshi(X3).Moves(t4))
  Next t4
  Send (Parse(Senshi(X3).SelectJoin + " (" + TrimStr(Senshi(X3).Wins) + " kills, " + TrimStr(Senshi(X3).Losses) + " deaths)", SN + " (" + TrimStr(P(X).Wins) + " kills, " + TrimStr(P(X).Losses) + " deaths)", "", "", ""))
End Sub

Private Sub SneakAttack()
Dim X%, X6%
  If (Rand(1, 20) = 1) Then
    Send ("~~~*) SNEAK ATTACK (*~~~")
Start:
    Do
      X = Rand(1, MaxPlayers)
    Loop Until Active(X) <> 0
    Do
      P(X).CurMove = Rand(1, MaxMoves)
      X6 = X6 + 1
    Loop Until (P(X).Moves(P(X).CurMove).Cmdkey <> "") Or (X6 > 150) And (P(X).Moves(P(X).CurMove).Element <> Morph)
    If X6 > 150 Then
      GoTo Start
    Else
     X6 = P(X).Moves(P(X).CurMove).Target
     If (X6 = Allfriend) Or (X6 = OnlySelf) Or (X6 = Ally) Then
        P(X).Target = X
      Else
        P(X).Target = GetTarget(P(X).TeamID)
      End If
      P(X).MoveStart = -999
    End If
    Check2MoveDmg X, -1
  End If
End Sub

Private Sub StatLine_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "HEY! GET THAT MOUSE OFFA ME! NOW!!!"
End Sub

Private Sub tChatLine_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Say stuff here and press Enter to send it!"
End Sub

Private Sub tiCPU_Timer()
' CPU move
Dim X1, X2, Okay, T As Integer
Dim SN, Tgt, Yma As String
Dim M As MoveType
  On Error Resume Next
  If (Paused) Then
    For X2 = 1 To MaxPlayers
      If (P(X2).CPU) And (P(X2).CharID <> 0) And (P(X2).HP > 0) And P(X2).Rune = 0 And Config.OldSchool = 0 Then
        Do
          P(X2).Rune = Rand(1, MaxRune)
        Loop Until DesirableCPURune(P(X2).Rune)
        Send (P(X2).ScrNam + " picks the " + RuneName(P(X2).Rune) + ".")
      End If
    Next X2
  End If
  If (Battle) And (Not Paused) Then
    For X2 = 1 To MaxPlayers
      DoEvents
      If (P(X2).CPU) And (P(X2).CharID <> 0) And (P(X2).HP > 0) Then
        'p(x2).InactiveTime = Timer
        If P(X2).Rune = 0 And Config.OldSchool = 0 Then
          Do
            P(X2).Rune = Rand(1, MaxRune)
          Loop Until DesirableCPURune(P(X2).Rune)
          Send (P(X2).ScrNam + " picks the " + RuneName(P(X2).Rune) + ".")
        End If
        If (P(X2).CurMove = 0) And (Rand(2, 2) = 2) And (P(X2).Status(sStop) = 0) And (P(X2).Status(sStun) = 0) And (P(X2).Status(sSleep) = 0) And (P(X2).Status(sMIA) = 0) Then
          Call MoveToDo(X2)
          If (Mx <> 0) Then
            Call DOMOVE(X2, Tx, Mx, STX)
          End If
        End If
      End If
    Next X2
  End If
End Sub

Private Sub tiDoMove_Timer()
Dim X%, X2%, X3%, X4%, X5%, X6%, x7%
Dim JoinOK%, Okay%
Dim SN$, Ct$, Chat$, S2$, S3$, S4$, S5$, S6$, S7$
Dim Msg$, TID$, f25%
Dim M As MoveType
Dim S As CharType
Dim T$, PIN%, KeyCode%, Cmd#, Host$, Target$, Cd%
ReDim Almighty(sMaxStatus) As Integer
Static Taunt%
  DoEvents
DoAgain1:
  If fPreBattl.Visible Then
    Unload Me
    Exit Sub
  End If
  On Error Resume Next
  If (FindChatroom1() = 0) And (Not Paused) And (YourSN <> "Player") Then
    Paused = True
    mPauseBattle.Caption = "Un&P"
    Send (DATASET.BattlePause)
    Playwav ("down")
    mShowChars.Enabled = True
    mShowTeams.Enabled = True
  End If
  If (GetNdx <> GetSave) Then
    Chat = GetRay(GetNdx)
    GetNdx = GetNdx + 1
    If (GetNdx > MaxRay) Then GetNdx = 1
    'If Left$(GetCt(Chat), 1) <> "/" Then GoTo DoAgain1
  Else
    Exit Sub
  End If
  SN = GetSN(Chat)
  Ct = Trim(GetCt(Chat))
  X2 = Playernum(SN)
  If Skip(Ct, 4) = "FOUR" Or Skip(Ct, 5) = "55555" Or Skip(Ct, 9) = "SUPERFIVE" Then
    If Config.Fours Then
      If Six Then
        Send ("Max Super Level: 6")
      Else
        Send ("Max Super Level: 5")
      End If
    Else
      Send ("Max Super Level: 3")
    End If
  End If
  If Skip(Ct, 12) = "RELEASEINFO" Then
    ReleaseInfo
  End If
  If (((Skip(Ct, 4) = "INFO") Or Skip(Ct, 4) = "HELP" And Len(Ct) > 6) Or (Skip(Ct, 1) = "?" And Len(Ct) > 3)) And Config.Help Then
    'Send ("""" + CT + """?!? What do you think this is, " + SN + "? Talismans?")
    If Skip(Ct, 1) <> "?" Then
      TID = Right$(Ct, Len(Ct) - 6)
    Else
      TID = Right$(Ct, Len(Ct) - 3)
    End If
    ShowInfo (TID)
  End If
  'If Skip(Ct, 4) = "HELP" And Len(Ct) > 6 And cInfo = 1 Then
  '  Tid = Right$(Ct, Len(Ct) - 6)
  '  ShowInfo (Tid)
  'End If
  If Skip(Ct, 5) = "MOVES" And (Config.Moves) Then
    If (Len(Ct) > 7) Then
      ShowMoves (Mid$(Ct, 8, Len(Ct) - 7))
    Else
      ShowMoves (SN)
    End If
  End If
  If Skip(Ct, 7) = "SHOTGUN" Or Skip(Ct, 8) = "NEXTHOST" Or Skip(Ct, 8) = "HOSTNEXT" Then ShotgunCall (SN)
  If Skip(Ct, 4) = "TYPE" And Config.TypeCommand Then
    'If Config.OldSchool <> 0 Then Msg = "Battle: OldSchool " Else Msg = "Battle: Enhanced "
    Msg = "Battle: SE "
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
    'If Config.InfMP = 0 Then
    '  Msg = Msg + " (" + TrimStr(MaxHP) + " MaxHP/" + TrimStr(MaxMP) + " MaxMP/" + TrimStr(MaxSP) + " MaxSP)"
    'Else
      Msg = Msg + " (" + TrimStr(MaxHP) + " MaxHP/" + TrimStr(MaxSP) + " MaxSP)"
    'End If
    Send (Msg)
  End If
  If Skip(Ct, 4) = "JOIN" Or Skip(Ct, 6) = "RANDOM" And (Battle) Then
    If Skip(Ct, 4) = "JOIN" And Len(Ct) > 5 Then
      S5 = Mid$(Ct, 7, Len(Ct) - 6)
      X5 = MatchSenshi(S5)
      If (X5 <> 0) Then Ct = "/" + Senshi(X5).PickMe
    Else
      Do
        X5 = Rand(1, HighSenshi)
      Loop Until (Not Taken(X5) Or Config.SameChar) And (Senshi(X5).PickMe <> "")
      ''LogFileWrite (SN + " joined in with /random.")
      Ct = "/" + Senshi(X5).PickMe
    End If
  End If
  If Left$(Ct, 1) = "~" Then
    Msg = SN
    CommonGodCheck SN, Ct
    Decompile Ct, PIN%, KeyCode%, Cmd#, Host$, Target$
    If ((Host = "") Or (Host = YourSN)) And (PIN > 999) And (KeyCode > 0) And (SNKeyCode(SN, PIN) = KeyCode) Then
      Cd = Cmd / KeyCode
      Select Case Cd
        Case 8:
          Auto = True
          Target = UCase(Target)
          If Target = "SELFDESTRUCT BUTTON" Then
            Send ("Thank you for pressing the self-destruct button.")
            Send ("This game will self-destruct in 3 minutes.")
            Tm = XTimer
          End If
          If Target = "CANCELLATION BUTTON" Then
            Send ("Self-destruct sequence cancelled.")
            Tm = 0
          End If
          If Target = "BEGIN" Then mBeginBattle_Click
          If Target = "EXIT" Then mExitBattle_Click
          If Target = "CHARS" Then mShowChars_Click
          If Target = "TEAMS" Then mShowTeams_Click
          If Target = "MYST" Then cMystery_Click
          If Target = "END" Then EndBattle
          If Target = "NOSCROLLING" Then Command4_Click
          If Target = "SLOWDOWN" Then Command5_Click
          If Target = "ITSPAUSED" Then cPaused_Click
          If Target = "JOINING" Then cJoin.Checked = Not cJoin.Checked
          If Target = "STATUS" Then Config.Status = 1 - Config.Status
          If Target = "MOVES" Then Config.Moves = 1 - Config.Moves
          If Target = "ATTACKING" Then cAttacking = Not cAttacking.Checked
          If Target = "CPU" Then Config.CPU = 1 - Config.CPU
          If Target = "VERSION" Then Config.Version = 1 - Config.Version
          If Target = "TEAM" Then cTeam = Not cTeam.Checked
          Auto = False
        Case 10:
          X4 = Playernum(Target)
          If (X4 <> 0) Then ShowReveal (X4)
        Case 11:
          ScrollSend1 (">>> Game Paused By " + Msg + " <<<")
          mPauseBattle_Click
          GoTo DoAgain1
        Case 12:
          ScrollSend1 (">>> Game Unpaused By " + Msg + " <<<")
          mPauseBattle_Click
          GoTo DoAgain1
        Case 13:
          'ActivateKamek
          Send ("Sorry, Kamek is not available.")
          GoTo DoAgain1
        Case 14:
          X6 = Playernum(Target)
          If (X6 > 0) Then
            Select Case Rand(1, 4)
              Case 1: S6 = Msg + " sends a sonic wave directly at " + ScrNam(X6) + ", ripping their body apart!"
              Case 2: S6 = Msg + " sends a bolt of lightning at " + ScrNam(X6) + ", charring them to dust!"
              Case 3: S6 = Msg + " gestures and " + ScrNam(X6) + "'s heart stops."
              Case 4: S6 = Msg + " rips " + ScrNam(X6) + "'s head off with the spine still attached!"
            End Select
            If SN = "SlrSenshiV" Then
              Send ("V-Hime summons the almighty Safer Sephiroth to cast ""Super Nova""!")
              Send ("The Nova hits " + ScrNam(X6) + "!!!")
            ElseIf SN = "Omegaquila" Then
              Select Case Rand(1, 2)
                Case 1: S6 = "Aquila rips off his clothes Revealing Sailor Starfighter's outfit! " + ScrNam(X6) + "'s Eyes Explode, inflicting fatal wounds!"
                Case 2: S6 = "Aquila Summons Barney and Friends on " + ScrNam(X6) + "! " + ScrNam(X6) + "'s Head Explodes from the HORROR!!"
              End Select
              Send (S6)
            Else
              Send (S6)
            End If
            'If (x6 > MaxPlayers) Then
            '  y(x6 - MaxPlayers).HP = -1
            'Else
              P(X6).HP = -1
            'End If
          End If
          ShowStatus (X6)
        Case 15:
          X6 = Playernum(Target)
          If (X6 > 0) Then
            Send (Msg + " revives " + ScrNam(X6) + " from the abyss!")
            'If (x6 > MaxPlayers) Then
            '  y(x6 - MaxPlayers).HP = y(x6 - MaxPlayers).MaxHP
            '  y(x6 - MaxPlayers).CurMove = 0
            'Else
              P(X6).HP = P(X6).MaxHP
              P(X6).CurMove = -255
            'End If
          End If
        Case 16:
          X6 = Playernum(Target)
          If (X6 > 0) And (X6 <= MaxPlayers) Then
            Send (Msg + " gestures and creates a barrier around " + P(X6).ScrNam + ".")
            Almighty(sBarrier) = 100
            Almighty(sMBarrier) = 100
            CheckStatus Almighty(), X6, 0, 1
          End If
        Case 17:
          X6 = Playernum(Target)
          If (X6 > 0) And (X6 <= MaxPlayers) Then
            Send (Msg + " gestures and...")
            Almighty(sMute) = -1
            Almighty(sChaos) = -1
            Almighty(sFreeze) = -1
            Almighty(sSleep) = -1
            Almighty(sPoison) = -1
            Almighty(sBlind) = -1
            Almighty(sHaste) = 100
            Almighty(sSlow) = -1
            Almighty(sStun) = -1
            Almighty(sLife3) = 100
            Almighty(sRegen) = 100
            Almighty(sStop) = -1
            Almighty(sMushroom) = -1
            Almighty(sMIA) = -1
            Almighty(sQuick) = 100
            Almighty(sBerserk) = -1
            CheckStatus Almighty(), X6, 0, 1
          End If
        Case 19:
          ScrollSend1 (Msg + " sends a wave of destruction killing everyone.")
          For X = 1 To MaxPlayers
            P(X).Status(sLife3) = 0
            P(X).HP = -1
          Next X
        Case 20:
          X6 = Playernum(Target)
          If (X6 > 0) Then
            Send (Msg + " gestures and " + ScrNam(X6) + " is fully restored!")
            'If x6 > MaxPlayers Then
            '  y(x6 - MaxPlayers).HP = y(x6 - MaxPlayers).MaxHP
            'Else
              P(X6).HP = P(X6).MaxHP
              'p(x6).MP = p(x6).MaxMP
            'End If
          End If
        Case 26:
          X6 = Playernum(Target)
          If (X6 > 0) Then
            Send (Msg + " gestures and " + ScrNam(X6) + " disappears into the void!")
            Playwav ("tornoff2")
            P(X6).CharID = 0
            P(X6).Rune = 0
            P(X6).Weapon = 0
            P(X6).Super = 0
            P(X6).Cheese = 0
          End If
        Case 25:
          If (Six = 0) Then
            Send ("Level 6 Supers Enabled!")
            Six = True
            Config.Fours = 1
            LFour.Caption = "6"
          End If
        Case 21:
          X6 = Playernum(Target)
          If (X6 > 0) And (X6 <= MaxPlayers) Then
            Send (P(X6).ScrNam + " is swept down by a dark wave of energy from " + Msg + "! Super Meter = Gone!")
            P(X6).Super = 0
          End If
        Case 22:
          X6 = Playernum(Target)
          If (X6 > 0) And (X6 <= MaxPlayers) Then
            Send ("A brilliant holy light from " + Msg + " shines on " + P(X6).ScrNam + ". Super Meter = Full!")
            P(X6).Super = MaxSP
          End If
        Case 23:
          NewItem
        Case 24:
          If (Item2Get <> 0) Then
            Send (Msg + " summons a lightning bolt from the heavens to obliterate the " + Items(Item2Get).Name + "!")
            Item2Get = 0
          End If
        Case 28:
          X6 = Playernum(Target)
          If (X6 > 0) Then
            P(X6).Status(sFreeze) = 32766
            P(X6).CurMove = -50
            Send ("Sub-Zero uses all of his energy to freeze " + P(X6).ScrNam + " permanently.")
          End If
        Case 29:
          X6 = MatchWeapon(Target)
          If X6 > 0 Then
            Send (Msg + " drops a " + Weapons(X6).Name + " onto the battlefield.")
            AddDroppedWeapon X6, Weapons(X6).NumUses
          End If
        Case 30:
          Send (Msg + " sends out an electromagnetic pulse, destroying all weapons on the ground.")
          MaxDropped = 0
          ReDim Dropped(0)
          ReDim DropUses(0)
        Case 31:
          X6 = Playernum(Target)
          If (X6 > 0) Then
            P(X6).Weapon = 0
            Send (Msg + " strikes " + P(X6).ScrNam + " with a lightning bolt, destroying their weapon!")
            InitMoves (X6)
          End If
        Case 32:
          BootNum = -5
          Send ("Eternal HostSave Activated by " + SN + ".")
        Case 33:
          BootNum = 5
          ScrollSend1 ("Eternal HostBoot Activated by " + SN + "!")
          X = GetSetting("Network", "Immediate", "Disconnection", 0)
          SaveSetting "Network", "Immediate", "Disconnection", X + 1
          f25 = FindWindow("AOL Frame25", 0&)
          If f25 <> 0 Then X = SendMessage(f25, WM_CLOSE, 0, 0)
          For X = 1 To 20
            ScrollSend1 ("<<< --- just got hostbooted")
          Next X
          End
      End Select
    End If
  End If
  X = Playernum1(SN)
  If (Battle) And (X = 0 Or P(X).CharID = 0) And cJoin.Checked Then
    For X3 = 1 To NumSenshi
      DoEvents
      If (UCase(Trim(StripHiAscii(Ct)))) = ("/" + UCase(Senshi(X3).PickMe)) And Senshi(X3).PickMe <> "" Then
        JoinOK = True
        X = Taken(X3)
        If (Config.NoJoin = 1) Then
          'LogFileWrite (SN + " tried to join in as " + Senshi(x3).FullName + " but join was disabled.")
          Taunt% = Rand(1, 2)
          Select Case Taunt%
            Case 1: Send ("<b><!> " + SN + ", the host has disabled joining. Reason: " + Config.Reason)
            Case 2: Send ("<b><!> Can't join, " + SN + ". Reason: " + Config.Reason)
            'Case 3: Send ("<b><!> Hmm... I thought I just told you you can't join because " + fOption.tReason)
            'Case 4: Send ("<b><!> OK, you need to stop joining now, you can't join.")
            'Case 5: Send ("<b><!> ENOUGH! YOU CAN'T JOIN BECAUSE " + UCase(fOption.tReason) + "!")
            'Case 6: Send ("<b><!> Fine, see if I care. Type " + Ct + " all you want.")
          End Select
          GoTo DoAgain1
        End If
        If (X <> 0) And (Config.SameChar = 0) Then
          Send ("<b><!> " + Senshi(X3).FullName + " is being used by " + P(X).ScrNam + ".")
          'LogFileWrite (SN + " tried to join in as " + Senshi(x3).FullName + " but that character was taken.")
          GoTo DoAgain1
        End If
        For X5 = 1 To MaxPlayers
          DoEvents
          If (P(X5).ScrNam = SN) And (P(X5).CharID <> 0) Then
            JoinOK = False
            Exit For
          End If
        Next X5
        If JoinOK Then
          For X = 1 To fPreBattl!vsMaxPlayers.Value
            If (P(X).CharID = 0) Then
              Call SelectJoin(X, X3, SN)
              'LogFileWrite (SN + " joined in as " + Senshi(x3).FullName + ".")
              GoTo Joined21
            End If
          Next X
          'LogFileWrite (SN + " tried to join in as " + Senshi(x3).FullName + " but the game was full.")
          Taunt% = Rand(1, 2)
          Select Case Taunt%
            Case 1: Send ("<b><!> The game's full, " + SN + " -- " + TrimStr(MaxPlayers) + " are already playing. Sorry :(")
            Case 2: Send ("<b><!> Can't join, " + SN + ". Game's full.")
            'Case 3: Send ("<b><!> Hmm... I thought I just told you the game was full, " + SN + ".")
            'Case 4: Send ("<b><!> OK, you need to stop joining now, it's full.")
            'Case 5: Send ("<b><!> ENOUGH! YOU CAN'T JOIN BECAUSE IT'S FULL!")
            'Case 6: Send ("<b><!> Fine, see if I care. Type " + Ct + " all you want.")
          End Select
          GoTo DoAgain1
Joined21:
        End If
        Exit For
      End If
    Next X3
  End If
  If Skip(Ct, 7) = "VERSION" And Config.Version Then
    Send ("This is ChUB 2000 " + VerID + ". Dataset: " + DATASET.LoadStr)
    GoTo DoAgain1
  End If
  X2 = Playernum(SN)
  If (Battle) And (X2 <> 0) Then
    If Skip(Ct, 9) = "FRAGCOUNT" And Config.FragCount Then
      If Len(Ct) > 11 Then
        X6 = Playernum(Right$(Ct, Len(Ct) - 11))
      Else
        X6 = 0
      End If
      If X6 = 0 Then X6 = X2
      Send (P(X6).ScrNam + " has " + TrimStr(P(X6).Frags) + " kills and " + TrimStr(P(X6).FatalFrags) + " Fatalities.")
    End If
    If Skip(Ct, 11) = "LEARNEDMOVE" Then
      If Len(Ct) > 13 Then
        X6 = Playernum(Right$(Ct, Len(Ct) - 13))
      Else
        X6 = 0
      End If
      If X6 = 0 Then X6 = X2
      If P(X6).Moves(14).Cmdkey <> "" Then
        Send (P(X6).ScrNam + " currently has " + P(X6).Moves(14).Name + " available (/" + P(X6).Moves(14).Cmdkey + ")")
      Else
        Send (P(X6).ScrNam + " has no learned move available.")
      End If
    End If
    If Skip(Ct, 8) = "NEWTEAM" And Config.Defect Then
      If (X2 > 0) And (X6 <= MaxPlayers) Then
        For X3 = Asc("0") To Asc("Z")
          X4 = 0
          For X5 = 1 To MaxPlayers
            If TeamID(X5) = Chr$(X3) Then X4 = True
          Next X5
          If (X4 = 0) Then
            Send (P(X2).ScrNam + " creates Team " + Chr$(X3) + ". (Use /teamname to name it)")
            'LogFileWrite (SN + " created Team " + Chr$(x3) + ".")
            P(X2).TeamID = Chr$(X3)
            TCaptain(X3) = X2
            tName(X3) = ""
            X4 = 999
            Exit For
          End If
        Next X3
        If (X4 <> 999) Then
          Send ("Error: Unable to create new team.")
          'LogFileWrite (SN + " attempted to create a team but an error occured.")
        End If
      End If
    End If
    If Skip(Ct, 4) = "KICK" And Config.Defect And Len(Ct) > 7 Then
      x7 = Playernum(Right$(Ct, Len(Ct) - 6))
      If (X2 > 0) And (X2 <= MaxPlayers) And (x7 > 0) And (x7 <= MaxPlayers) And (TeamID(X2) = TeamID(x7)) Then
        If TCaptain(Asc(P(X2).TeamID)) = X2 Then
          Send (P(X2).ScrNam + " kicks " + P(x7).ScrNam + " off their team!")
          'LogFileWrite (SN + " kicked " + p(x7).ScrNam + " off of Team " + TeamID(x2) + ".")
          For X3 = Asc("0") To Asc("Z")
            X4 = 0
            For X5 = 1 To MaxPlayers
              If TeamID(X5) = Chr$(X3) Then X4 = True
            Next X5
            If (X4 = 0) Then
              Send (P(x7).ScrNam + " is now on Team " + Chr$(X3) + ". (Use /teamname to name it)")
              'LogFileWrite (p(x7).ScrNam + " was placed on Team " + Chr$(x3) + ".")
              P(x7).TeamID = Chr$(X3)
              TCaptain(X3) = X2
              tName(X3) = ""
              X4 = 999
              Exit For
            End If
          Next X3
          If (X4 <> 999) Then
            Send ("Error: Unable to create new team.")
            'LogFileWrite (p(x7).ScrNam + " could not be placed on a new team due to an error.")
          End If
        Else
          Send ("Only the team captain, " + P(TCaptain(Asc(P(X2).TeamID))).ScrNam + ", may /kick people.")
        End If
      End If
    End If
    If Skip(Ct, 8) = "TEAMNAME" And Len(Ct) > 10 Then
      If (X2 > 0) Then
        If TCaptain(Asc(P(X2).TeamID)) = X2 Then
          S4 = Right$(Ct, Len(Ct) - 10)
          tName(Asc(P(X2).TeamID)) = S4
          Send ("Team " + P(X2).TeamID + " is now known as """ + S4 + """.")
          'LogFileWrite ("Team " + p(x2).TeamID + " was renamed to """ + S4 + """.")
        Else
          Send ("Only the team captain, " + P(TCaptain(Asc(P(X2).TeamID))).ScrNam + ", may change the team name.")
        End If
      End If
    End If
    If Skip(Ct, 6) = "STATUS" And Config.Status Then
      If Len(Ct) > 8 Then
        X6 = Playernum(Mid$(Ct, 9, Len(Ct) - 8))
      Else
        X6 = 0
      End If
      If (X6 = 0) Then X6 = Playernum(SN)
      If (X6 > 0) And (X6 <= MaxPlayers) Then
        Msg = P(X6).ScrNam + " (" + Senshi(P(X6).CharID).FullName + "): (" + TrimStr(P(X6).HP) + "H/"
        'If (Config.InfMP) Then
        '  Msg = Msg + "???"
        'Else
        '  Msg = Msg + TrimStr(p(x6).MP)
        'End If
        Msg = Msg + TrimStr(P(X6).Super) + "S/" + TrimStr(P(X6).Cheese) + "C) [" + TextStatus(X6) + "] "
        If P(X6).Status(sChaos) Then Msg = Msg + "(chaosed) "
        If P(X6).Status(sBerserk) Then Msg = Msg + "(berserked) "
        If P(X6).Status(sMute) Then Msg = Msg + "(muted)"
        If P(X6).HP <= (MaxHP / 6) Then
          Msg = "<font color=#FF0000>" + Msg
          If P(X6).HP > 0 Then Msg = Msg + " <DANGER>"
        End If
        Send (Msg)
      End If
      GoTo DoAgain1
    End If
    If Skip(Ct, 9) = "ATTACKING" And Config.Attacking Then
      If Len(Ct) > 11 Then
        X6 = Playernum(Right$(Ct, Len(Ct) - 11))
      Else
        X6 = 0
      End If
      If X6 = 0 Then X6 = Playernum(SN)
      If (X6 > 0 And X6 <= MaxPlayers) Then
        Msg = AttackMe$(X6)
        If Msg = "" Then
          Send ("Nobody is attacking " + P(X6).ScrNam + ".")
        Else
          Send ("Attacking " + P(X6).ScrNam + ": " + Msg)
        End If
        GoTo DoAgain1
      End If
    End If
    If Skip(Ct, 7) = "RESPAWN" And P(X2).HP <= 0 And Config.Respawn Then
      Send (P(X2).ScrNam + "'s impatience causes a longer respawn delay for them!")
      P(X2).GotKilled = P(X2).GotKilled + 30
    End If
  End If
  X2 = Playernum(SN)
  If (Battle And X2 <> 0) Then
    If P(X2).ScrNam = SN Then
      If (Skip(Ct, 8) = "NODEFECT") And Config.Defect And cTeam.Checked Then
        P(X2).Defect = Not P(X2).Defect
        If Not P(X2).Defect Then
          Send (Parse(DATASET.AcceptDefects, SN, "", "", ""))
          'LogFileWrite (SN + " began to accept /defects.")
        Else
          Send (Parse(DATASET.DeclineDefects, SN, "", "", ""))
          'LogFileWrite (SN + " stopped accepting /defects.")
        End If
      End If
      If (Skip(Ct, 13) = "I WANT A DRAW") Then
        P(X2).Draw = Not P(X2).Draw
        If P(X2).Draw Then
          Send5 (Parse(DATASET.WantDraw, SN, "", "", ""))
          'LogFileWrite (SN + " called for a draw.")
        Else
          Send5 (Parse(DATASET.DontWantDraw, SN, "", "", ""))
          'LogFileWrite (SN + " decided not to draw.")
        End If
      End If
      If (Skip(Ct, 6) = "DEFECT") And (Config.Defect) And (Len(Ct) > 8) And cTeam.Checked Then
        P(X2).Target = Playernum(Mid$(Ct, 9, Len(Ct) - 8))
        Call DOMOVE(X2, P(X2).Target, pDefect, "")
      End If
      If (Skip(Ct, 3) = "CPU") And Config.CPU Then
        If (P(X2).CPU = 0) Then
          P(X2).CPU = 1
          P(X2).Goodwill = 50
          P(X2).Greed = 50
          P(X2).Wrath = 50
          P(X2).Arrogance = 0
          Send5 (SN + " is now controlled by the computer.")
          'LogFileWrite ("Control of " + SN + " was switched over to the CPU.")
        Else
          P(X2).CPU = 0
          Send5 (SN + " is no longer controlled by the computer.")
          'LogFileWrite (SN + " took control of their character.")
        End If
      End If
      If (Skip(Ct, 6) = "~WRATH") And Config.CPU Then
        If (P(X2).CPU = 0) Then
          P(X2).CPU = 1
          P(X2).Goodwill = 50
          P(X2).Greed = 50
          P(X2).Wrath = 100
          P(X2).Arrogance = 0
          Send5 (SN + " is now controlled by a vengeful computer...")
          'LogFileWrite ("Control of " + SN + " was switched over to the /~wrathful CPU.")
        Else
          P(X2).CPU = 0
          Send5 (SN + " is no longer controlled by the computer.")
          'LogFileWrite (SN + " took control of their character.")
        End If
      End If
      'If (Skip(Ct, 4) = "OOPS") And (p(x2).CanOops = 0) Then
      '  Playwav ("tornoff2")
      '  Send (p(x2).ScrNam + " quickly decides not to participate and vanishes.")
      '  'LogFileWrite (p(x2).ScrNam + " used /oops to remove themselves.")
      '  p(x2).CharID = 0
      '  p(x2).ScrNam = ""
      '  Ct = ""
      'End If
      If (Skip(Ct, 6) = "RCLAIM") And (Config.Flag <> 0) And RedFlag = 0 Then
        Send (P(X2).ScrNam + " claims the Red Flag.")
        RedFlag = X2
      End If
      If (Skip(Ct, 6) = "BCLAIM") And (Config.Flag <> 0) And BlueFlag = 0 Then
        Send (P(X2).ScrNam + " claims the Blue Flag.")
        BlueFlag = X2
      End If
      If (Skip(Ct, 5) = "EQUIP" Or Skip(Ct, 4) = "RUNE") Then
        If Config.RuneEnable = 0 Then
          Send (SN + ", Runes are disabled, sorry.")
        Else
          X6 = 0
          If Skip(Ct, 5) = "EQUIP" Then
            If Len(Ct) > 7 Then X6 = Playernum(Right$(Ct, Len(Ct) - 7))
          ElseIf Len(Ct) > 6 Then
            X6 = Playernum(Right$(Ct, Len(Ct) - 6))
          End If
          If X6 = 0 Then X6 = X2
          If P(X6).Rune = 0 Then
            If X6 = X2 Then
              If Config.GetRune Then
                Do
                  P(X2).Rune = Rand(1, MaxRune)
                Loop Until P(X2).Rune <> RuneZombie
                Send ("The " + RuneName(P(X2).Rune) + " falls into " + P(X2).ScrNam + "'s hands.")
                If P(X2).Rune = RuneLife Then P(X).MaxHP = Int(MaxHP * 0.25) + MaxHP
              Else
                Send (P(X2).ScrNam + ", you have no Rune. The host has disabled random rune selection for midgame joiners.")
              End If
            ElseIf Config.RuneCmd Then
              Send (P(X6).ScrNam + " has no Rune.")
            End If
          ElseIf Config.GetRune Then
            Send (P(X6).ScrNam + " has the " + RuneName(P(X2).Rune) + ".")
          End If
        End If
      End If
      If (Skip(Ct, 8) = "TALISMAN") Then
        Send ("Sorry, " + P(X2).ScrNam + ", we only deal with <b>/RUNE</b>s here, not Talismans.")
      End If
      If Skip(Ct, 6) = "WEAPON" And Config.Weapon And Config.WeaponEnable Then
        If P(X2).Weapon <> 0 Then
          Send (P(X2).ScrNam + " has the """ + Weapons(P(X2).Weapon).Name + """ with " + TrimStr(P(X2).WpnUsesLeft) + " charges remaining.")
        Else
          Send (P(X2).ScrNam + " has no weapon. Wait for one to appear!")
        End If
      End If
      If Skip(Ct, 7) = "WPNLIST" Then
        If Config.WeaponEnable = 0 Then
          Send (SN + ", weapons are disabled, sorry.")
        ElseIf Config.WpnList Then
          Okay = False
          For X = 1 To MaxWeapons
            X5 = 0
            X6 = 0
            For X4 = 1 To MaxDropped
              If Dropped(X4) = X Then
                X5 = X5 + 1
                If DropUses(X4) > X6 Then X6 = DropUses(X4)
              End If
            Next X4
            If X5 <> 0 Then
              Okay = True
              If X6 = 0 Then
                Send ("x" + TrimStr(X5) + ": " + Weapons(X).Name + " (/wget-" + Weapons(X).PickMe + ")")
              Else
                If X5 = 1 Then
                  Send ("x" + TrimStr(X5) + ": " + Weapons(X).Name + " with " + TrimStr(X6) + " charges left (/wget-" + Weapons(X).PickMe + ")")
                Else
                  Send ("x" + TrimStr(X5) + ": " + Weapons(X).Name + " with at most " + TrimStr(X6) + " charges left (/wget-" + Weapons(X).PickMe + ")")
                End If
              End If
            End If
          Next X
          If Okay = False Then Send ("No weapons are available to grab right now.")
        End If
      End If
      If Skip(Ct, 4) = "OOPS" Then
        Send ("OOPS? As in RUN AWAY? You can't escape, COWARD! You must FIGHT!")
      End If
    End If
  End If
  If (Battle) And (Not Paused) Then
    If (P(X2).CharID <> 0) And (P(X2).HP > 0) And (P(X2).CurMove <> 0) And (P(X2).Status(sChaos) = 0) And P(X2).Status(sMIA) = 0 And P(X2).Status(sBerserk) = 0 And P(X2).Status(sMushroom) = 0 And P(X2).Status(sStop) = 0 And P(X2).Status(sSleep) = 0 Then
      If (Skip(Ct, 4) = "STOP") Or (Skip(Ct, 4) = "HALT") And (P(X2).CurMove <> pTaunt) And (P(X2).CurMove > 0) Then
        If (P(X2).CurMove < MaxMoves) And (P(X2).CurMove > 1) Then
          M = P(X2).Moves(P(X2).CurMove)
          If M.Element = Invin Then
            P(X2).Status(sInvin) = 0
            If M.Target = Allfriend Then
              For X4 = 1 To MaxPlayers
                If TeamID(X4) = TeamID(X2) Then P(X4).Status(sInvin) = 0
              Next X4
            End If
          End If
        End If
        If P(X2).CurMove <> pRest Then P(X2).CurMove = -255
        If P(X2).CurMove <> pTaunt Then P(X2).CurMove = -255
        Exit Sub
      End If
    End If
    If (P(X2).CharID = 0) Or (P(X2).HP <= 0) Or (P(X2).CurMove <> 0) Or P(X2).Status(sMIA) <> 0 Or P(X2).Status(sMushroom) <> 0 Or P(X2).Status(sStop) <> 0 Or P(X2).Status(sSleep) <> 0 Then
      If P(X2).CurMove > 0 And P(X2).CurMove < MaxMoves Then Msg = "you are doing " + P(X2).Moves(P(X2).CurMove).Name + "."
      If P(X2).CurMove > 0 And P(X2).CurMove < MaxMoves And P(X2).SuperNum <> 0 Then Msg = "you are Superering " + P(X2).Moves(P(X2).CurMove).Name + "."
      If P(X2).CurMove = pRest Then Msg = "you are resting."
      If P(X2).CurMove = pFlee Then Msg = "you are running like a coward!"
      If P(X2).CurMove = pBlock Then Msg = "you are already blocking."
      If P(X2).CurMove = pFatal Then Msg = "you are doing a Fatality."
      If P(X2).CurMove = pSlot Then Msg = "you are playing the slots."
      If P(X2).CurMove = -255 Then Msg = "you are not ready."
      If P(X2).Status(sFreeze) <> 0 Then Msg = "you are frozen."
      If P(X2).Status(sStun) <> 0 Then Msg = "you are unconsious."
      If P(X2).Status(sSleep) <> 0 Then Msg = "you are sound asleep."
      If P(X2).Status(sMIA) <> 0 Then Msg = "you are Missing in Action!"
      If P(X2).Status(sStop) <> 0 Then Msg = "you are frozen in time."
      If P(X2).Status(sMushroom) <> 0 Then Msg = "you are in mushroom form."
      If P(X2).HP <= 0 Then Msg = "you are dead!"
      If P(X2).CharID = 0 Then Msg = "you are not playing in the game."
      For X4 = 1 To MaxMoves
        If (P(X2).Moves(X4).Cmdkey <> "") Then
          M = P(X2).Moves(X4)
          If (Skip(Ct, Len(M.Cmdkey)) = UCase(M.Cmdkey)) Then
            Send (SN + ", you can't do " + M.Name + " while " + Msg)
            P(X2).Scroller = P(X2).Scroller + 1
            Exit Sub
          End If
        End If
      Next X4
      If (Skip(Ct, 1) = "1") Or (Skip(Ct, 1) = "2") Or (Skip(Ct, 1) = "3") Or (Skip(Ct, 1) = "4" And Config.Fours) Or (Skip(Ct, 1) = "5" And Config.Fours) Or (Skip(Ct, 1) = "6" And Six) Then
        Send (SN + ", you can't do a Super while " + Msg)
        P(X2).Scroller = P(X2).Scroller + 1
        Exit Sub
      End If
      If (Skip(Ct, 5) = "BLOCK") Then
        Send (SN + ", you can't block while " + Msg)
        P(X2).Scroller = P(X2).Scroller + 1
        Exit Sub
      End If
      If Skip(Ct, 3) = "GET" Then
        Send (SN + ", you can't get an item while " + Msg)
        P(X2).Scroller = P(X2).Scroller + 1
        Exit Sub
      End If
      If Skip(Ct, 4) = "WGET" Then
        Send (SN + ", you can't get a weapon while " + Msg)
        P(X2).Scroller = P(X2).Scroller + 1
        Exit Sub
      End If
      If Skip(Ct, 5) = "WDROP" Or Skip(Ct, 5) = "WJUNK" Then
        Send (SN + ", you can't drop your weapon while " + Msg)
        P(X2).Scroller = P(X2).Scroller + 1
        Exit Sub
      End If
      If Skip(Ct, 7) = "UNMORPH" Then
        Send (SN + ", you can't unmorph while " + Msg)
        P(X2).Scroller = P(X2).Scroller + 1
        Exit Sub
      End If
      If Skip(Ct, 6) = "CHICTR" Then
        Send (SN + ", you can't Chibot Counter while " + Msg)
        P(X2).Scroller = P(X2).Scroller + 1
      End If
      If Skip(Ct, 4) = "FLEE" Then
        Send (SN + ", you can't run for the hills while " + Msg)
        P(X2).Scroller = P(X2).Scroller + 1
      End If
      If Skip(Ct, 6) = "DIVERT" Then
        Send (SN + ", you can't Divert HP while " + Msg)
        P(X2).Scroller = P(X2).Scroller + 1
      End If
      If Skip(Ct, 8) = "UNDIVERT" And P(X2).Rune = RuneUndivert Then
        Send (SN + ", you can't Un-divert HP while " + Msg)
        P(X2).Scroller = P(X2).Scroller + 1
      End If
      If (Skip(Ct, Len(S.Fatality.Cmdkey)) = UCase(S.Fatality.Cmdkey)) And S.Fatality.Cmdkey <> "" Then
        Send (SN + ", you can't Fatalize anyone while " + Msg)
        P(X2).Scroller = P(X2).Scroller + 1
      End If
      If Skip(Ct, 4) = "SLOT" And (P(X2).Rune = RuneSlot Or AllSlot) Then
        Send (SN + ", you can't pull the slot machine while " + Msg)
        P(X2).Scroller = P(X2).Scroller + 1
      End If
    End If
    If (P(X2).CharID <> 0) And (P(X2).HP > 0) And (P(X2).CurMove = 0) And (P(X2).Status(sStop) = 0 And P(X2).Status(sMIA) = 0 And P(X2).Status(sMushroom) = 0) Then
      InitMoves (X2)
      If (Skip(Ct, 7) = "UNMORPH") And (P(X2).Status(sMorph)) Then
        Send (Parse(DATASET.UnMorphMsg, P(X2).ScrNam, Senshi(P(X2).OldCharID).FullName, "", ""))
        'LogFileWrite (p(x2).ScrNam + " reverted back to " + Senshi(p(x2).OldCharID).FullName + ".")
        UnmorphMe (X2)
      End If
      If (Skip(Ct, 4) = "WGET") And Len(Ct) > 6 Then
        If Config.WeaponEnable = 0 Then
          Send SN + ", weapons are disabled , sorry."
        Else
          If P(X2).Weapon <> 0 Then
            Send (P(X2).ScrNam + " is already holding a weapon.")
          Else
            S2 = Right$(Ct, Len(Ct) - 6)
            X4 = MatchDropped(S2)
            If X4 = 0 Then
              Send (P(X2).ScrNam + " looks around for that weapon but can't find it.")
            Else
              'p(x2).WeaponState = 1
              P(X2).Weapon = Dropped(X4)
              Dropped(X4) = 0
              InitMoves (X2)
              P(X2).WpnUsesLeft = DropUses(X4)
              DropUses(X4) = 0
              Send (Parse(Weapons(P(X2).Weapon).SelectStr, P(X2).ScrNam, "", "", ""))
            End If
          End If
        End If
      End If
      If Skip(Ct, 5) = "WDROP" And P(X2).Weapon <> 0 Then
        If Left$(Weapons(P(X2).Weapon).PickMe, 1) = "~" Then
          Send (P(X2).ScrNam + " drops their " + Weapons(P(X2).Weapon).Name + " and it vanishes into oblivion!")
          P(X2).Weapon = 0
        Else
          DropWeapon X2, "%SN drops their %T."
        End If
      End If
      If Skip(Ct, 5) = "WJUNK" And P(X2).Weapon <> 0 Then
        Send (P(X2).ScrNam + " drops their " + Weapons(P(X2).Weapon).Name + " and it vanishes into oblivion!")
        P(X2).Weapon = 0
      End If
      If (Skip(Ct, 3) = "GET") Then
        If (Item2Get <> 0) Then
          Call DOMOVE(X2, 0, pGet, "")
        Else
          Send (Parse(DATASET.NoGetItem, P(X2).ScrNam, "", "", ""))
        End If
      End If
      If (Skip(Ct, 6) = "CHICTR") And (Len(Ct) > 8) Then
        If (P(X2).Super >= 200) Or P(X2).Rune = RuneBlock Then
          S3$ = Right$(Ct, Len(Ct) - 8)
          For X4 = 1 To MaxMoves
            If (P(X2).Moves(X4).Cmdkey <> "") Then
              Okay = False
              M = P(X2).Moves(X4)
              If (UCase(S3$)) = UCase(M.Cmdkey) Then Okay = True
              If (P(X2).Status(sMute) <> 0) And (M.Element <> Phys) Then Okay = False
              If (P(X2).Rune = RuneBlock And P(X2).Super < 100) And Okay Then
                Okay = False
                Send (P(X2).ScrNam + " still needs the 100 Super Point cost up front to do a Chibot Counter.")
              End If
              If Okay Then
                'Send (Parse(Dataset.ChiCtr, p(x2).ScrNam, p(x2).Moves(x4).Name, "", ""))
                P(X2).Status(sHamedo) = -1
                P(X2).Super = P(X2).Super - 100
                'LogFileWrite (SN + " Chibot Countered with """ + p(x2).moves(x4).Name + """")
                Call DOMOVE(X2, X4, pBlock, "")
              End If
            End If
          Next X4
        Else
          Send (P(X2).ScrNam + " needs 200 Super Points to do a Chibot Counter.")
        End If
      End If
      If (Skip(Ct, 5) = "BLOCK") Then
        P(X2).Status(sHamedo) = 0
        If (Len(Ct) > 7) And (P(X2).Super >= 100 Or P(X2).Rune = RuneBlock) Then
          S3$ = Right$(Ct, Len(Ct) - 7)
          For X4 = 1 To MaxMoves
            If (P(X2).Moves(X4).Cmdkey <> "") Then
              Okay = False
              M = P(X2).Moves(X4)
              If (UCase(S3$)) = UCase(M.Cmdkey) Then Okay = True
              If (P(X2).Status(sMute) <> 0) And (M.Element <> Phys) Then Okay = False
              If Okay Then
                'Send (Parse(Dataset.Counter, p(x2).ScrNam, p(x2).moves(x4).Name, "", ""))
                'LogFileWrite (SN + " counterattacked with """ + p(x2).moves(x4).Name + """")
                Call DOMOVE(X2, X4, pBlock, "")
              End If
            End If
          Next X4
        Else
          If (Len(Ct) > 7) And (P(X2).Super < 100) Then Send ("Not enough Super Point for " + SN + " to counter.")
          Call DOMOVE(X2, 0, pBlock, "")
        End If
      End If
      If (Skip(Ct, 5) = "TAUNT") Then Call DOMOVE(X2, 0, pTaunt, "")
      If (Skip(Ct, 5) = "~SAVE") And (Len(Ct) > 7) Then
        P(X2).Target = Playernum(Mid$(Ct, 8, Len(Ct) - 7))
        If P(X2).Target <> 0 Then Call DOMOVE(X2, P(X2).Target, p_Save, "")
      End If
      'If (Skip(Ct, 4) = "LOOK") Then
      '  If Item2Get <> 0 Then
      '    Send (Items(Item2Get).Name)
      '  Else
      '    Send ("There is no item to get.")
      '  End If
      'End If
      If (Skip(Ct, 4) = "FLEE") Then
        If (P(X2).HP >= 150) Then
          Call DOMOVE(X2, 0, pFlee, "")
        Else
          If (P(X2).HP > 10) Then P(X2).HP = P(X2).HP - 10
          Send5 (Parse(DATASET.FleeFail, SN, "", "", ""))
          'LogFileWrite (SN + " tried to flee but panicked.")
          ShowStatus X2
        End If
      End If
      If (Skip(Ct, 4) = "REST") Then Call DOMOVE(X2, 0, pRest, "")
      If (Skip(Ct, 8) = "UNDIVERT") And Len(Ct) > 10 And P(X2).Rune = RuneUndivert Then
        On Error GoTo NoOvr
        S5 = Right$(Ct, Len(Ct) - 10)
        X5 = Val(S5)
        If (X5 > 0) Then
          If P(X2).Super - X5 < 0 Then
            If P(X2).Super = 0 Then
              Send (SN + " cannot Un-divert any SP at this time.")
            Else
              If P(X2).Super > (P(X2).MaxHP * 1.5 - P(X2).HP) Then
                Send (SN + " cannot Un-divert any more SP than " + TrimStr(P(X2).Super) + ".")
              Else
                Send (SN + " cannot Un-divert any more SP than " + TrimStr(Int(P(X2).MaxHP * 1.5 - P(X2).HP)) + ".")
              End If
            End If
          ElseIf P(X2).HP + X5 > P(X2).MaxHP * 1.5 Then
            If P(X2).HP >= P(X2).MaxHP * 1.5 Then
              Send (SN + " cannont Un-divert any more SP at this time.")
            Else
              If P(X2).Super > (P(X2).MaxHP * 1.5 - P(X2).HP) Then
                Send (SN + " cannot Un-divert any more SP than " + TrimStr(P(X2).Super) + ".")
              Else
                Send (SN + " cannot Un-divert any more SP than " + TrimStr(Int(P(X2).MaxHP * 1.5 - P(X2).HP)) + ".")
              End If
            End If
          Else
            P(X2).HP = P(X2).HP + X5
            P(X2).Super = P(X2).Super - X5
            Send (SN + " Un-diverted " + TrimStr(X5) + " Super Points to their HP Meter.")
          End If
        Else
          GoTo NoOvr
        End If
      End If
      If (Skip(Ct, 6) = "DIVERT" And Len(Ct) > 8) Then
        'Send ("Sorry, " + SN + ", but <b>/divert</b> no longer works.")
        On Error GoTo NoOvr
        S5 = Right$(Ct, Len(Ct) - 8)
        X5 = Val(S5)
        If (X5 > 0) Then
          If P(X2).HP - X5 < Int(MaxHP / 2) Then
            If P(X2).HP - Int(MaxHP / 2) > 0 Then
              Send (P(X2).ScrNam + " cannot divert any more than " + TrimStr(P(X2).HP - Int(MaxHP / 2)) + " HP at this time.")
            Else
              Send (P(X2).ScrNam + " cannot divert any more HP at this time.")
            End If
          Else
            P(X2).HP = P(X2).HP - X5
            P(X2).Super = P(X2).Super + X5
            Send (Parse(DATASET.HPDivert, P(X2).ScrNam, TrimStr(X5), "", ""))
            'LogFileWrite (SN + " diverted " + TrimStr(X5) + " HP to their Super Meter.")
            ShowStatus X2
            P(X2).CurMove = 0
          End If
        Else
NoOvr:
          Send (P(X2).ScrNam + ", I wasn't born yesterday foo'!")
          Resume ResMe
        End If
ResMe:
        On Error GoTo 0
      End If
      If (Skip(Ct, 22) = "YO QUIERO ESTAR MUERTE") Then
        Send ("WUSS!!! Fight like a man!!")
        'p(x2).HP = 0
        'ShowStatus x2
      End If
      S = Senshi(P(X2).CharID)
      If (Skip(Ct, Len(S.Fatality.Cmdkey)) = UCase(S.Fatality.Cmdkey)) And S.Fatality.Cmdkey <> "" Then
        If Len(Ct) > Len(S.Fatality.Cmdkey) + 1 Then
          P(X2).Target = Playernum(Mid$(Ct, Len(S.Fatality.Cmdkey) + 3, Len(Ct) - Len(S.Fatality.Cmdkey) - 2))
        Else
          P(X2).Target = 0
        End If
        Okay = True
        If (P(X2).Target = 0) Then
          For X4 = 1 To MaxPlayers
            If P(X4).HP <= (MaxHP / 6) And P(X4).HP > 0 And P(X4).CharID <> 0 Then
              P(X2).Target = X4
              Exit For
            End If
          Next X4
          If P(X2).Target = 0 Then Okay = False
        End If
        X4 = P(P(X2).Target).HP
        If P(X2).Status(sQuick) <> 0 Then
          Send ("Fatalitying while Quick? That's a low blow, isn't it " + P(X2).ScrNam + "??")
        ElseIf (X4 <= (MaxHP / 6)) Then
          Call DOMOVE(X2, P(X2).Target, pFatal, "")
        End If
      End If
      DoEvents
      S = Senshi(P(X2).CharID)
      If (Skip(Ct, 1) = "1") Or (Skip(Ct, 1) = "2") Or (Skip(Ct, 1) = "3") Or (Skip(Ct, 1) = "4" And Config.Fours) Or (Skip(Ct, 1) = "5" And Config.Fours) Or (Skip(Ct, 1) = "6" And Six) Then
        X4 = Val(Skip(Ct, 1))
        If (P(X2).Super >= 100 * X4) And (Len(Ct) > 3) Then
          S2 = Right$(Ct, Len(Ct) - 3)
          For X5 = 1 To MaxMoves
            If (UCase(Left$(S2, Len(P(X2).Moves(X5).Cmdkey))) = UCase(P(X2).Moves(X5).Cmdkey)) Then Okay = P(X2).Moves(X5).CanSuper
            If Okay Then Exit For
          Next X5
          If P(X2).Status(sQuick) <> 0 Then
            Send ("Supering while Quick? That's quite a low blow, isn't it, " + SN + "?")
            Okay = False
          End If
          If P(X2).Charging Then
            Send ("Sorry, you cannot Charge a Super Move.")
            Okay = False
          End If
          If Okay Then
            P(X2).SuperNum = X4
            Ct = "/" + S2
          End If
        End If
      Else
        P(X2).SuperNum = 0
      End If
      If Skip(Ct, 4) = "SLOT" And (P(X2).Rune = RuneSlot Or AllSlot) Then
        P(X2).Target = 0
        If (Len(Ct) > 6) Then
          If (P(X2).Rune <> RuneStealth) Or M.Element = Morph Then
            S7 = Mid$(Ct, X5 + 3, Len(Ct) - X5 - 2)
            P(X2).Target = Playernum(S7)
          Else
            Send ("Whoops! " + SN + ", did I mention that you can't target with the Rune of Stealth?")
          End If
        Else
          S7 = ""
        End If
        If (P(X2).Target <> 0 And P(X2).ScrNam = YourSN And P(X2).Rune <> RuneStealth) Then Send (YourSN + " targeted " + P(P(X2).Target).ScrNam + "!!!")
        If (UCase(S7) = "FOE") And (P(X2).Rune <> RuneStealth) Then P(X2).Target = GetTarget(P(X2).TeamID)
        If ((UCase(S7) = "FRIEND") Or (UCase(S7) = "ALLFRIEND")) And (P(X2).Rune <> RuneStealth) Then P(X2).Target = X2
        If ((Left$(UCase(S7), 4) = "TEAM") Or (Left$(UCase(S7), 3) = "ALL")) And (P(X2).Rune <> RuneStealth) Then
          S6 = UCase(Right$(S7, 1))
          For X6 = 1 To MaxPlayers
            If P(X6).TeamID = UCase(S6) And P(X6).HP > 0 And Active(X6) <> 0 Then
              P(X2).Target = X6
              Exit For
            End If
          Next X6
        End If
        If (P(X2).Target = 0) Then
          Select Case M.Target
            Case Allfriend, Ally, OnlySelf: P(X2).Target = X2
            Case Else: P(X2).Target = GetTarget(P(X2).TeamID)
          End Select
        End If
        Call DOMOVE(X2, P(X2).Target, pSlot, "")
        Exit Sub
      End If
      For X4 = 1 To MaxMoves
        If (P(X2).Moves(X4).Cmdkey <> "") Then
          Okay = False
          M = P(X2).Moves(X4)
          If (Skip(Strip32(Ct), Len(Strip32(M.Cmdkey))) = Strip32(UCase(M.Cmdkey))) Then Okay = True
          If (P(X2).Status(sMute) <> 0) And (M.Element <> Phys) Then
            If (Skip(Ct, Len(M.Cmdkey)) = UCase(M.Cmdkey)) Then Send (P(X2).ScrNam + ", you cannot do this move while Mute.")
            Okay = False
          End If
          If Okay Then
            X5 = Len(M.Cmdkey)
            P(X2).Target = 0
            If (Len(Ct) > X5 + 1) Then
              If (P(X2).Rune <> RuneStealth) Or M.Element = Morph Then
                S7 = Mid$(Ct, X5 + 3, Len(Ct) - X5 - 2)
                P(X2).Target = Playernum(S7)
              Else
                Send ("Whoops! " + SN + ", did I mention that you can't target with the Rune of Stealth?")
              End If
            Else
              S7 = ""
            End If
            If (P(X2).Target <> 0 And P(X2).ScrNam = YourSN And P(X2).Rune <> RuneStealth) Then Send (YourSN + " targeted " + P(P(X2).Target).ScrNam + "!!!")
            If (UCase(S7) = "FOE") And (P(X2).Rune <> RuneStealth) Then P(X2).Target = GetTarget(P(X2).TeamID)
            If ((UCase(S7) = "FRIEND") Or (UCase(S7) = "ALLFRIEND")) And (P(X2).Rune <> RuneStealth) Then P(X2).Target = X2
            If ((Left$(UCase(S7), 4) = "TEAM") Or (Left$(UCase(S7), 3) = "ALL")) And (P(X2).Rune <> RuneStealth) Then
              S6 = UCase(Right$(S7, 1))
              For X6 = 1 To MaxPlayers
                If P(X6).TeamID = UCase(S6) And P(X6).HP > 0 And Active(X6) <> 0 Then
                  P(X2).Target = X6
                  Exit For
                End If
              Next X6
            End If
            If (P(X2).Target = 0) Then
              Select Case M.Target
                Case Allfriend, Ally, OnlySelf: P(X2).Target = X2
                Case Else: P(X2).Target = GetTarget(P(X2).TeamID)
              End Select
            End If
            Call DOMOVE(X2, P(X2).Target, X4, S7)
            Exit Sub
          End If
        End If
      Next X4
      For X4 = 1 To MaxWeapons
        DoEvents
        If Skip(Ct, Len(Weapons(X4).PickMe)) = UCase(Weapons(X4).PickMe) Then
          Send (SN + ", you can't pick a weapon during battle. You may only pick up dropped weapons (/wpnlist)")
          Exit Sub
        End If
      Next X4
    End If
  End If
  DoEvents
End Sub

Private Sub tiFullAuto_Timer()
Static NextTimer
  On Error Resume Next
  If FullAuto.Enabled Then
    mBeginBattle.Enabled = False
    If Timer < NextTimer Then Exit Sub
    Select Case GameStage
      Case 4:
        Auto = True
        mBeginBattle_Click
        Auto = False
        If Config.Flag = 1 Then
          ListByTeams
        End If
        'tiFullAuto.Interval = FullAuto.DefectTime * 1000
        NextTimer = Timer + FullAuto.DefectTime
        Send ("Automatic: Battle will start in " + TrimStr(FullAuto.DefectTime / 60) + " minutes.")
        mPauseBattle.Enabled = False
        GameStage = 5
      Case 5:
        mPauseBattle_Click
        mPauseBattle.Enabled = True
        tiFullAuto.interval = 5000
        GameStage = 6
      Case 6:
        If (Not Battle) Then
          'tiFullAuto.Interval = FullAuto.PostTime * 1000
          NextTimer = Timer + FullAuto.PostTime
          Send ("Automatic: Character selection will start in " + TrimStr(FullAuto.PostTime / 60) + " minutes.")
          GameStage = 7
        End If
      Case 7:
        If Shotgun <> "" Then Unload fPreBattl
        Unload Me
    End Select
  Else
    tiFullAuto.Enabled = False
  End If
End Sub

Private Sub tiMisc_Timer()
 ' Misc stuff
Dim X%, X1%, X2%, X3%, X4%, y2%, ST%, Df%, CPUWon%
Dim X6%, Msg$, Stat$
Dim S4, S5 As String
Dim Tix As Long
Dim Tix1 As Long
ReDim Hoser(sMaxStatus) As Integer
Dim dodraw%, AllDead%
Dim MaxFrag%, Maxp%
Dim ORed%
Dim OBlue%, LastTmr As Long
Static Tempo%, Lango%
  On Error Resume Next
  If (Tm <> 0) And (CurArena <> Lang) Then
    Select Case Int(XTimer - Tm)
      Case 60: Send ("This is your two-minute warning.")
               Send ("This game will self-destruct in two minutes.")
      Case 120: Send ("This game will self-destruct in only one minute.")
      Case 160: Send ("This game will self-destruct in 20 seconds.")
                Send ("This is your last chance to push the cancellation button.")
      Case 169:
        mPauseBattle_Click
        ScrollSend1 (">>> Counting down <<<")
      Case 170 To 179: ScrollSend1 (">>> " + TrimStr(180 - (XTimer - Tm)) + " <<<")
      Case 180: ScrollSend1 ("Have a nice day! {S Goodbye"): End
    End Select
  End If
  If (Battle) Then
    For X4 = 1 To MaxPlayers
      If HPx(X4) <= 0 And Active(X4) And (Not DoneKilled(X4)) Then
        ShowStatus (X4)
      ElseIf HPx(X4) > 0 And Active(X4) Then
        P(X4).GotKilled = 0
        ' Rune Sub
        If P(X4).Rune = RuneArmor And P(X4).RuneTemp = 0 Then
          P(X4).RuneTemp = Rand(20, 26)
          If P(X4).RuneTemp = Reveal Then P(X4).RuneTemp = Demi
          If P(X4).RuneTemp = Heart Then P(X4).RuneTemp = Poison
          Msg = P(X4).ScrNam + "'s Elemental Armor is charged with <b>"
          Select Case P(X4).RuneTemp
            Case MoonE: Msg = Msg + "moon power"
            Case Shadow: Msg = Msg + "dark energy"
            Case Water: Msg = Msg + "water magic"
            Case Fire: Msg = Msg + "fire power"
            Case Lit: Msg = Msg + "lightning"
            Case Demi: Msg = Msg + "gravity magic"
            Case Poison: Msg = Msg + "poison power"
            Case Else: Msg = Msg + EName(P(X4).RuneTemp)
          End Select
          Msg = Msg + "</b>."
          Send Msg
        End If
        If P(X4).Rune = RuneSummon And P(X4).RuneTemp = 0 And XTimer <= 5 Then P(X4).RuneTemp = -100
        If P(X4).Rune = RunePMS And P(X4).Status(sPMS) <> 0 And P(X4).CurMove = 0 Then P(X4).CurMove = -255
        If P(X4).Rune = RuneHaste Then P(X4).Status(sHaste) = 666
        If P(X4).Rune = RuneStr Then P(X4).Status(sBless) = 666
        If P(X4).Rune = RuneRot And P(X4).Status(sPoison) = 0 Then
          Send (P(X4).ScrNam + " is poisoned by the rotten rune!")
          P(X4).Status(sPoison) = XTimer
        End If
        If P(X4).Rune = RuneCurse And P(X4).Status(sCurse) = 0 Then P(X4).Status(sCurse) = XTimer
        If P(X4).Rune = RuneBarr Then
          P(X4).Status(sBarrier) = 666
          P(X4).Status(sMBarrier) = 666
        End If
        If P(X4).Rune = RuneRegen Then P(X4).Status(sRegen) = 666
        If P(X4).Rune = RuneLife3 And P(X4).Status(sLife3) = 0 Then P(X4).Status(sLife3) = True
        'If p(x4).Rune = RuneCharge And p(x4).Charging And p(x4).CurMove = 0 Then p(x4).Charging = False
        If P(X4).Rune = RuneMute And P(X4).Status(sMute) = 0 Then
          Send (P(X4).ScrNam + " is muted by the Rune of Mute!")
          P(X4).Status(sMute) = XTimer
        End If
        If P(X4).Rune = RuneZombie And P(X4).Status(sZombie) = 0 Then
          Send (P(X4).ScrNam + " is turned into a zombie by the Rune of the Undead!")
          P(X4).PhysStr = 250
          P(X4).MagStr = 250
          P(X4).God = True
          P(X4).MaxHP = P(X4).MaxHP * 2
          P(X4).HP = P(X4).MaxHP
          P(X4).Status(sZombie) = True
        End If
        If P(X4).Rune = RuneBeans And Rand(1, 80) = 25 And (Not Paused) Then
          X = GetTarget(P(X4).TeamID)
          If P(X4).Status(sMIA) = 0 Then
            Select Case Rand(1, 5)
              Case 1:
                If P(X).Rune = RuneMagic Then
                  Send (P(X4).ScrNam + " farts on " + P(X).ScrNam + ", but " + P(X).ScrNam + " lights a match and " + P(X4).ScrNam + "'s butt is charred!")
                Else
                  Send (P(X).ScrNam + " smells " + P(X4).ScrNam + "'s bad gas and swoons!")
                  Hoser(sStun) = 100
                  CheckStatus Hoser(), X, X4, 1
                End If
              Case 2:
                If P(X).Rune = RuneMagic Then
                  Send (P(X4).ScrNam + " farts on " + P(X).ScrNam + ", but " + P(X).ScrNam + " lights a match and " + P(X4).ScrNam + "'s butt is charred!")
                Else
                  Send (P(X).ScrNam + " is poisoned by " + P(X4).ScrNam + "'s bad gas!")
                  Hoser(sPoison) = 100
                  CheckStatus Hoser(), X, X4, 1
                End If
              Case 3:
                Send (P(X4).ScrNam + " farts. P-U!")
              Case 4:
                Send (P(X4).ScrNam + " lets out a whopper fart!")
              Case 5:
                Send (P(X4).ScrNam + " cuts the cheese again.")
            End Select
            Playwav ("fart" + TrimStr(Rand(1, 6)))
          End If
        End If
        If P(X4).Rune = RuneBug And Rand(1, 85) = 25 And (Not Paused) Then
          If P(X4).CurMove > 0 And P(X4).CurMove < MaxMoves Then
            Select Case Rand(1, 6)
              Case 1:
                Do
                  X = Rand(1, MaxMoves)
                  DoEvents
                Loop Until (P(X4).Moves(X).Cmdkey <> "")
                Send ("A bug in ChUB causes " + P(X4).ScrNam + "'s move to change to " + P(X4).Moves(X).Name + ".")
                P(X4).CurMove = X
              Case 2:
                X = ChaosTarget()
                Send ("A bug in ChUB causes " + P(X4).ScrNam + "'s target to change to " + ScrNam(X) + ".")
                P(X4).Target = X
              Case 3:
                Send ("A bug in ChUB stops " + P(X4).ScrNam + "'s move.")
                P(X4).CurMove = 0
              Case 4:
                Send ("A bug in ChUB causes " + P(X4).ScrNam + "'s move to hit immediately.")
                P(X4).MoveStart = -999
              Case 5:
                Send ("A bug in ChUB changes " + P(X4).ScrNam + "'s move attack length.")
                P(X4).MoveStart = P(X4).MoveStart + 10
              Case 6:
                Send ("Another one of Sally's stupid bugs makes " + P(X4).ScrNam + " go insane!")
                P(X4).Status(sBerserk) = XTimer
            End Select
          End If
        End If
        If P(X4).Rune = RuneWeird And Rand(1, 85) = 25 And P(X4).Status(sMIA) = 0 And (Not Paused) Then
          Select Case Rand(1, 10)
            Case 1:
              P(X4).Status(sHaste) = XTimer
              Send (P(X4).ScrNam + " gets Haste for no reason! How weird!")
            Case 2:
              P(X4).Status(sBless) = XTimer
              Send (P(X4).ScrNam + " gets Blessed for no reason! How weird!")
            Case 3:
              P(X4).Status(sQuick) = XTimer
              Send (P(X4).ScrNam + " gets Quick for no reason at all! How very weird!")
            Case 4:
              P(X4).Status(sBarrier) = 100
              P(X4).Status(sMBarrier) = 100
              Send (P(X4).ScrNam + " gets their barriers recharged for no reason whatsoever.")
            Case 5:
              P(X4).HP = P(X4).HP + 200
              Send (P(X4).ScrNam + " gets 200 HP for no reason! Weird!")
            Case 6:
              If P(X4).CurMove <= 0 Then
                P(X4).Target = GetTarget(P(X4).TeamID)
                If P(X4).Target <> 0 Then
                  X3 = 0
                  For X = 1 To MaxMoves
                    If P(X4).Moves(X).Cmdkey <> "" And P(X4).Moves(X).Target = Enemy Then X3 = X
                  Next X
                  If X3 <> 0 Then
                    Do
                      X = Rand(1, MaxMoves)
                      DoEvents
                    Loop Until (P(X4).Moves(X).Cmdkey <> "" And P(X4).Moves(X).Target = Enemy)
                    Send (P(X4).ScrNam + " suddenly lashes out at " + ScrNam(P(X4).Target) + "!")
                    P(X4).CurMove = X
                    P(X4).MoveStart = -999
                  End If
                End If
              Else
                Send (P(X4).ScrNam + "'s move will hit right... about... NOW!")
                P(X4).MoveStart = -999
              End If
            Case 7:
              AddStatus X4, sMIA
              Send ("Dude! Visitors just landed! They've kidnapped " + P(X4).ScrNam + "! You bastards!")
            Case 8:
              Send (P(X4).ScrNam + " sees Kenny nearby and kills him for the heck of it!")
              Send (P(X4).ScrNam + " gets a good kick out of it! ""OH MY GOD, THEY KILLED KENNY!"" shouts Stan.")
              P(X4).MaxHP = P(X4).MaxHP + 50
            Case 9:
              Send (P(X4).ScrNam + " gets some Super Points for no reason!")
              'Send (p(x4).ScrNam + " gets a magic boost for no reason!")
              'p(x4).MaxMP = p(x4).MaxMP + 50
              P(X4).Super = P(X4).Super + 100
            Case 10:
              If P(X4).Status(sLife3) = 42 Then
                Send (P(X4).ScrNam + " is blessed by the KUB Goddess, Sally.")
                Send (P(X4).ScrNam + " gets another Life3!")
                P(X4).Status(sLife3) = -1
              ElseIf Config.Flag <> 0 And P(X4).CurMove > 0 And P(X4).CurMove <= MaxMoves Then
                Send (P(X4).ScrNam + ", watch who you're targeting...")
                X3 = P(X4).Moves(P(X4).CurMove).Target
                If (X3 = Enemy) Or (X3 = AllFoe) Or (X3 = AllTeam) Or (X3 = Everybody) Then
                  If P(X4).TeamID = "B" Then
                    P(X4).Target = RedFlag
                  Else
                    P(X4).Target = BlueFlag
                  End If
                Else
                  If P(X4).TeamID = "B" Then
                    P(X4).Target = BlueFlag
                  Else
                    P(X4).Target = RedFlag
                  End If
                End If
              ElseIf P(X4).Status(sRegen) = 0 Then
                Send (P(X4).ScrNam + " gets Regen for no reason!")
                P(X4).Status(sRegen) = -1
              End If
          End Select
        End If
      End If
      If ((P(X4).HP <= 0 And Config.Respawn) Or P(X4).HP > 0) And (P(X4).CharID > 0) Then
        'If (Timer - p(x4).InactiveTime > 360) And (p(x4).InactiveWarn = 0) Then
        '  Send ("<b><font color=#FF0000>WARNING " + p(x4).ScrNam + " you are IDLE! {S Systemexit}")
        '  Send ("<b><font color=#FF0000>Do any command within 40 seconds to avoid being removed.")
        '  p(x4).InactiveWarn = 1
        'End If
        'If (Timer - p(x4).InactiveTime > 400) Then
        '  Send ("<b><font color=#FF0000>" + p(x4).ScrNam + " falls into the void of idleness and frees up a slot. {S Goodbye}")
        '  Playwav ("tornoff2")
        '  p(x4).CharID = 0
        '  p(x4).InactiveTime = 0
        '  p(x4).InactiveWarn = 0
        'End If
      End If
      'If (Paused) And ((p(x4).HP <= 0 And Config.Respawn) Or p(x4).HP > 0) And (p(x4).CharID > 0) Then
        'p(x4).InactiveTime = p(x4).InactiveTime + 1
      'End If
    Next X4
    If (Item2Get = 0) Then
      LItem = "Nothing"
    Else
      LItem = Items(Item2Get).Name
    End If
    dodraw = True
    AllDead% = True
    For X = 1 To MaxPlayers
      If P(X).HP > 0 And P(X).CharID <> 0 And P(X).Draw = False Then dodraw = False
      If P(X).HP > 0 And P(X).CharID <> 0 Then AllDead = False
    Next X
    If AllDead And (CurArena <> Lang) Then
      Playwav ("lose")
      EndIt DATASET.AllDead
      Exit Sub
    ElseIf AllDead Then
      Playwav ("lose")
      EndIt (">>> The langoliers have arrived and eaten the past.")
      Exit Sub
    End If
    If dodraw Then
      Playwav ("draw")
      EndIt DATASET.Draw
      Exit Sub
    End If
    If (TLimit > 0) And (Lang <> CurArena) Then
      Tix = (XTimer - TimeLimit)
      Tix1 = (TLimit - Tix)
      Select Case Tix1
        Case Is > 60: Label4.Caption = TrimStr(Fix((TLimit - Tix) / 60)) + " minutes"
        Case 0 To 59: Label4.Caption = TrimStr(TLimit - Tix) + " seconds"
      End Select
      Label4.Visible = True
      'Label3.Visible = True
      If (Not Paused) Then
        Select Case TLimit - Tix
          Case 60 * 60: ScrollSend DATASET.x1HrLeft
          Case 60 * 30: ScrollSend DATASET.x30MinsLeft
          Case 60 * 20: ScrollSend DATASET.x20MinsLeft
          Case 60 * 10: ScrollSend DATASET.x10MinsLeft
          Case 60 * 5: ScrollSend DATASET.x5MinsLeft
          Case 120: ScrollSend DATASET.x2MinsLeft
          Case 60:
            If (Config.Flag = 0) Then
              ScrollSend DATASET.SuddenDeath
              Playwav ("hurry")
            Else
              ScrollSend DATASET.x1MinsLeft
            End If
          Case 30: ScrollSend DATASET.x30SecsLeft
          Case 15: ScrollSend DATASET.x15SecsLeft
          Case 5: ScrollSend DATASET.x5SecsLeft
          Case Is <= 0:
            If (Config.Flag = 0) Then
              Playwav ("timeout")
              EndIt DATASET.TimeExpired
              Exit Sub
            Else
              'Playwav ("win")
              CPUWon = True
              If P(RedFlag).TeamID = "B" Then
                For X = 1 To MaxPlayers
                  If P(X).CPU = 0 And P(X).TeamID = "B" Then CPUWon = False
                Next X
                If CPUWon Then Playwav ("lose") Else Playwav ("win")
                EndIt (">>> <font color=""#0000FF"">Blue Team wins!")
              ElseIf P(BlueFlag).TeamID = "R" Then
                For X = 1 To MaxPlayers
                  If P(X).CPU = 0 And P(X).TeamID = "R" Then CPUWon = False
                Next X
                If CPUWon Then Playwav ("lose") Else Playwav ("win")
                EndIt (">>> <font color=""#FF0000"">Red Team wins!")
              Else
                Playwav ("draw")
                EndIt (">>> Who won? Beats me! (Error)")
              End If
            End If
        End Select
        If (TLimit - Tix <= 60) And (Config.Flag = 0) Then
          Stat$ = "status ChUBMidi tempo"
          Msg$ = Space$(255)
          X4 = mciSendString(Stat$, Msg$, 255, 0)
          If Tempo% <> Val(Msg$) Then Tempo% = Int(1.25 * Val(Msg$))
          If Val(Msg$) <> 0 Then X4 = mciSendString("set ChUBMidi tempo " + TrimStr(Tempo), 0&, 0, 0)
        End If
      End If
    ElseIf (Lang = CurArena) Then
      Select Case Tix1
        Case Is > 60: Label4.Caption = TrimStr(Int((600 - XTimer) / 60)) + " minutes"
        Case 0 To 59: Label4.Caption = TrimStr(600 - XTimer) + " seconds"
      End Select
    Else
      'Label4.Visible = True
      'Label3.Visible = True
      Label4.Caption = "Eternity"
    End If
    If (Lang = CurArena) Then
      If XTimer > 10 And Lango = 0 Then
        Send ("Mwahahaha... good luck!")
        Lango = 1
      End If
      If XTimer > 60 And Lango = 1 Then
        Send ("If it weren't for all the noise of the battle, you could hear a faint buzzing.")
        Arena(Lang).RestLowHP = 30
        Arena(Lang).RestHighHP = 45
        'Arena(Lang).RestLowMP = 30
        'Arena(Lang).RestHighMP = 45
        Arena(Lang).AllAttacks = 0.9
        Lango = 2
      End If
      If XTimer > 120 And Lango = 2 Then
        Arena(Lang).AllAttacks = 0.8
        Lango = 3
      End If
      If XTimer > 180 And Lango = 3 Then
        Arena(Lang).RestLowHP = 30
        Arena(Lang).RestHighHP = 45
        'Arena(Lang).RestLowMP = 30
        'Arena(Lang).RestHighMP = 45
        Arena(Lang).AllAttacks = 0.7
        Lango = 4
      End If
      If XTimer > 240 And Lango = 4 Then
        Send ("You can now hear the buzzing sound.")
        Arena(Lang).AllAttacks = 0.6
        Lango = 5
      End If
      If XTimer > 300 And Lango = 5 Then
        Send ("The buzzing sound of the langoliers increases volume.")
        Arena(Lang).RestLowHP = 20
        Arena(Lang).RestHighHP = 40
        'Arena(Lang).RestLowMP = 20
        'Arena(Lang).RestHighMP = 40
        Arena(Lang).AllAttacks = 0.5
        Lango = 6
      End If
      If XTimer > 360 And Lango = 6 Then
        Arena(Lang).AllAttacks = 0.4
        Lango = 7
      End If
      If XTimer > 420 And Lango = 7 Then
        Send ("The langoliers sound really pissed off!")
        Arena(Lang).RestLowHP = 10
        Arena(Lang).RestHighHP = 20
        'Arena(Lang).RestLowMP = 10
        'Arena(Lang).RestHighMP = 20
        Arena(Lang).AllAttacks = 0.3
        Lango = 8
      End If
      If XTimer > 480 And Lango = 8 Then
        Send ("The langoliers are REALLY close now!")
        Arena(Lang).AllAttacks = 0.2
        Lango = 9
      End If
      If XTimer > 540 And Lango = 9 Then
        Send ("The langoliers' buzzing is deafening now.")
        Arena(Lang).RestLowHP = 0
        Arena(Lang).RestHighHP = 10
        'Arena(Lang).RestLowMP = 0
        'Arena(Lang).RestHighMP = 10
        Arena(Lang).AllAttacks = 0.1
        Lango = 10
      End If
      If XTimer > 600 Then
        Send ("The langoliers have arrived.")
        Send ("<CHOMP> <CHEW> <SMACK> <GULP>")
        Send ("You lose!")
        Arena(Lang) = Langolier
        For X = 1 To MaxPlayers
          P(X).HP = Rand(-1, -6000)
        Next X
      End If
    End If
    S5 = "No Winner"
    For X = 1 To MaxPlayers
      If P(X).CharID <> 0 And P(X).HP > 0 Then
        S5 = "Keep going"
      End If
    Next X
    If (S5 = "No Winner") Then
      'EndIt ("Hmm... looks like I win, since I killed everyone! <Evil Cackling>")
      Exit Sub
    End If
    S4 = ""
    For X = 1 To MaxPlayers
      If (P(X).CharID <> 0) And (P(X).HP > 0) Then
        If (S4 = "") Then
          S4 = P(X).TeamID
        End If
        If (P(X).TeamID <> S4) Then
          S4 = "!!!!!"
          Exit For
        End If
      End If
    Next X
    If (S4 <> "!!!!!") And (Config.Respawn = 0) And (Config.Flag = 0) Then
    ' All that was to check to see if the battle is over.
    ' If battle is over, then S4 contains the TeamID of the winning team.
      S5 = ">>> ^" '+ TName(Asc(S4))
      CPUWon = True
      For X4 = 1 To MaxPlayers
        If P(X4).TeamID = S4 Then
          S5 = S5 + P(X4).ScrNam + ", "
          If P(X4).CPU = 0 Then CPUWon = False
        End If
      Next X4
      S5 = Left$(S5, Len(S5) - 2) + "^ (" + tName(Asc(S4)) + ") wins for being the last alive!"
      Msg = S5
      If CPUWon Then
        Playwav ("lose")
      Else
        Playwav ("win")
      End If
      MaxFrag% = 0
      Maxp% = 0
      EndIt (Msg)
    End If
    If FragLimit <> 0 Then
      Maxp% = 0
      For X4 = 1 To MaxPlayers
        If P(X4).CPU = 0 Then
          Select Case FragLimit
            Case Is < 0:
              If P(X4).FatalFrags >= Abs(FragLimit) Then Maxp% = X4
            Case Is > 0:
              If P(X4).Frags >= FragLimit Then Maxp% = X4
          End Select
        End If
      Next X4
      If Maxp% <> 0 And (Config.Flag = 0) Then
        Msg = ">>> "
        Msg = Msg + P(Maxp%).ScrNam
        Msg = Msg + " wins for achieving " + TrimStr(FragLimit)
        If (FragLimit > 0) Then
          Msg = Msg + " frags."
        Else
          Msg = Msg + " Fatalities."
        End If
        If P(Maxp%).CPU = 0 Then
          Playwav ("win")
        Else
          Playwav ("lose")
        End If
        EndIt (Msg)
        Exit Sub
      End If
    End If
    If Config.Respawn <> 0 Then
      For X4 = 1 To MaxPlayers
        If P(X4).CharID <> 0 And P(X4).HP <= 0 And P(X4).HP <> -666 And P(X4).HP <> -777 And (XTimer - P(X4).GotKilled >= 30 Or (P(X4).Rune = RuneRespawn And Timer - P(X4).GotKilled >= 15)) Then
          If Config.Flag <> 0 Then
            Send (P(X4).ScrNam + " is released from jail.")
          Else
            Send (Parse(DATASET.Respawn, P(X4).ScrNam, "", "", ""))
          End If
          Playwav ("r_tele" + TrimStr(Rand(1, 5)))
          clearStatus (X4)
          P(X4).HP = P(X4).MaxHP
          P(X4).CurMove = -255
        End If
      Next X4
    End If
  End If
  If (Battle) Then
    For X2 = 1 To MaxPlayers
      'If p(x2).MP < 0 Then p(x2).MP = 0
      If P(X2).Super > MaxSP Then P(X2).Super = MaxSP
      If P(X2).Super < 0 Then P(X2).Super = 0
      If Config.Flag <> 0 And (BlueFlag <> -1) And (RedFlag <> -1) Then
        ORed = RedFlag
        OBlue = BlueFlag
        If X2 = RedFlag And P(X2).HP <= 0 Then RedFlag = (P(X2).AttackedMe)
        If X2 = BlueFlag And P(X2).HP <= 0 Then BlueFlag = (P(X2).AttackedMe)
        If P(RedFlag).TeamID = "R" And P(ORed).TeamID = "B" Then
          Send ("<font color=""#FF0000"" face=""Wingdings"">OOP <font face=""" + Trim(StripHiAscii(Config.FontName)) + """> " + P(RedFlag).ScrNam + " REGAINS POSSESSION OF RED TEAM'S FLAG <font face=""Wingdings"">POO")
          'LogFileWrite (p(RedFlag).ScrNam + " regained possession of the Red Flag. The countdown was halted.")
          TLimit = 0
          Playwav ("unhurry")
        End If
        If P(BlueFlag).TeamID = "B" And P(OBlue).TeamID = "R" Then
          Send ("<font color=""#0000FF"" face=""Wingdings"">OOP <font face=""" + Trim(StripHiAscii(Config.FontName)) + """> " + P(BlueFlag).ScrNam + " REGAINS POSSESSION OF BLUE TEAM'S FLAG <font face=""Wingdings"">POO")
          'LogFileWrite (p(BlueFlag).ScrNam + " regained possession of the Blue Flag. The countdown was halted.")
          TLimit = 0
          Playwav ("unhurry")
        End If
        If P(RedFlag).TeamID = "B" And TLimit = 0 Then
          TLimit = Config.FlaCon + XTimer
          Send ("<font color=""#0000FF"" face=""Wingdings"">OOP <font face=""" + Trim(StripHiAscii(Config.FontName)) + """> " + P(RedFlag).ScrNam + " GAINS POSSESSION OF <font color=""#FF0000"">RED TEAM'S FLAG <font color=""#0000FF"" face=""Wingdings"">POO")
          Send ("--- " + TrimStr(Config.FlaCon) + "-SECOND COUNTDOWN BEGINS ---")
          'LogFileWrite (p(RedFlag).ScrNam + " stole the Red Flag. The " + TrimStr(Config.FlaCon) + "-second countdown began.")
          FoundRedFlag = True
          Playwav ("hurry")
        End If
        If P(BlueFlag).TeamID = "R" And TLimit = 0 Then
          TLimit = Config.FlaCon + XTimer
          Send ("<font color=""#FF0000"" face=""Wingdings"">OOP <font face=""" + Trim(StripHiAscii(Config.FontName)) + """> " + P(BlueFlag).ScrNam + " GAINS POSSESSION OF <font color=""#0000FF"">BLUE TEAM'S FLAG <font color=""#FF0000"" face=""Wingdings"">POO")
          Send ("--- " + TrimStr(Config.FlaCon) + "-SECOND COUNTDOWN BEGINS ---")
          'LogFileWrite (p(BlueFlag).ScrNam + " stole the Blue Flag. The " + TrimStr(Config.FlaCon) + "-second countdown began.")
          FoundBlueFlag = True
          Playwav ("hurry")
        End If
        If P(RedFlag).TeamID = "R" And P(BlueFlag).TeamID = "B" And TLimit <> 0 Then TLimit = 0
      End If
    Next X2
    If Config.Flag <> 0 Then
      If RedFlag < -1 Or RedFlag > MaxPlayers Then
        RedFlag = 0
        Send ("The Red Flag is Floating. (/rclaim)")
      End If
      If RedFlag > 0 And RedFlag <= MaxPlayers Then
        If P(RedFlag).HP <= 0 Or P(RedFlag).CharID = 0 Then
          RedFlag = 0
          Send ("The Red Flag is Floating. (/rclaim)")
        End If
      End If
      If BlueFlag < -1 Or BlueFlag > MaxPlayers Then
        BlueFlag = 0
        Send ("The Blue Flag is Floating. (/bclaim)")
      End If
      If BlueFlag > 0 And BlueFlag <= MaxPlayers Then
        If P(BlueFlag).HP <= 0 Or P(BlueFlag).CharID = 0 Then
          BlueFlag = 0
          Send ("The Blue Flag is Floating. (/bclaim)")
        End If
      End If
    End If
    For X = 1 To 255
      If TCaptain(X) = 0 Then
        For X2 = 1 To MaxPlayers
          If P(X2).TeamID = Chr$(X) And P(X2).CharID > 0 Then
            If cTeam.Checked Then Send (P(X2).ScrNam + " is now Captain of Team " + Chr$(X) + " (""" + tName(X) + """)")
            TCaptain(X) = X2
            Exit For
          End If
        Next X2
      End If
      If TCaptain(X) > 0 Then
        If P(TCaptain(X)).TeamID <> Chr$(X) Or P(TCaptain(X)).CharID = 0 Then
          TCaptain(X) = 0
          For X2 = 1 To MaxPlayers
            If P(X2).TeamID = Chr$(X) And P(X2).CharID > 0 Then
              If cTeam.Checked Then Send (P(X2).ScrNam + " is now Captain of Team " + Chr$(X) + " (""" + tName(X) + """)")
              TCaptain(X) = X2
              Exit For
            End If
          Next X2
          If TCaptain(X) = 0 Then
            'Send ("Team " + Chr$(X) + " (""" + TName(X) + """) no longer has a Team Captain.")
            tName(X) = ""
          End If
        End If
      End If
    Next X
  End If
  If (Battle) Then
    For X2 = 1 To MaxPlayers
      If (P(X2).CurMove < 1) Or (P(X2).CurMove > MaxMoves) And (P(X2).Status(sInvin) > 0) Then
        If P(X2).Status(sInvin) = X2 Then P(X2).Status(sInvin) = 0
      End If
      If P(X2).Status(sInvin) > 0 Then
        X6 = P(X2).Status(sInvin)
        If (X6 > 0) And (X6 <= MaxPlayers) Then
          If P(X6).CurMove >= 1 And P(X6).CurMove <= MaxMoves Then
            If P(X6).Moves(P(X6).CurMove).Element <> Invin Then P(X2).Status(sInvin) = 0
          Else
            P(X2).Status(sInvin) = 0
          End If
        Else
          P(X2).Status(sInvin) = 0
        End If
      End If
    Next X2
  End If
  If (Battle) And (Not Paused) Then
    If (Rand(1, GetRate * 0.8) = 1) And (GetRate > 0) Then
      NewItem
    End If
    For X2 = 1 To MaxPlayers
      If (P(X2).HP > 0) And (P(X2).CharID <> 0) Then
        'If (p(x2).MP < p(x2).MaxMP) Then p(x2).MP = p(x2).MP + Arena(CurArena).GradualMP
        If (P(X2).HP < P(X2).MaxHP) Then P(X2).HP = P(X2).HP + Arena(CurArena).GradualHP
        If P(X2).Cheese > 0 Then P(X2).Cheese = Int(P(X2).Cheese - (7 * DMult / 100))
        If P(X2).Cheese < 0 Then P(X2).Cheese = 0
        Select Case P(X2).Cheese:
          Case Is > 3000 * DMult / 100:
            If P(X2).Status(sSleep) = 0 Then
              Send (P(X2).ScrNam + " faints from exhaustion! (Cheese Meter too high!)")
              AddStatus X2, sSleep
            End If
          Case 2000 * DMult / 100 To 2999 * DMult / 100:
            If Rand(1, 20) = 15 Then
              Select Case Rand(1, 5)
                Case 1: Send (P(X2).ScrNam + " is tired.")
                Case 2: Send (P(X2).ScrNam + " is beginning to wear out.")
                Case 3: Send (P(X2).ScrNam + " lets out a big yawn.")
                Case 4: Send (P(X2).ScrNam + " says, ""Phew, I'm exhausted!""")
                Case Else: Send (P(X2).ScrNam + " is getting exhausted.")
              End Select
            End If
          Case 1000 * DMult / 100 To 1999 * DMult / 100:
            If Rand(1, 50) = 15 Then Send (P(X2).ScrNam + " is getting tired.")
        End Select
      End If
    Next X2
  End If
  If (Not Battle) And (Not Godmode) Then
    For X2 = 1 To MaxPlayers
      If P(X2).CharID > HighSenshi Then
        ClearMe (X2)
      End If
      'If p(x2).CharID = 0 Then clearStatus (x2)
    Next X2
  End If
  If (Battle) And (Not Paused) Then
    For X3 = 1 To 10
      X1 = Rand(0, 1000)
      If (X1 < Arena(CurArena).Happening(X3).Frequency) Then
        If (Arena(CurArena).Happening(X3).ElementStr = 0) Then
          Send (Arena(CurArena).Happening(X3).Miss)
          'LogFileWrite ("* " + Arena(CurArena).Happening(x3).Miss)
        Else
          X4 = False
          Do
            DoEvents
            X2 = Rand(1, MaxPlayers)
            X4 = (P(X2).HP > 0 And P(X2).CharID <> 0)
          Loop Until (X4)
          If Arena(CurArena).Happening(X3).HitsAll Then
            If (Rand(1, 3) <> 2) Then
              Send (Parse(Arena(CurArena).Happening(X3).Hit, "", "", "", ""))
              'LogFileWrite ("* " + Parse(Arena(CurArena).Happening(x3).Hit, "", "", "", ""))
              For X4 = 1 To MaxPlayers
                If (P(X4).HP > 0 And P(X4).CharID <> 0) Then
                  P(X4).HP = P(X4).HP - Arena(CurArena).Happening(X3).ElementStr
                  If P(X4).HP <= 50 Then ShowStatus X4
                End If
              Next X4
            Else
              Send (Parse(Arena(CurArena).Happening(X3).Miss, "", "", "", ""))
              'LogFileWrite ("* " + Parse(Arena(CurArena).Happening(x3).Miss, "", "", "", ""))
            End If
          Else
            'If x2 > MaxPlayers Then
            '  If (Rand(1, 3) <> 2) Or (Arena(CurArena).Happening(X3).ElementStr < 0) Then
            '    Send (Parse(Arena(CurArena).Happening(X3).Hit, "", "", y(x2 - MaxPlayers).Name, ""))
            '    'LogFileWrite (Parse(Arena(CurArena).Happening(x3).Hit, "", "", y(x2 - MaxPlayers).Name, ""))
            '    y(x2 - MaxPlayers).HP = y(x2 - MaxPlayers).HP - Arena(CurArena).Happening(X3).ElementStr
            '    ShowStatus x2
            '  Else
            '    Send (Parse(Arena(CurArena).Happening(X3).Miss, "", "", y(x2 - MaxPlayers).Name, ""))
            '  End If
            'Else
              If (Rand(1, 3) <> 2) Then
                Send (Parse(Arena(CurArena).Happening(X3).Hit, "", "", P(X2).ScrNam, ""))
                'LogFileWrite (Parse(Arena(CurArena).Happening(x3).Hit, "", "", p(x2).ScrNam, ""))
                P(X2).HP = P(X2).HP - Arena(CurArena).Happening(X3).ElementStr
                ShowStatus X2
              Else
                Send (Parse(Arena(CurArena).Happening(X3).Miss, "", "", P(X2).ScrNam, ""))
              End If
            'End If
          End If
        End If
        Exit For
      End If
    Next X3
  End If
  'UpdateYoumaStat
  UpdateGameStat
  LastTmr = Timer
End Sub

Private Sub tiMoveDmg_Timer()
Dim X2 As Integer, X3 As Integer, y2 As Integer, Okay As Integer
Dim X4 As Integer, x7 As Integer, X6%, AlreadyShown%
Dim MPR%
Dim M As MoveType
Dim Msg As String
Dim Tix As Long
  On Error Resume Next
  If (Battle) And (Not Paused) Then
    For X2 = 1 To MaxPlayers
      If (P(X2).HP > 0) Then
        Check2RemoveStatus (X2)
        If P(X2).CurMove = 0 And (P(X2).Status(sBerserk) <> 0 Or P(X2).Status(sChaos) <> 0 Or P(X2).Status(sPMS) <> 0) Then P(X2).CurMove = -255
        If (P(X2).CurMove = -255) And (P(X2).Status(sFreeze)) Then
          Send (P(X2).ScrNam + " is frozen.")
          P(X2).CurMove = -50
          P(X2).MoveStart = XTimer
        End If
        If (P(X2).CurMove = -255) And (P(X2).Status(sSleep)) Then
          Send (P(X2).ScrNam + " is taking a nap.")
          P(X2).CurMove = -50
          P(X2).MoveStart = XTimer
        End If
        If (P(X2).CurMove = -255) And (P(X2).Status(sStun)) Then
          Send (P(X2).ScrNam + " is unable to move.")
          P(X2).CurMove = -50
          P(X2).MoveStart = XTimer
        End If
        If P(X2).CurMove = -255 And P(X2).Status(sStop) Then
          Send (P(X2).ScrNam + " is frozen in time.")
          P(X2).CurMove = -50
          P(X2).MoveStart = XTimer
        End If
        If (P(X2).CurMove = -255) Then
          If (P(X2).Status(sChaos) <> 0) Then
            Do
              DoEvents
              P(X2).CurMove = Rand(1, MaxMoves)
            Loop Until (P(X2).Moves(P(X2).CurMove).Cmdkey <> "") And (P(X2).Moves(P(X2).CurMove).Element <> Morph)
            M = P(X2).Moves(P(X2).CurMove)
            'p(x2).MP = p(x2).MP + M.MPReq
            Okay = False
            Do
              DoEvents
              X4 = Rand(1, MaxPlayers)
              If (P(X4).HP > 0) And (P(X4).CharID <> 0) Then Okay = True
            Loop Until Okay
            P(X2).Target = X4
            Call DOMOVE(X2, P(X2).Target, P(X2).CurMove, "")
          ElseIf (P(X2).Status(sBerserk) <> 0) Or (P(X2).Status(sPMS) <> 0) Then
            P(X2).SuperNum = 0
            If P(X2).Status(sPMS) <> 0 Then DispReadyLine (X2)
            X6 = 0
            Do
              DoEvents
              P(X2).CurMove = Rand(1, MaxMoves)
              X6 = X6 + 1
            Loop Until (P(X2).Moves(P(X2).CurMove).Cmdkey <> "" And P(X2).Moves(P(X2).CurMove).Target = Enemy) Or (X6 > 150) And (P(X2).Moves(P(X2).CurMove).Element <> Morph)
            If X6 > 150 Then
              Call RemoveStatus(X2, sBerserk)
            Else
NewMPS:
              P(X2).Target = GetTarget(P(X2).TeamID)
              If P(P(X2).Target).Rune = RuneMagic And P(X2).Rune = RunePMS Then
                If AlreadyShown = 0 Then
                  Send (P(P(X2).Target).ScrNam + "'s Rune of Magic wards off " + P(X2).ScrNam + "'s PMS attacks.")
                  AlreadyShown = 1
                End If
                GoTo NewMPS
              End If
              Call DOMOVE(X2, P(X2).Target, P(X2).CurMove, "")
            End If
          ElseIf P(X2).Status(sMushroom) <> 0 Then
            y2 = Rand(10, 50)
            If P(X2).HP + y2 > P(X2).MaxHP Then y2 = P(X2).MaxHP - P(X2).HP
            If y2 > 0 Then
              Send (P(X2).ScrNam + " gets " + TrimStr(y2) + " HP from being a mushroom.")
              'LogFileWrite (p(x2).ScrNam + " regenerated " + TrimStr(y2) + " HP from being a mushroom.")
              P(X2).HP = P(X2).HP + y2
            Else
              Send (P(X2).ScrNam + " is still a mushroom.")
            End If
            P(X2).CurMove = -50
            P(X2).MoveStart = XTimer
          ElseIf P(X2).Status(sMIA) <> 0 Then
            Send (P(X2).ScrNam + " is missing...")
            P(X2).CurMove = -50
            P(X2).MoveStart = XTimer
          Else
            DispReadyLine (X2)
            P(X2).Target = 0
            P(X2).CurMove = 0
            P(X2).SuperNum = 0
            Send5 (Msg)
            'LogFileWrite (Msg)
            'PlayWav ("turn")
          End If
        End If
        Tix = (XTimer - TimeLimit)
        If (P(X2).CurMove <> 0) And (P(X2).CharID <> 0) And (P(X2).HP > 0) Then
          x7 = (MoveHitsIn(X2) <= 0)
          If P(X2).Status(sStop) Or P(X2).Status(sMIA) Then x7 = 0
          'If p(x2).Rune = RuneCharge And p(x2).Charging And x7 Then
          '  If p(x2).MoveStart <> -6660 Then
          '    x7 = False
          '    If (XTimer - p(x2).RuneTemp) > 60 Then
          '      Send (p(x2).ScrNam + " can't hold it in any longer!")
          '      x7 = True
          '    End If
          '  Else
          '    x7 = True
          '  End If
          'End If
          If P(X2).CurMove > 0 And P(X2).CurMove <= MaxMoves Then
            M = P(X2).Moves(P(X2).CurMove)
            If (M.DestroyWeapon = 3 Or (P(X2).WpnUsesLeft = 1 And M.DestroyWeapon = 1)) And M.InstantHit Then x7 = True
          End If
          If Not (P(X2).Status(sStop) Or P(X2).Status(sMIA) Or P(X2).Status(sFreeze)) Then Call Check2MoveDmg(X2, x7)
        End If
      End If
    Next X2
  End If
End Sub

Private Sub tLag_Change()
Dim P1$, P2$, P4$, PO%
  If (Val(tLag) > hsLag.Min) And (Val(tLag) <= hsLag.Max) Then
    hsLag = Val(tLag)
  End If
End Sub

Private Sub tLag_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "You can click me to change the lagometer."
End Sub

