VERSION 2.00
Begin Form fBattle 
   BackColor       =   &H0050017E&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Phude Battle?"
   ClientHeight    =   2775
   ClientLeft      =   4620
   ClientTop       =   3645
   ClientWidth     =   4395
   ForeColor       =   &H00FFFFFF&
   Height          =   3180
   Icon            =   FBATTLE.FRX:0000
   Left            =   4560
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   2775
   ScaleWidth      =   4395
   Top             =   3300
   Width           =   4515
   Begin CommandButton cGodMode 
      Caption         =   "GOD"
      Height          =   255
      Left            =   600
      MousePointer    =   12  'No Drop
      TabIndex        =   2
      Top             =   1920
      Width           =   615
   End
   Begin CommandButton cTwit 
      Caption         =   "Twit"
      Height          =   255
      Left            =   0
      TabIndex        =   41
      Top             =   1920
      Width           =   615
   End
   Begin CheckBox cInfo 
      BackColor       =   &H0050017E&
      Caption         =   "/help"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2280
      TabIndex        =   40
      Top             =   720
      Value           =   1  'Checked
      Width           =   615
   End
   Begin Timer tiFullAuto 
      Interval        =   1
      Left            =   2520
      Top             =   3240
   End
   Begin CommandButton cIMOff 
      Caption         =   "OFF"
      Height          =   255
      Left            =   3600
      TabIndex        =   0
      Top             =   1680
      Width           =   615
   End
   Begin CommandButton cIMOn 
      Caption         =   "ON"
      Height          =   255
      Left            =   3000
      TabIndex        =   1
      Top             =   1680
      Width           =   615
   End
   Begin CommandButton cPaused 
      Caption         =   "It's Paused"
      Height          =   255
      Left            =   3000
      TabIndex        =   11
      Top             =   1440
      Width           =   1215
   End
   Begin CommandButton Command5 
      Caption         =   "Slow Down"
      Height          =   255
      Left            =   3000
      TabIndex        =   22
      Top             =   1200
      Width           =   1215
   End
   Begin CommandButton Command4 
      Caption         =   "No Scrolling"
      Height          =   255
      Left            =   3000
      TabIndex        =   21
      Top             =   960
      Width           =   1215
   End
   Begin CommandButton cMaxP 
      Caption         =   "More Peepz"
      Height          =   255
      Left            =   3000
      TabIndex        =   25
      Top             =   720
      Width           =   1215
   End
   Begin TextBox tLag 
      Alignment       =   2  'Center
      BackColor       =   &H0050017E&
      BorderStyle     =   0  'None
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   3720
      TabIndex        =   32
      Text            =   "1000"
      Top             =   1920
      Width           =   495
   End
   Begin HScrollBar hsLag 
      Height          =   255
      LargeChange     =   100
      Left            =   1200
      Max             =   10000
      Min             =   100
      SmallChange     =   10
      TabIndex        =   29
      Top             =   1920
      Value           =   1000
      Width           =   2415
   End
   Begin CommandButton cMystery 
      Caption         =   "????"
      Height          =   255
      Left            =   600
      TabIndex        =   14
      Top             =   1680
      Width           =   615
   End
   Begin CommandButton mShowTeams 
      Caption         =   "&Teams"
      Height          =   255
      Left            =   0
      TabIndex        =   10
      Top             =   1680
      Width           =   615
   End
   Begin CommandButton mShowChars 
      Caption         =   "&Chars"
      Height          =   255
      Left            =   600
      TabIndex        =   9
      Top             =   1440
      Width           =   615
   End
   Begin CommandButton mShowMoves 
      Caption         =   "&Moves"
      Height          =   255
      Left            =   0
      TabIndex        =   8
      Top             =   1440
      Width           =   615
   End
   Begin CommandButton cStat2 
      Caption         =   "&NStat"
      Height          =   255
      Left            =   600
      TabIndex        =   39
      Top             =   1200
      Width           =   615
   End
   Begin CommandButton cbStats 
      Caption         =   "Stats"
      Height          =   255
      Left            =   0
      TabIndex        =   3
      Top             =   1200
      Width           =   615
   End
   Begin CommandButton mPauseBattle 
      Caption         =   "&Pause"
      Enabled         =   0   'False
      Height          =   255
      Left            =   600
      TabIndex        =   7
      Top             =   960
      Width           =   615
   End
   Begin CommandButton cbMidi 
      Caption         =   "Midi"
      Height          =   255
      Left            =   0
      TabIndex        =   35
      Top             =   960
      Width           =   615
   End
   Begin CommandButton mBeginBattle 
      Caption         =   "&Begin"
      Height          =   255
      Left            =   600
      TabIndex        =   6
      Top             =   720
      Width           =   615
   End
   Begin CommandButton mExitBattle 
      Caption         =   "E&xit"
      Height          =   255
      Left            =   0
      TabIndex        =   5
      Top             =   720
      Width           =   615
   End
   Begin HScrollBar hsScroll 
      Height          =   240
      LargeChange     =   100
      Left            =   1320
      Max             =   4000
      Min             =   1
      SmallChange     =   10
      TabIndex        =   15
      Top             =   1200
      Value           =   1
      Width           =   1575
   End
   Begin TextBox tChatLine 
      FontBold        =   -1  'True
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   315
      Left            =   120
      TabIndex        =   20
      Top             =   2160
      Width           =   4095
   End
   Begin CheckBox cVersion 
      BackColor       =   &H0050017E&
      Caption         =   "/version"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1320
      TabIndex        =   4
      Top             =   720
      Value           =   1  'Checked
      Width           =   855
   End
   Begin Timer CTimer 
      Interval        =   1000
      Left            =   600
      Top             =   3240
   End
   Begin OptionButton oLin 
      BackColor       =   &H00000000&
      Caption         =   "4 Lines"
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   4
      Left            =   3000
      TabIndex        =   19
      Top             =   2880
      Width           =   855
   End
   Begin OptionButton oLin 
      BackColor       =   &H00000000&
      Caption         =   "3 Lines"
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   3
      Left            =   2040
      TabIndex        =   18
      Top             =   2880
      Width           =   975
   End
   Begin OptionButton oLin 
      BackColor       =   &H00000000&
      Caption         =   "2 Lines"
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   2
      Left            =   1080
      TabIndex        =   17
      Top             =   2880
      Width           =   975
   End
   Begin OptionButton oLin 
      BackColor       =   &H00000000&
      Caption         =   "1 Line"
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   1
      Left            =   240
      TabIndex        =   16
      Top             =   2880
      Width           =   855
   End
   Begin Timer tiMoveDmg 
      Interval        =   100
      Left            =   2040
      Top             =   3240
   End
   Begin Timer tiCPU 
      Interval        =   750
      Left            =   1560
      Top             =   3240
   End
   Begin Timer tiMisc 
      Interval        =   2000
      Left            =   1080
      Top             =   3240
   End
   Begin CommandButton bSend 
      Caption         =   "Send"
      Default         =   -1  'True
      Height          =   375
      Left            =   3600
      TabIndex        =   23
      Top             =   3240
      Width           =   615
   End
   Begin Timer tiDoMove 
      Interval        =   100
      Left            =   120
      Top             =   3240
   End
   Begin CheckBox cMoves 
      BackColor       =   &H0050017E&
      Caption         =   "/moves"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1680
      TabIndex        =   13
      Top             =   480
      Value           =   1  'Checked
      Width           =   855
   End
   Begin CheckBox cCPU 
      BackColor       =   &H0050017E&
      Caption         =   "/cpu"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2520
      TabIndex        =   24
      Top             =   480
      Width           =   615
   End
   Begin CheckBox cAttacking 
      BackColor       =   &H0050017E&
      Caption         =   "/attacking"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3240
      TabIndex        =   26
      Top             =   480
      Value           =   1  'Checked
      Width           =   975
   End
   Begin CheckBox cStatus 
      BackColor       =   &H0050017E&
      Caption         =   "/status"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   840
      TabIndex        =   12
      Top             =   480
      Value           =   1  'Checked
      Width           =   735
   End
   Begin CheckBox cJoin 
      BackColor       =   &H0050017E&
      Caption         =   "Joining"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   27
      Top             =   480
      Value           =   1  'Checked
      Width           =   735
   End
   Begin CheckBox cTeam 
      BackColor       =   &H0050017E&
      Caption         =   "Team Functions"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1320
      TabIndex        =   28
      Top             =   960
      Value           =   1  'Checked
      Width           =   1575
   End
   Begin Label Label3 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Turn IM's"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2160
      TabIndex        =   30
      Top             =   1680
      Width           =   735
   End
   Begin Label LFour 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "3"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2040
      TabIndex        =   31
      Top             =   0
      Width           =   255
   End
   Begin Label LLag 
      Alignment       =   1  'Right Justify
      BackColor       =   &H0050017E&
      Caption         =   "Speed"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   600
      TabIndex        =   33
      Top             =   1920
      Width           =   495
   End
   Begin Label StatLine 
      Alignment       =   2  'Center
      BackColor       =   &H00C0C0C0&
      Caption         =   "???"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial Narrow"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   0
      TabIndex        =   34
      Top             =   2520
      Width           =   4395
   End
   Begin Line Line1 
      X1              =   0
      X2              =   4440
      Y1              =   360
      Y2              =   360
   End
   Begin Label LItem 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "darnedifiknow!"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2160
      TabIndex        =   36
      Top             =   0
      Width           =   2055
   End
   Begin Label Label4 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "neverneverland"
      FontBold        =   0   'False
      FontItalic      =   -1  'True
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   37
      Top             =   0
      Width           =   2175
   End
   Begin Label Label2 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Scroll Delay: ????"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1320
      TabIndex        =   38
      Top             =   1440
      Width           =   1575
   End
End

Option Explicit

Const GoodStatus = 60
Const BadStatus = 20

Const HostageLength = 30

Dim Tm As Long

Const MyName = "fBattle"

Sub bSend_Click ()
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

Sub cAttacking_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Enables or disables the use of /attacking"
End Sub

Sub cbMidi_Click ()
  fMidi.Show
End Sub

Sub cbMidi_MouseDown (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If (Button = 2) Then fWav.Show
End Sub

Sub cbMidi_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Play some music!"
End Sub

Sub cbStats_Click ()
  fStats.Show
End Sub

Sub cbStats_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Show game statistics"
End Sub

Sub cCPU_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Enables or disables the use of /cpu"
End Sub

Sub cGodMode_GotFocus ()
  cGodMode.Visible = False
End Sub

Sub cGodMode_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  cGodMode_GotFocus
End Sub

Sub cIMOff_Click ()
  IMControl ("OFF")
End Sub

Sub cIMOff_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Turn IM's Off"
End Sub

Sub cIMOn_Click ()
  IMControl ("ON")
End Sub

Sub cIMOn_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Turn IM's On"
End Sub

Sub cInfo_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Enables or disables the use of /info"
End Sub

Sub cJoin_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Enables or disables joining"
End Sub

Sub cMaxP_Click ()
Dim X As Integer
Dim y2 As Integer
  X = Val(kDlgBoxInput("Enter the number of players to increase MaxPlayers by:", "MaxPlayer Increase", "1"))
  If (X > 0) And (MaxPlayers + X < fPreBattl!vsMaxPlayers.Max) Then
    'For y2 = 1 To MaxPlayers
    '  If p(y2).Target >= MaxPlayers Then p(y2).Target = p(y2).Target + X
    'Next y2
    'For y2 = 1 To Maxyouma
    '  If y(y2).Target > MaxPlayers Then y(y2).Target = y(y2).Target + X
    'Next y2
    y2 = MaxPlayers
    MaxPlayers = MaxPlayers + X
    ReDim Preserve p(MaxPlayers)
    ReDim Preserve vote(MaxPlayers)
    Send ("MaxPlayers increased by " + TrimStr(X) + ".")
    For X = y2 To MaxPlayers
      p(X).MaxHP = MaxHP
    Next X
    fPreBattl!vsMaxPlayers = MaxPlayers
  End If
End Sub

Sub cMaxP_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Allow more people to play."
End Sub

Sub cMoves_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Disables the /moves command."
End Sub

Sub cMystery_Click ()
Dim X As Integer
Dim Z As Integer
Dim x1%, x2%, x6%, x7%
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
      x1% = Val(kDlgBoxInput("Enter a number.", "It Does Math, Too!", "2"))
      If x1% = 0 Then x1% = 2
      x2% = Val(kDlgBoxInput("Enter another number.", "It Does Math, Too!", "2"))
      If x2% = 0 Then x2% = 2
      Send ("Mystery Button do math. " + TrimStr(x1) + "+" + TrimStr(x2) + "=" + TrimStr(Int((x1 + x2 + 1) * 3 / 2)) + ". Huh huh. Mystery Button gonna graduate.")
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
	x6 = 0
	x6 = kDlgBoxfn("", Rand(32, 37), "")
	x6 = kDlgBoxfn("", Rand(32, 37), "")
	x6 = kDlgBoxfn("", Rand(32, 37), "")
	x6 = kDlgBoxfn("", Rand(32, 37), "")
	x6 = kDlgBoxfn("", Rand(32, 37), "")
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

Sub cMystery_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Property of Team Rocket. Do not touch!"
End Sub

Sub Command4_Click ()
  Send ("<font color=""#FF0000""><b><i><u>>>>---!!!~~~ NO SCROLLING ~~~!!!---<<<")
End Sub

Sub Command4_MouseDown (Button As Integer, Shift As Integer, X As Single, Y As Single)
Dim S$
  If (Button = 2) Then
    S$ = kDlgBoxInput("No what?", "No...?", "scrolling")
    S$ = UCase(S$)
    Send ("<font color=""#FF0000""><b><i><u>>>>---!!!~~~ NO " + S$ + " ~~~!!!---<<<")
  End If
End Sub

Sub Command4_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "I'll tell them to stop scrolling!"
End Sub

Sub Command5_Click ()
  If (Command5.Caption = "No Guts") Then
    Send ("<font color=""#FF0000""><b><i><u>No guts, no glory.")
  Else
    Send ("<font color=""#FF0000""><b><i><u>@@@### SLOW DOWN ###@@@")
  End If
End Sub

Sub Command5_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If (Command5.Caption = "No Guts") Then
    StatLine = "No guts, no glory."
  Else
    StatLine = "I'll tell them to slow down!!!"
  End If
End Sub

Sub cPaused_Click ()
  Send ("<font color=""#FF0000""><b><i><u>!!!@@@ IT'S PAUSED @@@!!!")
End Sub

Sub cPaused_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Reminds the people that the BATTLE IS PAUSED YOU MORONS!!!"
End Sub

Sub cStat2_Click ()
  fNeoStat.Show
End Sub

Sub cStat2_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shows the cool new Status Window"
End Sub

Sub cStatus_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Ignores the /status command."
End Sub

Sub cTeam_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Enables or disables the use of team commands"
End Sub

Sub CTimer_Timer ()
Dim x2 As Integer
  If (Battle And Not Paused) Then
    XTimer = XTimer + 1
    'For x2 = 1 To MaxPlayers
    '  If p(x2).Status(sStop) <> 0 Or p(x2).Status(sMIA) <> 0 Then
    '    p(x2).MoveStart = p(x2).MoveStart + 1
    '  End If
    'Next x2
  End If
End Sub

Sub cTwit_Click ()
  fTwit.Show
End Sub

Sub cVersion_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Enables or disables the use of /version"
End Sub

Function DetermineTeam (X As Integer) As String
Dim A As String
  'If FFABattle >= bFFA Then
    If (X <= 10) Then
      A = Chr$(47 + X)
    Else
      A = Chr$(65 + X - 10)
    End If
  'Else
  '  A = "S"
  'End If
  DetermineTeam = A
End Function

Sub EndBattle ()
Dim ok As Integer
Dim X%
  If (Not Auto) Then ok = (kDlgBoxfn("Really end the battle?", 36, "End Battle") = 6)
  If (Auto Or ok) Then
    Playwav ("lose")
    EndIt Dataset.GameAborted
    For X = 1 To 255
      TCaptain(X) = 0
      TName(X) = ""
    Next X
    For X = 1 To MaxPlayers
      If p(X).CPU <> 0 Then p(X).Rune = 0
    Next X
  End If
End Sub

Sub EndIt (ByVal Reason$)
Dim x4%
  Unload fMidi
  Battle = False
  Paused = False
  mBeginBattle.Enabled = True
  mBeginBattle.Caption = "&Begin"
  mPauseBattle.Enabled = False
  Send (Dataset.BattleEnd)
  Send (Reason$)
  DispFrags
  For x4 = 1 To MaxPlayers
    If p(x4).Status(sMorph) Then
      p(x4).CharID = p(x4).OldCharID
    End If
  Next x4
End Sub

Sub FlagTeams ()
Dim X%, x2%
Dim Td As String
  For X = 1 To MaxPlayers
    p(X).TeamID = ""
  Next X
  For X = 1 To Int(MaxPlayers / 2)
    Do
      x2 = Rand(1, MaxPlayers)
    Loop Until p(x2).TeamID = ""
    p(x2).TeamID = "R"
  Next X
  For X = 1 To MaxPlayers - Int(MaxPlayers / 2)
    Do
      x2 = Rand(1, MaxPlayers)
    Loop Until p(x2).TeamID = ""
    p(x2).TeamID = "B"
  Next X
  If (NumPlaying() < 4) Then Exit Sub
  TName(Asc("B")) = "<font color=""#0000FF"">Blue Team</font>"
  TName(Asc("R")) = "<font color=""#FF0000"">Red Team</font>"
  cTeam = 0
  cJoin = 0
  TLimit = 0
  'y(1).Enabled = False
  'y(2).Enabled = False
  'y(3).Enabled = False
  'y(4).Enabled = False
  'y(5).Enabled = False
  Do
    RedFlag = Rand(1, MaxPlayers)
    DoEvents
  Loop Until (p(RedFlag).CharID) And p(RedFlag).TeamID = "R"
  FoundRedFlag = 0
  Do
    BlueFlag = Rand(1, MaxPlayers)
    DoEvents
  Loop Until (p(BlueFlag).CharID) And p(BlueFlag).TeamID = "B"
  FoundBlueFlag = 0
End Sub

Sub Form_Load ()
Dim X As Integer
Dim S As String
Dim P2$, P4$, PO%
  LoadPosition Me, MyName
  Unload fMidi
  Me.Enabled = True
  Me.Caption = Dataset.LoadStr + " Battle"
  SetWindowPos fBattle.hWnd, -1, 0, 0, 0, 0, 3
  hsScroll.Value = fIntro!tiOutput.Interval
  Label2.Caption = "Scroll Delay: " + TrimStr(hsScroll.Value)
  oLin(Scra).Value = True
  P2$ = "Lag-O-Meter"
  P4$ = String$(6, 32)
  PO% = GetPrivateProfileString("Millenium", P2$, "2000", P4$, 81, "ChUB2000.ini")
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

Sub Form_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = Dataset.LoadStr + " Battle"
End Sub

Sub Form_QueryUnload (Cancel As Integer, UnloadMode As Integer)
  If Battle Then
    EndIt Dataset.GameAborted
    mBeginBattle.Caption = "&Begin"
  End If
  If (Not Battle) Then
    Cancel = False
  Else
    Cancel = True
  End If
End Sub

Sub Form_Unload (Cancel As Integer)
  SavePosition Me, MyName
  'Unload fyControl
  Unload fNeoStat
  Unload fMidi
  Unload fStats
  GameStage = 0
  Me.Enabled = False
  fPreBattl!tiPreBatt.Enabled = True
  fPreBattl!hsScroll.Value = Me!hsScroll.Value
  fPreBattl!oLin(Scra).Enabled = True
  'If kDlgBox("Clear characters?", 36, "Exit Battle") = 6 Then
  If FullAuto.Enabled Then
    ClearChars
    ScrollSend Dataset.ClearChars
  Else
    ScrollSend Dataset.CharsNotCleared
  End If
  ScrollSend (Dataset.BeginSelect)
  fPreBattl.Show
  Playwav ("interlud")
End Sub

Sub hsLag_Change ()
Dim P2$, P4$, PO%
  CTimer.Interval = hsLag
  tiMisc.Interval = hsLag
  tLag.Text = TrimStr(hsLag)
  P2$ = "Lag-O-Meter"
  P4$ = TrimStr(hsLag.Value)
  PO% = WritePrivateProfileString("Millenium", P2$, P4$, "ChUB2000.ini")
End Sub

Sub hsScroll_Change ()
  fIntro!tiOutput.Interval = hsScroll
  Label2.Caption = "Scroll Delay: " + TrimStr(hsScroll.Value)
End Sub

Sub hsScroll_Scroll ()
  hsScroll_Change
End Sub

Sub Label2_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Changes the scroll delay"
End Sub

Sub Label4_Click ()
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

Sub Label4_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "How much time do we have left?..."
End Sub

Sub LItem_MouseDown (Button As Integer, Shift As Integer, X As Single, Y As Single)
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

Sub LItem_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Click here to be evil and drop/destroy an item."
End Sub

Sub mBeginBattle_Click ()
Dim S4 As String
Dim X As Integer, T%
Dim ok As Integer
Dim Msg$, P1$, P2$, P4$, PO%
 If mBeginBattle.Caption = "&Begin" Then
   S4 = ""
   If Config.Flag <> 0 Then FlagTeams
   For X = 1 To MaxPlayers
     If (p(X).CharID <> 0) Then
       If (S4 = "") Then
	 S4 = p(X).TeamID
       End If
       If (p(X).TeamID <> S4) Then
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
       p(X).Ready = 0
       p(X).AttackedMe = 0
       p(X).SuperNum = 0
       'p(x).InactiveTime = Timer
       If p(X).TeamID <> "" Then
	 If TCaptain(Asc(p(X).TeamID)) = 0 Then TCaptain(Asc(p(X).TeamID)) = X
       End If
       p(X).WpnUsesLeft = Weapons(p(X).Weapon).NumUses
       If Config.OldSchool Then
	 p(X).Rune = 0
	 p(X).Weapon = 0
       End If
     Next X
     GameNum = GameNum + 1
     P1$ = "Millenium"
     P2$ = "GameNum"
     P4$ = TrimStr(GameNum)
     PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
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
     ScrollSend ("Current Dataset: " + Dataset.LoadStr)
     If FragLimit = 0 And Config.Respawn <> 0 And Config.Flag = 0 Then ScrollSend ("<font color=#FF0000>WARNING: Frag Limit not set for Respawn FFA! Game will continue infinitely")
     Delay (.5)
     Item2Get = 0
     TKills = 0
     TFatal = 0
     THP = 0
     TMP = 0
     TSP = 0
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

Sub mBeginBattle_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If mBeginBattle.Caption = "&Begin Battle" Then
    StatLine = "Start your engines!"
  Else
    StatLine = "Abort Game"
  End If
End Sub

Sub mExitBattle_Click ()
  Unload Me
End Sub

Sub mExitBattle_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Leaving already?"
End Sub

Sub mPauseBattle_Click ()
Dim X%
  If (Not Paused) Then
    Paused = True
    mPauseBattle.Caption = "Un&P"
    Send (Dataset.BattlePause)
    Playwav ("down")
    mShowChars.Enabled = True
    mShowTeams.Enabled = True
  ElseIf (AOLCheck() = 333 Or AOLCheck() = 444 Or YourSN = "Player") Then
    mPauseBattle.Caption = "&Pause"
    mShowChars.Enabled = False
    mShowTeams.Enabled = False
    'For X = 1 To MaxPlayers
    '  p(X).InactiveTime = Timer
    '  p(X).InactiveWarn = 0
    'Next X
    'Stop
    If (TimeLimit = XTimer) Then
      Send (Dataset.BattleBegin)
      Playwav ("beat2")
    Else
      Playwav ("up")
      Send (Dataset.BattleUnPause)
    End If
    Paused = False
  Else
    kDlgBox "Not logged on -- please log back on before resuming!", 16, "Not Logged On"
  End If
  tChatLine.SetFocus
End Sub

Sub mPauseBattle_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Pause or Unpause"
End Sub

Sub mShowChars_Click ()
  If (Paused) Then ScrollChars
End Sub

Sub mShowChars_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shows list of characters playing."
End Sub

Sub mShowMoves_Click ()
  fMoves.Show
End Sub

Sub mShowMoves_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shows moves"
End Sub

Sub mShowTeams_Click ()
  If (Paused) Then ListByTeams
End Sub

Sub mShowTeams_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shows Teams"
End Sub

Sub NewItem ()
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

Function NotHostage% (X As Integer)
Dim x2 As Integer
  'For x2 = 1 To Maxyouma
  '  If y(x2).Hostage = X Then
  '    NotHostage = False
  '    Exit Function
  '  End If
  'Next x2
  NotHostage = True
End Function

Sub oLin_Click (Index As Integer)
  Scra = Index
End Sub

Sub SelectJoin (ByVal X%, ByVal X3%, ByVal SN$)
Dim x2 As Integer
Dim t4 As Integer, P1$, P2$, P4$, PO%
  clearStatus (X)
  P1$ = SN
  P2$ = "Wins"
  P4$ = String$(6, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
  p(X).Wins = Val(P4$)
  P1$ = SN
  P2$ = "Losses"
  P4$ = String$(6, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
  p(X).Losses = Val(P4$)
  p(X).CPU = 0
  p(X).ScrNam = SN
  p(X).MaxHP = MaxHP
  p(X).HP = p(X).MaxHP
  p(X).CharID = X3
  p(X).TeamID = DetermineTeam(X)
  p(X).PhysStr = Senshi(X3).PhysStr
  p(X).MagStr = Senshi(X3).MagStr
  p(X).PhysDef = Senshi(X3).PhysDef
  p(X).MagDef = Senshi(X3).MagDef
  For t4 = 1 To MaxMoves - 1
    p(X).Moves(t4) = Moves(Senshi(X3).Moves(t4))
  Next t4
  Send (Parse(Senshi(X3).SelectJoin + " (" + TrimStr(Senshi(X3).Wins) + " kills, " + TrimStr(Senshi(X3).Losses) + " deaths)", SN + " (" + TrimStr(p(X).Wins) + " kills, " + TrimStr(p(X).Losses) + " deaths)", "", "", ""))
End Sub

Sub SneakAttack ()
Dim X%, x6%
  If (Rand(1, 20) = 1) Then
    Send ("~~~*) SNEAK ATTACK (*~~~")
Start:
    Do
      X = Rand(1, MaxPlayers + Maxyouma)
    Loop Until Active(X) <> 0
    Do
      p(X).CurMove = Rand(1, MaxMoves)
      x6 = x6 + 1
    Loop Until (p(X).Moves(p(X).CurMove).Cmdkey <> "") Or (x6 > 150) And (p(X).Moves(p(X).CurMove).Element <> Morph)
    If x6 > 150 Then
      GoTo Start
    Else
     x6 = p(X).Moves(p(X).CurMove).Target
     If (x6 = AllFriend) Or (x6 = OnlySelf) Or (x6 = Friend) Then
	p(X).Target = X
      Else
	p(X).Target = GetTarget(p(X).TeamID)
      End If
      p(X).MoveStart = -999
    End If
    Check2MoveDmg X, -1
  End If
End Sub

Sub StatLine_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "HEY! GET THAT MOUSE OFFA ME! NOW!!!"
End Sub

Sub tChatLine_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Say stuff here and press Enter to send it!"
End Sub

Sub tiCPU_Timer ()
' CPU move
Dim x1, x2, Okay, T As Integer
Dim SN, Tgt, Yma As String
Dim M As MoveType
  On Error Resume Next
  If (Paused) Then
    For x2 = 1 To MaxPlayers
      If (p(x2).CPU) And (p(x2).CharID <> 0) And (p(x2).HP > 0) And p(x2).Rune = 0 And Config.OldSchool = 0 Then
	Do
	  p(x2).Rune = Rand(1, MaxRune)
	Loop Until DesirableCPURune(p(x2).Rune)
	Send (p(x2).ScrNam + " picks the " + RuneName(p(x2).Rune) + ".")
      End If
    Next x2
  End If
  If (Battle) And (Not Paused) Then
    For x2 = 1 To MaxPlayers
      DoEvents
      If (p(x2).CPU) And (p(x2).CharID <> 0) And (p(x2).HP > 0) Then
	'p(x2).InactiveTime = Timer
	If p(x2).Rune = 0 And Config.OldSchool = 0 Then
	  Do
	    p(x2).Rune = Rand(1, MaxRune)
	  Loop Until DesirableCPURune(p(x2).Rune)
	  Send (p(x2).ScrNam + " picks the " + RuneName(p(x2).Rune) + ".")
	End If
	If (p(x2).CurMove = 0) And (Rand(2, 2) = 2) And (p(x2).Status(sStop) = 0) And (p(x2).Status(sStun) = 0) And (p(x2).Status(sSleep) = 0) Then
	  Call MoveToDo(x2)
	  If (Mx <> 0) Then
	    Call DoMove(x2, Tx, Mx, Stx)
	  End If
	End If
      End If
    Next x2
  End If
End Sub

Sub tiDoMove_Timer ()
Dim X%, x2%, X3%, x4%, X5%, x6%, x7%
Dim JoinOK%, Okay%
Dim SN$, Ct$, Chat$, s2$, S3$, S4$, S5$, S6$, S7$
Dim Msg$, Tid$
Dim M As MoveType
Dim S As CharType
Dim T$, Pin%, Keycode%, Cmd#, Host$, Target$, Cd%
ReDim Almighty(sMaxStatus) As Integer
Static Taunt%, Six%
  DoEvents
DoAgain1:
  If fPreBattl.Visible Then
    Unload Me
    Exit Sub
  End If
  On Error Resume Next
  If (Findchatroom1() = 0) And (Not Paused) And (YourSN <> "Player") Then
    Paused = True
    mPauseBattle.Caption = "Un&P"
    Send (Dataset.BattlePause)
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
  x2 = Playernum(SN)
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
  If (((Skip(Ct, 4) = "INFO") Or Skip(Ct, 4) = "HELP" And Len(Ct) > 6) Or (Skip(Ct, 1) = "?" And Len(Ct) > 3)) And cInfo = 1 Then
    'Send ("""" + CT + """?!? What do you think this is, " + SN + "? Talismans?")
    If Skip(Ct, 1) <> "?" Then
      Tid = Right$(Ct, Len(Ct) - 6)
    Else
      Tid = Right$(Ct, Len(Ct) - 3)
    End If
    ShowInfo (Tid)
  End If
  'If Skip(Ct, 4) = "HELP" And Len(Ct) > 6 And cInfo = 1 Then
  '  Tid = Right$(Ct, Len(Ct) - 6)
  '  ShowInfo (Tid)
  'End If
  If Skip(Ct, 5) = "MOVES" And (cMoves = 1) Then
    If (Len(Ct) > 7) Then
      ShowMoves (Mid$(Ct, 8, Len(Ct) - 7))
    Else
      ShowMoves (SN)
    End If
  End If
  If Skip(Ct, 7) = "SHOTGUN" Or Skip(Ct, 8) = "NEXTHOST" Or Skip(Ct, 8) = "HOSTNEXT" Then ShotgunCall (SN)
  If Skip(Ct, 4) = "TYPE" Then
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
    Decompile Ct, Pin%, Keycode%, Cmd#, Host$, Target$
    If ((Host = "") Or (Host = YourSN)) And (Pin > 999) And (Keycode > 0) And (SNKeyCode(SN, Pin) = Keycode) Then
      Cd = Cmd / Keycode
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
	  If Target = "JOINING" Then cJoin = (1 - cJoin)
	  If Target = "STATUS" Then cStatus = (1 - cStatus)
	  If Target = "MOVES" Then cMoves = (1 - cMoves)
	  If Target = "ATTACKING" Then cAttacking = (1 - cAttacking)
	  If Target = "CPU" Then cCPU = (1 - cCPU)
	  If Target = "VERSION" Then cVersion = (1 - cVersion)
	  If Target = "TEAM" Then cTeam = (1 - cTeam)
	  Auto = False
	Case 10:
	  x4 = Playernum(Target)
	  If (x4 <> 0) Then ShowReveal (x4)
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
	  x6 = Playernum(Target)
	  If (x6 > 0) Then
	    Select Case Rand(1, 4)
	      Case 1: S6 = Msg + " sends a sonic wave directly at " + ScrNam(x6) + ", ripping their body apart!"
	      Case 2: S6 = Msg + " sends a bolt of lightning at " + ScrNam(x6) + ", charring them to dust!"
	      Case 3: S6 = Msg + " gestures and " + ScrNam(x6) + "'s heart stops."
	      Case 4: S6 = Msg + " rips " + ScrNam(x6) + "'s head off with the spine still attached!"
	    End Select
	    If SN = "SlrSenshiV" Then
	      Send ("V-Hime summons the almighty Safer Sephiroth to cast ""Super Nova""!")
	      Send ("The Nova hits " + ScrNam(x6) + "!!!")
	    ElseIf SN = "Omegaquila" Then
	      Select Case Rand(1, 2)
		Case 1: S6 = "Aquila rips off his clothes Revealing Sailor Starfighter's outfit! " + ScrNam(x6) + "'s Eyes Explode, inflicting fatal wounds!"
		Case 2: S6 = "Aquila Summons Barney and Friends on " + ScrNam(x6) + "! " + ScrNam(x6) + "'s Head Explodes from the HORROR!!"
	      End Select
	      Send (S6)
	    Else
	      Send (S6)
	    End If
	    'If (x6 > MaxPlayers) Then
	    '  y(x6 - MaxPlayers).HP = -1
	    'Else
	      p(x6).HP = -1
	    'End If
	  End If
	  ShowStatus (x6)
	Case 15:
	  x6 = Playernum(Target)
	  If (x6 > 0) Then
	    Send (Msg + " revives " + ScrNam(x6) + " from the abyss!")
	    'If (x6 > MaxPlayers) Then
	    '  y(x6 - MaxPlayers).HP = y(x6 - MaxPlayers).MaxHP
	    '  y(x6 - MaxPlayers).CurMove = 0
	    'Else
	      p(x6).HP = p(x6).MaxHP
	      p(x6).CurMove = -255
	    'End If
	  End If
	Case 16:
	  x6 = Playernum(Target)
	  If (x6 > 0) And (x6 <= MaxPlayers) Then
	    Send (Msg + " gestures and creates a barrier around " + p(x6).ScrNam + ".")
	    Almighty(sBarrier) = 100
	    Almighty(sMBarrier) = 100
	    CheckStatus Almighty(), x6, 0, 1
	  End If
	Case 17:
	  x6 = Playernum(Target)
	  If (x6 > 0) And (x6 <= MaxPlayers) Then
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
	    CheckStatus Almighty(), x6, 0, 1
	  End If
	Case 19:
	  ScrollSend1 (Msg + " sends a wave of destruction killing everyone.")
	  For X = 1 To MaxPlayers
	    p(X).Status(sLife3) = 0
	    p(X).HP = -1
	  Next X
	  'For X = 1 To Maxyouma
	  '  y(X).Status(sLife3) = 0
	  '  y(X).HP = -1
	  'Next X
	Case 20:
	  x6 = Playernum(Target)
	  If (x6 > 0) Then
	    Send (Msg + " gestures and " + ScrNam(x6) + " is fully restored!")
	    'If x6 > MaxPlayers Then
	    '  y(x6 - MaxPlayers).HP = y(x6 - MaxPlayers).MaxHP
	    'Else
	      p(x6).HP = p(x6).MaxHP
	      'p(x6).MP = p(x6).MaxMP
	    'End If
	  End If
	Case 26:
	  x6 = Playernum(Target)
	  If (x6 > 0) Then
	    Send (Msg + " gestures and " + ScrNam(x6) + " disappears into the void!")
	    Playwav ("tornoff2")
	    p(x6).CharID = 0
	    p(x6).Rune = 0
	    p(x6).Weapon = 0
	    p(x6).Super = 0
	    p(x6).Cheese = 0
	  End If
	Case 25:
	  If (Six = 0) Then
	    Send ("Level 6 Supers Enabled!")
	    Six = True
	    Config.Fours = 1
	    lFour.Caption = "6"
	  End If
	Case 21:
	  x6 = Playernum(Target)
	  If (x6 > 0) And (x6 <= MaxPlayers) Then
	    Send (p(x6).ScrNam + " is swept down by a dark wave of energy from " + Msg + "! Super Meter = Gone!")
	    p(x6).Super = 0
	  End If
	Case 22:
	  x6 = Playernum(Target)
	  If (x6 > 0) And (x6 <= MaxPlayers) Then
	    Send ("A brilliant holy light from " + Msg + " shines on " + p(x6).ScrNam + ". Super Meter = Full!")
	    p(x6).Super = MaxSP
	  End If
	Case 23:
	  NewItem
	Case 24:
	  If (Item2Get <> 0) Then
	    Send (Msg + " summons a lightning bolt from the heavens to obliterate the " + Items(Item2Get).Name + "!")
	    Item2Get = 0
	  End If
	Case 28:
	  x6 = Playernum(Target)
	  If (x6 > 0) Then
	    p(x6).Status(sFreeze) = 32766
	    p(x6).CurMove = -50
	    Send ("Sub-Zero uses all of his energy to freeze " + p(x6).ScrNam + " permanently.")
	  End If
	Case 29:
	  x6 = MatchWeapon(Target)
	  If x6 > 0 Then
	    Send (Msg + " drops a " + Weapons(x6).Name + " onto the battlefield.")
	    AddDroppedWeapon x6, Weapons(x6).NumUses
	  End If
	Case 30:
	  Send (Msg + " sends out an electromagnetic pulse, destroying all weapons on the ground.")
	  MaxDropped = 0
	  ReDim Dropped(0)
	  ReDim DropUses(0)
	Case 31:
	  x6 = Playernum(Target)
	  If (x6 > 0) Then
	    p(x6).Weapon = 0
	    Send (Msg + " strikes " + p(x6).ScrNam + " with a lightning bolt, destroying their weapon!")
	    InitMoves (x6)
	  End If
      End Select
    End If
  End If
  X = Playernum1(SN)
  If (Battle) And (X = 0 Or p(X).CharID = 0) And cJoin = 1 Then
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
	  Send ("<b><!> " + Senshi(X3).FullName + " is being used by " + p(X).ScrNam + ".")
	  'LogFileWrite (SN + " tried to join in as " + Senshi(x3).FullName + " but that character was taken.")
	  GoTo DoAgain1
	End If
	For X5 = 1 To MaxPlayers
	  DoEvents
	  If (p(X5).ScrNam = SN) And (p(X5).CharID <> 0) Then
	    JoinOK = False
	    Exit For
	  End If
	Next X5
	If JoinOK Then
	  For X = 1 To fPreBattl!vsMaxPlayers.Value
	    If (p(X).CharID = 0) Then
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
  If Skip(Ct, 7) = "VERSION" And cVersion = 1 Then
    Send ("This is ChUB 2000 " + VerID + ". Dataset: " + Dataset.LoadStr)
    GoTo DoAgain1
  End If
  x2 = Playernum(SN)
  If (Battle) And (x2 <> 0) Then
    If Skip(Ct, 9) = "FRAGCOUNT" Then
      Send (SN + ", you have " + TrimStr(p(x2).Frags) + " kills and " + TrimStr(p(x2).FatalFrags) + " Fatalities.")
    End If
    If Skip(Ct, 11) = "LEARNEDMOVE" Then
      If p(x2).Moves(14).Cmdkey <> "" Then
	Send (SN + ", you currently have " + p(x2).Moves(14).Name + " available (/" + p(x2).Moves(14).Cmdkey + ")")
      Else
	Send (SN + ", you have no learned move available.")
      End If
    End If
    If Skip(Ct, 8) = "NEWTEAM" And Config.Defect And cTeam = 1 Then
      If (x2 > 0) And (x6 <= MaxPlayers) Then
	For X3 = Asc("0") To Asc("Z")
	  x4 = 0
	  For X5 = 1 To MaxPlayers
	    If TeamID(X5) = Chr$(X3) Then x4 = True
	  Next X5
	  If (x4 = 0) Then
	    Send (p(x2).ScrNam + " creates Team " + Chr$(X3) + ". (Use /teamname to name it)")
	    'LogFileWrite (SN + " created Team " + Chr$(x3) + ".")
	    p(x2).TeamID = Chr$(X3)
	    TCaptain(X3) = x2
	    TName(X3) = ""
	    x4 = 999
	    Exit For
	  End If
	Next X3
	If (x4 <> 999) Then
	  Send ("Error: Unable to create new team.")
	  'LogFileWrite (SN + " attempted to create a team but an error occured.")
	End If
      End If
    End If
    If Skip(Ct, 4) = "KICK" And Config.Defect And Len(Ct) > 7 And cTeam = 1 Then
      x7 = Playernum(Right$(Ct, Len(Ct) - 6))
      If (x2 > 0) And (x2 <= MaxPlayers) And (x7 > 0) And (x7 <= MaxPlayers) And (TeamID(x2) = TeamID(x7)) Then
	If TCaptain(Asc(p(x2).TeamID)) = x2 Then
	  Send (p(x2).ScrNam + " kicks " + p(x7).ScrNam + " off their team!")
	  'LogFileWrite (SN + " kicked " + p(x7).ScrNam + " off of Team " + TeamID(x2) + ".")
	  For X3 = Asc("0") To Asc("Z")
	    x4 = 0
	    For X5 = 1 To MaxPlayers
	      If TeamID(X5) = Chr$(X3) Then x4 = True
	    Next X5
	    If (x4 = 0) Then
	      Send (p(x7).ScrNam + " is now on Team " + Chr$(X3) + ". (Use /teamname to name it)")
	      'LogFileWrite (p(x7).ScrNam + " was placed on Team " + Chr$(x3) + ".")
	      p(x7).TeamID = Chr$(X3)
	      TCaptain(X3) = x2
	      TName(X3) = ""
	      x4 = 999
	      Exit For
	    End If
	  Next X3
	  If (x4 <> 999) Then
	    Send ("Error: Unable to create new team.")
	    'LogFileWrite (p(x7).ScrNam + " could not be placed on a new team due to an error.")
	  End If
	Else
	  Send ("Only the team captain, " + p(TCaptain(Asc(p(x2).TeamID))).ScrNam + ", may /kick people.")
	End If
      End If
    End If
    If Skip(Ct, 8) = "TEAMNAME" And Len(Ct) > 10 And cTeam = 1 Then
      If (x2 > 0) Then
	If TCaptain(Asc(p(x2).TeamID)) = x2 Then
	  S4 = Right$(Ct, Len(Ct) - 10)
	  TName(Asc(p(x2).TeamID)) = S4
	  Send ("Team " + p(x2).TeamID + " is now known as """ + S4 + """.")
	  'LogFileWrite ("Team " + p(x2).TeamID + " was renamed to """ + S4 + """.")
	Else
	  Send ("Only the team captain, " + p(TCaptain(Asc(p(x2).TeamID))).ScrNam + ", may change the team name.")
	End If
      End If
    End If
    If Skip(Ct, 6) = "STATUS" And cStatus = 1 Then
      If Len(Ct) > 8 Then
	x6 = Playernum(Mid$(Ct, 9, Len(Ct) - 8))
      Else
	x6 = 0
      End If
      If (x6 = 0) Then x6 = Playernum(SN)
      If (x6 > 0) And (x6 <= MaxPlayers) Then
	Msg = p(x6).ScrNam + " (" + Senshi(p(x6).CharID).FullName + "): (" + TrimStr(p(x6).HP) + "H/"
	'If (Config.InfMP) Then
	'  Msg = Msg + "???"
	'Else
	'  Msg = Msg + TrimStr(p(x6).MP)
	'End If
	Msg = Msg + TrimStr(p(x6).Super) + "S/" + TrimStr(p(x6).Cheese) + "C) [" + TextStatus(x6) + "] "
	If p(x6).Status(sChaos) Then Msg = Msg + "(chaosed) "
	If p(x6).Status(sBerserk) Then Msg = Msg + "(berserked) "
	If p(x6).Status(sMute) Then Msg = Msg + "(muted)"
	If p(x6).HP <= (MaxHP / 6) Then
	  Msg = "<font color=#FF0000>" + Msg
	  If p(x6).HP > 0 Then Msg = Msg + " <DANGER>"
	End If
	Send (Msg)
      End If
      GoTo DoAgain1
    End If
    If Skip(Ct, 9) = "ATTACKING" And cAttacking = 1 Then
      If Len(Ct) > 11 Then
	x6 = Playernum(Right$(Ct, Len(Ct) - 11))
      Else
	x6 = 0
      End If
      If x6 = 0 Then x6 = Playernum(SN)
      If (x6 > 0 And x6 <= MaxPlayers) Then
	Msg = AttackMe$(x6)
	If Msg = "" Then
	  Send ("Nobody is attacking " + p(x6).ScrNam + ".")
	Else
	  Send ("Attacking " + p(x6).ScrNam + ": " + Msg)
	End If
	GoTo DoAgain1
      End If
    End If
    If Skip(Ct, 7) = "RESPAWN" And p(x2).HP <= 0 And Config.Respawn Then
      Send (p(x2).ScrNam + "'s impatience causes a longer respawn delay for them!")
      p(x2).GotKilled = p(x2).GotKilled + 30
    End If
  End If
  x2 = Playernum(SN)
  If (Battle And x2 <> 0) Then
    If p(x2).ScrNam = SN Then
      If (Skip(Ct, 8) = "NODEFECT") And Config.Defect And cTeam = 1 Then
	p(x2).Defect = Not p(x2).Defect
	If Not p(x2).Defect Then
	  Send (Parse(Dataset.AcceptDefects, SN, "", "", ""))
	  'LogFileWrite (SN + " began to accept /defects.")
	Else
	  Send (Parse(Dataset.DeclineDefects, SN, "", "", ""))
	  'LogFileWrite (SN + " stopped accepting /defects.")
	End If
      End If
      If (Skip(Ct, 13) = "I WANT A DRAW") Then
	p(x2).Draw = Not p(x2).Draw
	If p(x2).Draw Then
	  Send5 (Parse(Dataset.WantDraw, SN, "", "", ""))
	  'LogFileWrite (SN + " called for a draw.")
	Else
	  Send5 (Parse(Dataset.DontWantDraw, SN, "", "", ""))
	  'LogFileWrite (SN + " decided not to draw.")
	End If
      End If
      If (Skip(Ct, 6) = "DEFECT") And (Config.Defect) And (Len(Ct) > 8) And cTeam = 1 Then
	p(x2).Target = Playernum(Mid$(Ct, 9, Len(Ct) - 8))
	Call DoMove(x2, p(x2).Target, pDefect, "")
      End If
      If (Skip(Ct, 3) = "CPU") And cCPU = 1 Then
	If (p(x2).CPU = 0) Then
	  p(x2).CPU = 1
	  p(x2).Goodwill = 50
	  p(x2).Greed = 50
	  p(x2).Wrath = 50
	  p(x2).Arrogance = 0
	  Send5 (SN + " is now controlled by the computer.")
	  'LogFileWrite ("Control of " + SN + " was switched over to the CPU.")
	Else
	  p(x2).CPU = 0
	  Send5 (SN + " is no longer controlled by the computer.")
	  'LogFileWrite (SN + " took control of their character.")
	End If
      End If
      If (Skip(Ct, 6) = "~WRATH") And cCPU = 1 Then
	If (p(x2).CPU = 0) Then
	  p(x2).CPU = 1
	  p(x2).Goodwill = 50
	  p(x2).Greed = 50
	  p(x2).Wrath = 100
	  p(x2).Arrogance = 0
	  Send5 (SN + " is now controlled by a vengeful computer...")
	  'LogFileWrite ("Control of " + SN + " was switched over to the /~wrathful CPU.")
	Else
	  p(x2).CPU = 0
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
	Send (p(x2).ScrNam + " claims the Red Flag.")
	RedFlag = x2
      End If
      If (Skip(Ct, 6) = "BCLAIM") And (Config.Flag <> 0) And BlueFlag = 0 Then
	Send (p(x2).ScrNam + " claims the Blue Flag.")
	BlueFlag = x2
      End If
      If (Skip(Ct, 5) = "EQUIP" Or Skip(Ct, 4) = "RUNE") Then
	If Config.OldSchool Then
	  Send (SN + ", you cannot pick a Rune in OldSchool Kombat Mode.")
	Else
	  'If Skip(CT, 5) = "EQUIP" Then
	  '  Send (CT + "? What do you think this is, " + SN + "? Talismans?")
	  'Else
	  '  Send ("Runes? What are they?")
	  'End If
	  'Exit Sub
	  If p(x2).Rune = 0 Then
	    Do
	      p(x2).Rune = Rand(1, MaxRune)
	    Loop Until p(x2).Rune <> RuneZombie
	    Send ("The " + RuneName(p(x2).Rune) + " falls into " + p(x2).ScrNam + "'s hands.")
	    If p(x2).Rune = RuneLife Then p(X).MaxHP = Int(MaxHP * .25) + MaxHP
	  Else
	    Send ("You have the " + RuneName(p(x2).Rune) + ", " + p(x2).ScrNam + ".")
	  End If
	End If
      End If
      If (Skip(Ct, 8) = "TALISMAN") Then
	Send ("Sorry, " + p(x2).ScrNam + ", we only deal with <b>/RUNE</b>s here, not Talismans.")
      End If
      If Skip(Ct, 6) = "WEAPON" Then
	If p(x2).Weapon <> 0 Then
	  Send (p(x2).ScrNam + " has the """ + Weapons(p(x2).Weapon).Name + """ with " + TrimStr(p(x2).WpnUsesLeft) + " charges remaining.")
	Else
	  Send (p(x2).ScrNam + " has no weapon. Wait for one to appear!")
	End If
      End If
      If Skip(Ct, 7) = "WPNLIST" Then
	If Config.OldSchool Then
	  Send (SN + ", weapons are disabled in OldSchool Kombat Mode.")
	Else
	  Okay = False
	  For X = 1 To MaxWeapons
	    X5 = 0
	    x6 = 0
	    For x4 = 1 To MaxDropped
	      If Dropped(x4) = X Then
		X5 = X5 + 1
		If DropUses(x4) > x6 Then x6 = DropUses(x4)
	      End If
	    Next x4
	    If X5 <> 0 Then
	      Okay = True
	      If x6 = 0 Then
		Send ("x" + TrimStr(X5) + ": " + Weapons(X).Name + " (/wget-" + Weapons(X).PickMe + ")")
	      Else
		If X5 = 1 Then
		  Send ("x" + TrimStr(X5) + ": " + Weapons(X).Name + " with " + TrimStr(x6) + " charges left (/wget-" + Weapons(X).PickMe + ")")
		Else
		  Send ("x" + TrimStr(X5) + ": " + Weapons(X).Name + " with at most " + TrimStr(x6) + " charges left (/wget-" + Weapons(X).PickMe + ")")
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
    If (p(x2).CharID <> 0) And (p(x2).HP > 0) And (p(x2).CurMove <> 0) And (p(x2).Status(sChaos) = 0) And p(x2).Status(sMIA) = 0 And p(x2).Status(sBerserk) = 0 And p(x2).Status(sMushroom) = 0 And p(x2).Status(sStop) = 0 And p(x2).Status(sSleep) = 0 Then
      If (Skip(Ct, 4) = "STOP") Or (Skip(Ct, 4) = "HALT") And (p(x2).CurMove <> pTaunt) And (p(x2).CurMove > 0) Then
	If (p(x2).CurMove < MaxMoves) And (p(x2).CurMove > 1) Then
	  M = p(x2).Moves(p(x2).CurMove)
	  If M.Element = Invin Then
	    p(x2).Status(sInvin) = 0
	    If M.Target = AllFriend Then
	      For x4 = 1 To MaxPlayers
		If TeamID(x4) = TeamID(x2) Then p(x4).Status(sInvin) = 0
	      Next x4
	    End If
	  End If
	End If
	If p(x2).CurMove <> pRest Then p(x2).CurMove = -255
	If p(x2).CurMove <> pTaunt Then p(x2).CurMove = -255
      End If
    End If
    If (p(x2).CharID = 0) Or (p(x2).HP <= 0) Or (p(x2).CurMove <> 0) Or p(x2).Status(sMIA) <> 0 Or p(x2).Status(sMushroom) <> 0 Or p(x2).Status(sStop) <> 0 Or p(x2).Status(sSleep) <> 0 Then
      If p(x2).CurMove > 0 And p(x2).CurMove < MaxMoves Then Msg = "you are doing " + p(x2).Moves(p(x2).CurMove).Name + "."
      If p(x2).CurMove > 0 And p(x2).CurMove < MaxMoves And p(x2).SuperNum <> 0 Then Msg = "you are Superering " + p(x2).Moves(p(x2).CurMove).Name + "."
      If p(x2).CurMove = pRest Then Msg = "you are resting."
      If p(x2).CurMove = pFlee Then Msg = "you are running like a coward!"
      If p(x2).CurMove = pBlock Then Msg = "you are already blocking."
      If p(x2).CurMove = pFatal Then Msg = "you are doing a Fatality."
      If p(x2).Status(sFreeze) <> 0 Then Msg = "you are frozen."
      If p(x2).Status(sStun) <> 0 Then Msg = "you are unconsious."
      If p(x2).Status(sSleep) <> 0 Then Msg = "you are sound asleep."
      If p(x2).Status(sMIA) <> 0 Then Msg = "you are Missing in Action!"
      If p(x2).Status(sStop) <> 0 Then Msg = "you are frozen in time."
      If p(x2).Status(sMushroom) <> 0 Then Msg = "you are in mushroom form."
      If p(x2).HP <= 0 Then Msg = "you are dead!"
      If p(x2).CharID = 0 Then Msg = "you are not playing in the game."
      For x4 = 1 To MaxMoves
	If (p(x2).Moves(x4).Cmdkey <> "") Then
	  M = p(x2).Moves(x4)
	  If (Skip(Ct, Len(M.Cmdkey)) = UCase(M.Cmdkey)) Then
	    Send (SN + ", you can't do a move while " + Msg)
	    p(x2).Scroller = p(x2).Scroller + 1
	    Exit Sub
	  End If
	End If
      Next x4
      If (Skip(Ct, 1) = "1") Or (Skip(Ct, 1) = "2") Or (Skip(Ct, 1) = "3") Or (Skip(Ct, 1) = "4" And Config.Fours) Or (Skip(Ct, 1) = "5" And Config.Fours) Or (Skip(Ct, 1) = "6" And Six) Then
	Send (SN + ", you can't do a Super while " + Msg)
	p(x2).Scroller = p(x2).Scroller + 1
	Exit Sub
      End If
      If (Skip(Ct, 5) = "BLOCK") Then
	Send (SN + ", you can't block while " + Msg)
	p(x2).Scroller = p(x2).Scroller + 1
	Exit Sub
      End If
      If Skip(Ct, 3) = "GET" Then
	Send (SN + ", you can't get an item while " + Msg)
	p(x2).Scroller = p(x2).Scroller + 1
	Exit Sub
      End If
      If Skip(Ct, 4) = "WGET" Then
	Send (SN + ", you can't get a weapon while " + Msg)
	p(x2).Scroller = p(x2).Scroller + 1
	Exit Sub
      End If
      If Skip(Ct, 5) = "WDROP" Or Skip(Ct, 5) = "WJUNK" Then
	Send (SN + ", you can't drop your weapon while " + Msg)
	p(x2).Scroller = p(x2).Scroller + 1
	Exit Sub
      End If
      If Skip(Ct, 7) = "UNMORPH" Then
	Send (SN + ", you can't unmorph while " + Msg)
	p(x2).Scroller = p(x2).Scroller + 1
	Exit Sub
      End If
      If Skip(Ct, 6) = "CHICTR" Then
	Send (SN + ", you can't Chibot Counter while " + Msg)
	p(x2).Scroller = p(x2).Scroller + 1
      End If
      If Skip(Ct, 4) = "FLEE" Then
	Send (SN + ", you can't run for the hills while " + Msg)
	p(x2).Scroller = p(x2).Scroller + 1
      End If
      If Skip(Ct, 6) = "DIVERT" Then
	Send (SN + ", you can't Divert HP while " + Msg)
	p(x2).Scroller = p(x2).Scroller + 1
      End If
      If Skip(Ct, 8) = "UNDIVERT" And p(x2).Rune = RuneUndivert Then
	Send (SN + ", you can't Un-divert HP while " + Msg)
	p(x2).Scroller = p(x2).Scroller + 1
      End If
      If (Skip(Ct, Len(S.Fatality.Cmdkey)) = UCase(S.Fatality.Cmdkey)) And S.Fatality.Cmdkey <> "" Then
	Send (SN + ", you can't Fatalize anyone while " + Msg)
	p(x2).Scroller = p(x2).Scroller + 1
      End If
    End If
    If (p(x2).CharID <> 0) And (p(x2).HP > 0) And (p(x2).CurMove = 0) And (p(x2).Status(sStop) = 0 And p(x2).Status(sMIA) = 0 And p(x2).Status(sMushroom) = 0) Then
      InitMoves (x2)
      If (Skip(Ct, 7) = "UNMORPH") And (p(x2).Status(sMorph)) Then
	Send (Parse(Dataset.UnMorphMsg, p(x2).ScrNam, Senshi(p(x2).OldCharID).FullName, "", ""))
	'LogFileWrite (p(x2).ScrNam + " reverted back to " + Senshi(p(x2).OldCharID).FullName + ".")
	UnmorphMe (x2)
      End If
      If (Skip(Ct, 4) = "WGET") And Len(Ct) > 6 Then
	If Config.OldSchool Then
	  Send SN + ", weapons are disabled in OldSchool Kombat."
	Else
	  If p(x2).Weapon <> 0 Then
	    Send (p(x2).ScrNam + " is already holding a weapon.")
	  Else
	    s2 = Right$(Ct, Len(Ct) - 6)
	    x4 = MatchDropped(s2)
	    If x4 = 0 Then
	      Send (p(x2).ScrNam + " looks around for that weapon but can't find it.")
	    Else
	      'p(x2).WeaponState = 1
	      p(x2).Weapon = Dropped(x4)
	      Dropped(x4) = 0
	      InitMoves (x2)
	      p(x2).WpnUsesLeft = DropUses(x4)
	      DropUses(x4) = 0
	      Send (Parse(Weapons(p(x2).Weapon).SelectStr, p(x2).ScrNam, "", "", ""))
	    End If
	  End If
	End If
      End If
      If Skip(Ct, 5) = "WDROP" And p(x2).Weapon <> 0 Then
	If Left$(Weapons(p(x2).Weapon).PickMe, 1) = "~" Then
	  Send (p(x2).ScrNam + " drops their " + Weapons(p(x2).Weapon).Name + " and it vanishes into oblivion!")
	  p(x2).Weapon = 0
	Else
	  DropWeapon x2, "%SN drops their %T."
	End If
      End If
      If Skip(Ct, 5) = "WJUNK" And p(x2).Weapon <> 0 Then
	Send (p(x2).ScrNam + " drops their " + Weapons(p(x2).Weapon).Name + " and it vanishes into oblivion!")
	p(x2).Weapon = 0
      End If
      If (Skip(Ct, 3) = "GET") Then
	If (Item2Get <> 0) Then
	  Call DoMove(x2, 0, pGet, "")
	Else
	  Send (Parse(Dataset.NoGetItem, p(x2).ScrNam, "", "", ""))
	End If
      End If
      If (Skip(Ct, 6) = "CHICTR") And (Len(Ct) > 8) Then
	If (p(x2).Super >= 200) Or p(x2).Rune = RuneBlock Then
	  S3$ = Right$(Ct, Len(Ct) - 8)
	  For x4 = 1 To MaxMoves
	    If (p(x2).Moves(x4).Cmdkey <> "") Then
	      Okay = False
	      M = p(x2).Moves(x4)
	      If (UCase(S3$)) = UCase(M.Cmdkey) Then Okay = True
	      If (p(x2).Status(sMute) <> 0) And (M.Element <> Phys) Then Okay = False
	      If (p(x2).Rune = RuneBlock And p(x2).Super < 100) And Okay Then
		Okay = False
		Send (p(x2).ScrNam + " still needs the 100 Super Point cost up front to do a Chibot Counter.")
	      End If
	      If Okay Then
		'Send (Parse(Dataset.ChiCtr, p(x2).ScrNam, p(x2).Moves(x4).Name, "", ""))
		p(x2).Status(sHamedo) = -1
		p(x2).Super = p(x2).Super - 100
		'LogFileWrite (SN + " Chibot Countered with """ + p(x2).moves(x4).Name + """")
		Call DoMove(x2, x4, pBlock, "")
	      End If
	    End If
	  Next x4
	Else
	  Send (p(x2).ScrNam + " needs 200 Super Points to do a Chibot Counter.")
	End If
      End If
      If (Skip(Ct, 5) = "BLOCK") Then
	p(x2).Status(sHamedo) = 0
	If (Len(Ct) > 7) And (p(x2).Super >= 100 Or p(x2).Rune = RuneBlock) Then
	  S3$ = Right$(Ct, Len(Ct) - 7)
	  For x4 = 1 To MaxMoves
	    If (p(x2).Moves(x4).Cmdkey <> "") Then
	      Okay = False
	      M = p(x2).Moves(x4)
	      If (UCase(S3$)) = UCase(M.Cmdkey) Then Okay = True
	      If (p(x2).Status(sMute) <> 0) And (M.Element <> Phys) Then Okay = False
	      If Okay Then
		'Send (Parse(Dataset.Counter, p(x2).ScrNam, p(x2).moves(x4).Name, "", ""))
		'LogFileWrite (SN + " counterattacked with """ + p(x2).moves(x4).Name + """")
		Call DoMove(x2, x4, pBlock, "")
	      End If
	    End If
	  Next x4
	Else
	  If (Len(Ct) > 7) And (p(x2).Super < 100) Then Send ("Not enough Super Point for " + SN + " to counter.")
	  Call DoMove(x2, 0, pBlock, "")
	End If
      End If
      If (Skip(Ct, 5) = "TAUNT") Then Call DoMove(x2, 0, pTaunt, "")
      If (Skip(Ct, 5) = "~SAVE") And (Len(Ct) > 7) Then
	p(x2).Target = Playernum(Mid$(Ct, 8, Len(Ct) - 7))
	If p(x2).Target <> 0 Then Call DoMove(x2, p(x2).Target, p_Save, "")
      End If
      'If (Skip(Ct, 4) = "LOOK") Then
      '  If Item2Get <> 0 Then
      '    Send (Items(Item2Get).Name)
      '  Else
      '    Send ("There is no item to get.")
      '  End If
      'End If
      If (Skip(Ct, 4) = "FLEE") Then
	If (p(x2).HP >= 150) Then
	  Call DoMove(x2, 0, pFlee, "")
	Else
	  If (p(x2).HP > 10) Then p(x2).HP = p(x2).HP - 10
	  Send5 (Parse(Dataset.FleeFail, SN, "", "", ""))
	  'LogFileWrite (SN + " tried to flee but panicked.")
	  ShowStatus x2
	End If
      End If
      If (Skip(Ct, 4) = "REST") Then Call DoMove(x2, 0, pRest, "")
      If (Skip(Ct, 8) = "UNDIVERT") And Len(Ct) > 10 And p(x2).Rune = RuneUndivert Then
	On Error GoTo NoOvr
	S5 = Right$(Ct, Len(Ct) - 10)
	X5 = Val(S5)
	If (X5 > 0) Then
	  If p(x2).Super - X5 < 0 Then
	    If p(x2).Super = 0 Then
	      Send (SN + " cannot Un-divert any SP at this time.")
	    Else
	      If p(x2).Super > (p(x2).MaxHP * 1.5 - p(x2).HP) Then
		Send (SN + " cannot Un-divert any more SP than " + TrimStr(p(x2).Super) + ".")
	      Else
		Send (SN + " cannot Un-divert any more SP than " + TrimStr(Int(p(x2).MaxHP * 1.5 - p(x2).HP)) + ".")
	      End If
	    End If
	  ElseIf p(x2).HP + X5 > p(x2).MaxHP * 1.5 Then
	    If p(x2).HP >= p(x2).MaxHP * 1.5 Then
	      Send (SN + " cannont Un-divert any more SP at this time.")
	    Else
	      If p(x2).Super > (p(x2).MaxHP * 1.5 - p(x2).HP) Then
		Send (SN + " cannot Un-divert any more SP than " + TrimStr(p(x2).Super) + ".")
	      Else
		Send (SN + " cannot Un-divert any more SP than " + TrimStr(Int(p(x2).MaxHP * 1.5 - p(x2).HP)) + ".")
	      End If
	    End If
	  Else
	    p(x2).HP = p(x2).HP + X5
	    p(x2).Super = p(x2).Super - X5
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
	  If p(x2).HP - X5 < Int(MaxHP / 2) Then
	    If p(x2).HP - Int(MaxHP / 2) > 0 Then
	      Send (p(x2).ScrNam + " cannot divert any more than " + TrimStr(p(x2).HP - Int(MaxHP / 2)) + " HP at this time.")
	    Else
	      Send (p(x2).ScrNam + " cannot divert any more HP at this time.")
	    End If
	  Else
	    p(x2).HP = p(x2).HP - X5
	    p(x2).Super = p(x2).Super + X5
	    Send (Parse(Dataset.HPDivert, p(x2).ScrNam, TrimStr(X5), "", ""))
	    'LogFileWrite (SN + " diverted " + TrimStr(X5) + " HP to their Super Meter.")
	    ShowStatus x2
	    p(x2).CurMove = 0
	  End If
	Else
NoOvr:
	  Send (p(x2).ScrNam + ", I wasn't born yesterday foo'!")
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
      S = Senshi(p(x2).CharID)
      If (Skip(Ct, Len(S.Fatality.Cmdkey)) = UCase(S.Fatality.Cmdkey)) And S.Fatality.Cmdkey <> "" Then
	If Len(Ct) > Len(S.Fatality.Cmdkey) + 1 Then
	  p(x2).Target = Playernum(Mid$(Ct, Len(S.Fatality.Cmdkey) + 3, Len(Ct) - Len(S.Fatality.Cmdkey) - 2))
	Else
	  p(x2).Target = 0
	End If
	Okay = True
	If (p(x2).Target = 0) Then
	  For x4 = 1 To MaxPlayers
	    If p(x4).HP <= (MaxHP / 6) And p(x4).HP > 0 And p(x4).CharID <> 0 Then
	      p(x2).Target = x4
	      Exit For
	    End If
	  Next x4
	  If p(x2).Target = 0 Then Okay = False
	End If
	x4 = p(p(x2).Target).HP
	If p(x2).Status(sQuick) <> 0 Then
	  Send ("Fatalitying while Quick? That's a low blow, isn't it " + p(x2).ScrNam + "??")
	ElseIf (x4 <= (MaxHP / 6)) Then
	  Call DoMove(x2, p(x2).Target, pFatal, "")
	End If
      End If
      DoEvents
      S = Senshi(p(x2).CharID)
      If (Skip(Ct, 1) = "1") Or (Skip(Ct, 1) = "2") Or (Skip(Ct, 1) = "3") Or (Skip(Ct, 1) = "4" And Config.Fours) Or (Skip(Ct, 1) = "5" And Config.Fours) Or (Skip(Ct, 1) = "6" And Six) Then
	x4 = Val(Skip(Ct, 1))
	If (p(x2).Super >= 100 * x4) And (Len(Ct) > 3) Then
	  s2 = Right$(Ct, Len(Ct) - 3)
	  For X5 = 1 To MaxMoves
	    If (UCase(Left$(s2, Len(p(x2).Moves(X5).Cmdkey))) = UCase(p(x2).Moves(X5).Cmdkey)) Then Okay = p(x2).Moves(X5).CanSuper
	    If Okay Then Exit For
	  Next X5
	  If p(x2).Status(sQuick) <> 0 Then
	    Send ("Supering while Quick? That's quite a low blow, isn't it, " + SN + "?")
	    Okay = False
	  End If
	  If p(x2).Charging Then
	    Send ("Sorry, you cannot Charge a Super Move.")
	    Okay = False
	  End If
	  If Okay Then
	    p(x2).SuperNum = x4
	    Ct = "/" + s2
	  End If
	End If
      Else
	p(x2).SuperNum = 0
      End If
      For x4 = 1 To MaxMoves
	If (p(x2).Moves(x4).Cmdkey <> "") Then
	  Okay = False
	  M = p(x2).Moves(x4)
	  If (Skip(Strip32(Ct), Len(Strip32(M.Cmdkey))) = Strip32(UCase(M.Cmdkey))) Then Okay = True
	  If (p(x2).Status(sMute) <> 0) And (M.Element <> Phys) Then
	    If (Skip(Ct, Len(M.Cmdkey)) = UCase(M.Cmdkey)) Then Send (p(x2).ScrNam + ", you cannot do this move while Mute.")
	    Okay = False
	  End If
	  If Okay Then
	    X5 = Len(M.Cmdkey)
	    p(x2).Target = 0
	    If (Len(Ct) > X5 + 1) Then
	      If (p(x2).Rune <> RuneStealth) Or M.Element = Morph Then
		S7 = Mid$(Ct, X5 + 3, Len(Ct) - X5 - 2)
		p(x2).Target = Playernum(S7)
	      Else
		Send ("Whoops! " + SN + ", did I mention that you can't target with the Rune of Stealth?")
	      End If
	    Else
	      S7 = ""
	    End If
	    If (p(x2).Target <> 0 And p(x2).ScrNam = YourSN And p(x2).Rune <> RuneStealth) Then Send (YourSN + " targeted " + p(p(x2).Target).ScrNam + "!!!")
	    If (UCase(S7) = "FOE") And (p(x2).Rune <> RuneStealth) Then p(x2).Target = GetTarget(p(x2).TeamID)
	    If ((UCase(S7) = "FRIEND") Or (UCase(S7) = "ALLFRIEND")) And (p(x2).Rune <> RuneStealth) Then p(x2).Target = x2
	    If ((Left$(UCase(S7), 4) = "TEAM") Or (Left$(UCase(S7), 3) = "ALL")) And (p(x2).Rune <> RuneStealth) Then
	      S6 = UCase(Right$(S7, 1))
	      For x6 = 1 To MaxPlayers
		If p(x6).TeamID = UCase(S6) And p(x6).HP > 0 And Active(x6) <> 0 Then
		  p(x2).Target = x6
		  Exit For
		End If
	      Next x6
	    End If
	    If (p(x2).Target = 0) Then
	      Select Case M.Target
		Case AllFriend, Friend, OnlySelf: p(x2).Target = x2
		Case Else: p(x2).Target = GetTarget(p(x2).TeamID)
	      End Select
	    End If
	    Call DoMove(x2, p(x2).Target, x4, S7)
	    Exit Sub
	  End If
	End If
      Next x4
      For x4 = 1 To MaxWeapons
	DoEvents
	If Skip(Ct, Len(Weapons(x4).PickMe)) = UCase(Weapons(x4).PickMe) Then
	  Send (SN + ", you can't pick a weapon during battle. You may only pick up dropped weapons (/wpnlist)")
	  Exit Sub
	End If
      Next x4
    End If
  End If
  DoEvents
End Sub

Sub tiFullAuto_Timer ()
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
	tiFullAuto.Interval = 5000
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

Sub tiMisc_Timer ()
 ' Misc stuff
Dim X%, x1%, x2%, X3%, x4%, y2%, ST%, Df%, CPUWon%
Dim x6%, Msg$, Stat$
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
    For x4 = 1 To MaxPlayers
      If HPx(x4) <= 0 And Active(x4) And (Not DoneKilled(x4)) Then
	ShowStatus (x4)
      ElseIf HPx(x4) > 0 And Active(x4) Then
	p(x4).GotKilled = 0
	' Rune Sub
	If p(x4).Rune = RuneArmor And p(x4).RuneTemp = 0 Then
	  p(x4).RuneTemp = Rand(20, 26)
	  If p(x4).RuneTemp = Reveal Then p(x4).RuneTemp = Demi
	  If p(x4).RuneTemp = Heart Then p(x4).RuneTemp = Poison
	  Msg = p(x4).ScrNam + "'s Elemental Armor is charged with <b>"
	  Select Case p(x4).RuneTemp
	    Case MoonE: Msg = Msg + "moon power"
	    Case Shadow: Msg = Msg + "dark energy"
	    Case Water: Msg = Msg + "water magic"
	    Case Fire: Msg = Msg + "fire power"
	    Case Lit: Msg = Msg + "lightning"
	    Case Demi: Msg = Msg + "gravity magic"
	    Case Poison: Msg = Msg + "poison power"
	    Case Else: Msg = Msg + EName(p(x4).RuneTemp)
	  End Select
	  Msg = Msg + "</b>."
	  Send Msg
	End If
	If p(x4).Rune = RunePMS And p(x4).Status(sPMS) <> 0 And p(x4).CurMove = 0 Then p(x4).CurMove = -255
	If p(x4).Rune = RuneHaste Then p(x4).Status(sHaste) = 666
	If p(x4).Rune = RuneStr Then p(x4).Status(sBless) = 666
	If p(x4).Rune = RuneRot And p(x4).Status(sPoison) = 0 Then
	  Send (p(x4).ScrNam + " is poisoned by the rotten rune!")
	  p(x4).Status(sPoison) = XTimer
	End If
	If p(x4).Rune = RuneCurse And p(x4).Status(sCurse) = 0 Then p(x4).Status(sCurse) = XTimer
	If p(x4).Rune = RuneBarr Then
	  p(x4).Status(sBarrier) = 666
	  p(x4).Status(sMBarrier) = 666
	End If
	If p(x4).Rune = RuneRegen Then p(x4).Status(sRegen) = 666
	'If p(x4).Rune = RuneCharge And p(x4).Charging And p(x4).CurMove = 0 Then p(x4).Charging = False
	If p(x4).Rune = RuneMute And p(x4).Status(sMute) = 0 Then
	  Send (p(x4).ScrNam + " is muted by the Rune of Mute!")
	  p(x4).Status(sMute) = XTimer
	End If
	If p(x4).Rune = RuneZombie And p(x4).Status(sZombie) = 0 Then
	  Send (p(x4).ScrNam + " is turned into a zombie by the Rune of the Undead!")
	  p(x4).PhysStr = 250
	  p(x4).MagStr = 250
	  p(x4).God = True
	  p(x4).MaxHP = p(x4).MaxHP * 2
	  p(x4).HP = p(x4).MaxHP
	  p(x4).Status(sZombie) = True
	End If
	If p(x4).Rune = RuneBeans And Rand(1, 80) = 25 And (Not Paused) Then
	  X = GetTarget(p(x4).TeamID)
	  If p(x4).Status(sMIA) = 0 Then
	    Select Case Rand(1, 5)
	      Case 1:
		If p(X).Rune = RuneMagic Then
		  Send (p(x4).ScrNam + " farts on " + p(X).ScrNam + ", but " + p(X).ScrNam + " lights a match and " + p(x4).ScrNam + "'s butt is charred!")
		Else
		  Send (p(X).ScrNam + " smells " + p(x4).ScrNam + "'s bad gas and swoons!")
		  Hoser(sStun) = 100
		  CheckStatus Hoser(), X, x4, 1
		End If
	      Case 2:
		If p(X).Rune = RuneMagic Then
		  Send (p(x4).ScrNam + " farts on " + p(X).ScrNam + ", but " + p(X).ScrNam + " lights a match and " + p(x4).ScrNam + "'s butt is charred!")
		Else
		  Send (p(X).ScrNam + " is poisoned by " + p(x4).ScrNam + "'s bad gas!")
		  Hoser(sPoison) = 100
		  CheckStatus Hoser(), X, x4, 1
		End If
	      Case 3:
		Send (p(x4).ScrNam + " farts. P-U!")
	      Case 4:
		Send (p(x4).ScrNam + " lets out a whopper fart!")
	      Case 5:
		Send (p(x4).ScrNam + " cuts the cheese again.")
	    End Select
	    Playwav ("fart" + TrimStr(Rand(1, 6)))
	  End If
	End If
	If p(x4).Rune = RuneBug And Rand(1, 85) = 25 And (Not Paused) Then
	  If p(x4).CurMove > 0 And p(x4).CurMove < MaxMoves Then
	    Select Case Rand(1, 6)
	      Case 1:
		Do
		  X = Rand(1, MaxMoves)
		  DoEvents
		Loop Until (p(x4).Moves(X).Cmdkey <> "")
		Send ("A bug in ChUB causes " + p(x4).ScrNam + "'s move to change to " + p(x4).Moves(X).Name + ".")
		p(x4).CurMove = X
	      Case 2:
		X = ChaosTarget()
		Send ("A bug in ChUB causes " + p(x4).ScrNam + "'s target to change to " + ScrNam(X) + ".")
		p(x4).Target = X
	      Case 3:
		Send ("A bug in ChUB stops " + p(x4).ScrNam + "'s move.")
		p(x4).CurMove = 0
	      Case 4:
		Send ("A bug in ChUB causes " + p(x4).ScrNam + "'s move to hit immediately.")
		p(x4).MoveStart = -999
	      Case 5:
		Send ("A bug in ChUB changes " + p(x4).ScrNam + "'s move attack length.")
		p(x4).MoveStart = p(x4).MoveStart + 10
	      Case 6:
		Send ("Another one of Sally's stupid bugs makes " + p(x4).ScrNam + " go insane!")
		p(x4).Status(sBerserk) = XTimer
	    End Select
	  End If
	End If
	If p(x4).Rune = RuneWeird And Rand(1, 85) = 25 And p(x4).Status(sMIA) = 0 And (Not Paused) Then
	  Select Case Rand(1, 10)
	    Case 1:
	      p(x4).Status(sHaste) = XTimer
	      Send (p(x4).ScrNam + " gets Haste for no reason! How weird!")
	    Case 2:
	      p(x4).Status(sBless) = XTimer
	      Send (p(x4).ScrNam + " gets Blessed for no reason! How weird!")
	    Case 3:
	      p(x4).Status(sQuick) = XTimer
	      Send (p(x4).ScrNam + " gets Quick for no reason at all! How very weird!")
	    Case 4:
	      p(x4).Status(sBarrier) = 100
	      p(x4).Status(sMBarrier) = 100
	      Send (p(x4).ScrNam + " gets their barriers recharged for no reason whatsoever.")
	    Case 5:
	      p(x4).HP = p(x4).HP + 200
	      Send (p(x4).ScrNam + " gets 200 HP for no reason! Weird!")
	    Case 6:
	      If p(x4).CurMove <= 0 Then
		p(x4).Target = GetTarget(p(x4).TeamID)
		If p(x4).Target <> 0 Then
		  X3 = 0
		  For X = 1 To MaxMoves
		    If p(x4).Moves(X).Cmdkey <> "" And p(x4).Moves(X).Target = Enemy Then X3 = X
		  Next X
		  If X3 <> 0 Then
		    Do
		      X = Rand(1, MaxMoves)
		      DoEvents
		    Loop Until (p(x4).Moves(X).Cmdkey <> "" And p(x4).Moves(X).Target = Enemy)
		    Send (p(x4).ScrNam + " suddenly lashes out at " + ScrNam(p(x4).Target) + "!")
		    p(x4).CurMove = X
		    p(x4).MoveStart = -999
		  End If
		End If
	      Else
		Send (p(x4).ScrNam + "'s move will hit right... about... NOW!")
		p(x4).MoveStart = -999
	      End If
	    Case 7:
	      AddStatus x4, sMIA
	      Send ("Dude! Visitors just landed! They've kidnapped " + p(x4).ScrNam + "! You bastards!")
	    Case 8:
	      Send (p(x4).ScrNam + " sees Kenny nearby and kills him for the heck of it!")
	      Send (p(x4).ScrNam + " gets a good kick out of it! ""OH MY GOD, THEY KILLED KENNY!"" shouts Stan.")
	      p(x4).MaxHP = p(x4).MaxHP + 50
	    Case 9:
	      Send (p(x4).ScrNam + " gets some Super Points for no reason!")
	      'Send (p(x4).ScrNam + " gets a magic boost for no reason!")
	      'p(x4).MaxMP = p(x4).MaxMP + 50
	      p(x4).Super = p(x4).Super + 100
	    Case 10:
	      If p(x4).Status(sLife3) = 42 Then
		Send (p(x4).ScrNam + " is blessed by the KUB Goddess, Sally.")
		Send (p(x4).ScrNam + " gets another Life3!")
		p(x4).Status(sLife3) = -1
	      ElseIf Config.Flag <> 0 And p(x4).CurMove > 0 And p(x4).CurMove <= MaxMoves Then
		Send (p(x4).ScrNam + ", watch who you're targeting...")
		X3 = p(x4).Moves(p(x4).CurMove).Target
		If (X3 = Enemy) Or (X3 = AllFoe) Or (X3 = AllTeam) Or (X3 = Everybody) Then
		  If p(x4).TeamID = "B" Then
		    p(x4).Target = RedFlag
		  Else
		    p(x4).Target = BlueFlag
		  End If
		Else
		  If p(x4).TeamID = "B" Then
		    p(x4).Target = BlueFlag
		  Else
		    p(x4).Target = RedFlag
		  End If
		End If
	      ElseIf p(x4).Status(sRegen) = 0 Then
		Send (p(x4).ScrNam + " gets Regen for no reason!")
		p(x4).Status(sRegen) = -1
	      End If
	  End Select
	End If
      End If
      If ((p(x4).HP <= 0 And Config.Respawn) Or p(x4).HP > 0) And (p(x4).CharID > 0) Then
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
    Next x4
    If (Item2Get = 0) Then
      LItem = "Nothing"
    Else
      LItem = Items(Item2Get).Name
    End If
    dodraw = True
    AllDead% = True
    For X = 1 To MaxPlayers
      If p(X).HP > 0 And p(X).CharID <> 0 And p(X).Draw = False Then dodraw = False
      If p(X).HP > 0 And p(X).CharID <> 0 Then AllDead = False
    Next X
    'For X = 1 To Maxyouma
    '  If y(X).HP > 0 And y(X).Enabled Then dodraw = False
    '  If y(X).HP > 0 And y(X).Enabled Then AllDead = False
    'Next X
    If AllDead And (CurArena <> Lang) Then
      Playwav ("lose")
      EndIt Dataset.AllDead
      Exit Sub
    ElseIf AllDead Then
      Playwav ("lose")
      EndIt (">>> The langoliers have arrived and eaten the past.")
      Exit Sub
    End If
    If dodraw Then
      Playwav ("draw")
      EndIt Dataset.Draw
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
	  Case 60 * 60: ScrollSend Dataset.X1HrLeft
	  Case 60 * 30: ScrollSend Dataset.X30MinsLeft
	  Case 60 * 20: ScrollSend Dataset.X20MinsLeft
	  Case 60 * 10: ScrollSend Dataset.X10MinsLeft
	  Case 60 * 5: ScrollSend Dataset.X5MinsLeft
	  Case 120: ScrollSend Dataset.X2MinsLeft
	  Case 60:
	    If (Config.Flag = 0) Then
	      ScrollSend Dataset.SuddenDeath
	      Playwav ("hurry")
	    Else
	      ScrollSend Dataset.x1MinsLeft
	    End If
	  Case 30: ScrollSend Dataset.x30SecsLeft
	  Case 15: ScrollSend Dataset.x15SecsLeft
	  Case 5: ScrollSend Dataset.x5SecsLeft
	  Case Is <= 0:
	    If (Config.Flag = 0) Then
	      Playwav ("timeout")
	      EndIt Dataset.TimeExpired
	      Exit Sub
	    Else
	      'Playwav ("win")
	      CPUWon = True
	      If p(RedFlag).TeamID = "B" Then
		For X = 1 To MaxPlayers
		  If p(X).CPU = 0 And p(X).TeamID = "B" Then CPUWon = False
		Next X
		If CPUWon Then Playwav ("lose") Else Playwav ("win")
		EndIt (">>> <font color=""#0000FF"">Blue Team wins!")
	      ElseIf p(BlueFlag).TeamID = "R" Then
		For X = 1 To MaxPlayers
		  If p(X).CPU = 0 And p(X).TeamID = "R" Then CPUWon = False
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
	  x4 = mciSendString(Stat$, Msg$, 255, 0)
	  If Tempo% <> Val(Msg$) Then Tempo% = Int(1.25 * Val(Msg$))
	  If Val(Msg$) <> 0 Then x4 = mciSendString("set ChUBMidi tempo " + TrimStr(Tempo), 0&, 0, 0)
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
	Arena(Lang).AllAttacks = .9
	Lango = 2
      End If
      If XTimer > 120 And Lango = 2 Then
	Arena(Lang).AllAttacks = .8
	Lango = 3
      End If
      If XTimer > 180 And Lango = 3 Then
	Arena(Lang).RestLowHP = 30
	Arena(Lang).RestHighHP = 45
	'Arena(Lang).RestLowMP = 30
	'Arena(Lang).RestHighMP = 45
	Arena(Lang).AllAttacks = .7
	Lango = 4
      End If
      If XTimer > 240 And Lango = 4 Then
	Send ("You can now hear the buzzing sound.")
	Arena(Lang).AllAttacks = .6
	Lango = 5
      End If
      If XTimer > 300 And Lango = 5 Then
	Send ("The buzzing sound of the langoliers increases volume.")
	Arena(Lang).RestLowHP = 20
	Arena(Lang).RestHighHP = 40
	'Arena(Lang).RestLowMP = 20
	'Arena(Lang).RestHighMP = 40
	Arena(Lang).AllAttacks = .5
	Lango = 6
      End If
      If XTimer > 360 And Lango = 6 Then
	Arena(Lang).AllAttacks = .4
	Lango = 7
      End If
      If XTimer > 420 And Lango = 7 Then
	Send ("The langoliers sound really pissed off!")
	Arena(Lang).RestLowHP = 10
	Arena(Lang).RestHighHP = 20
	'Arena(Lang).RestLowMP = 10
	'Arena(Lang).RestHighMP = 20
	Arena(Lang).AllAttacks = .3
	Lango = 8
      End If
      If XTimer > 480 And Lango = 8 Then
	Send ("The langoliers are REALLY close now!")
	Arena(Lang).AllAttacks = .2
	Lango = 9
      End If
      If XTimer > 540 And Lango = 9 Then
	Send ("The langoliers' buzzing is deafening now.")
	Arena(Lang).RestLowHP = 0
	Arena(Lang).RestHighHP = 10
	'Arena(Lang).RestLowMP = 0
	'Arena(Lang).RestHighMP = 10
	Arena(Lang).AllAttacks = .1
	Lango = 10
      End If
      If XTimer > 600 Then
	Send ("The langoliers have arrived.")
	Send ("<CHOMP> <CHEW> <SMACK> <GULP>")
	Send ("You lose!")
	Arena(Lang) = Langolier
	For X = 1 To MaxPlayers
	  p(X).HP = Rand(-1, -6000)
	Next X
	'For X = 1 To Maxyouma
	'  y(X).HP = Rand(-1, -6000)
	'Next X
      End If
    End If
    S5 = "No Winner"
    For X = 1 To MaxPlayers
      If p(X).CharID <> 0 And p(X).HP > 0 Then
	S5 = "Keep going"
      End If
    Next X
    'For X = 1 To Maxyouma
    '  If y(X).Enabled And y(X).HP > 0 Then
    '    S5 = "Keep going"
    '  End If
    'Next X
    If (S5 = "No Winner") Then
      'EndIt ("Hmm... looks like I win, since I killed everyone! <Evil Cackling>")
      Exit Sub
    End If
    S4 = ""
    For X = 1 To MaxPlayers
      If (p(X).CharID <> 0) And (p(X).HP > 0) Then
	If (S4 = "") Then
	  S4 = p(X).TeamID
	End If
	If (p(X).TeamID <> S4) Then
	  S4 = "!!!!!"
	  Exit For
	End If
      End If
    Next X
    'If (S4 <> "!!!!!") Then
      'For X = 1 To Maxyouma
      '  If (y(X).Enabled) And (y(X).HP > 0) Then
      '    If (S4 = "") Then
      '      S4 = y(X).TeamID
      '    End If
      '    If (y(X).TeamID <> S4) Then
      '      S4 = "!!!!!"
      '      Exit For
      '    End If
      '  End If
      'Next X
    'End If
    If (S4 <> "!!!!!") And (Config.Respawn = 0) And (Config.Flag = 0) Then
    ' All that was to check to see if the battle is over.
    ' If battle is over, then S4 contains the TeamID of the winning team.
      S5 = ">>> ^" '+ TName(Asc(S4))
      CPUWon = True
      For x4 = 1 To MaxPlayers
	If p(x4).TeamID = S4 Then
	  S5 = S5 + p(x4).ScrNam + ", "
	  If p(x4).CPU = 0 Then CPUWon = False
	End If
      Next x4
      S5 = Left$(S5, Len(S5) - 2) + "^ (" + TName(Asc(S4)) + ") wins for being the last alive!"
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
      For x4 = 1 To MaxPlayers
	Select Case FragLimit
	  Case Is < 0:
	    If p(x4).FatalFrags >= Abs(FragLimit) Then Maxp% = x4
	  Case Is > 0:
	    If p(x4).Frags >= FragLimit Then Maxp% = x4
	End Select
      Next x4
      If Maxp% <> 0 And (Config.Flag = 0) Then
	Msg = ">>> "
	Msg = Msg + p(Maxp%).ScrNam
	Msg = Msg + " wins for achieving " + TrimStr(FragLimit)
	If (FragLimit > 0) Then
	  Msg = Msg + " frags."
	Else
	  Msg = Msg + " Fatalities."
	End If
	If p(Maxp%).CPU = 0 Then
	  Playwav ("win")
	Else
	  Playwav ("lose")
	End If
	EndIt (Msg)
	Exit Sub
      End If
    End If
    If Config.Respawn <> 0 Then
      For x4 = 1 To MaxPlayers
	If p(x4).CharID <> 0 And p(x4).HP <= 0 And p(x4).HP <> -666 And p(x4).HP <> -777 And XTimer - p(x4).GotKilled >= 30 Then
	  If Config.Flag <> 0 Then
	    Send (p(x4).ScrNam + " is released from jail.")
	  Else
	    Send (Parse(Dataset.Respawn, p(x4).ScrNam, "", "", ""))
	  End If
	  Playwav ("r_tele" + TrimStr(Rand(1, 5)))
	  clearStatus (x4)
	  p(x4).HP = p(x4).MaxHP
	  p(x4).CurMove = -255
	End If
      Next x4
    End If
  End If
  If (Battle) Then
    For x2 = 1 To MaxPlayers
      'If p(x2).MP < 0 Then p(x2).MP = 0
      If p(x2).Super > MaxSP Then p(x2).Super = MaxSP
      If p(x2).Super < 0 Then p(x2).Super = 0
      If Config.Flag <> 0 And (BlueFlag <> -1) And (RedFlag <> -1) Then
	ORed = RedFlag
	OBlue = BlueFlag
	If x2 = RedFlag And p(x2).HP <= 0 Then RedFlag = (p(x2).AttackedMe)
	If x2 = BlueFlag And p(x2).HP <= 0 Then BlueFlag = (p(x2).AttackedMe)
	If p(RedFlag).TeamID = "R" And p(ORed).TeamID = "B" Then
	  Send ("<font color=""#FF0000"" face=""Wingdings"">OOP <font face=""" + Trim(StripHiAscii(Config.FontName)) + """> " + p(RedFlag).ScrNam + " REGAINS POSSESSION OF RED TEAM'S FLAG <font face=""Wingdings"">POO")
	  'LogFileWrite (p(RedFlag).ScrNam + " regained possession of the Red Flag. The countdown was halted.")
	  TLimit = 0
	  Playwav ("unhurry")
	End If
	If p(BlueFlag).TeamID = "B" And p(OBlue).TeamID = "R" Then
	  Send ("<font color=""#0000FF"" face=""Wingdings"">OOP <font face=""" + Trim(StripHiAscii(Config.FontName)) + """> " + p(BlueFlag).ScrNam + " REGAINS POSSESSION OF BLUE TEAM'S FLAG <font face=""Wingdings"">POO")
	  'LogFileWrite (p(BlueFlag).ScrNam + " regained possession of the Blue Flag. The countdown was halted.")
	  TLimit = 0
	  Playwav ("unhurry")
	End If
	If p(RedFlag).TeamID = "B" And TLimit = 0 Then
	  TLimit = Config.FlaCon + XTimer
	  Send ("<font color=""#0000FF"" face=""Wingdings"">OOP <font face=""" + Trim(StripHiAscii(Config.FontName)) + """> " + p(RedFlag).ScrNam + " GAINS POSSESSION OF <font color=""#FF0000"">RED TEAM'S FLAG <font color=""#0000FF"" face=""Wingdings"">POO")
	  Send ("--- " + TrimStr(Config.FlaCon) + "-SECOND COUNTDOWN BEGINS ---")
	  'LogFileWrite (p(RedFlag).ScrNam + " stole the Red Flag. The " + TrimStr(Config.FlaCon) + "-second countdown began.")
	  FoundRedFlag = True
	  Playwav ("hurry")
	End If
	If p(BlueFlag).TeamID = "R" And TLimit = 0 Then
	  TLimit = Config.FlaCon + XTimer
	  Send ("<font color=""#FF0000"" face=""Wingdings"">OOP <font face=""" + Trim(StripHiAscii(Config.FontName)) + """> " + p(BlueFlag).ScrNam + " GAINS POSSESSION OF <font color=""#0000FF"">BLUE TEAM'S FLAG <font color=""#FF0000"" face=""Wingdings"">POO")
	  Send ("--- " + TrimStr(Config.FlaCon) + "-SECOND COUNTDOWN BEGINS ---")
	  'LogFileWrite (p(BlueFlag).ScrNam + " stole the Blue Flag. The " + TrimStr(Config.FlaCon) + "-second countdown began.")
	  FoundBlueFlag = True
	  Playwav ("hurry")
	End If
	If p(RedFlag).TeamID = "R" And p(BlueFlag).TeamID = "B" And TLimit <> 0 Then TLimit = 0
      End If
    Next x2
    If Config.Flag <> 0 Then
      If RedFlag < -1 Or RedFlag > MaxPlayers Then
	RedFlag = 0
	Send ("The Red Flag is Floating. (/rclaim)")
      End If
      If RedFlag > 0 And RedFlag <= MaxPlayers Then
	If p(RedFlag).HP <= 0 Or p(RedFlag).CharID = 0 Then
	  RedFlag = 0
	  Send ("The Red Flag is Floating. (/rclaim)")
	End If
      End If
      If BlueFlag < -1 Or BlueFlag > MaxPlayers Then
	BlueFlag = 0
	Send ("The Blue Flag is Floating. (/bclaim)")
      End If
      If BlueFlag > 0 And BlueFlag <= MaxPlayers Then
	If p(BlueFlag).HP <= 0 Or p(BlueFlag).CharID = 0 Then
	  BlueFlag = 0
	  Send ("The Blue Flag is Floating. (/bclaim)")
	End If
      End If
    End If
    For X = 1 To 255
      If TCaptain(X) = 0 Then
	For x2 = 1 To MaxPlayers
	  If p(x2).TeamID = Chr$(X) And p(x2).CharID > 0 Then
	    If cTeam = 1 Then Send (p(x2).ScrNam + " is now Captain of Team " + Chr$(X) + " (""" + TName(X) + """)")
	    TCaptain(X) = x2
	    Exit For
	  End If
	Next x2
      End If
      If TCaptain(X) > 0 Then
	If p(TCaptain(X)).TeamID <> Chr$(X) Or p(TCaptain(X)).CharID = 0 Then
	  TCaptain(X) = 0
	  For x2 = 1 To MaxPlayers
	    If p(x2).TeamID = Chr$(X) And p(x2).CharID > 0 Then
	      If cTeam = 1 Then Send (p(x2).ScrNam + " is now Captain of Team " + Chr$(X) + " (""" + TName(X) + """)")
	      TCaptain(X) = x2
	      Exit For
	    End If
	  Next x2
	  If TCaptain(X) = 0 Then
	    'Send ("Team " + Chr$(X) + " (""" + TName(X) + """) no longer has a Team Captain.")
	    TName(X) = ""
	  End If
	End If
      End If
    Next X
  End If
  If (Battle) Then
    For x2 = 1 To MaxPlayers
      If (p(x2).CurMove < 1) Or (p(x2).CurMove > MaxMoves) And (p(x2).Status(sInvin) > 0) Then
	If p(x2).Status(sInvin) = x2 Then p(x2).Status(sInvin) = 0
      End If
      If p(x2).Status(sInvin) > 0 Then
	x6 = p(x2).Status(sInvin)
	If (x6 > 0) And (x6 <= MaxPlayers) Then
	  If p(x6).CurMove >= 1 And p(x6).CurMove <= MaxMoves Then
	    If p(x6).Moves(p(x6).CurMove).Element <> Invin Then p(x2).Status(sInvin) = 0
	  Else
	    p(x2).Status(sInvin) = 0
	  End If
	Else
	  p(x2).Status(sInvin) = 0
	End If
      End If
    Next x2
  End If
  If (Battle) And (Not Paused) Then
    If (Rand(1, GetRate * .8) = 1) And (GetRate > 0) Then
      NewItem
    End If
    For x2 = 1 To MaxPlayers
      If (p(x2).HP > 0) And (p(x2).CharID <> 0) Then
	'If (p(x2).MP < p(x2).MaxMP) Then p(x2).MP = p(x2).MP + Arena(CurArena).GradualMP
	If (p(x2).HP < p(x2).MaxHP) Then p(x2).HP = p(x2).HP + Arena(CurArena).GradualHP
	If p(x2).Cheese > 0 Then p(x2).Cheese = Int(p(x2).Cheese - (7 * DMult / 100))
	If p(x2).Cheese < 0 Then p(x2).Cheese = 0
	Select Case p(x2).Cheese:
	  Case Is > 3000 * DMult / 100:
	    If p(x2).Status(sSleep) = 0 Then
	      Send (p(x2).ScrNam + " faints from exhaustion! (Cheese Meter too high!)")
	      AddStatus x2, sSleep
	    End If
	  Case 2000 * DMult / 100 To 2999 * DMult / 100:
	    If Rand(1, 20) = 15 Then
	      Select Case Rand(1, 5)
		Case 1: Send (p(x2).ScrNam + " is tired.")
		Case 2: Send (p(x2).ScrNam + " is beginning to wear out.")
		Case 3: Send (p(x2).ScrNam + " lets out a big yawn.")
		Case 4: Send (p(x2).ScrNam + " says, ""Phew, I'm exhausted!""")
		Case Else: Send (p(x2).ScrNam + " is getting exhausted.")
	      End Select
	    End If
	  Case 1000 * DMult / 100 To 1999 * DMult / 100:
	    If Rand(1, 50) = 15 Then Send (p(x2).ScrNam + " is getting tired.")
	End Select
      End If
    Next x2
  End If
  If (Not Battle) And (Not GodMode) Then
    For x2 = 1 To MaxPlayers
      If p(x2).CharID > HighSenshi Then
	ClearMe (x2)
      End If
      'If p(x2).CharID = 0 Then clearStatus (x2)
    Next x2
  End If
  If (Battle) And (Not Paused) Then
    For X3 = 1 To 10
      x1 = Rand(0, 1000)
      If (x1 < Arena(CurArena).Event(X3).Frequency) Then
	If (Arena(CurArena).Event(X3).ElementStr = 0) Then
	  Send (Arena(CurArena).Event(X3).Miss)
	  'LogFileWrite ("* " + Arena(CurArena).Event(x3).Miss)
	Else
	  x4 = False
	  Do
	    DoEvents
	    x2 = Rand(1, MaxPlayers + Maxyouma)
	    'If (x2 > MaxPlayers) Then
	    '  x4 = (y(x2 - MaxPlayers).Enabled And y(x2 - MaxPlayers).HP > 0)
	    'Else
	      x4 = (p(x2).HP > 0 And p(x2).CharID <> 0)
	    'End If
	  Loop Until (x4)
	  If Arena(CurArena).Event(X3).HitsAll Then
	    If (Rand(1, 3) <> 2) Then
	      Send (Parse(Arena(CurArena).Event(X3).Hit, "", "", "", ""))
	      'LogFileWrite ("* " + Parse(Arena(CurArena).Event(x3).Hit, "", "", "", ""))
	      For x4 = 1 To MaxPlayers
		If (p(x4).HP > 0 And p(x4).CharID <> 0) Then
		  p(x4).HP = p(x4).HP - Arena(CurArena).Event(X3).ElementStr
		  If p(x4).HP <= 50 Then ShowStatus x4
		End If
	      Next x4
	      'For x4 = 1 To Maxyouma
	      '  If (y(x4).Enabled And y(x4).HP > 0) Then
	      '    y(x4).HP = y(x4).HP - Arena(CurArena).Event(X3).ElementStr
	      '    If y(x4).HP <= 50 Then ShowStatus x4
	      '  End If
	      'Next x4
	    Else
	      Send (Parse(Arena(CurArena).Event(X3).Miss, "", "", "", ""))
	      'LogFileWrite ("* " + Parse(Arena(CurArena).Event(x3).Miss, "", "", "", ""))
	    End If
	  Else
	    'If x2 > MaxPlayers Then
	    '  If (Rand(1, 3) <> 2) Or (Arena(CurArena).Event(X3).ElementStr < 0) Then
	    '    Send (Parse(Arena(CurArena).Event(X3).Hit, "", "", y(x2 - MaxPlayers).Name, ""))
	    '    'LogFileWrite (Parse(Arena(CurArena).Event(x3).Hit, "", "", y(x2 - MaxPlayers).Name, ""))
	    '    y(x2 - MaxPlayers).HP = y(x2 - MaxPlayers).HP - Arena(CurArena).Event(X3).ElementStr
	    '    ShowStatus x2
	    '  Else
	    '    Send (Parse(Arena(CurArena).Event(X3).Miss, "", "", y(x2 - MaxPlayers).Name, ""))
	    '  End If
	    'Else
	      If (Rand(1, 3) <> 2) Then
		Send (Parse(Arena(CurArena).Event(X3).Hit, "", "", p(x2).ScrNam, ""))
		'LogFileWrite (Parse(Arena(CurArena).Event(x3).Hit, "", "", p(x2).ScrNam, ""))
		p(x2).HP = p(x2).HP - Arena(CurArena).Event(X3).ElementStr
		ShowStatus x2
	      Else
		Send (Parse(Arena(CurArena).Event(X3).Miss, "", "", p(x2).ScrNam, ""))
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

Sub tiMoveDmg_Timer ()
Dim x2 As Integer, X3 As Integer, y2 As Integer, Okay As Integer
Dim x4 As Integer, x7 As Integer, x6%, AlreadyShown%
Dim MPR%
Dim M As MoveType
Dim Msg As String
Dim Tix As Long
  On Error Resume Next
  If (Battle) And (Not Paused) Then
    For x2 = 1 To MaxPlayers
      If (p(x2).HP > 0) Then
	Check2RemoveStatus (x2)
	If p(x2).CurMove = 0 And (p(x2).Status(sBerserk) <> 0 Or p(x2).Status(sChaos) <> 0 Or p(x2).Status(sPMS) <> 0) Then p(x2).CurMove = -255
	If (p(x2).CurMove = -255) And (p(x2).Status(sFreeze)) Then
	  Send (p(x2).ScrNam + " is frozen.")
	  p(x2).CurMove = -50
	  p(x2).MoveStart = XTimer
	End If
	If (p(x2).CurMove = -255) And (p(x2).Status(sSleep)) Then
	  Send (p(x2).ScrNam + " is taking a nap.")
	  p(x2).CurMove = -50
	  p(x2).MoveStart = XTimer
	End If
	If (p(x2).CurMove = -255) And (p(x2).Status(sStun)) Then
	  Send (p(x2).ScrNam + " is unable to move.")
	  p(x2).CurMove = -50
	  p(x2).MoveStart = XTimer
	End If
	If p(x2).CurMove = -255 And p(x2).Status(sStop) Then
	  Send (p(x2).ScrNam + " is frozen in time.")
	  p(x2).CurMove = -50
	  p(x2).MoveStart = XTimer
	End If
	If (p(x2).CurMove = -255) Then
	  If (p(x2).Status(sChaos) <> 0) Then
	    Do
	      DoEvents
	      p(x2).CurMove = Rand(1, MaxMoves)
	    Loop Until (p(x2).Moves(p(x2).CurMove).Cmdkey <> "") And (p(x2).Moves(p(x2).CurMove).Element <> Morph)
	    M = p(x2).Moves(p(x2).CurMove)
	    'p(x2).MP = p(x2).MP + M.MPReq
	    Okay = False
	    Do
	      DoEvents
	      x4 = Rand(1, MaxPlayers)
	      If (p(x4).HP > 0) And (p(x4).CharID <> 0) Then Okay = True
	    Loop Until Okay
	    p(x2).Target = x4
	    Call DoMove(x2, p(x2).Target, p(x2).CurMove, "")
	  ElseIf (p(x2).Status(sBerserk) <> 0) Or (p(x2).Status(sPMS) <> 0) Then
	    p(x2).SuperNum = 0
	    If p(x2).Status(sPMS) <> 0 Then DispReadyLine (x2)
	    x6 = 0
	    Do
	      DoEvents
	      p(x2).CurMove = Rand(1, MaxMoves)
	      x6 = x6 + 1
	    Loop Until (p(x2).Moves(p(x2).CurMove).Cmdkey <> "" And p(x2).Moves(p(x2).CurMove).Target = Enemy) Or (x6 > 150) And (p(x2).Moves(p(x2).CurMove).Element <> Morph)
	    If x6 > 150 Then
	      Call RemoveStatus(x2, sBerserk)
	    Else
NewMPS:
	      p(x2).Target = GetTarget(p(x2).TeamID)
	      If p(p(x2).Target).Rune = RuneMagic And p(x2).Rune = RunePMS Then
		If AlreadyShown = 0 Then
		  Send (p(p(x2).Target).ScrNam + "'s Rune of Magic wards off " + p(x2).ScrNam + "'s PMS attacks.")
		  AlreadyShown = 1
		End If
		GoTo NewMPS
	      End If
	      Call DoMove(x2, p(x2).Target, p(x2).CurMove, "")
	    End If
	  ElseIf p(x2).Status(sMushroom) <> 0 Then
	    y2 = Rand(10, 50)
	    If p(x2).HP + y2 > p(x2).MaxHP Then y2 = p(x2).MaxHP - p(x2).HP
	    If y2 > 0 Then
	      Send (p(x2).ScrNam + " gets " + TrimStr(y2) + " HP from being a mushroom.")
	      'LogFileWrite (p(x2).ScrNam + " regenerated " + TrimStr(y2) + " HP from being a mushroom.")
	      p(x2).HP = p(x2).HP + y2
	    Else
	      Send (p(x2).ScrNam + " is still a mushroom.")
	    End If
	    p(x2).CurMove = -50
	    p(x2).MoveStart = XTimer
	  ElseIf p(x2).Status(sMIA) <> 0 Then
	    Send (p(x2).ScrNam + " is missing...")
	    p(x2).CurMove = -50
	    p(x2).MoveStart = XTimer
	  Else
	    DispReadyLine (x2)
	    p(x2).Target = 0
	    p(x2).CurMove = 0
	    p(x2).SuperNum = 0
	    Send5 (Msg)
	    'LogFileWrite (Msg)
	    'PlayWav ("turn")
	  End If
	End If
	Tix = (XTimer - TimeLimit)
	If (p(x2).CurMove <> 0) And (p(x2).CharID <> 0) And (p(x2).HP > 0) Then
	  x7 = (MoveHitsIn(x2) <= 0)
	  If p(x2).Status(sStop) Or p(x2).Status(sMIA) Then x7 = 0
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
	  If p(x2).CurMove > 0 And p(x2).CurMove <= MaxMoves Then
	    M = p(x2).Moves(p(x2).CurMove)
	    If (M.DestroyWeapon = 3 Or (p(x2).WpnUsesLeft = 1 And M.DestroyWeapon = 1)) And M.InstantHit Then x7 = True
	  End If
	  If Not (p(x2).Status(sStop) Or p(x2).Status(sMIA) Or p(x2).Status(sFreeze)) Then Call Check2MoveDmg(x2, x7)
	End If
      End If
    Next x2
  End If
End Sub

Sub tLag_Change ()
Dim P1$, P2$, P4$, PO%
  If (Val(tLag) > hsLag.Min) And (Val(tLag) <= hsLag.Max) Then
    hsLag = Val(tLag)
  End If
End Sub

Sub tLag_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "You can click me to change the lagometer."
End Sub

