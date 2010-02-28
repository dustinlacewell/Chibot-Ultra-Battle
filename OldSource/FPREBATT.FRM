VERSION 2.00
Begin Form fPreBattl 
   BackColor       =   &H00000040&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Milennium Anteroom"
   ClientHeight    =   2415
   ClientLeft      =   3450
   ClientTop       =   2730
   ClientWidth     =   4560
   FontBold        =   -1  'True
   FontItalic      =   0   'False
   FontName        =   "Marlett"
   FontSize        =   8.25
   FontStrikethru  =   0   'False
   FontUnderline   =   0   'False
   ForeColor       =   &H00000000&
   Height          =   2820
   Icon            =   FPREBATT.FRX:0000
   Left            =   3390
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   2415
   ScaleWidth      =   4560
   Top             =   2385
   Visible         =   0   'False
   Width           =   4680
   Begin CommandButton cBegin 
      Caption         =   "Begin!"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   1560
      TabIndex        =   26
      Top             =   960
      Width           =   735
   End
   Begin CommandButton bMoves 
      Caption         =   "&Moves"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   3000
      TabIndex        =   12
      Top             =   720
      Width           =   1455
   End
   Begin CommandButton cIMOff 
      Caption         =   "IM's Off"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   2280
      TabIndex        =   20
      Top             =   720
      Width           =   735
   End
   Begin CommandButton cIMOn 
      Caption         =   "IM's On"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   1560
      TabIndex        =   24
      Top             =   720
      Width           =   735
   End
   Begin CommandButton bAvail 
      Caption         =   "&Available Chars"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   3000
      TabIndex        =   13
      Top             =   480
      Width           =   1455
   End
   Begin CommandButton cWeapons 
      Caption         =   "Available &Weapons"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   1560
      TabIndex        =   25
      Top             =   480
      Width           =   1455
   End
   Begin CommandButton nds 
      Caption         =   "Load New &Dataset"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   120
      TabIndex        =   22
      Top             =   480
      Width           =   1455
   End
   Begin CommandButton bClear 
      Caption         =   "Clear Chars"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   3000
      TabIndex        =   6
      Top             =   240
      Width           =   1455
   End
   Begin CommandButton bEditChar 
      Caption         =   "Edit &Chars"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   1560
      TabIndex        =   8
      Top             =   240
      Width           =   1455
   End
   Begin CommandButton cRune 
      Caption         =   "&Runes"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   840
      TabIndex        =   23
      Top             =   240
      Width           =   735
   End
   Begin CommandButton cTwit 
      Caption         =   "&Twit"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   120
      MousePointer    =   1  'Arrow
      TabIndex        =   29
      Top             =   240
      Width           =   735
   End
   Begin CheckBox cInfo 
      BackColor       =   &H00000040&
      Caption         =   "/help"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3240
      TabIndex        =   28
      Top             =   960
      Value           =   1  'Checked
      Width           =   855
   End
   Begin CommandButton bOptions 
      Caption         =   "&Options"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   3000
      TabIndex        =   10
      Top             =   0
      Width           =   1455
   End
   Begin CommandButton OuttaHere 
      Caption         =   "E&xit"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   1560
      TabIndex        =   9
      Top             =   0
      Width           =   1455
   End
   Begin CommandButton bVote 
      Caption         =   "&Vote"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   840
      TabIndex        =   7
      Top             =   0
      Width           =   735
   End
   Begin CommandButton cCredz 
      Caption         =   "&Credz"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   120
      MousePointer    =   1  'Arrow
      TabIndex        =   27
      Top             =   0
      Width           =   735
   End
   Begin CommandButton cPat 
      Caption         =   "Patience"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   240
      TabIndex        =   21
      Top             =   1800
      Width           =   735
   End
   Begin Timer tiFullAuto 
      Interval        =   1000
      Left            =   120
      Top             =   3000
   End
   Begin CommonDialog cmd 
      CancelError     =   -1  'True
      Filter          =   "Init Files (*.ini)|*.ini"
      Left            =   600
      Top             =   2520
   End
   Begin HScrollBar vsMaxPlayers 
      Height          =   240
      Left            =   1200
      Max             =   80
      Min             =   2
      TabIndex        =   5
      Top             =   960
      Value           =   5
      Width           =   375
   End
   Begin TextBox tMaxPlayers 
      Height          =   285
      Left            =   1200
      MaxLength       =   2
      TabIndex        =   3
      Text            =   "15"
      Top             =   720
      Width           =   375
   End
   Begin CheckBox cMoves 
      BackColor       =   &H00000040&
      Caption         =   "/moves"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2400
      TabIndex        =   19
      Top             =   960
      Value           =   1  'Checked
      Width           =   855
   End
   Begin OptionButton oLin 
      BackColor       =   &H00000040&
      Caption         =   "4 Lines"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   4
      Left            =   3600
      TabIndex        =   18
      Top             =   1800
      Width           =   855
   End
   Begin OptionButton oLin 
      BackColor       =   &H00000040&
      Caption         =   "3 Lines"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   3
      Left            =   2760
      TabIndex        =   17
      Top             =   1800
      Width           =   855
   End
   Begin OptionButton oLin 
      BackColor       =   &H00000040&
      Caption         =   "2 Lines"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   2
      Left            =   1920
      TabIndex        =   16
      Top             =   1800
      Width           =   855
   End
   Begin OptionButton oLin 
      BackColor       =   &H00000040&
      Caption         =   "1 Line"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   1
      Left            =   1200
      TabIndex        =   15
      Top             =   1800
      Width           =   735
   End
   Begin HScrollBar hsScroll 
      Height          =   240
      LargeChange     =   100
      Left            =   1800
      Max             =   4000
      Min             =   1
      SmallChange     =   10
      TabIndex        =   14
      Top             =   1560
      Value           =   1
      Width           =   2655
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
      TabIndex        =   0
      Top             =   1200
      Width           =   4335
   End
   Begin Timer tiPreBatt 
      Interval        =   500
      Left            =   120
      Top             =   2520
   End
   Begin CommandButton bSend 
      Caption         =   "Send"
      Default         =   -1  'True
      Height          =   255
      Left            =   3840
      TabIndex        =   1
      Top             =   480
      Width           =   615
   End
   Begin Label StatLine 
      Alignment       =   2  'Center
      BackColor       =   &H00C0C0C0&
      Caption         =   "ChUB 2000"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   0
      TabIndex        =   11
      Top             =   2160
      Width           =   4575
   End
   Begin Label Label1 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Maximium # of Players:"
      FontBold        =   0   'False
      FontItalic      =   -1  'True
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   495
      Left            =   120
      TabIndex        =   4
      Top             =   720
      Width           =   1095
   End
   Begin Label Label666 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00808080&
      BackStyle       =   0  'Transparent
      Caption         =   "Scroll Delay: ????"
      FontBold        =   -1  'True
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   2
      Top             =   1560
      Width           =   1575
   End
End

' FPREBATTL.FRM
' A crucial form. Controls the Selection and Voting of players. The main control
' center for Sailor Moon K.

Option Explicit
Option Compare Text

Dim Selection As Integer
Dim Voting As Integer
Dim Runic As Integer

Const MyName = "fPreBatt"

Sub bAvail_Click ()
  ShowUnpicked
End Sub

Sub bAvail_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "List characters in dataset " + Dataset.LoadStr
End Sub

Sub bChars_Click ()
  ScrollChars
End Sub

Sub bClear_Click ()
Dim Ok As Integer
  If (Not Auto) Then Ok = (kDlgBoxfn("REALLY clear all the characters?", 36, "Clear Chars") = 6)
  If (Auto Or Ok) Then
    Send Dataset.ClearChars
    ClearChars
    UpdateWin
    Runic = 0
  End If
End Sub

Sub bClear_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Clear Characters"
End Sub

Sub bDisable_Click ()
'Dim X As Integer
'  For X = 1 To MaxYouma
'    Y(X).Enabled = False
'  Next X
'  If (fYoumaEditLoaded) Then UpdateYoumaStr
'  'SaveYouma
End Sub

Sub bEditChar_Click ()
  fCharEdit.Show
End Sub

Sub bEditChar_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Edit characters"
End Sub

Sub bEnable_Click ()
'Dim X As Integer
'  For X = 1 To MaxYouma
'    Y(X).Enabled = 1
'  Next X
'  If (fYoumaEditLoaded) Then UpdateYoumaStr
'  'SaveYouma
End Sub

Sub bMoves_Click ()
  fMoves.Show
End Sub

Sub bMoves_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Show Moves"
End Sub

Sub bOptions_Click ()
  fOption.Show
End Sub

Sub bOptions_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Change Options"
End Sub

Sub bSend_Click ()
Dim s As String
Dim Q$
  s = tChatLine.Text
  tChatLine.Text = ""
  'If (DebugWindow) Then
  '  fOffline!LSendStr.Caption = YourSN + ": " + S
  'Else
    If ((Mid$(s, 1, 1) = "/") Or Mid$(s, 1, 1) = "~") Then
      Q$ = YourSN
      GetRay(GetSave) = Q$ + ":  " + Trim(s)
      If (GetSave + 1 > MaxRay) Then
	GetSave = 1
      Else
	GetSave = GetSave + 1
      End If
    Else
      ScrollSend ("<i>" + s)
    End If
  'End If
End Sub

Sub bSend_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Click here and I'll say " + tChatLine
End Sub

Sub bTeamY_Click ()
'Dim X As Integer
'  For X = 1 To MaxYouma
'    Y(X).TeamID = "Y"
'  Next X
'  If (fYoumaEditLoaded) Then UpdateYoumaStr
'  'SaveYouma
End Sub

Sub bVote_Click ()
Dim X%, Ok%, x3%
Dim Teams%, FFA%, Normal%, Respawn%, Nodefect%, Defect%, Flag%, NoFlag%
Dim Msg$
  If (Not Voting) Then
    Voting = True
    bVote.Caption = "End &Vote"
    Send Dataset.BeginVote
    Send ("Select one option from each group -OR- /flag for Capture the Flag.")
    Send ("Group 1 -[]- <T>eams  <F>ree-For-All")
    Send ("Group 2 -[]- <N>ormal  <R>espawn")
    Send ("Group 3 -[]- <D>efects  <N>o-Defects")
    Send ("Examples: /F N D        /flag        /T N N")
    If FullAuto.Enabled Then Send ("Note: Teams is not available in Automatic Mode, FFA will always be used.")
    'Send ("VOTE: /respawn /flag /")
    For X = 1 To MaxPlayers
      Vote(X) = 0
    Next X
  Else
    Voting = False
    bVote.Caption = "&Vote"
    For x3 = 1 To MaxPlayers
      DoEvents
      If Vote(x3) = 0 Then GoTo NoVoteHere
      If (Vote(x3) = -1) Then
	Teams = Teams + 1
	Normal = Normal + 1
	Nodefect = Nodefect + 1
	NoFlag = NoFlag + 1
	GoTo NoVoteHere
      End If
      If (Vote(x3) And 1) = 1 Then
	FFA = FFA + 1
      Else
	Teams = Teams + 1
      End If
      If (Vote(x3) And 2) = 2 Then
	Respawn = Respawn + 1
      Else
	Normal = Normal + 1
      End If
      If (Vote(x3) And 4) = 4 Then
	Defect = Defect + 1
      Else
	Nodefect = Nodefect + 1
      End If
      If (Vote(x3) And 8) = 8 Then
	Flag = Flag + 1
      Else
	NoFlag = NoFlag + 1
      End If
NoVoteHere:
    Next x3
    DoEvents
    If (Flag > NoFlag) Then
      Send ("Vote Ended. Winner: Capture the Flag")
    Else
      Msg$ = "Vote Ended. Winner: "
      If (Respawn > Normal) Then
	Msg = Msg + "Respawn "
      ElseIf (Respawn = Normal) Then
	Send ("(Respawn tied with Normal)")
      End If
      If (Teams > FFA) Then
	Msg = Msg + "Teams "
      ElseIf (Teams = FFA) Then
	Send ("(Teams tied with FFA)")
      Else
	Msg = Msg + "FFA "
      End If
      If (Defect > Nodefect) Then
	Msg = Msg + "with Defects"
      ElseIf (Defect = Nodefect) Then
	Send ("(Defect tied with No-Defect)")
      Else
	Msg = Msg + "without Defects"
      End If
      If (Msg <> "Vote Ended. Winner: ") Then
	Send (Msg)
      Else
	Send ("It's a tie! Host decides!")
      End If
    End If
  End If
End Sub

Sub bVote_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Takes a vote, set voting options over to the right"
End Sub

Sub bVote_MouseUp (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 Then
    Send ("Select one option from each group -OR- /flag for Capture the Flag.")
    Send ("Group 1 -[]- <T>eams  <F>ree-For-All")
    Send ("Group 2 -[]- <N>ormal  <R>espawn")
    Send ("Group 3 -[]- <D>efects  <N>o-Defects")
    Send ("Examples: /F N D        /flag        /T N N")
  End If
End Sub

Sub cBegin_Click ()
  Form_Click
End Sub

Sub cCredz_Click ()
  fCredits.Show
End Sub

Sub cCredz_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "The people who put it all together."
End Sub

Sub cFFA_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Allow or disallow votes for FFA"
End Sub

Sub Check1_Click ()
  'Select Case Check1.Value
  '  Case 0: SetWindowPos Me.hWnd, -2, 0, 0, 0, 0, 3
  '  Case 1: SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3
  'End Select
End Sub

Sub cIgnore_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Ignore /moves commands"
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

Sub cPat_Click ()
  Send ("<font color=""#FF0000""><b><i><u>BE PATIENT!!!")
End Sub

Sub cPat_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "I'll tell everyone to be patient."
End Sub

Sub cRune_Click ()
  Send ("== Available Runes ==")
  Send ("[/haste] [/str] [/barr] [/regen] [/evade] [/luck] [/beans] [/lib] [/undivert]")
  Send ("[/magic] [/counter] [/super] [/fatal] [/weird] [/armor] [/pms] [/desp]")
  Send ("[/idiot] [/wonder] [/life] [/theft] [/death] [/learn] [/stealth] [/reflect]")
  Send ("Type /help-<rune> for info on that rune!")
End Sub

Sub cRune_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Show the list of Runes"
End Sub

Sub cTeams_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Allow or disallow votes for Teams"
End Sub

Sub cTwit_Click ()
  fTwit.Show
End Sub

Sub cWeapons_Click ()
  ShowWeapons
End Sub

Sub cWeapons_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Show a list of Weapons"
End Sub

Sub cYed_Click ()
  'Yed.Show
End Sub

Sub cYed_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Edit Youma"
End Sub

Sub cYouma_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Allow or disallow votes for Youma"
End Sub

Sub Displace (ByVal Ct As String, ByVal x2 As Integer)
' /displace-<Char ID.>-<Move Command>-<Slot #>
Dim Cid$, Cmdkey$, Slot$
Dim Cid1%, Cmdkey1%, Slot1%
Dim S4$, SN$
Dim X%, Dash%
Dim Msg$
  On Error GoTo Screwy
  SN$ = p(x2).ScrNam
  S4$ = Mid$(Ct, 11, Len(Ct) - 10)
  Dash% = 0
  For X% = 1 To Len(S4)
    If Mid$(S4, X, 1) = "-" Then
      Dash% = X
      Exit For
    End If
  Next X
  If (Dash% = 0) Then
    Send (SN + ": Invalid Character ID")
    Exit Sub
  End If
  Cid$ = Mid$(S4, 1, Dash% - 1)
  S4 = Mid$(S4, Len(Cid$) + 2, Len(S4) - Len(Cid$) + 1)
  Dash% = 0
  For X% = 1 To Len(S4)
    If Mid$(S4, X, 1) = "-" Then
      Dash% = X
      Exit For
    End If
  Next X%
  If (Dash% = 0) Then
    Send (SN + ": Invalid Move Command Key")
    Exit Sub
  End If
  Cmdkey$ = Mid$(S4, 1, Dash% - 1)
  Slot$ = Mid$(S4, Dash% + 1, Len(S4) - Dash% + 1)
  'Send ("Cid$: " + Cid$)
  'Send ("Cmdkey$: " + Cmdkey$)
  'Send ("Slot$: " + Slot$)
  Cid1% = MatchSenshi(Cid$)
  If (Cid1% = 0) Then
    Send (SN + ": Invalid Character ID")
    Exit Sub
  End If
  Cmdkey1% = 0
  For X% = 1 To MaxMoves
    If UCase(Cmdkey$) = UCase(Moves(Senshi(Cid1%).Moves(X%)).Cmdkey) Then
      Cmdkey1% = X%
      Exit For
      End If
  Next X%
  If (Cmdkey1% = 0) Then
    Send (SN + ": Invalid Move Command Key")
    Exit Sub
  End If
  Slot1% = Val(Slot$)
  If (Slot1% > MaxMoves) Or (Slot1% < 1) Then
    Send (SN + ": Invalid Slot Number")
    Exit Sub
  End If
  Msg$ = "Displacing move (/" + p(x2).Moves(Slot1%).Cmdkey + ")"
  If p(x2).Moves(Slot1%).CanSuper Then Msg$ = Msg$ + "*"
  Msg$ = Msg$ + " with " + Senshi(Cid1%).FullName + "'s (/" + Moves(Senshi(Cid1%).Moves(Cmdkey1%)).Cmdkey + ")"
  If Moves(Senshi(Cid1%).Moves(Cmdkey1%)).CanSuper Then Msg$ = Msg$ + "*"
  p(x2).Moves(Slot1%) = Moves(Senshi(Cid1%).Moves(Cmdkey1%))
  Send (Msg$)
  Exit Sub
Screwy:
  Send ("Unknown Error: Unable to displace move")
  Exit Sub
End Sub

Sub Form_Click ()
Dim Ok As Integer
  If Voting Then
    If (Not Auto) Then Ok = (kDlgBoxfn("The vote is still on. Proceed to battle anyway?", 36, "Vote Still On") = 6)
    If (Auto Or Ok) Then
      bVote_Click
    Else
      Exit Sub
    End If
  End If
  If (Not Auto) Then Ok = (kDlgBoxfn("Ready to Battle?", 36, "ChUB 2000") = 6)
  If (Auto Or Ok) Then
    ScrollSend (Dataset.EndSelect)
    Playwav ("beat")
    Unload fCharEdit
    tiPreBatt.Enabled = False
    fPreBattl.Hide
    fBattle.Show
  End If
End Sub

Sub Form_Load ()
Dim X As Integer
Dim x2 As Integer
Dim x3 As Long
Dim s As String, P2$, P4$, Po%
  On Error Resume Next
  LoadPosition Me, MyName
  P2$ = "ScrDelay"
  P4$ = String$(6, 32)
  Po% = GetPrivateProfileString("Millenium", P2$, "400", P4$, 81, "ChUB2000.ini")
  fIntro!tiOutput.Interval = Val(P4$)
  hsScroll.Value = fIntro!tiOutput.Interval
  oLin(Scra).Value = True
  Label666.Caption = "Scroll Delay: " + TrimStr(fIntro!tiOutput.Interval)
  Me.Show                                             ' Show me!
  BalancingAct
  Load fCharEdit
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3             ' Always on top
  vsMaxPlayers.Value = MaxPlayers
  tMaxPlayers.Text = TrimStr(MaxPlayers)
  tChatLine.SetFocus                                  ' Give the focus to the Chat Line
  ScrollSend (Dataset.BeginSelect)
  Playwav ("interlud")
End Sub

Sub Form_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Click the brown area to begin battle!"
End Sub

Sub Form_QueryUnload (Cancel As Integer, UnloadMode As Integer)
Dim X As Integer
Dim Msg As String
Dim Ok As Integer
  X = Int(Rnd * 22) + 1
  If FullAuto.Enabled = True Then Auto = True
  Select Case X ' Random Taunt Line
    Case 1: Msg = "Quit? ChUB? Naaahhh!"
    Case 2: Msg = "Are you sure you want to run away like a wussy?"
    Case 3: Msg = "Come on! Real winners never quit."
    Case 4: Msg = "Quitting never got anyone anywhere."
    Case 5: Msg = "Thoreau says, ""Simplify,"" but I say, ""Play more ChUB!"""
    Case 6: Msg = "I DO NOT LIKE YOU. CLICK 'YES' AND YOU WILL SUFFER A PAINFUL DEATH."
    Case 7: Msg = "I AM THE WORLD'S SMARTEST COMPUTER. I RECOMMENT YOU CLICK 'NO' FOR MAXIMUM ENJOYMENT."
    Case 8: Msg = "Don't give in to the temptations of the dark side, " + YourSN + "."
    Case 9: Msg = "Noo! I don't WANT to quit!"
    Case 10: Msg = "Kamek dislikes his disciples exiting ChUB... Click 'Yes' to exit."
    Case 11: Msg = "Are you SURE you want to quit? (Y/y)"
    Case 12: Msg = "Leave now and I'll summon Monica Lewinsky to seduce your hard drive!"
    Case 13: Msg = "Surgeon General's Warning: Clicking 'yes' can be hazardous to your health."
    Case 14: Msg = "Hypno... Hypno... Hypno... Hypno... Click No... Hypno... Hypno... Hypno..."
    Case 15: Msg = "You're not gonna catch 'em all with THAT attitude."
    Case 16: Msg = "Winners never quit!"
    Case 17: Msg = "Well, screw you guys, I'm going home then!"
    Case 18: Msg = "WARNING: Clicking 'yes' will install a virus on your system."
    Case 19: Msg = "What? You want to quit? I oughta smack you! Click 'yes' to get smacked out."
    Case 20: Msg = "BOW TO THE CSC. THE CSC IS ALMIGHTY. BOW TO THE FLAMING FORK."
    Case 21: Msg = "Don't forget to check out http://kamek.8m.com."
    Case Else: Msg = "Are you SURE you want to quit ChUB?"
  End Select
  If (Not Auto) Then Ok = (kDlgBoxfn(Msg, 36, "Quit ChUB?") = 6)
  If (Auto Or Ok) Then
    Cancel = False
  Else
    Cancel = True
  End If
End Sub

Sub Form_Unload (Cancel As Integer)
Dim X%, Po%
  SavePosition Me, MyName
  fIntro!tiOutput.Enabled = 0
  ScrollSend1 ("It's all over. ChUB 2000 has been unloaded by " + YourSN + ". <b><i><u>IT'S OFF SO STOP!!!</B></i></u>")
  If Shotgun <> "" Then ScrollSend1 (Shotgun + " will now host!")
  Po% = WritePrivateProfileString("FullAuto", "Enabled", "0", "ChUB2000.ini")
  Config.Arena = CurArena
  X = Len(Config)
  LogFileClose
  End
End Sub

Sub hsScroll_Change ()
Dim P1$, P2$, P4$, Po%
  fIntro!tiOutput.Interval = hsScroll.Value
  Label666.Caption = "Scroll Delay: " + TrimStr(fIntro!tiOutput.Interval)
  P2$ = "ScrDelay"
  P4$ = TrimStr(hsScroll.Value)
  Po% = WritePrivateProfileString("Millenium", P2$, P4$, "ChUB2000.ini")
End Sub

Sub hsScroll_Scroll ()
  hsScroll_Change
End Sub

Sub Label666_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Delay determines how fast I say stuff."
End Sub

Sub mBeginBattle_Click ()
  If Voting Then
    If kDlgBoxfn("The vote is still on. Proceed to battle anyway?", 36, "Vote Still On") = 6 Then
      bVote_Click
    Else
      Exit Sub
    End If
  End If
  Unload fCharEdit
  tiPreBatt.Enabled = False
  fPreBattl.Hide
  fBattle.Show
End Sub

Sub mEditCharacters_Click ()

End Sub

Sub mExit_Click ()
  Unload Me
End Sub

Sub nds_Click ()
Dim s$
  If kDlgBoxfn("Unload " + Dataset.LoadStr + " and load a new dataset?", 36, "Load New DS") = 6 Then
    ScrollSend ("Unloading Dataset " + Dataset.LoadStr + "...")
    ClearChars
    fIniLoad.Show
    Do
      DoEvents
    Loop Until fIniLoad.Visible = False
    ScrollSend ("ChUB2000 " + VerID + " has been re-loaded by " + YourSN + ".")
  End If
End Sub

Sub nds_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Load a different dataset"
End Sub

Sub oLin_Click (Index As Integer)
  Scra = Index
End Sub

Sub oLin_MouseMove (Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Determines how many lines I scroll every " + TrimStr(hsScroll) + " milliseconds."
End Sub

Sub OuttaHere_Click ()
  Unload Me
End Sub

Sub OuttaHere_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "What me quit? You quit!"
End Sub

Sub StatLine_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Hey, no fair peeking!"
End Sub

Sub tCCmd_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Change custom voting command"
End Sub

Sub tCDesc_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Change custom voting description"
End Sub

Sub tChatLine_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Type stuff here and click 'Send' and I'll say it!"
End Sub

Sub tiFullAuto_Timer ()
Static NextTimer
  If FullAuto.Enabled = True Then
    bVote.Enabled = False
    bEditChar.Enabled = False
    cRune.Enabled = False
    bOptions.Enabled = False
    bClear.Enabled = False
    nds.Enabled = False
    If Timer < NextTimer Then Exit Sub
    Select Case GameStage
      Case 0:
	Send ("Automatic: You have " + TrimStr(FullAuto.SelectTime / 60) + " minutes for selection.")
	'tiFullAuto.Interval = FullAuto.SelectTime * 1000
	NextTimer = Timer + FullAuto.SelectTime
	GameStage = 1
      Case 1:
	Auto = True
	bVote_Click
	Auto = False
	Send ("Automatic: You have " + TrimStr(FullAuto.VoteTime / 60) + " minutes for voting.")
	'tiFullAuto.Interval = FullAuto.VoteTime * 1000
	NextTimer = Timer + FullAuto.VoteTime
	GameStage = 2
      Case 2:
	Auto = True
	bVote_Click
	Auto = False
	Send ("Automatic: You have " + TrimStr(FullAuto.RuneTime / 60) + " minutes for rune selection.")
	'tiFullAuto.Interval = FullAuto.RuneTime * 1000
	NextTimer = Timer + FullAuto.RuneTime
	cRune_Click
	GameStage = 3
      Case 3:
	If tiFullAuto.Interval = 1000 Then
	  Send ("Warning: You have 30 seconds to make any last-minute changes!")
	  tiFullAuto.Interval = 30000
	Else
	  Send ("Time's up, prepare for battle!")
	  tiFullAuto.Enabled = False
	  GameStage = 4
	  Auto = True
	  Form_Click
	  Auto = False
	End If
    End Select
  Else
    tiFullAuto.Enabled = False
  End If
End Sub

Sub tiPreBatt_Timer ()
Dim SN, Ct, Chat As String
Dim X, x2, x3, X4, Tk As Integer
Dim X6 As Integer
Dim Tid As String
Dim Ok As Integer, P1$, P2$, P4$, Po%
Dim Msg$, Pin%, Keycode%, Cmd#, Host$, Target$, Cd%
Static Taunt%
Static Tard%
  On Error Resume Next
DoAgain:
  DoEvents
  If (GetNdx <> GetSave) Then
    Chat = GetRay(GetNdx)
    GetNdx = GetNdx + 1
    If (GetNdx > MaxRay) Then GetNdx = 1
    'If Left$(GetCt(Chat), 1) <> "/" Then GoTo DoAgain
  Else
    Exit Sub
  End If
  SN = GetSN(Chat)
  Ct = Trim(GetCt(Chat))
  DoEvents
    If (Skip(Ct, 25) = "RUBBER BABY BUGGY BUMPERS") Then
      MeGod = SN
    End If
  If Left$(Ct, 1) = "~" Then
    Msg = SN
    CommonGodCheck SN, Ct
    Decompile Ct, Pin%, Keycode%, Cmd#, Host$, Target$
    If ((Host$ = "") Or (Host$ = YourSN)) And (Pin > 999) And (SNKeyCode(SN, Pin) = Keycode) Then
      Cd = Cmd / Keycode
      Select Case Cd
	Case 8:
	  Auto = True
	  If Target$ = "VOTE" Then bVote_Click
	  If Target$ = "AVAILABLE" Then bAvail_Click
	  If Target$ = "OUTTAHERE" Then OuttaHere_Click
	  If Target$ = "BATTLE" Then Form_Click
	  If Target$ = "CLEAR" Then bClear_Click
	  Auto = False
      End Select
    End If
    If (Skip(Ct, 7) = "GIVE ME") Then
      If Len(Ct) > 9 Then
	X4 = MatchSenshi(Right$(Ct, Len(Ct) - 9))
      Else
	X4 = 0
      End If
      If (X4 <> 0) Then
	Ct = "/" + Senshi(X4).PickMe
      End If
    End If
  End If
  If Skip(Ct, 12) = "RELEASEINFO" Then
    ReleaseInfo
  End If
  If Skip(Ct, 7) = "SHOTGUN" Or Skip(Ct, 8) = "NEXTHOST" Or Skip(Ct, 8) = "HOSTNEXT" Then ShotgunCall (SN)
  If Skip(Ct, 3) = "MOO" And (SN = "IM1BIGTard" Or SN = "AlienCowz" Or SN = "Senor Cow") Then
    Tard = Tard + 1
    Select Case Tard
      Case 1: Send ("Uhh... Mooing is bad.")
      Case 2: Send ("Knock off the stupid mooing!!")
      Case 3: Send ("I'm warning you! Stop mooing!")
      Case 4: Send ("That does it! You're TOSed for harassing me with your stupid mooing!")
    End Select
  End If
  If (Skip(Ct, 4) = "LIST") Or (Skip(Ct, 9) = "AVAILABLE") Or (Skip(Ct, 9) = "AVAILIBLE") Or (Skip(Ct, 10) = "CHARACTERS") Or (UCase(Ct) = "/WHO") Or (Skip(Ct, 6) = "JENOVA") Then
    Taunt% = Taunt% + 1
    Select Case Taunt
      'Case 1: Send ("You idiot, " + SN + ", that's <b>not</b> a command.")
      'Case 2: Send ("That's not a command either, " + SN + ".")
      'Case 3: Send ("OK, you need to stop that now.")
      'Case 4: Send ("STOP PUSHING " + UCase(Ct) + "!!!")
      'Case 5: Send ("If you push " + Ct + " again, I am going to be Very Angry.")
      'Case 6: Send ("OK, fine. See if I care. Push " + Ct + " all you want.")
      Case 1: Send ("Forget it " + SN + ", that's not gonna work.")
    End Select
    GoTo DoAgain
  End If
  If (Skip(Ct, 5) = "MOVES") And (cMoves.Value = 1) And Len(Ct) > 7 Then ShowMoves (Mid$(Ct, 8, Len(Ct) - 7))
  If Skip(Ct, 7) = "VERSION" Then
    Send ("ChUB 2000 Version " + VerID + ". Dataset: " + Dataset.LoadStr)
  End If
  If (SNPlayer(SN) <> 0) Then
    DoEvents
    X = SNPlayer(SN)
    If (Skip(Ct, 4) = "OOPS") Or (Skip(Ct, 6) = "REMOVE") Then
      If (X <> 0) Then
	Send (Parse(Dataset.Remove, SN, Senshi(p(X).CharID).FullName, "", ""))
	p(X).CharID = 0
	p(X).ScrNam = "???"
	p(X).Rune = 0
	p(X).Weapon = 0
	Vote(X) = 0
	UpdateWin
      End If
    End If
    If (Skip(Ct, 4) = "OPPS") Then
    ' For those numskulls who can't spell ^_^
      If (X <> 0) Then
	Send ("YOU FOOL! IT'S OOPS! O-O-P-S! You might have gotten away with /opps in the past, but never again!")
	Send ("If you want to oops, do /oops and SPELL IT RIGHT!")
	'Send (SN + ", you spelled 'oops' wrong, but I get the idea. You're removed.")
	'p(X).CharID = 0
	'p(X).ScrNam = "???"
	'UpdateWin
      End If
    End If
    If Skip(Ct, 4) = "DROP" And p(X).Rune <> 0 Then
      Send (SN + " drops the " + RuneName(p(X).Rune) + " and it vanishes in a puff of smoke.")
      p(X).Rune = 0
    End If
  End If
  If (Skip(Ct, 6) = "RANDOM") Or (Skip(Ct, 4) = "JOIN") Then
    Do
      X = Rand(1, NumSenshi)
    Loop Until (Not Taken(X) Or Config.SameChar) And (Senshi(X).PickMe <> "")
    Send (Parse(Dataset.Random, SN, Senshi(X).FullName, "", ""))
    Ct = "/" + Senshi(X).PickMe
  End If
  For X = 1 To NumSenshi
    DoEvents
    If (UCase(Trim(StripHiAscii(Ct)))) = ("/" + UCase(Senshi(X).PickMe)) And (Senshi(X).PickMe <> "") Then
      Ok = True
      x3 = SNPlayer(SN)
      If x3 <> 0 Then
	Send ("<*> " + SN + ", you're already " + Senshi(p(x3).CharID).FullName + "!! {S WellDuh!}")
	Exit For
      End If
      'If (Not Ok) Then Exit For
      If (Config.NoJoin) Then
	Send ("Oops! Can't join right now. " + Config.Reason)
	Exit For
      End If
      X4 = -1
      For x2 = 1 To MaxPlayers
	If p(x2).CharID = 0 Then
	  X4 = x2
	  Exit For
	End If
      Next x2
      If (X4 <= 0) Or (X4 > Val(tMaxPlayers.Text)) Then
	Send ("Oops! " + SN + ", the game is full (" + tMaxPlayers.Text + " people already!).")
	Exit For
      End If
      Tk = False
      If (Config.SameChar = 0) Then
	For x2 = 1 To MaxPlayers
	  If p(x2).CharID = X Then Tk = x2
	Next x2
	If (Tk) Then
	  Send (Parse(Dataset.Taken, SN, Senshi(X).FullName, p(Tk).ScrNam, ""))
	  Exit For
	End If
      End If
      P1$ = SN
      P2$ = "Wins"
      P4$ = String$(6, 32)
      Po% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
      p(X4).Wins = Val(P4$)
      P1$ = SN
      P2$ = "Losses"
      P4$ = String$(6, 32)
      Po% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
      p(X4).Losses = Val(P4$)
      ScrollSend (Parse(Senshi(X).SelectStr + " (" + TrimStr(Senshi(X).Wins) + " kills, " + TrimStr(Senshi(X).Losses) + " deaths)", SN + " (" + TrimStr(p(X4).Wins) + " kills, " + TrimStr(p(X4).Losses) + " deaths)", "", "", ""))
      p(X4).ScrNam = SN
      p(X4).CharID = X
      p(X4).TeamID = Chr$(X4 + 64)
      p(X4).MaxHP = MaxHP
      'p(X4).MaxMP = MaxMP
      For X6 = 1 To MaxMoves - 1
	p(X4).Moves(X6) = Moves(Senshi(X).Moves(X6))
      Next X6
      p(X4).PhysStr = Senshi(X).PhysStr
      p(X4).PhysDef = Senshi(X).PhysDef
      p(X4).MagStr = Senshi(X).MagStr
      p(X4).MagDef = Senshi(X).MagDef
      p(X4).CPU = 0
      UpdateWin
      Exit Sub
    End If
  Next X
  x2 = Playernum(SN)
  For X = 1 To MaxWeapons
    DoEvents
    If (UCase(Trim(StripHiAscii(Ct)))) = ("/" + UCase(Weapons(X).PickMe)) And (Weapons(X).PickMe <> "") Then
      If x2 = 0 Then
	Send (SN + ", you need to join before picking a weapon.")
      Else
	If Config.OldSchool Then GoTo OldSkewl
	If p(x2).Weapon <> 0 Then Send (p(x2).ScrNam + " drops the " + Weapons(p(x2).Weapon).Name + ".")
	Send (Parse(Weapons(X).SelectStr, SN, "", "", ""))
	p(x2).Weapon = X
	'p(x2).WeaponState = 1
	Exit Sub
      End If
    End If
  Next X
  If Voting Then
    DoEvents
    'For X = 1 To MaxPlayers
    '  If p(X).CharID = 0 Then Vote(X) = 0
    'Next X
    X = Playernum(SN)
    If X <> 0 Then
      x2 = -99
      If Skip(Ct, 4) = "FLAG" Then x2 = 8
      If Skip(Ct, 6) = "T N N" Then x2 = -1
      If Skip(Ct, 6) = "F N N" Then x2 = 1
      If Skip(Ct, 6) = "T R N" Then x2 = 2
      If Skip(Ct, 6) = "F R N" Then x2 = 3
      If Skip(Ct, 6) = "T N D" Then x2 = 4
      If Skip(Ct, 6) = "F N D" Then x2 = 5
      If Skip(Ct, 6) = "T R D" Then x2 = 6
      If Skip(Ct, 6) = "F R D" Then x2 = 7
      DoEvents
      If x2 <> -99 Then
	If Vote(X) <> 0 Then
	  Vote(X) = x2
	  Send (SN + ", your vote was changed. [" + TrimStr(x2) + "]")
	ElseIf x2 <> 0 Then
	  Vote(X) = x2
	  Send (SN + ", your vote has been recorded. [" + TrimStr(x2) + "]")
	End If
	DoEvents
      End If
    End If
  End If
  DoEvents
  X = Playernum(SN)
  If (((Skip(Ct, 4) = "INFO") Or Skip(Ct, 4) = "HELP" And Len(Ct) > 6) Or (Skip(Ct, 1) = "?" And Len(Ct) > 3)) And cInfo = 1 Then
    'Send ("""" + Ct + """?!? What do you think this is, " + SN + "? ChUB Talismans?")
    If Skip(Ct, 1) <> "?" Then
      Tid = Right$(Ct, Len(Ct) - 6)
    Else
      Tid = Right$(Ct, Len(Ct) - 3)
    End If
    ShowInfo (Tid)
  End If
  'Exit Sub
  'If ((Skip(Ct, 4) = "HELP" And Len(Ct) > 6) And cInfo = 1) Then
  '  Tid = Right$(Ct, Len(Ct) - 6)
  '  ShowInfo (Tid)
  'End If
  If Skip(Ct, 5) = "HASTE" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneHaste
    Send (SN + " has chosen the Rune of Haste.")
  ElseIf Skip(Ct, 3) = "STR" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneStr
    Send (SN + " has chosen the Rune of Strength.")
  ElseIf Skip(Ct, 4) = "BARR" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneBarr
    Send (SN + " has chosen the Rune of Barriers.")
  ElseIf Skip(Ct, 5) = "REGEN" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneRegen
    Send (SN + " has chosen the Rune of Regeneration.")
  ElseIf Skip(Ct, 4) = "LUCK" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneLuck
    Send (SN + " has chosen the Rune of Luckiness.")
  ElseIf Skip(Ct, 4) = "LIFE" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneLife
    Send (SN + " has chosen the Rune of Life.")
    p(X).MaxHP = Int(MaxHP * .25) + MaxHP
  ElseIf Skip(Ct, 5) = "EVADE" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneEvade
    Send (SN + " has chosen the Rune of Evasion.")
  ElseIf Skip(Ct, 5) = "MAGIC" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneMagic
    Send (SN + " has chosen the Rune of Magic.")
  ElseIf Skip(Ct, 7) = "COUNTER" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneBlock
    Send (SN + " has chosen the Rune of Counterattacks.")
  ElseIf Skip(Ct, 5) = "SUPER" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneSuper
    Send (SN + " has chosen the Rune of Rage.")
  ElseIf Skip(Ct, 5) = "FATAL" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneFatal
    Send (SN + " has chosen the Rune of Fatalities.")
  ElseIf Skip(Ct, 5) = "WEIRD" Or Skip(Ct, 5) = "WIERD" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneWeird
    Send (SN + " has chosen the Rune of Weirdness.")
  ElseIf Skip(Ct, 4) = "HIGH" Or Skip(Ct, 5) = "IDIOT" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneHigh
    Send (SN + " has chosen the Idiot Killer.")
  ElseIf Skip(Ct, 6) = "WONDER" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneWonder
    Send (SN + " has chosen the WonderRune.")
  ElseIf Skip(Ct, 5) = "THEFT" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneTheft
    Send (SN + " has chosen the Rune of Theft.")
  ElseIf Skip(Ct, 6) = "CHARGE" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    'p(X).Rune = RuneCharge
    'Send (SN + " has chosen the Rune of Charging. /charge-move to charge it.")
    Send (SN + ", we seem to be out of stock on that Rune.")
  ElseIf Skip(Ct, 5) = "VIRUS" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    'p(X).Rune = RuneVirus
    'Send (SN + " has picked the Rune of Virii.")
    Send ("Funny, " + SN + ", it looks like we're out of stock on that Rune.")
  ElseIf Skip(Ct, 7) = "STEALTH" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneStealth
    Send (SN + " has chosen the Rune of Stealth. Did I mention you can't target with this rune?")
  ElseIf Skip(Ct, 5) = "ARMOR" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneArmor
    Send (SN + " has chosen the Elemental Armor.")
  ElseIf Skip(Ct, 7) = "REFLECT" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneReflect
    Send (SN + " has chosen the Rune of Reflection.")
  ElseIf Rot13(Skip(Ct, 33)) = "1%D`7M`70;`7(aN`<7a'7M%WV7LaW0<aC" Then
    ' /give me the power of mind control
    If X = 0 Then Exit Sub
    If p(X).Rune <> 0 Then Exit Sub
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneJedi
    Send (SN + " found a secret Rune, the Rune of Jedi Mind Control!")
  ElseIf Rot13(Skip(Ct, 20)) = "(%98L;:7%7L;aa&`7ba:" Then
    ' /pikachu i choose you
    If X = 0 Then Exit Sub
    If p(X).Rune <> 0 Then Exit Sub
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RunePikachu
    Send (SN + " found a secret Rune, the Rune of Pikachu!")
  ElseIf Rot13(Skip(Ct, 17)) = "%7N8W070a7B`7V`8V" Then
    ' /i want to be dead
    If X = 0 Then Exit Sub
    If p(X).Rune <> 0 Then Exit Sub
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneZombie
    Send (SN + " found a secret Rune, the Rune of the Undead!")
  ElseIf Rot13(Skip(Ct, 17)) = "M89`7M`7%WD%&%BC`" Then
    ' /make me invisible
    If X = 0 Then Exit Sub
    If p(X).Rune <> 0 Then Exit Sub
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneShadows
    Send (SN + " found a secret Rune, the Rune of Shadows!")
  ElseIf Rot13(Skip(Ct, 24)) = "%78M70;`7<`8C7M<@7'<``)`" Then
    ' /i am the real mr. freeze
    If X = 0 Then Exit Sub
    If p(X).Rune <> 0 Then Exit Sub
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneIce
    Send (SN + " found a secret Rune, the Rune of Ice!")
  ElseIf Rot13(Skip(Ct, 22)) = "V8MW70;%&7Ba07%&7B:11b" Then
    ' /damn this bot is buggy
    If X = 0 Then Exit Sub
    If p(X).Rune <> 0 Then Exit Sub
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneBug
    Send (SN + " found a secret Rune, the Rune of ChUB Bugs!")
  ElseIf Skip(Ct, 5) = "BEANS" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneBeans
    Send (SN + " gobbles down a Can of Pork 'n Beans... stay away!")
  ElseIf Skip(Ct, 3) = "PMS" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RunePMS
    Send (SN + " has chosen the Rune of PMS! {S pmsing2}")
  ElseIf Skip(Ct, 3) = "LIB" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneCrit
    Send (SN + " has chosen the Rune of Liberation.")
  ElseIf Skip(Ct, 4) = "DESP" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneDesp
    Send (SN + " has chosen the Rune of Desperation.")
  ElseIf Skip(Ct, 8) = "UNDIVERT" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneUndivert
    Send (SN + " has chosen the Rune of Undivertion.")
    'Send (SN + ", that Rune is not available at this time.")
  ElseIf Skip(Ct, 8) = "DEATH" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneDeath
    Send (SN + " has chosen the Death Blossom.")
  ElseIf Skip(Ct, 8) = "LEARN" Then
    If X = 0 Then GoTo NotJoined
    If p(X).Rune <> 0 Then GoTo AlreadyRunic
    If Config.OldSchool Then GoTo OldSkewl
    p(X).Rune = RuneLearn
    Send (SN + " has chosen the Rune of Knowledge.")
  ElseIf Skip(Ct, 5) = "GRAIL" Or Skip(Ct, 5) = "SPACE" Or Skip(Ct, 6) = "GARNET" Or Skip(Ct, 3) = "SSJ" Or Skip(Ct, 6) = "SUMMON" Or Skip(Ct, 6) = "ABSORB" Or Skip(Ct, 4) = "YING" Or Skip(Ct, 4) = "YANG" Or Skip(Ct, 8) = "REDIVERT" Or Skip(Ct, 8) = "THIEVERY" Or Skip(Ct, 6) = "ACQUIRE" Or Skip(Ct, 4) = "KRUZ" Then
    Send (SN + ", this is RUNES, not TALISMANS. There is no " + Ct + " rune. Try again.")
  End If
  Exit Sub
AlreadyRunic:
  Send (SN + ", you have already picked the " + RuneName(p(X).Rune) + ". To pick another rune, type /drop.")
  Exit Sub
NotJoined:
  Send (SN + ", you need to join before you can pick a rune... DUH!!! {S WellDuh!}")
  Exit Sub
OldSkewl:
  Send (SN + ", weapons and runes are DISABLED in OldSchool Kombat mode.")
  Exit Sub
Fucked:
  ErrorBox Error$, 0, "tiPreBatt"
  Stop
End Sub

Sub tMaxPlayers_LostFocus ()
Dim X As Integer, P1$, P2$, P4$, Po%
  X = Val(tMaxPlayers.Text)
  If (X >= vsMaxPlayers.Min) And (X <= vsMaxPlayers.Max) Then
    vsMaxPlayers.Value = X
  Else
    tMaxPlayers.Text = TrimStr(vsMaxPlayers.Value)
  End If
  ReDim Preserve p(vsMaxPlayers.Value)
  ReDim Preserve Vote(vsMaxPlayers.Value)
  MaxPlayers = vsMaxPlayers.Value
  P1$ = "Millenium"
  P2$ = "MaxPlayers"
  P4$ = TrimStr(MaxPlayers)
  Po% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  UpdateWin
End Sub

Sub tMaxPlayers_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Change the maximum number of people that can play."
End Sub

Sub vsMaxPlayers_Change ()
Dim P1$, P2$, P4$, Po%
  tMaxPlayers.Text = TrimStr(vsMaxPlayers.Value)
  ReDim Preserve p(vsMaxPlayers.Value)
  ReDim Preserve Vote(vsMaxPlayers.Value)
  MaxPlayers = vsMaxPlayers.Value
  P1$ = "Millenium"
  P2$ = "MaxPlayers"
  P4$ = TrimStr(MaxPlayers)
  Po% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  UpdateWin
End Sub

