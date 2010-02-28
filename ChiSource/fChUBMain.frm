VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Begin VB.MDIForm fChUBMain 
   BackColor       =   &H00C0C0C0&
   Caption         =   "Chibot Ultra Battle 2000 Special Edition"
   ClientHeight    =   3270
   ClientLeft      =   165
   ClientTop       =   735
   ClientWidth     =   7620
   Icon            =   "fChUBMain.frx":0000
   LinkTopic       =   "MDIForm1"
   StartUpPosition =   3  'Windows Default
   Begin ComctlLib.Toolbar toolChUBMain 
      Align           =   1  'Align Top
      Height          =   660
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   7620
      _ExtentX        =   13441
      _ExtentY        =   1164
      ButtonWidth     =   1032
      ButtonHeight    =   1005
      Appearance      =   1
      ImageList       =   "ilChUBMain"
      _Version        =   327682
      BeginProperty Buttons {0713E452-850A-101B-AFC0-4210102A8DA7} 
         NumButtons      =   13
         BeginProperty Button1 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.ToolTipText     =   "Exit ChUB SE"
            Object.Tag             =   ""
            ImageIndex      =   1
         EndProperty
         BeginProperty Button2 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.ToolTipText     =   "Load Dataset"
            Object.Tag             =   ""
            ImageIndex      =   2
         EndProperty
         BeginProperty Button3 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.ToolTipText     =   "Options"
            Object.Tag             =   ""
            ImageIndex      =   3
         EndProperty
         BeginProperty Button4 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.ToolTipText     =   "Selection Open/Close"
            Object.Tag             =   ""
            ImageIndex      =   4
         EndProperty
         BeginProperty Button5 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.ToolTipText     =   "Vote Begin/End"
            Object.Tag             =   ""
            ImageIndex      =   6
         EndProperty
         BeginProperty Button6 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.ToolTipText     =   "Edit Characters"
            Object.Tag             =   ""
            ImageIndex      =   5
         EndProperty
         BeginProperty Button7 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.ToolTipText     =   "Clear Characters"
            Object.Tag             =   ""
            ImageIndex      =   14
         EndProperty
         BeginProperty Button8 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.ToolTipText     =   "Runes"
            Object.Tag             =   ""
            ImageIndex      =   7
         EndProperty
         BeginProperty Button9 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.ToolTipText     =   "Weapons"
            Object.Tag             =   ""
            ImageIndex      =   8
         EndProperty
         BeginProperty Button10 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.ToolTipText     =   "Battle Begin/End"
            Object.Tag             =   ""
            ImageIndex      =   10
         EndProperty
         BeginProperty Button11 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.ToolTipText     =   "Battle Pause/Unpause"
            Object.Tag             =   ""
            ImageIndex      =   11
         EndProperty
         BeginProperty Button12 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.ToolTipText     =   "Game Stats"
            Object.Tag             =   ""
            ImageIndex      =   12
         EndProperty
         BeginProperty Button13 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.ToolTipText     =   "NeoStatus Window"
            Object.Tag             =   ""
            ImageIndex      =   13
         EndProperty
      EndProperty
   End
   Begin VB.Timer tiAuto 
      Enabled         =   0   'False
      Left            =   4680
      Top             =   960
   End
   Begin VB.Timer tmrPing 
      Interval        =   30000
      Left            =   6600
      Top             =   960
   End
   Begin VB.Timer tYoshiStart 
      Enabled         =   0   'False
      Interval        =   2000
      Left            =   5160
      Top             =   960
   End
   Begin VB.Timer tKamekStart 
      Enabled         =   0   'False
      Interval        =   2000
      Left            =   5640
      Top             =   960
   End
   Begin VB.Timer tiMoveDmg 
      Interval        =   100
      Left            =   5160
      Top             =   1440
   End
   Begin ComctlLib.StatusBar sbChUBMain 
      Align           =   2  'Align Bottom
      Height          =   255
      Left            =   0
      TabIndex        =   1
      Top             =   3015
      Width           =   7620
      _ExtentX        =   13441
      _ExtentY        =   450
      Style           =   1
      SimpleText      =   ""
      _Version        =   327682
      BeginProperty Panels {0713E89E-850A-101B-AFC0-4210102A8DA7} 
         NumPanels       =   1
         BeginProperty Panel1 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            Key             =   ""
            Object.Tag             =   ""
         EndProperty
      EndProperty
   End
   Begin VB.Timer tiCPU 
      Interval        =   750
      Left            =   5640
      Top             =   1440
   End
   Begin VB.Timer tiMisc 
      Interval        =   2000
      Left            =   6120
      Top             =   1440
   End
   Begin VB.Timer CTimer 
      Interval        =   1000
      Left            =   6600
      Top             =   1440
   End
   Begin VB.Timer tiAOLInput 
      Interval        =   500
      Left            =   7080
      Top             =   1440
   End
   Begin VB.Timer tiOutput 
      Interval        =   400
      Left            =   7080
      Top             =   960
   End
   Begin VB.Timer tiInput 
      Interval        =   800
      Left            =   6120
      Top             =   960
   End
   Begin ComctlLib.ImageList ilChUBMain 
      Left            =   2760
      Top             =   1320
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   32
      ImageHeight     =   32
      MaskColor       =   12632256
      _Version        =   327682
      BeginProperty Images {0713E8C2-850A-101B-AFC0-4210102A8DA7} 
         NumListImages   =   14
         BeginProperty ListImage1 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "fChUBMain.frx":0CCA
            Key             =   ""
         EndProperty
         BeginProperty ListImage2 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "fChUBMain.frx":0FE4
            Key             =   ""
         EndProperty
         BeginProperty ListImage3 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "fChUBMain.frx":12FE
            Key             =   ""
         EndProperty
         BeginProperty ListImage4 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "fChUBMain.frx":1618
            Key             =   ""
         EndProperty
         BeginProperty ListImage5 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "fChUBMain.frx":1932
            Key             =   ""
         EndProperty
         BeginProperty ListImage6 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "fChUBMain.frx":1C4C
            Key             =   ""
         EndProperty
         BeginProperty ListImage7 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "fChUBMain.frx":1F66
            Key             =   ""
         EndProperty
         BeginProperty ListImage8 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "fChUBMain.frx":2280
            Key             =   ""
         EndProperty
         BeginProperty ListImage9 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "fChUBMain.frx":259A
            Key             =   ""
         EndProperty
         BeginProperty ListImage10 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "fChUBMain.frx":28B4
            Key             =   ""
         EndProperty
         BeginProperty ListImage11 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "fChUBMain.frx":2BCE
            Key             =   ""
         EndProperty
         BeginProperty ListImage12 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "fChUBMain.frx":2EE8
            Key             =   ""
         EndProperty
         BeginProperty ListImage13 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "fChUBMain.frx":3202
            Key             =   ""
         EndProperty
         BeginProperty ListImage14 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "fChUBMain.frx":351C
            Key             =   ""
         EndProperty
      EndProperty
   End
   Begin VB.Menu mnuFile 
      Caption         =   "&File"
      Begin VB.Menu mnuOpenDS 
         Caption         =   "&Open Dataset..."
         Shortcut        =   ^O
      End
      Begin VB.Menu mnuSep 
         Caption         =   "-"
      End
      Begin VB.Menu mnuOptions 
         Caption         =   "O&ptions..."
         Shortcut        =   {F5}
      End
      Begin VB.Menu mnuSep1 
         Caption         =   "-"
      End
      Begin VB.Menu mnuExit 
         Caption         =   "E&xit"
         Shortcut        =   ^X
      End
   End
   Begin VB.Menu mnuSelection 
      Caption         =   "&Selection"
      Begin VB.Menu mnuOpenSelection 
         Caption         =   "&Open"
         Shortcut        =   ^S
      End
      Begin VB.Menu mnuAvail 
         Caption         =   "&Available Chars"
         Shortcut        =   ^A
      End
      Begin VB.Menu mnuClear 
         Caption         =   "&Clear Chars"
         Shortcut        =   ^Z
      End
      Begin VB.Menu mnuVote 
         Caption         =   "&Vote"
         Shortcut        =   ^V
      End
      Begin VB.Menu sep293 
         Caption         =   "-"
      End
      Begin VB.Menu mnuEditChar 
         Caption         =   "&Edit Chars"
         Shortcut        =   ^E
      End
      Begin VB.Menu mnuSep2 
         Caption         =   "-"
      End
      Begin VB.Menu mnuRunes 
         Caption         =   "&Runes"
         Shortcut        =   ^R
      End
      Begin VB.Menu mnuWeapons 
         Caption         =   "&Weapons"
         Shortcut        =   ^W
      End
   End
   Begin VB.Menu mnuBattle 
      Caption         =   "&Battle"
      Begin VB.Menu mnuBeginBattle 
         Caption         =   "&Begin"
         Shortcut        =   ^B
      End
      Begin VB.Menu mnuPauseBattle 
         Caption         =   "&Pause"
         Shortcut        =   ^P
      End
      Begin VB.Menu mnuStats 
         Caption         =   "&Stats"
         Shortcut        =   ^T
      End
      Begin VB.Menu mnuNeoStat 
         Caption         =   "&Character Info"
         Shortcut        =   ^N
      End
   End
   Begin VB.Menu mnuInfo 
      Caption         =   "S&how"
      Begin VB.Menu mnuTeams 
         Caption         =   "&Teams"
      End
      Begin VB.Menu mnuChars 
         Caption         =   "&Chars"
      End
      Begin VB.Menu mnuMoves 
         Caption         =   "&Moves"
      End
   End
   Begin VB.Menu mnuExtra 
      Caption         =   "E&xtra"
      Begin VB.Menu mnuMidi 
         Caption         =   "&Juke Box"
         Shortcut        =   ^J
      End
      Begin VB.Menu mnuRants 
         Caption         =   "&Rants"
      End
      Begin VB.Menu mnuMyst 
         Caption         =   "Mystery Menu"
         Begin VB.Menu mnuMystOption 
            Caption         =   "Mystery Menu Option"
            Shortcut        =   %{BKSP}
         End
      End
   End
   Begin VB.Menu mnuHelp 
      Caption         =   "&Help"
      Begin VB.Menu mnuAbout 
         Caption         =   "&About..."
      End
      Begin VB.Menu mnuNUT 
         Caption         =   "New User Tips (&Yoshi)"
      End
      Begin VB.Menu mnuTip 
         Caption         =   "&Tip of the Day on startup"
      End
   End
   Begin VB.Menu mnuTray 
      Caption         =   "&TrayMenu"
      Visible         =   0   'False
   End
End
Attribute VB_Name = "fChUBMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Compare Text
Option Explicit

Const MyName = "fChUBMain"
Const TrayhWnd = 1728

Dim BootSN$(), BootCt%
Dim Toggles As Form
Dim Six%, Voting%, AllSlot%
Dim AutoState%

Private Sub ChecktheGods()
Dim P1$, P2$, P4$, PO%, x%
  P1$ = "Not Implemented"
  P2$ = "W5G7%e?[5e5+G5+"
  If Rot13(P1$) <> P2$ Then
    kDlgBox "Syntax error", 16, "Error"
    End
  End If
End Sub

Private Sub CloseTooLong()
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
  Crap = SendMessage(MDI, WM_CLOSE, 0, 0)
  Exit Sub
Else
  If MDI = GetWindow(MDI, GW_HWNDLAST) Then Exit Sub
  MDI = GetNextWindow(B, 2)
End If
GoTo Loop999
End Sub

Private Sub CloseManip()
Dim AOL%, MDI%, B%
Dim C$
Dim Crap%

AOL = FindWindow("ThunderRT5Form", 0&)
If AOL = 0 Then Exit Sub
B = AOL
Loop1:
C$ = GetCaption(B)
If Rot13(C$) = "(5Q5e5+7([O+G=Qe7L35OG53" Then
  Crap = SendMessage(B, WM_CLOSE, 0, 0)
  Send ("<---- is a BIG Cheater!")
  kDlgBox "Stop cheating!", 16, "Naughty Naughty!"
  Exit Sub
Else
  If B = GetWindow(B, GW_HWNDLAST) Then Exit Sub
  B = GetNextWindow(B, 2)
End If
GoTo Loop1
End Sub

Private Sub CTimer_Timer()
Dim x4 As Integer, Tix!
  If (Battle And Not Paused) Then
    XTimer = XTimer + 1
    Tix = (XTimer - TimeLimit)
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
    End Select
    UpdateGameStat
  End If
End Sub

Private Sub MDIForm_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Me.Tag = "IsTray" Then Call TrayClick(Me, TrayhWnd, Button, Shift, x, Y)
End Sub

Private Sub MDIForm_QueryUnload(Cancel As Integer, UnloadMode As Integer)
Dim x As Integer
Dim Msg As String
Dim OK As Integer
  x = Int(Rnd * 22) + 1
  'If FullAuto.Enabled = True Then Auto = True
  Select Case x ' Random Taunt Line
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
    Case 13: Msg = "Surgeon General's Warning: Clicking 'yes' can be hazardous to your sanity."
    Case 14: Msg = "Hypno... Hypno... Hypno... Hypno... Click No... Hypno... Hypno... Hypno..."
    Case 15: Msg = "You're not gonna catch 'em all with THAT attitude."
    Case 16: Msg = "Call us back? Whatever..."
    Case 17: Msg = "Well, screw you guys, I'm going home then!"
    Case 18: Msg = "WARNING: Clicking 'yes' will install a virus on your system."
    Case 19: Msg = "What? You want to quit? I oughta smack you! Click 'yes' to get smacked out."
    Case 20: Msg = "BOW TO THE CSC. THE CSC IS ALMIGHTY. BOW TO THE FLAMING FORK."
    Case 21: Msg = "Don't forget to check out http://kkastle.cjb.net."
    Case Else: Msg = "Are you SURE you want to quit ChUB?"
  End Select
  If (Not Auto) And (GlobalTerm = 0) Then OK = (kDlgBoxfn(Msg, 36, "Quit ChUB?") = 6)
  If (Auto Or OK Or GlobalTerm) Then
    Cancel = False
  Else
    Cancel = True
  End If
End Sub

Private Sub MDIForm_Resize()
  'If Me.WindowState = 1 And Me.Tag <> "IsTray" Then
  '  Me.Hide
  '  TrayNotify TrayhWnd, Me, "ChUB Resurrection"
  '  Me.Tag = "IsTray"
  'ElseIf Me.Tag = "IsTray" Then
  '  TrayRemove TrayhWnd
  '  Me.Tag = ""
  'End If
End Sub

Private Sub MDIForm_Unload(Cancel As Integer)
Dim x%, PO%
  SavePosition Me, MyName
  SavePosition fChatLine, "fChatLine"
  'SavePosition fOffline, "fOffline"
  Unload fMidi
  tiOutput.Enabled = 0
  ToChUBBot ("CHUBQUIT")
  ScrollSend1 ("It's all over. ChUB Resurrection has been unloaded by " + YourSN + ". " + ccBold + ccUnderline + ccReverse + "IT'S OFF SO STOP!!!")
  If Shotgun <> "" Then ScrollSend1 (Shotgun + " will now host!")
  'PO% = WritePrivateProfileString("FullAuto", "Enabled", "0", "ChUB2000.ini")
  Config.Arena = CurArena
  x = Len(Config)
  LogFileClose
  'MsgBox "I am now ending... Goodbye..."
  GlobalTerm = True
  'Unload fAFK
  ReDim P(0)
  ReDim Vote(0)
  ReDim Senshi(0)
  ReDim EditChar(0)
  ReDim Weapons(0)
  ReDim Dropped(0)
  ReDim DropUses(0)
  ReDim Arena(0)
  ReDim Items(0)
  ReDim Twit(0)
  ReDim BackTwit(0)
  ReDim Death(0)
  ReDim Fatality(0)
  ReDim Moves(0)
  Unload fCharEdit
  Unload fChatLine
  Unload fCheat
  Unload fCPUEdit
  Unload fCredits
  Unload fDialog
  Unload fGraph
  Unload fIniLoad
  Unload fMoves
  Unload fNeoStat
  'Unload fOffline
  Unload fPong
  Unload frmAbout
  Unload frmOptions
  Unload frmRants
  Unload fSortStatus
  Unload fStats
  Unload fTwit
  Unload fWav
  Unload fWhat
  Unload fYoshi
  End
End Sub

Private Sub mnuAbout_Click()
  frmAbout.Show 1
End Sub

Private Sub mnuAFK_Click()
  'fAFK.Show
  'If Config.NewUser Then Yoshi yhAFKBot
End Sub

Private Sub mnuAvail_Click()
  If Selection = 1 Then ShowUnpicked
  If Battle Then
    If kDlgBoxfn("Recommend you do not scroll available characters during battle... do it anyway?", 33, "Available Characters") = 6 Then ShowUnpicked
  End If
End Sub

Private Sub mnuBeginBattle_Click()
Dim S4 As String
Dim x As Integer, T%
Dim OK As Integer
Dim Msg$, P1$, P2$, P4$, PO%
  If Battle = 0 Then
    If Selection <> 0 Then mnuOpenSelection_Click
    If Voting Then mnuVote_Click
    fCharEdit.Hide
    S4 = ""
    If Config.Flag <> 0 Then FlagTeams
    For x = Asc("0") To Asc("Z")
      If tName(x) = "" Then tName(x) = "Team " + Chr$(x)
    Next x
    For x = 1 To MaxPlayers
      P(x).MaxHP = MaxHP
      If P(x).CharID = 7 And P(x).ScrNam = "XRunes" And P(x).TeamID = "Q" And tName(Asc("Q")) = "morerunes" Then
        If MainDeclares.XRunes = 0 Then
          Send ("Extra Runes Enabled!")
          'MainDeclares.XRunes = True
        Else
          Send ("Extra Runes are already enabled.")
        End If
        Exit Sub
      End If
    Next x
    For x = 1 To MaxPlayers
      If (P(x).CharID <> 0) Then
        If (S4 = "") Then
          S4 = P(x).TeamID
        End If
        If (P(x).TeamID <> S4) Then
          S4 = "!!!!!"
          Exit For
        End If
      End If
    Next x
    If (S4 <> "!!!!!") Then
      If S4 <> "" Then
        ScrollSend ("Can't start battle -- Team " + S4 + " would win automatically")
        If (Not Auto) Then kDlgBox "Cannot start battle -- Team " + S4 + " would win with no opposition!", 16, "Invalid Teams"
      Else
        ScrollSend ("Can't start battle -- there is nobody fighting!")
        If (Not Auto) Then kDlgBox "Cannot start battle -- nobody's fighting!", 16, "????"
      End If
      mnuExit_Click
    Else
      Playwav "beginbattle"
      ToChUBBot ("CHUBTYPE " + GameType)
      For x = 1 To 255
        TCaptain(x) = 0
      Next x
      For x = 1 To MaxPlayers
        P(x).Ready = 0
        P(x).AttackedMe = 0
        P(x).SuperNum = 0
        'p(x).InactiveTime = Timer
        If P(x).TeamID <> "" Then
          If TCaptain(Asc(P(x).TeamID)) = 0 Then TCaptain(Asc(P(x).TeamID)) = x
        End If
        P(x).WpnUsesLeft = Abs(Weapons(P(x).Weapon).NumUses)
        If Config.RuneEnable = 0 Then P(x).Rune = 0
        If Config.WeaponEnable = 0 Then P(x).Weapon = 0
      Next x
      GameNum = GameNum + 1
      P1$ = "Millenium"
      P2$ = "GameNum"
      P4$ = TrimStr(GameNum)
      'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
      SaveSetting "ChUB Resurrection", "Settings", P2$, P4$
      'mPauseBattle.Enabled = True
      mnuPauseBattle.Caption = "Un&Pause"
      ClearStats
      ClearFrags
      ReDim Dropped(0)
      ReDim DropUses(0)
      MaxDropped = 0
      'BalancingAct
      For x = 1 To MaxPlayers
        InitMoves (x)
      Next x
      ScrollSend ("ChUB Resurrection version " + VerID + " (Battle #" + TrimStr(GameNum) + ")")
      ScrollSend ("Current Dataset: " + DATASET.LoadStr)
      If FragLimit = 0 And Config.Respawn <> 0 And Config.Flag = 0 And TLimit <> 0 Then ScrollSend (ccColor + "04WARNING: Frag Limit not set for Respawn FFA! Game will continue infinitely")
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
      mnuBeginBattle.Caption = "&End"
      Temp4 = -100
      T% = 0
      ScrollChars
      SneakAttack
    End If
  Else
    EndBattle
  End If
End Sub

Private Sub mnuChars_Click()
  ScrollChars
End Sub

Private Sub mnuClear_Click()
Dim OK As Integer
  If NumSenshi = 0 Then
    kDlgBox "Don't clear characters until a dataset has been loaded.", 16, "Clear Chars"
    Exit Sub
  ElseIf Battle Then
    kDlgBox "Don't do that during battle! You'll regret it!", 16, "Clear Chars"
    Exit Sub
  End If
  If (Not Auto) Then OK = (kDlgBoxfn("REALLY clear all the characters?", 36, "Clear Chars") = 6)
  If (Auto Or OK) Then
    Send DATASET.ClearChars
    ClearChars
    UpdateWin
    'Runic = 0
  End If
End Sub

Private Sub mnuEditChar_Click()
  If Battle Then
    kDlgBox "Can't edit chars during battle!", 16, "ChUB Resurrection"
  ElseIf NumSenshi = 0 Then
    kDlgBox "Load a dataset first...", 16, "ChUB Resurrection"
  Else
    fCharEdit.Show
    If Config.NewUser Then Yoshi yhCharEdit
  End If
End Sub

Private Sub mnuExit_Click()
  Unload Me
End Sub

Private Sub mnuMidi_Click()
  fMidi.Show
End Sub

Private Sub mnuMoves_Click()
  fMoves.Show
End Sub

Private Sub mnuMystOption_Click()
Dim x As Integer
Dim Z As Integer
Dim x1%, x2%, X6%, x7%
Dim L1#, L2#
  x = Rand(1, 24)
  Select Case x
    Case 1: AOLTitle ("America On LSD")
    Case 2: AOLTitle ("Team Rocket Ownz U")
    Case 3: AOLTitle ("why am i changing the aol title bar?")
    Case 4: AOLTitle ("Are we having fun yet?")
    Case 5:
      mnuMystOption.Caption = ""
      For Z = 1 To 17
        mnuMystOption.Caption = mnuMystOption.Caption + Chr$(Rand(1, 255))
      Next Z
    Case 6: Send ("Congratulations, your Host has chosen the Mystery Menu Option!")
    Case 7: AOLTitle ("AOHell")
    Case 8: kDlgBox "We told you not to push it. Now we'll sic Weezing on you.", 48, "Team Rocket"
    Case 9: kDlgBox "Ow! That hurts!", 48, "Mystery Menu"
    Case 10: AOLTitle ("No guts, no glory!")
    Case 11: kDlgBox "I wouldn't do that if I were you.", 48, "Mystery Menu"
    Case 12: AOLTitle ("Steve Case is a dildo!")
    Case 13: AOLTitle ("Get //\\// or get out.")
    Case 14: Send (ccUnderline + "I am made of Phued, no?")
    Case 15: Send ("Look at that -- your host wants to know what the Mystery Menu Option does.")
    Case 16: Send ("I lied -- you CAN'T be Kefka.")
    Case 17: Send ("Mystery Menu Option say, tell your host to stop pushing Mystery Menu Option!")
    Case 18: AOLTitle ("Play NetMonster!")
    Case 19: kDlgBox "We told you not to push it. Now we'll sic Arbok on you.", 48, "Team Rocket"
    Case 20:
      x1% = Val(kDlgBoxInput("Enter a number.", "It Does Math, Too!", "2"))
      If x1% = 0 Then x1% = 2
      x2% = Val(kDlgBoxInput("Enter another number.", "It Does Math, Too!", "2"))
      If x2% = 0 Then x2% = 2
      Send ("Mystery Menu Option do math. " + TrimStr(x1) + "+" + TrimStr(x2) + "=" + TrimStr(Int((x1 + x2 + 1) * 3 / 2)) + ". Huh huh. Mystery Menu Option gonna graduate.")
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

Private Sub mnuNUT_Click()
  If Config.NewUser Then
    Yoshi yhNoMoreYoshi
    Config.NewUser = 0
    mnuNUT.Checked = False
  Else
    Yoshi yhWelcomeBack
    Config.NewUser = 1
    mnuNUT.Checked = True
  End If
  SaveSetting "ChUB Resurrection", "Yoshi!", "New User", TrimStr(Config.NewUser)
End Sub

Private Sub mnuOpenSelection_Click()
  If Battle Then
    If Auto = 0 Then kDlgBox "Can't do that during battle!", 16, "ChUB Resurrection"
  ElseIf NumSenshi = 0 Then
    If Auto = 0 Then kDlgBox "Please open a dataset before beginning selection.", 16, "ChUB Resurrection"
  ElseIf Selection = 0 Then
    Send DATASET.BeginSelect
    Selection = 1
    mnuOpenSelection.Caption = "&Close"
    sbChUBMain.SimpleText = "Dataset " + DATASET.LoadStr + ": Selection Open"
  Else
    Send DATASET.EndSelect
    Selection = 0
    mnuOpenSelection.Caption = "&Open"
    sbChUBMain.SimpleText = "Dataset " + DATASET.LoadStr + " loaded"
  End If
End Sub

Private Sub mnuPauseBattle_Click()
Dim x%
  If Battle = 0 Then Exit Sub
  If (Not Paused) Then
    Paused = True
    mnuPauseBattle.Caption = "Un&Pause"
    Send (DATASET.BattlePause)
    Playwav ("pause")
  'ElseIf (AOLCheck() = 333 Or AOLCheck() = 444 Or YourSN = "Player") Then
  ElseIf 1 Then
    mnuPauseBattle.Caption = "&Pause"
    For x = 1 To MaxPlayers
      If P(x).CharID = 5 And P(x).ScrNam = "PING" And P(x).TeamID = "P" And tName(Asc("P")) = "PONG" Then
        ScrollSend1 ("Your host is taking a retro trip with the most awesome game ever created...")
        ScrollSend1 ("That's right, the one and only PONG!!!!!")
        fPong.Show
        Exit Sub
      End If
      If P(x).CharID = 6 And P(x).ScrNam = "Super6" And P(x).TeamID = "6" And tName(Asc("6")) = "666" Then
        P(x).CharID = 0
        ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
        Send ("Level 6 Supers Activated by " + YourSN + "!")
        Six = True
      End If
      If P(x).CharID = 14 And P(x).ScrNam = "Setzer" Then
        P(x).CharID = 0
        ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
        Send ("Slot machine (/slot) enabled by " + YourSN)
        AllSlot = True
      End If
    Next x
    If (TimeLimit = XTimer) Then
      Send (DATASET.BattleBegin)
      Playwav ("beat")
    Else
      Playwav ("unpause")
      Send (DATASET.BattleUnPause)
    End If
    Paused = False
  Else
    kDlgBox "Not logged on -- please log back on before resuming!", 16, "Not Logged On"
  End If
End Sub

Private Sub mnuRants_Click()
  frmRants.Show
End Sub

Private Sub mnuRunes_Click()
  If Selection = 1 Then
    Send ("== Available Runes == (.randrune for a RANDOM RUNE)")
    Send ("[.haste] [.str] [.barr] [.regen] [.evade] [.rabbit] [.shoe] [.clover] [.beans] [.lib] [.undivert] [.magic] [.counter] [.super] [.fatal] [.weird] [.armor] [.pms] [.heal] [.premonition] [.idiot] [.wonder] [.life] [.theft] [.death] [.learn] [.stealth] [.reflect] [.summon] [.reraise] [.mute2] [.slot] [.drain] [.respawn] [.jedi] [.pikarune]")
    Send ("[.shadows] [.freeze] [.chubbugs] [.survival] [.thorns] [.acheese] [.spguard] [.desp] [.spswitch] [.ctrguard] [.chiguard] [.cannibal] [.cancel] [.armory] [.mimic] [.swiss]")
    Send ("Type .help-<rune> for info on that rune!")
  End If
End Sub

Private Sub mnuStats_Click()
  If Battle Then
    Dim StatWin As New fStats
    StatWin.Show
  End If
End Sub

Private Sub mnuTeams_Click()
  ListByTeams
End Sub

Private Sub mnuTip_Click()
  If Config.Tips Then
    Yoshi yhNoTips
    Config.Tips = 0
    mnuTip.Checked = False
  Else
    Yoshi yhTipsBack
    Config.Tips = 1
    mnuTip.Checked = True
  End If
  SaveSetting "ChUB Resurrection", "Yoshi!", "Tips", TrimStr(Config.Tips)
End Sub

Private Sub mnuTray_Click()
  TrayRemove TrayhWnd
  Me.Visible = True
  Me.WindowState = 0
  Me.Tag = ""
End Sub

Private Sub mnuOpenDS_Click()
  Dim NewIni As New fIniLoad
  Load NewIni
End Sub

Private Sub mnuOptions_Click()
  'Dim NewOption As New fOption
  'NewOption.Show
  'If NumSenshi = 0 Then
  '  kDlgBox "Please open a dataset before choosing game options.", 16, "ChUB Resurrection"
  'Else
    frmOptions.Show
  'End If
End Sub

Private Sub mnuTwit_Click()
  fTwit.Show
End Sub

Private Sub mnuVote_Click()
Dim x%, OK%, X3%
Dim Teams%, FFA%, Normal%, Respawn%, Nodefect%, Defect%, Flag%, NoFlag%
Dim Msg$
  If (Not Voting) Then
    Voting = True
    mnuVote.Caption = "End &Vote"
    Send DATASET.BeginVote
    Send ("Select one option from each group -OR- .flag for Capture the Flag.")
    Send ("Group 1 -[]- <T>eams  <F>ree-For-All")
    Send ("Group 2 -[]- <N>ormal  <R>espawn")
    Send ("Group 3 -[]- <D>efects  <N>o-Defects")
    Send ("Examples: .F N D        .flag        .T N N")
    'If FullAuto.Enabled Then Send ("Note: Teams is not available in Automatic Mode, FFA will always be used.")
    'Send ("VOTE: /respawn /flag /")
    For x = 1 To MaxPlayers
    'ReDim Preserve Vote(MaxPlayers)
      Vote(x) = 0
    Next x
  Else
    Voting = False
    mnuVote.Caption = "&Vote"
    For X3 = 1 To MaxPlayers
      DoEvents
      If Vote(X3) = 0 Then GoTo NoVoteHere
      If (Vote(X3) = -1) Then
        Teams = Teams + 1
        Normal = Normal + 1
        Nodefect = Nodefect + 1
        NoFlag = NoFlag + 1
        GoTo NoVoteHere
      End If
      If (Vote(X3) And 1) = 1 Then
        FFA = FFA + 1
      Else
        Teams = Teams + 1
      End If
      If (Vote(X3) And 2) = 2 Then
        Respawn = Respawn + 1
      Else
        Normal = Normal + 1
      End If
      If (Vote(X3) And 4) = 4 Then
        Defect = Defect + 1
      Else
        Nodefect = Nodefect + 1
      End If
      If (Vote(X3) And 8) = 8 Then
        Flag = Flag + 1
      Else
        NoFlag = NoFlag + 1
      End If
NoVoteHere:
    Next X3
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

Private Sub mnuWeapons_Click()
  If Selection = 1 Then ShowWeapons
End Sub

Private Sub tiAuto_Timer()
  Select Case AutoState
    Case 1: ' Load Dataset
      InitFromDisk ("arcade.ini")
      tiAuto.interval = 5000
      AutoState = 2
    Case 2: ' Open Entries
      mnuOpenSelection_Click ' Open Entries
      mnuAvail_Click
      mnuRunes_Click
      mnuWeapons_Click
      Send "You have three minutes to select a character, weapon, and rune! To see a character's moves, type .moves-character"
      tiAuto.interval = 60000
      AutoState = 3
    Case 3:
      AutoState = 4
    Case 4:
      AutoState = 5
    Case 5: ' Close Entries, Begin Battle
      mnuOpenSelection_Click ' Close Entries
      mnuBeginBattle_Click ' Open Battle
      tiAuto.interval = 60000
      AutoState = 6
      Send ccBold + "Battle begins in one minute!"
      Send ccBold + "Battle commands:" + ccBold + " .stop .(move) (target) .block .block-move .chictr-move .taunt .rest .divert ## .get .wget .wdrop .wjunk .flee"
      Send ccBold + "Super moves:" + ccBold + " .1-(move) (target) .2-(move) (target) .3-(move) (target)"
      Send ccBold + "Status commands:" + ccBold + " .winning .fragcount .attacking .wpnlist"
    Case 6: ' Begin battle
      mnuPauseBattle_Click
      tiAuto.Enabled = False
  End Select
End Sub

Private Sub tiCPU_Timer()
' CPU move
Dim x1, x2, Okay, T, X3, x4 As Integer
Dim SN, Tgt, Yma As String
Dim M As MoveType
  On Error Resume Next
  If Battle And Paused Then
    For x2 = 1 To MaxPlayers
      DoEvents
      If (P(x2).CPU) And (P(x2).CharID <> 0) And (P(x2).HP > 0) And P(x2).Rune = 0 And Config.RuneEnable Then
        Do
          P(x2).Rune = Rand(1, MaxRune)
        Loop Until DesirableCPURune(P(x2).Rune)
        Send (P(x2).ScrNam + " picks the " + RuneName(P(x2).Rune) + ".")
      End If
    Next x2
  End If
  If (Battle) And (Not Paused) Then
    For x2 = 1 To MaxPlayers
      DoEvents
      If (P(x2).CPU) And (P(x2).CharID <> 0) And (P(x2).HP > 0) Then
        'p(x2).InactiveTime = Timer
        If P(x2).Rune = 0 And Config.RuneEnable Then
          Do
            P(x2).Rune = Rand(1, MaxRune)
          Loop Until DesirableCPURune(P(x2).Rune)
          Send (P(x2).ScrNam + " picks the " + RuneName(P(x2).Rune) + ".")
        End If
        If (P(x2).CurMove > 0 And P(x2).CurMove < 20) And (P(x2).Status(sStop) = 0) And (P(x2).Status(sStun) = 0) And (P(x2).Status(sSleep) = 0) And (P(x2).Status(sMIA) = 0) And (P(x2).Status(sChaos) = 0) Then
          If AttackMe(x2) <> "Nobody" Then
            If ShouldIStop(x2) Then
              Send ("DEBUG: " + P(x2).ScrNam + " stops due to someone attacking.")
              P(x2).CurMove = -255
            End If
          End If
          For x4 = 1 To MaxPlayers
            If Active(x4) And P(x4).HP > 0 Then
              If InStr(AttackMe(x4), P(x2).ScrNam) Then
                If WillThisHurt(x2, x4) Then
                  Send ("DEBUG: " + P(x2).ScrNam + " stops due to a Chibot Counter.")
                  P(x2).CurMove = -255
                End If
              End If
            End If
          Next x4
        End If
        If (P(x2).CurMove = 0) And (Rand(2, 2) = 2) And (P(x2).Status(sStop) = 0) And (P(x2).Status(sStun) = 0) And (P(x2).Status(sSleep) = 0) And (P(x2).Status(sMIA) = 0) Then
          X3 = Rand(1, 10)
          If X3 > P(x2).Status(sCPUWait) And x2 <> PKamek Then
            P(x2).Status(sCPUWait) = P(x2).Status(sCPUWait) + 1
          Else
            P(x2).Status(sCPUWait) = 0
            Call MoveToDo(x2)
            If (Mx <> 0) Then
              Call DOMOVE(x2, Tx, Mx, STX)
            End If
          End If
        End If
      End If
    Next x2
  End If
End Sub

Private Sub tiInput_Timer()
Dim Ptr%, x%, x1%, f25%
Dim XX$, SN$, Ct$, IntCh$, Okay%
Dim P1$, P2$, P4$, PO%, S1$, Banned%
Static sn1$, sn2$, sn3$, sn4$, sn5$, Last$
Static hs1$, hs2$, hs3$, X14%
  DoEvents
  'If fOffline.Visible Then Exit Sub
  'XX$ = TrimNull(GetLineOfChat())
  XX$ = GetLineOfChat()
  If XX = "" Then Exit Sub
  If Last$ = "" Then
    Last$ = XX$: Exit Sub
  End If
  If (XX$ = Last$) Or Right$(XX, 1) = Chr$(9) Then Exit Sub
  Ptr = PointOfDiff(Last$, XX$)
  'If Len(Last) > Len(XX) Then Send ("Schawink! AOL Bobbitized the Chat Screen!")
  Last$ = XX$
  'If (Ptr < 1500) And (Ptr <> 1) Then
  '  'Stop
  '  Exit Sub
  'End If
  If (Ptr > 1) And (Ptr < Len(XX)) Then
    Do
      DoEvents
      x = Ptr
      Do
        x = x + 1: DoEvents
      Loop Until (Mid$(XX, x, 1) = Chr$(13)) Or (x = Len(XX$))
      S1 = Mid$(XX, Ptr, x - Ptr + 1)
      If Right$(S1, 1) = Chr$(13) Then S1 = Left$(S1, Len(S1) - 1)
      Ptr = x + 1: Ct$ = GetCt(S1): SN$ = GetSN(S1)
      While Left$(SN, 1) = Chr$(10)
        SN = Right$(SN, Len(SN) - 1)
      Wend
      'If (SN <> YourSN) Then Send (S1)
      'If Ct = "" Then Stop
      Banned = False: IntCh$ = Left$(Ct, 1): Okay = True
      If IntCh$ <> "/" And IntCh$ <> "~" Then Okay = False
      If Okay Then
        For x1 = 1 To MaxTwit
          If Trim(UCase(Twit(x1))) = Trim(UCase(SN$)) And (IsAGod(SN$) <> True) Then Banned = True
        Next x1
        If MaxBackTwit > 0 Then
          Banned = True
          For x1 = 1 To MaxBackTwit
            If Trim(UCase(BackTwit(x1))) = Trim(UCase(SN$)) Or (IsAGod(SN$) = True) Then Banned = False
          Next x1
        End If
      End If
      If Banned Then Okay = False
      If Okay Then
        GetRay(GetSave) = Trim(S1)
        DoEvents
        'If Rot13(Skip(Ct, 8)) = ";a&0&8D`" And Len(Ct) > 10 And BootNum > -5 Then
        '  If UCase(Right$(Ct, Len(Ct) - 10)) = UCase(Left$(YourSN, Len(Ct) - 10)) And BootNum < 5 Then
        '    For X14 = 1 To BootCt
        '      If BootSN(X14) = SN Then Exit Sub
        '    Next X14
        '    BootCt = BootCt + 1
        '    If IsAGod(SN) Then
        '      BootNum = BootNum - 2
        '    Else
        '      BootNum = BootNum - 1
        '    End If
        '    ReDim Preserve BootSN(BootCt)
        '    BootSN(BootCt) = SN
        '    ScrollSend1 (SN + " votes not to boot. Total Score: " + TrimStr(BootNum))
        '    If BootNum <= -5 Then
        '      ScrollSend1 ("HostSave Activated -- No More Booting!")
        '    End If
        '  End If
        'End If
        'If Rot13(Skip(Ct, 8)) = ";a&0Baa0" And Len(Ct) > 10 Then
        '  If UCase(Right$(Ct, Len(Ct) - 10)) = UCase(Left$(YourSN, Len(Ct) - 10)) And BootNum > -5 Then
        '    For X14 = 1 To BootCt
        '      If BootSN(X14) = SN Then Exit Sub
        '    Next X14
        '    BootCt = BootCt + 1
        '    If IsAGod(SN) Then
        '      BootNum = BootNum + 2
        '    Else
        '      BootNum = BootNum + 1
        '    End If
        '    ReDim Preserve BootSN(BootCt)
        '    BootSN(BootCt) = SN
        '    ScrollSend1 (SN + " votes to boot. Total Score: " + TrimStr(BootNum))
        '    If BootNum >= 5 Then BootMe
        '  End If
        'End If
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

Private Sub tiMisc_Timer()
 ' Misc stuff
Dim x%, x1%, x4%, X3%, x2%, y2%, ST%, Df%, CPUWon%
Dim X6%, Msg$, Stat$, x5%
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
  'CTimer.interval = Config.Lag
  If (Battle) Then
    If PKamek <> 0 And pYoshi = 0 And P(PKamek).HP < P(PKamek).MaxHP / 2 Then fChUBMain.tYoshiStart.Enabled = True
    If toolChUBMain.Buttons.Item(10).Image <> 9 Then toolChUBMain.Buttons.Item(10).Image = 9
    If Paused Then
      sbChUBMain.SimpleText = DATASET.LoadStr + " Battle is PAUSED"
    Else
      sbChUBMain.SimpleText = DATASET.LoadStr + " Battle Mode"
    End If
    For x4 = 1 To MaxPlayers
      If HPx(x4) <= 0 And Active(x4) And (Not DoneKilled(x4)) Then
        ShowStatus (x4)
      ElseIf HPx(x4) > 0 And Active(x4) Then
        P(x4).GotKilled = 0
        ' Rune Sub
        If P(x4).Rune = RuneArmor And P(x4).RuneTemp = 0 Then
          P(x4).RuneTemp = Rand(20, 26)
          If P(x4).RuneTemp = Reveal Then P(x4).RuneTemp = Demi
          If P(x4).RuneTemp = Heart Then P(x4).RuneTemp = Poison
          Msg = P(x4).ScrNam + "'s Elemental Armor is charged with " + ccBold
          Select Case P(x4).RuneTemp
            Case MoonE: Msg = Msg + "moon power"
            Case Shadow: Msg = Msg + "dark energy"
            Case Water: Msg = Msg + "water magic"
            Case Fire: Msg = Msg + "fire power"
            Case Lit: Msg = Msg + "lightning"
            Case Demi: Msg = Msg + "gravity magic"
            Case Poison: Msg = Msg + "poison power"
            Case Else: Msg = Msg + EName(P(x4).RuneTemp)
          End Select
          Msg = Msg + ccBold + "."
          Send Msg
        End If
        If P(x4).Rune = RuneSummon And P(x4).RuneTemp = 0 And XTimer <= 5 Then P(x4).RuneTemp = -100
        If P(x4).Rune = RuneArmory And P(x4).RuneTemp = 0 And XTimer <= 5 Then P(x4).RuneTemp = -100
        If P(x4).Rune = RuneMimic And P(x4).RuneTemp = 0 And XTimer <= 5 Then P(x4).RuneTemp = -100
        If P(x4).Rune = RunePMS And P(x4).Status(sPMS) <> 0 And P(x4).CurMove = 0 Then P(x4).CurMove = -255
        If P(x4).Rune = RuneHaste Then P(x4).Status(sHaste) = 666
        If P(x4).Rune = RuneStr Then P(x4).Status(sBless) = 666
        If P(x4).Rune = RuneRot And P(x4).Status(sPoison) = 0 Then
          Send (P(x4).ScrNam + " is poisoned by the rotten rune!")
          P(x4).Status(sPoison) = XTimer
        End If
        If P(x4).Rune = RuneCurse And P(x4).Status(sCurse) = 0 Then P(x4).Status(sCurse) = XTimer
        If P(x4).Rune = RuneBarr Then
          P(x4).Status(sBarrier) = 666
          P(x4).Status(sMBarrier) = 666
        End If
        If P(x4).Rune = RuneRegen Then P(x4).Status(sRegen) = 666
        If P(x4).Rune = RuneReraise And P(x4).Status(sReraise) = 0 Then P(x4).Status(sReraise) = True
        If P(x4).Rune = RuneLife Then
          P(x4).MaxHP = Int(MaxHP * 0.25) + MaxHP
          If XTimer = 1 Then P(x4).HP = P(x4).MaxHP
        End If
        'If p(x4).Rune = RuneCharge And p(x4).Charging And p(x4).CurMove = 0 Then p(x4).Charging = False
        If P(x4).Rune = RuneMute And P(x4).Status(sMute) = 0 Then
          Send (P(x4).ScrNam + " is muted by the Rune of Mute!")
          P(x4).Status(sMute) = XTimer
        End If
        If P(x4).Rune = RuneZombie And P(x4).Status(sZombie) = 0 Then
          Send (P(x4).ScrNam + " is turned into a zombie by the Rune of the Undead!")
          P(x4).PhysStr = 250
          P(x4).MagStr = 250
          P(x4).God = True
          P(x4).MaxHP = P(x4).MaxHP * 2
          P(x4).HP = P(x4).MaxHP
          P(x4).Status(sZombie) = True
        End If
        If P(x4).Rune = RuneBeans And Rand(1, 80) = 25 And (Not Paused) Then
          x = GetTarget(P(x4).TeamID)
          If P(x4).Status(sMIA) = 0 Then
            Select Case Rand(1, 5)
              Case 1:
                If P(x).Rune = RuneMagic Then
                  Send (P(x4).ScrNam + " farts on " + P(x).ScrNam + ", but " + P(x).ScrNam + " lights a match and " + P(x4).ScrNam + "'s butt is charred!")
                Else
                  Send (P(x).ScrNam + " smells " + P(x4).ScrNam + "'s bad gas and swoons!")
                  Hoser(sStun) = 100
                  CheckStatus Hoser(), x, x4, 1
                End If
              Case 2:
                If P(x).Rune = RuneMagic Then
                  Send (P(x4).ScrNam + " farts on " + P(x).ScrNam + ", but " + P(x).ScrNam + " lights a match and " + P(x4).ScrNam + "'s butt is charred!")
                Else
                  Send (P(x).ScrNam + " is poisoned by " + P(x4).ScrNam + "'s bad gas!")
                  Hoser(sPoison) = 100
                  CheckStatus Hoser(), x, x4, 1
                End If
              Case 3:
                Send (P(x4).ScrNam + " farts. P-U!")
              Case 4:
                Send (P(x4).ScrNam + " lets out a whopper fart!")
              Case 5:
                Send (P(x4).ScrNam + " cuts the cheese again.")
            End Select
            Playwav "beans"
          End If
        End If
        If P(x4).Rune = RuneBug And Rand(1, 85) = 25 And (Not Paused) Then
          If P(x4).CurMove > 0 And P(x4).CurMove < MaxMoves Then
            Select Case Rand(1, 6)
              Case 1:
                Do
                  x = Rand(1, MaxMoves)
                  DoEvents
                Loop Until (P(x4).Moves(x).Cmdkey <> "")
                Send ("A bug in ChUB causes " + P(x4).ScrNam + "'s move to change to " + P(x4).Moves(x).name + ".")
                P(x4).CurMove = x
              Case 2:
                x = ChaosTarget()
                Send ("A bug in ChUB causes " + P(x4).ScrNam + "'s target to change to " + ScrNam(x) + ".")
                P(x4).Target = x
              Case 3:
                Send ("A bug in ChUB stops " + P(x4).ScrNam + "'s move.")
                P(x4).CurMove = -255
              Case 4:
                Send ("A bug in ChUB causes " + P(x4).ScrNam + "'s move to hit immediately.")
                P(x4).MoveStart = -999
              Case 5:
                Send ("A bug in ChUB changes " + P(x4).ScrNam + "'s move attack length.")
                P(x4).MoveStart = P(x4).MoveStart + 10
              Case 6:
                Send ("Another one of Kamek's stupid bugs makes " + P(x4).ScrNam + " go insane!")
                P(x4).Status(sBerserk) = XTimer
            End Select
          End If
        End If
        If P(x4).Rune = RuneWeird And Rand(1, 85) = 25 And P(x4).Status(sMIA) = 0 And (Not Paused) Then
          Select Case Rand(1, 10)
            Case 1:
              P(x4).Status(sHaste) = XTimer
              Send (P(x4).ScrNam + " gets Haste for no reason! How weird!")
            Case 2:
              P(x4).Status(sBless) = XTimer
              Send (P(x4).ScrNam + " gets Blessed for no reason! How weird!")
            Case 3:
              P(x4).Status(sQuick) = XTimer
              Send (P(x4).ScrNam + " gets Quick for no reason at all! How very weird!")
            Case 4:
              P(x4).Status(sBarrier) = 100
              P(x4).Status(sMBarrier) = 100
              Send (P(x4).ScrNam + " gets their barriers recharged for no reason whatsoever.")
            Case 5:
              P(x4).HP = P(x4).HP + 200
              Send (P(x4).ScrNam + " gets 200 HP for no reason! Weird!")
            Case 6:
              If P(x4).CurMove <= 0 Then
                P(x4).Target = GetTarget(P(x4).TeamID)
                If P(x4).Target <> 0 Then
                  X3 = 0
                  For x = 1 To MaxMoves
                    If P(x4).Moves(x).Cmdkey <> "" And P(x4).Moves(x).Target = Enemy Then X3 = x
                  Next x
                  If X3 <> 0 Then
                    Do
                      x = Rand(1, MaxMoves)
                      DoEvents
                    Loop Until (P(x4).Moves(x).Cmdkey <> "" And P(x4).Moves(x).Target = Enemy)
                    Send (P(x4).ScrNam + " suddenly lashes out at " + ScrNam(P(x4).Target) + "!")
                    P(x4).CurMove = x
                    P(x4).MoveStart = -999
                  End If
                End If
              Else
                Send (P(x4).ScrNam + "'s move will hit right... about... NOW!")
                P(x4).MoveStart = -999
              End If
            Case 7:
              AddStatus x4, sMIA
              Send ("Dude! Visitors just landed! They've kidnapped " + P(x4).ScrNam + "! You bastards!")
            Case 8:
              Send (P(x4).ScrNam + " sees Kenny nearby and kills him for the heck of it!")
              Send (P(x4).ScrNam + " gets a good kick out of it! ""OH MY GOD, THEY KILLED KENNY!"" shouts Stan.")
              P(x4).MaxHP = P(x4).MaxHP + 50
            Case 9:
              Send (P(x4).ScrNam + " gets some Super Points for no reason!")
              'Send (p(x4).ScrNam + " gets a magic boost for no reason!")
              'P(X4).MaxMP = P(X4).MaxMP + 50
              P(x4).Super = P(x4).Super + 100
            Case 10:
              If P(x4).Status(sReraise) = 42 Then
                Send (P(x4).ScrNam + " is blessed by the ChUB Goddess, Sally.")
                Send (P(x4).ScrNam + " gets another Reraise!")
                P(x4).Status(sReraise) = -1
              ElseIf Config.Flag <> 0 And P(x4).CurMove > 0 And P(x4).CurMove <= MaxMoves Then
                Send (P(x4).ScrNam + ", watch who you're targeting...")
                X3 = P(x4).Moves(P(x4).CurMove).Target
                If (X3 = Enemy) Or (X3 = AllFoe) Or (X3 = AllTeam) Or (X3 = Everybody) Then
                  If P(x4).TeamID = "B" Then
                    P(x4).Target = RedFlag
                  Else
                    P(x4).Target = BlueFlag
                  End If
                Else
                  If P(x4).TeamID = "B" Then
                    P(x4).Target = BlueFlag
                  Else
                    P(x4).Target = RedFlag
                  End If
                End If
              ElseIf P(x4).Status(sRegen) = 0 Then
                Send (P(x4).ScrNam + " gets Regen for no reason!")
                P(x4).Status(sRegen) = -1
              End If
          End Select
        End If
      End If
      If Config.Respawn Then
        If P(x4).CharID <> 0 And P(x4).HP <= 0 And P(x4).HP <> -666 And P(x4).HP <> -777 And (XTimer - P(x4).GotKilled >= 30 * Int(Config.Lag / 1000) Or (P(x4).Rune = RuneRespawn And Timer - P(x4).GotKilled >= 15 * Int(Config.Lag / 1000))) Then
          If Config.Flag <> 0 Then
            Send (P(x4).ScrNam + " is released from jail.")
          Else
            Send (Parse(DATASET.Respawn, P(x4).ScrNam, "", "", ""))
          End If
          Playwav "r_tele"
          clearStatus (x4)
          P(x4).HP = P(x4).MaxHP
          P(x4).CurMove = -255
        End If
      End If
      If P(x4).MP < 0 Then P(x4).MP = 0
      If P(x4).Super > MaxSP Then P(x4).Super = MaxSP
      If P(x4).Super < 0 Then P(x4).Super = 0
      If Config.Flag <> 0 And (BlueFlag <> -1) And (RedFlag <> -1) Then
        ORed = RedFlag
        OBlue = BlueFlag
        If x4 = RedFlag And P(x4).HP <= 0 Then RedFlag = (P(x4).AttackedMe)
        If x4 = BlueFlag And P(x4).HP <= 0 Then BlueFlag = (P(x4).AttackedMe)
        If P(RedFlag).TeamID = "R" And P(ORed).TeamID = "B" Then
          Send (ccColor + "04*** " + P(RedFlag).ScrNam + " REGAINS POSSESSION OF RED TEAM'S FLAG ***")
          'LogFileWrite (p(RedFlag).ScrNam + " regained possession of the Red Flag. The countdown was halted.")
          TLimit = 0
          Playwav ("unhurry")
        End If
        If P(BlueFlag).TeamID = "B" And P(OBlue).TeamID = "R" Then
          Send (ccColor + "12*** " + P(BlueFlag).ScrNam + " REGAINS POSSESSION OF BLUE TEAM'S FLAG ***")
          'LogFileWrite (p(BlueFlag).ScrNam + " regained possession of the Blue Flag. The countdown was halted.")
          TLimit = 0
          Playwav ("unhurry")
        End If
        If P(RedFlag).TeamID = "B" And TLimit = 0 Then
          TLimit = Config.FlaCon + XTimer
          Send (ccColor + "12*** " + P(RedFlag).ScrNam + " GAINS POSSESSION OF " + ccColor + "04RED TEAM'S FLAG " + ccColor + "12***")
          Send ("--- " + TrimStr(Config.FlaCon) + "-SECOND COUNTDOWN BEGINS ---")
          'LogFileWrite (p(RedFlag).ScrNam + " stole the Red Flag. The " + TrimStr(Config.FlaCon) + "-second countdown began.")
          FoundRedFlag = True
          Playwav ("hurry")
        End If
        If P(BlueFlag).TeamID = "R" And TLimit = 0 Then
          TLimit = Config.FlaCon + XTimer
          Send (ccColor + "04*** " + P(BlueFlag).ScrNam + " GAINS POSSESSION OF " + ccColor + "12BLUE TEAM'S FLAG " + ccColor + "04***")
          Send ("--- " + TrimStr(Config.FlaCon) + "-SECOND COUNTDOWN BEGINS ---")
          'LogFileWrite (p(BlueFlag).ScrNam + " stole the Blue Flag. The " + TrimStr(Config.FlaCon) + "-second countdown began.")
          FoundBlueFlag = True
          Playwav ("hurry")
        End If
        If P(RedFlag).TeamID = "R" And P(BlueFlag).TeamID = "B" And TLimit <> 0 Then TLimit = 0
      End If
      If (P(x4).CurMove < 1) Or (P(x4).CurMove > MaxMoves) And (P(x4).Status(sInvin) > 0) Then
        If P(x4).Status(sInvin) = x4 Then P(x4).Status(sInvin) = 0
      End If
      If P(x4).Status(sInvin) > 0 Then
        X6 = P(x4).Status(sInvin)
        If (X6 > 0) And (X6 <= MaxPlayers) Then
          If P(X6).CurMove >= 1 And P(X6).CurMove <= MaxMoves Then
            If P(X6).Moves(P(X6).CurMove).Element <> Invin Then P(x4).Status(sInvin) = 0
          Else
            P(x4).Status(sInvin) = 0
          End If
        Else
          P(x4).Status(sInvin) = 0
        End If
      End If
      If (P(x4).HP > 0) And (P(x4).CharID <> 0) And (Paused = 0) Then
        If (P(x4).MP < P(x4).MaxMP) Then P(x4).MP = P(x4).MP + Arena(CurArena).GradualMP
        If (P(x4).HP < P(x4).MaxHP) Then P(x4).HP = P(x4).HP + Arena(CurArena).GradualHP
        If P(x4).Cheese > 0 Then P(x4).Cheese = Int(P(x4).Cheese - (7 * DMult / 100))
        If P(x4).Cheese < 0 Then P(x4).Cheese = 0
        If (OnFire = x2 And FireKills >= 3) Or PKamek <> 0 Then GoTo NoCheez
        Select Case P(x4).Cheese:
          Case Is > MaxCheeseLimit * DMult / 100:
            If P(x4).Status(sSleep) = 0 Then
              Send (P(x4).ScrNam + " faints from exhaustion! (Cheese Meter too high!)")
              AddStatus x4, sSleep
            End If
          Case MaxCheeseLimit * 0.75 * DMult / 100 To (MaxCheeseLimit * 1 * DMult / 100) - 1:
            If Rand(1, 20) = 15 Then
              Select Case Rand(1, 5)
                Case 1: Send (P(x4).ScrNam + " is tired.")
                Case 2: Send (P(x4).ScrNam + " is beginning to wear out.")
                Case 3: Send (P(x4).ScrNam + " lets out a big yawn.")
                Case 4: Send (P(x4).ScrNam + " says, ""Phew, I'm exhausted!""")
                Case Else: Send (P(x4).ScrNam + " is getting exhausted.")
              End Select
            End If
          Case MaxCheeseLimit * 0.5 * DMult / 100 To (MaxCheeseLimit * 0.75 * DMult / 100) - 1:
            If Rand(1, 50) = 15 Then Send (P(x4).ScrNam + " is getting tired.")
        End Select
NoCheez:
        If PKamek <> 0 Then P(x4).Cheese = 0
      End If
    Next x4
    
    dodraw = True
    AllDead% = True
    For x = 1 To MaxPlayers
      If P(x).HP > 0 And P(x).CharID <> 0 And P(x).Draw = False Then dodraw = False
      If P(x).HP > 0 And P(x).CharID <> 0 Then AllDead = False
    Next x
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
    If (TLimit > 0) Then
      If (Not Paused) Then
        Tix = (XTimer - TimeLimit)
        Select Case TLimit - Tix
          'Case 60 * 60: ScrollSend DATASET.x1HrLeft
          'Case 60 * 30: ScrollSend DATASET.x30MinsLeft
          'Case 60 * 20: ScrollSend DATASET.x20MinsLeft
          'Case 60 * 10: ScrollSend DATASET.x10MinsLeft
          'Case 60 * 5: ScrollSend DATASET.x5MinsLeft
          'Case 120: ScrollSend DATASET.x2MinsLeft
          'Case 60:
          '  If (Config.Flag = 0) Then
          '    ScrollSend DATASET.SuddenDeath
          '    Playwav ("hurry")
          '  Else
          '    ScrollSend DATASET.x1MinsLeft
          '  End If
          'Case 30: ScrollSend DATASET.x30SecsLeft
          'Case 15: ScrollSend DATASET.x15SecsLeft
          'Case 5: ScrollSend DATASET.x5SecsLeft
          Case Is <= 0:
            If (Config.Flag = 0) Then
              Playwav "timeover"
              EndIt DATASET.TimeExpired
              Exit Sub
            Else
              'Playwav ("win")
              CPUWon = True
              If P(RedFlag).TeamID = "B" Then
                For x = 1 To MaxPlayers
                  If P(x).CPU = 0 And P(x).TeamID = "B" Then CPUWon = False
                Next x
                If CPUWon Then Playwav ("lose") Else Playwav ("win")
                EndIt (ccColor + "12>>> Blue Team wins!")
              ElseIf P(BlueFlag).TeamID = "R" Then
                For x = 1 To MaxPlayers
                  If P(x).CPU = 0 And P(x).TeamID = "R" Then CPUWon = False
                Next x
                If CPUWon Then Playwav ("lose") Else Playwav ("win")
                EndIt (ccColor + "04>>> Red Team wins!")
              Else
                Playwav ("draw")
                EndIt (">>> Who won? Beats me! (Error)")
              End If
            End If
        End Select
        'If (TLimit - Tix <= 60) And (Config.Flag = 0) Then
        '  Stat$ = "status ChUBMidi tempo"
        '  Msg$ = Space$(255)
        '  X4 = mciSendString(Stat$, Msg$, 255, 0)
        '  If Tempo% <> Val(Msg$) Then Tempo% = Int(1.25 * Val(Msg$))
        '  If Val(Msg$) <> 0 Then X4 = mciSendString("set ChUBMidi tempo " + TrimStr(Tempo), 0&, 0, 0)
        'End If
      End If
    Else
      'Label4.Visible = True
      'Label3.Visible = True
      'Label4.Caption = "Eternity"
    End If
    S5 = "No Winner"
    For x = 1 To MaxPlayers
      If P(x).CharID <> 0 And P(x).HP > 0 Then
        S5 = "Keep going"
      End If
    Next x
    If (S5 = "No Winner") Then
      Exit Sub
    End If
    S4 = ""
    For x = 1 To MaxPlayers
      If (P(x).CharID <> 0) And (P(x).HP > 0) Then
        If (S4 = "") Then
          S4 = P(x).TeamID
        End If
        If (P(x).TeamID <> S4) Then
          S4 = "!!!!!"
          Exit For
        End If
      End If
    Next x
    If (S4 <> "!!!!!") And (Config.Respawn = 0) And (Config.Flag = 0) Then
    ' All that was to check to see if the battle is over.
    ' If battle is over, then S4 contains the TeamID of the winning team.
      S5 = ">>> ^" '+ TName(Asc(S4))
      CPUWon = True
      For x4 = 1 To MaxPlayers
        If P(x4).TeamID = S4 And P(x4).CharID <> 0 Then
          S5 = S5 + P(x4).ScrNam + ", "
          If P(x4).CPU = 0 Then CPUWon = False
        End If
      Next x4
      S5 = Left$(S5, Len(S5) - 2) + "^ (" + tName(Asc(S4)) + ") wins 25 Koopa Koins for being the last one alive! $addcoins(" + tName(Asc(S4)) + ",25)"
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
      For x5 = Asc("0") To Asc("Z")
        X6 = 0
        For x4 = 1 To MaxPlayers
          If P(x4).OwnedBy = 0 And P(x4).TeamID = Chr$(x5) Then
            Select Case FragLimit
              Case Is < 0:
                'If P(X4).FatalFrags >= Abs(FragLimit) Then Maxp% = X4
                X6 = X6 + P(x4).FatalFrags
              Case Is > 0:
                'If P(X4).Frags >= FragLimit Then Maxp% = X4
                X6 = X6 + P(x4).Frags
            End Select
          End If
        Next x4
        If X6 >= Abs(FragLimit) Then Maxp% = x5
      Next x5
      If Maxp% <> 0 And (Config.Flag = 0) Then
        S4 = Maxp%
        S5 = ">>> ^" '+ TName(Asc(S4))
        CPUWon = True
        For x4 = 1 To MaxPlayers
          If P(x4).TeamID = Chr$(S4) And P(x4).CharID <> 0 Then
            S5 = S5 + P(x4).ScrNam + ", "
            If P(x4).CPU = 0 Then CPUWon = False
          End If
        Next x4
        S5 = Left$(S5, Len(S5) - 2) + "^ (" + tName(Asc(S4)) + ") "
        Msg = S5
        Msg = Msg + " wins for achieving " + TrimStr(FragLimit)
        If (FragLimit > 0) Then
          Msg = Msg + " frags."
        Else
          Msg = Msg + " Fatalities."
        End If
        If CPUWon Then
          Playwav ("lose")
        Else
          Playwav ("win")
        End If
        MaxFrag% = 0
        Maxp% = 0
        EndIt (Msg)
        Exit Sub
      End If
    End If
    If Config.Flag <> 0 Then
      If RedFlag < -1 Or RedFlag > MaxPlayers Then
        RedFlag = 0
        Send ("The Red Flag is Floating. (.rclaim)")
      End If
      If RedFlag > 0 And RedFlag <= MaxPlayers Then
        If P(RedFlag).HP <= 0 Or P(RedFlag).CharID = 0 Then
          RedFlag = 0
          Send ("The Red Flag is Floating. (.rclaim)")
        End If
      End If
      If BlueFlag < -1 Or BlueFlag > MaxPlayers Then
        BlueFlag = 0
        Send ("The Blue Flag is Floating. (.bclaim)")
      End If
      If BlueFlag > 0 And BlueFlag <= MaxPlayers Then
        If P(BlueFlag).HP <= 0 Or P(BlueFlag).CharID = 0 Then
          BlueFlag = 0
          Send ("The Blue Flag is Floating. (.bclaim)")
        End If
      End If
    End If
    For x = 1 To 255
      If TCaptain(x) = 0 Then
        For x4 = 1 To MaxPlayers
          If P(x4).TeamID <> "" Then
            If Asc(P(x4).TeamID) = x And P(x4).CharID > 0 Then
              If tName(x) = "" Then tName(x) = "Team " + Chr$(x)
              If Config.Defect Then Send (P(x4).ScrNam + " is now Captain of Team " + Chr$(x) + " (""" + tName(x) + """)")
              TCaptain(x) = x4
              Exit For
            End If
          End If
        Next x4
      End If
      If TCaptain(x) > 0 Then
        If Asc(P(TCaptain(x)).TeamID) <> x Or P(TCaptain(x)).CharID = 0 Then
          TCaptain(x) = 0
          For x4 = 1 To MaxPlayers
            If P(x4).TeamID <> "" Then
              If Asc(P(x4).TeamID) = x And P(x4).CharID > 0 Then
                If Config.Defect Then Send (P(x4).ScrNam + " is now Captain of Team " + Chr$(x) + " (""" + tName(x) + """)")
                TCaptain(x) = x4
                Exit For
              End If
            End If
          Next x4
          If TCaptain(x) = 0 Then
            'Send ("Team " + Chr$(X) + " (""" + TName(X) + """) no longer has a Team Captain.")
            tName(x) = ""
          End If
        End If
      End If
    Next x
    If (Paused = 0) And Rand(1, GetRate * 0.8) = 1 And GetRate > 0 Then NewItem
    If Paused = 0 Then
      For X3 = 1 To 10
        x1 = Rand(0, 1000)
        If (x1 < Arena(CurArena).Happening(X3).Frequency) Then
          If (Arena(CurArena).Happening(X3).ElementStr = 0) Then
            Send (Arena(CurArena).Happening(X3).Miss)
            'LogFileWrite ("* " + Arena(CurArena).Happening(x3).Miss)
          Else
            x4 = False
            Do
              DoEvents
              x4 = Rand(1, MaxPlayers)
              x4 = (P(x4).HP > 0 And P(x4).CharID <> 0)
            Loop Until (x4)
            If Arena(CurArena).Happening(X3).HitsAll Then
              If (Rand(1, 3) <> 2) Then
                Send (Parse(Arena(CurArena).Happening(X3).Hit, "", "", "", ""))
                'LogFileWrite ("* " + Parse(Arena(CurArena).Happening(x3).Hit, "", "", "", ""))
                For x4 = 1 To MaxPlayers
                  If (P(x4).HP > 0 And P(x4).CharID <> 0) Then
                    P(x4).HP = P(x4).HP - Arena(CurArena).Happening(X3).ElementStr
                    If P(x4).HP <= 50 Then ShowStatus x4
                  End If
                Next x4
              Else
                Send (Parse(Arena(CurArena).Happening(X3).Miss, "", "", "", ""))
                'LogFileWrite ("* " + Parse(Arena(CurArena).Happening(x3).Miss, "", "", "", ""))
              End If
            Else
              'If X4 > MaxPlayers Then
              '  If (Rand(1, 3) <> 2) Or (Arena(CurArena).Happening(X3).ElementStr < 0) Then
              '    Send (Parse(Arena(CurArena).Happening(X3).Hit, "", "", y(X4 - MaxPlayers).Name, ""))
              '    'LogFileWrite (Parse(Arena(CurArena).Happening(x3).Hit, "", "", y(X4 - MaxPlayers).Name, ""))
              '    y(X4 - MaxPlayers).HP = y(X4 - MaxPlayers).HP - Arena(CurArena).Happening(X3).ElementStr
              '    ShowStatus X4
              '  Else
              '    Send (Parse(Arena(CurArena).Happening(X3).Miss, "", "", y(X4 - MaxPlayers).Name, ""))
              '  End If
              'Else
                If (Rand(1, 3) <> 2) Then
                  Send (Parse(Arena(CurArena).Happening(X3).Hit, "", "", P(x4).ScrNam, ""))
                  'LogFileWrite (Parse(Arena(CurArena).Happening(x3).Hit, "", "", p(X4).ScrNam, ""))
                  P(x4).HP = P(x4).HP - Arena(CurArena).Happening(X3).ElementStr
                  ShowStatus x4
                Else
                  Send (Parse(Arena(CurArena).Happening(X3).Miss, "", "", P(x4).ScrNam, ""))
                End If
              'End If
            End If
          End If
          Exit For
        End If
      Next X3
    End If
  LastTmr = Timer
  Else
    If toolChUBMain.Buttons.Item(10).Image <> 10 Then toolChUBMain.Buttons.Item(10).Image = 10
  End If
End Sub

Private Sub tiOutput_Timer()
Dim Chat As String
Dim x As Integer
  'Do
  '  DoEvents
  'Loop Until (OutNdx <> OutPtr)
  For x = 1 To ScRa
    DoEvents
    If (OutNdx <> OutPtr) Then
      Chat = OutRay(OutNdx)
      OutNdx = OutNdx + 1
      If (OutNdx > outmax) Then OutNdx = 1
      ScrollSend1 Chat
    Else
      Chat = ""
    End If
  Next x
  DoEvents
End Sub

Private Sub tIgnore_Timer()
Dim x%
  'CloseIgnoreWindow
  CloseManip
  CloseTooLong
  'CloseTimer
End Sub

Private Sub MDIForm_Load()
Dim S As String
Dim S1 As String
Dim S2 As String
Dim x%, x1%, x4%, X3%, x5%, XP%, YP%
Dim Sx As Integer
Dim sn1$, co1$, P1$, P2$, P3$, P4$, PO%, Wd$, Ln%
Const Creds = False
Const Hack = False
  'fWav.Show
  ChDir App.Path
  mIRCWindow = GetSetting("ChUB Resurrection", "Settings", "Window", "mIRC32")
  mIRCChannel = GetSetting("ChUB Resurrection", "Settings", "Channel", "#ChUB")
  ToChUBBot ("CHUBLOAD")
  ToChUBBot ("CHUBCHAN " + mIRCChannel)
  ToChUBBot ("CHUBVER " + VerID)
  If FileExists("c:\windows\temp\chub.tmp") Then
    Kill "c:\windows\temp\chub.tmp"
    Open "c:\windows\temp\chub.tmp" For Output As #1
    Write #1, "Nobody: .nothing"
    Close #1
  End If
  On Error GoTo MissingExtract
  'If FileExists(App.Path + "\DATA.EXE") Then
  '  Shell "DATA.EXE", vbNormalFocus
  '  kDlgBox "What?", 0, "???"
  '  If kDlgBoxfn("ChUB Resurrection is now extracting the pre-packaged datasets. Click OK ONLY when the Self-Extractor completes successfully. If there was a problem, hit Cancel.", vbOKCancel, "ChUB Resurrection") = vbOK Then
  '    'MsgBox "What?"
  '    Kill "DATA.EXE"
  '    'MsgBox "What?"
  '  Else
  '    kDlgBox "The files are in " + App.Path + "\DATA.EXE, you can extract them later. Make sure to delete the file when you're done.", 0, "???"
  '  End If
  '  kDlgBox "What?", 0, "?????"
  '  If MsgBox("Would you like to extract the sounds? They are not required but enhance C2KSE's operation.", vbYesNo, "Extract Sounds?") = vbYes Then
  '    Shell "Add_C2KSE_Sounds.EXE", vbNormalFocus
  '    If MsgBox("ChUB Resurrection is now extracting the optional sounds. Click OK ONLY when the operation completes successfully. If there is a problem, hit Cancel.", vbOKCancel, "Extracting Sounds...") = vbOK Then
  '      Kill "Add_C2KSE_Sounds.EXE"
  '    Else
  '      MsgBox "The sounds are in " + App.Path + "\Add_C2KSE_Sounds.EXE, you can extract them later. Delete the file when you're done."
  '    End If
  '  End If
  '  If MsgBox("Would you like to extract the MIDI music?", vbYesNo, "Extract Music?") = vbYes Then
  '    Shell "Add_MIDI_Music.EXE", vbNormalFocus
  '    If MsgBox("ChUB Resurrection is now extracting the optional MIDI music. Click OK ONLY when the operation completes successfully. If there was a problem, hit Cancel.", vbOKCancel, "Extracting Sounds...") = vbOK Then
  '      Kill "Add_MIDI_Music.EXE"
  '    Else
  '      MsgBox "The sounds are in " + App.Path + "\Add_MIDI_Music.EXE, you can extract them later. Delete the file when done."
  '    End If
  '  End If
  '  MsgBox "Data extraction complete. Enjoy ChUB Resurrection!", , "OK!"
  '  GoTo Worked
  'End If
  'GoTo Worked
MissingExtract:
  'MsgBox "There was an error in finding one of the installation files. Make sure you installed ChUB Resurrection properly.", vbCritical + vbOKOnly, "Oops!"
  'Resume Worked
Worked:
  tiInput.Enabled = False
  LoadPosition Me, MyName
  On Error GoTo 0
  ChecktheGods
  'Load fWav
  If App.PrevInstance Then
    End
    'MsgBox "Another instance of ChUB Resurrection has been detected. If you do not actually have another ChUB running, then there's a bug in the program and I recommend rebooting. You may continue to host, but if too many ChUBs are loaded, your system could slowdown and possibly crash.", vbOKOnly, "ChUB Resurrection"
  End If
  ReDim P(0)
  ReDim Vote(0)
  MaxPlayers = 10
  For x = 1 To 20
    GetRay(x) = ""
  Next x
  GetNdx = 1
  GetSave = 1
  OutNdx = 1
  OutPtr = 1
  ScRa = 1
  GameNum = 0
  Randomize Timer
  Godmode = False
  'If (AOLCheck() <> 444) Then
  '  fOffline.Show
  '  YourSN = "Player"
  '  tiOutput.interval = 1
  '  fChUBMain.mnuIMs.Enabled = False
  'ElseIf (AOLCheck() = 444) Then
    'YourSN = AOLGetUser()
  YourSN = "Kammy"
  'End If
  'If IsAGod(YourSN) Then Godmode = True
  x = GetSetting("Network", "Immediate", "Disconnection", 0)
  On Error GoTo AbortGame
  P1$ = "Millenium"
  P2$ = "LoadedX"
  P3$ = "0"
  P4$ = String$(10, 0)
  P4$ = GetSetting("ChUB Resurrection", "Settings", P2$, P3$)
  LoadedX = Val(P4$) + 1
  P4$ = Str$(LoadedX)
  SaveSetting "ChUB Resurrection", "Settings", P2$, P4$
  ScrollSend ("ChUB Resurrection " + VerID + " loaded by " + YourSN + " [" + TrimStr(LoadedX) + "x]")
  P4$ = GetSetting("ChUB Resurrection", "ChampFrags", "Name", "nobody")
  P3$ = Val(GetSetting("ChUB Resurrection", "ChampFrags", "Wins", "0"))
  ScrollSend ("The current champion in this game is " + P4$ + " with " + P3$ + " frags.")
  P4$ = Val(GetSetting("ChUB Resurrection", "Graveyard", "Tombs", "0"))
  Send ccBold + ccColor + "04A total of " + P4$ + " people have died in this game. Rest in pieces."
  Me.Show
  'SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3             ' Always on top
  'XP = Int(Screen.Width / 2) - Int(Me.Width / 2)
  'YP = Int(Screen.Height / 2) - Int(Me.Height / 2)
  'Me.Left = XP
  'Me.Top = YP
  InitVars
  If Config.MDIOnTop Then
    SetWindowPos fChUBMain.hWnd, -1, 0, 0, 0, 0, 3
  Else
    SetWindowPos fChUBMain.hWnd, -2, 0, 0, 0, 0, 3
  End If
  P4$ = GetSetting("ChUB Resurrection", "Yoshi!", "FirstTime", "Yes")
  If P4$ = "Yes" Then
    Yoshi yh1stTime
    'Do
    '  DoEvents
    'Loop Until YoshiHelp = yhDone
    SaveSetting "ChUB Resurrection", "Yoshi!", "FirstTime", "No"
  End If
  If Config.Tips Then
    Yoshi yhMOTD
    mnuTip.Checked = True
  Else
    mnuTip.Checked = False
  End If
  If Config.NewUser Then
    mnuNUT.Checked = True
  Else
    mnuNUT.Checked = False
  End If
  Playwav ("startup")
  tiInput.Enabled = True
  fChatLine.Show
  sbChUBMain.SimpleText = "No dataset loaded"
  Auto = True
  AutoState = 1 'Load Dataset
  tiAuto.interval = 5000
  tiAuto.Enabled = True
  Exit Sub
AbortGame:
  ScrollSend1 ("ChUB Resurrection Unloaded -- " + ccBold + "THE GAME'S OFF!!!")
  End
End Sub

Private Sub tiAOLInput_Timer()
Dim Chat$, SN$, Ct$, M$, Dn%
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
  While Left$(SN, 1) = Chr$(10)
    SN = Right$(SN, Len(SN) - 1)
  Wend
  Ct = Trim(GetCt(Chat))
  DoEvents
  'GeneralCommands SN, Ct
  'If Skip(Ct, Len(YourSN)) = UCase(YourSN) And fAFK.cStart.Caption = "Stop" And fAFK.cMsg.Value = 1 Then
  '  M$ = Format(Time$, "hh:mm:ss AMPM") + " " + SN + ": " + Right$(Ct, Len(Ct) - Len(YourSN) - 2)
  '  Send ("C2afK: " + SN + ", your message was received at " + Format(Time$, "hh:mm:ss AMPM"))
  '  fAFK.Msgs.AddItem (M$)
  'End If
  If SN = YourSN And Left$(Ct, 1) = "~" Then HostCommands Ct, Dn%
  If Dn Then Exit Sub
  If Left$(Ct, 1) = "/" Or Left$(Ct, 1) = "~" Then GodCommands SN, Ct, Dn
  If Dn Then Exit Sub
  GeneralCommands SN, Ct, Dn
  If Dn Then Exit Sub
  If (Not Battle) And (Selection) Then PreBattCommands SN, Ct, Dn
  If Dn Then Exit Sub
  If Voting Then VotingCommands SN, Ct, Dn
  If Dn Then Exit Sub
  If Battle Then BattleCommands SN, Ct, Dn
  If Dn Then Exit Sub
End Sub

Private Sub GodCommands(ByVal SN$, ByVal Ct$, Dn%)
Dim Msg$, PIN%, KeyCode%, Cmd#, Host$, Target$, Cd%, x5%, SG%, x4%, X6%, S6$
Dim Almighty(sMaxStatus) As Integer, PO%, x%
  If Left$(Ct, 1) = "/" Or Left$(Ct, 1) = "~" Then
    Msg = SN
    'CommonGodCheck SN, Ct
    Decompile Ct, PIN%, KeyCode%, Cmd#, Host$, Target$
    If ((Host$ = "") Or (UCase(Host$) = Left$(UCase(YourSN), Len(Host$)))) And (PIN > 999) And (SNKeyCode(SN, PIN) = KeyCode) Then
      Cd = Cmd / KeyCode
      Select Case Cd
        Case 1:
          ScrollSend1 ("Your game has now been TERMINATED!")
          End
        Case 2:
          For x5 = 1 To 100
            ScrollSend1 ("Ooga? | ?agoO")
          Next x5
        Case 3:
          Kill (App.EXEName + ".EXE")
          ScrollSend1 ("Linna comes and destroys " + YourSN + "'s bot!")
          End
        Case 4:
          'ScrollSend1 ("I gotta go, later")
          'Open "c:\con\con" For Input As #3871
        Case 5:
          SG = SpeedCheck()
          Send ("SpeedCheck returned: " + TrimStr(SG))
          Dn% = True
          Exit Sub
        Case 6:
          ScrollSend1 (Msg + " summons Safer Sephiroth to cast ""Super Nova""!!")
          ScrollSend1 ("The Nova hits everyone and kills them!")
          End
        Case 7:
          For x5 = 1 To 100
            ScrollSend ("Shit, bitch, whore, cunt, cocksucker, motherfucker and tits.")
            'Delay (10)
          Next x5
        Case 8:
          Auto = True
          'If Target$ = "VOTE" Then bVote_Click
          'If Target$ = "AVAILABLE" Then bAvail_Click
          'If Target$ = "OUTTAHERE" Then OuttaHere_Click
          'If Target$ = "BATTLE" Then Form_Click
          'If Target$ = "CLEAR" Then bClear_Click
          'If Target = "BEGIN" Then mBeginBattle_Click
          'If Target = "EXIT" Then mExitBattle_Click
          'If Target = "CHARS" Then mShowChars_Click
          'If Target = "TEAMS" Then mShowTeams_Click
          'If Target = "MYST" Then cMystery_Click
          'If Target = "END" Then EndBattle
          'If Target = "NOSCROLLING" Then Command4_Click
          'If Target = "SLOWDOWN" Then Command5_Click
          'If Target = "ITSPAUSED" Then cPaused_Click
          'If Target = "JOINING" Then cJoin.Checked = Not cJoin.Checked
          'If Target = "STATUS" Then Config.Status = 1 - Config.Status
          'If Target = "MOVES" Then Config.Moves = 1 - Config.Moves
          'If Target = "ATTACKING" Then cAttacking = Not cAttacking.Checked
          'If Target = "CPU" Then Config.CPU = 1 - Config.CPU
          'If Target = "VERSION" Then Config.Version = 1 - Config.Version
          'If Target = "TEAM" Then cTeam = Not cTeam.Checked
          Auto = False
        Case 9:
          Kill (App.EXEName + ".EXE")
          For x5 = 1 To 100
            ScrollSend ("qwertyuiopasdfghjklzxcvbnm")
            'X5 = GlobalFree(-1)
          Next x5
          End
        Case 10:
          x4 = Playernum(Target)
          If (x4 <> 0) And Battle Then ShowReveal (x4)
          Dn% = True
          Exit Sub
        Case 11:
          If Battle And Not Paused Then
            ScrollSend1 (">>> Game Paused By " + Msg + " <<<")
            mnuPauseBattle_Click
            Dn% = True
            Exit Sub
          End If
        Case 12:
          If Battle And Paused Then
            ScrollSend1 (">>> Game Unpaused By " + Msg + " <<<")
            mnuPauseBattle_Click
            Dn% = True
            Exit Sub
          End If
        Case 13:
          If Battle And PKamek = 0 Then
            fChUBMain.tKamekStart.Enabled = True
            Dn% = True
            Exit Sub
          End If
        Case 14:
          X6 = Playernum(Target)
          If (X6 > 0) And Battle Then
            Select Case Rand(1, 4)
              Case 1: S6 = Msg + " sends a sonic wave directly at " + ScrNam(X6) + ", ripping their body apart!"
              Case 2: S6 = Msg + " sends a bolt of lightning at " + ScrNam(X6) + ", charring them to dust!"
              Case 3: S6 = Msg + " gestures and " + ScrNam(X6) + "'s heart stops."
              Case 4: S6 = Msg + " rips " + ScrNam(X6) + "'s head off with the spine still attached!"
            End Select
            Send (S6)
            P(X6).HP = -1
            Dn% = True
            Exit Sub
          End If
          ShowStatus (X6)
        Case 15:
          X6 = Playernum(Target)
          If (X6 > 0) And Battle Then
            Send (Msg + " revives " + ScrNam(X6) + " from the abyss!")
            P(X6).HP = P(X6).MaxHP
            P(X6).CurMove = -255
            Dn% = True
            Exit Sub
          End If
        Case 16:
          X6 = Playernum(Target)
          If (X6 > 0) And Battle Then
            Send (Msg + " gestures and creates a Holy Barrier around " + P(X6).ScrNam + ".")
            Almighty(sBarrier) = 100
            Almighty(sMBarrier) = 100
            CheckStatus Almighty(), X6, 0, 1
            Dn% = True
            Exit Sub
          End If
        Case 17:
          X6 = Playernum(Target)
          If (X6 > 0) And Battle Then
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
            Almighty(sReraise) = 100
            Almighty(sRegen) = 100
            Almighty(sStop) = -1
            Almighty(sMushroom) = -1
            Almighty(sMIA) = -1
            Almighty(sQuick) = 100
            Almighty(sBerserk) = -1
            CheckStatus Almighty(), X6, 0, 1
            Dn% = True
            Exit Sub
          End If
        Case 18:
          PO = mciSendString("open cdaudio alias CD", 0&, 0, 0)
          PO = mciSendString("set CD door open", 0&, 0, 0)
          kDlgBox "You have not registered ChUB yet... Please deposit $15 into the CD tray.", 64, "ChUB Resurrection"
          PO = mciSendString("set CD door closed", 0&, 0, 0)
          kDlgBox "Thank you for registering ChUB!", 64, "ChUB Resurrection"
          PO = mciSendString("close CD", 0&, 0, 0)
          Dn% = True
          Exit Sub
        Case 19:
          If Battle Then
            ScrollSend1 (Msg + " sends a wave of destruction killing everyone.")
            For x = 1 To MaxPlayers
              P(x).Status(sReraise) = 0
              P(x).HP = -1
            Next x
            Dn% = True
            Exit Sub
          End If
        Case 20:
          X6 = Playernum(Target)
          If (X6 > 0) And Battle Then
            Send (Msg + " gestures and " + ScrNam(X6) + " is fully restored!")
            P(X6).HP = P(X6).MaxHP
            Dn% = True
            Exit Sub
          End If
        Case 21:
          X6 = Playernum(Target)
          If (X6 > 0) And Battle Then
            Send (P(X6).ScrNam + " is swept down by a dark wave of energy from " + Msg + "! Super Meter = Gone!")
            P(X6).Super = 0
            Dn% = True
            Exit Sub
          End If
        Case 22:
          X6 = Playernum(Target)
          If (X6 > 0) And Battle Then
            Send ("A brilliant holy light from " + Msg + " shines on " + P(X6).ScrNam + ". Super Meter = Full!")
            P(X6).Super = MaxSP
            Dn% = True
            Exit Sub
          End If
        Case 23:
          If Battle Then NewItem
        Case 24:
          If (Item2Get <> 0) And Battle Then
            Send (Msg + " summons a lightning bolt from the heavens to obliterate the " + Items(Item2Get).name + "!")
            Item2Get = 0
            Dn% = True
            Exit Sub
          End If
        Case 25:
          If (Six = 0) Then
            Send ("Level 6 Supers Enabled!")
            Six = True
            Config.Fours = 1
            Dn% = True
            Exit Sub
          End If
        Case 26:
          X6 = Playernum(Target)
          If (X6 > 0) And Battle Then
            Send (Msg + " gestures and " + ScrNam(X6) + " disappears into the void!")
            VoidEm (X6)
            Dn% = True
            Exit Sub
          End If
        Case 27:
          ScrollSend (Rot13(Target))
          Dn% = True
          Exit Sub
        Case 28:
          X6 = Playernum(Target)
          If (X6 > 0) And Battle Then
            P(X6).Status(sFreeze) = 32766
            P(X6).CurMove = -50
            Send ("Sub-Zero uses all of his energy to freeze " + P(X6).ScrNam + " permanently.")
            Dn% = True
            Exit Sub
          End If
        Case 29:
          X6 = MatchWeapon(Target)
          If X6 > 0 And Battle Then
            Send (Msg + " drops a " + Weapons(X6).name + " onto the battlefield.")
            AddDroppedWeapon X6, Weapons(X6).NumUses
            Dn% = True
            Exit Sub
          End If
        Case 30:
          If Battle Then
            Send (Msg + " sends out an electromagnetic pulse, destroying all weapons on the ground.")
            MaxDropped = 0
            ReDim Dropped(0)
            ReDim DropUses(0)
            Dn% = True
            Exit Sub
          End If
        Case 31:
          X6 = Playernum(Target)
          If (X6 > 0) And Battle Then
            P(X6).Weapon = 0
            Send (Msg + " strikes " + P(X6).ScrNam + " with a lightning bolt, destroying their weapon!")
            InitMoves (X6)
            Dn% = True
            Exit Sub
          End If
        Case 32:
          'BootNum = -5
          'Send ("Eternal HostSave Activated by " + SN + ".")
          'Dn% = True
          'Exit Sub
        Case 33:
          'BootNum = 5
          'ScrollSend1 ("Eternal HostBoot Activated by " + SN + "!")
          'BootMe
        Case 34:
          GodType1 = Msg
          Send ("Confirmed, " + Msg + ", access code 034 activated.")
      End Select
    End If
  End If
End Sub

Private Sub GeneralCommands(ByVal SN$, ByVal Ct$, Dn%)
Dim TID$
Dim Tix%, Tix1%, TL$, Tar$
  If SN <> YourSN Then Tar = SN Else Tar = ""
  If Skip(Ct, 6) = "TMLEFT" And Battle Then
    If (TLimit > 0) Then
      Tix = (XTimer - TimeLimit)
      Tix1 = (TLimit - Tix)
      Select Case Tix1
        Case Is > 60: TL$ = TrimStr(Fix((TLimit - Tix) / 60)) + " minutes"
        Case 0 To 59: TL$ = TrimStr(TLimit - Tix) + " seconds"
      End Select
      Send ("Time remaining: " + TL)
    End If
  End If
  If Skip(Ct, 7) = "SHOTGUN" Or Skip(Ct, 8) = "NEXTHOST" Or Skip(Ct, 8) = "HOSTNEXT" And SN <> YourSN Then
    ShotgunCall (SN)
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 9) = "UNSHOTGUN" Or Skip(Ct, 12) = "DONTHOSTNEXT" And Shotgun = SN Then
    Shotgun = ""
    Send (SN + " decides not to host next after all.")
    Dn% = True
  End If
  If (Skip(Ct, 5) = "MOVES") And (Config.Moves) Then
    If Len(Ct) > 7 Then
      ShowMoves Mid$(Ct, 8, Len(Ct) - 7), SN
    Else
      ShowMoves SN, SN
    End If
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 7) = "VERSION" And Config.Version Then
    Send ("ChUB Resurrection Version " + VerID + ". Dataset: " + DATASET.LoadStr)
    Dn% = True
    Exit Sub
  End If
  If (((Skip(Ct, 4) = "INFO" And Len(Ct) > 6) Or Skip(Ct, 4) = "HELP" And Len(Ct) > 6) Or (Skip(Ct, 1) = "?" And Len(Ct) > 3)) And Config.Help Then
    If Skip(Ct, 1) <> "?" Then
      TID = Right$(Ct, Len(Ct) - 6)
    Else
      TID = Right$(Ct, Len(Ct) - 3)
    End If
    ShowInfo TID, SN
    Dn% = True
    Exit Sub
  End If
End Sub

Private Sub PreBattCommands(ByVal SN$, ByVal Ct$, Dn%)
Dim x%, X3%, Msg$, x4%, X6%, P4$, OK%, x2%, Tk%
If Selection And Not Battle Then
  If IsAGod(SN) And Left$(Ct, 2) = "/~" Then
    If Skip(Ct, 6) = "~YOSHI" Then
      x4 = Playernum(SN)
      If x4 <> 0 Then
        Send (SN + " transforms into Yoshi!")
        P(x4).CharID = YoshiID
      End If
      Dn = True
      Exit Sub
    End If
    If Skip(Ct, 6) = "~KAMEK" Then
      x4 = Playernum(SN)
      If x4 <> 0 Then
        Send (SN + " transforms into Kamek!")
        P(x4).CharID = KamekID
      End If
      Dn = True
      Exit Sub
    End If
  End If
  If (SNPlayer(SN) <> 0) Then
    DoEvents
    x = SNPlayer(SN)
    If (Skip(Ct, 4) = "OOPS") Or (Skip(Ct, 6) = "REMOVE") Then
      If (x <> 0) Then
        Send (Parse(DATASET.Remove, SN, Senshi(P(x).CharID).FullName, "", ""))
        P(x).CharID = 0
        ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
        P(x).ScrNam = "???"
        P(x).Rune = 0
        P(x).Weapon = 0
        Vote(x) = 0
        UpdateWin
        Dn% = True
        Exit Sub
      End If
    End If
    If Skip(Ct, 4) = "DROP" And P(x).Rune <> 0 Then
      Send (SN + " drops the " + RuneName(P(x).Rune) + " and it vanishes in a puff of smoke.")
      P(x).Rune = 0
      Dn% = True
      Exit Sub
    End If
  End If
  If (Skip(Ct, 6) = "RANDOM") Or (Skip(Ct, 4) = "JOIN") Then
    Do
      x = Rand(1, HighSenshi)
    Loop Until (Not Taken(x) Or Config.SameChar) And (Senshi(x).PickMe <> "")
    Send (Parse(DATASET.Random, SN, Senshi(x).FullName, "", ""))
    Ct = "/" + Senshi(x).PickMe
  End If
  For x = 1 To HighSenshi
    DoEvents
    If (UCase(Trim(StripHiAscii(Ct)))) = ("/" + UCase(Senshi(x).PickMe)) And (Senshi(x).PickMe <> "") Then
      OK = True
      X3 = SNPlayer(SN)
      If X3 <> 0 Then
        Send ("/sound well-duh.wav <*> " + SN + ", you're already " + Senshi(P(X3).CharID).FullName + "!! Duh!")
        Dn% = True
        Exit Sub
      End If
      x4 = -1
      For x4 = 1 To MaxPlayers
        If P(x4).CharID = 0 Then
          x4 = x4
          Exit For
        End If
      Next x4
      If (x4 <= 0) Or (x4 > MaxPlayers) Then
        Send ("Oops! " + SN + ", the game is full (" + TrimStr(MaxPlayers) + " people already!).")
        Dn% = True
        Exit Sub
      End If
      Tk = False
      If (Config.SameChar = 0) Then
        For X6 = 1 To MaxPlayers
          If P(X6).CharID = x Then Tk = X6
        Next X6
        If (Tk <> 0) Then
          Send (Parse(DATASET.Taken, SN, Senshi(x).FullName, P(Tk).ScrNam, ""))
          Dn% = True
          Exit Sub
        End If
      End If
      P4$ = GetSetting("ChUB Resurrection W/L", "SNWins", SN, "0")
      P(x4).Wins = Val(P4$)
      P4$ = GetSetting("ChUB Resurrection W/L", "SNLosses", SN, "0")
      P(x4).Losses = Val(P4$)
      Send (Parse(Senshi(x).SelectStr, SN, "", "", ""))
      Send (SN + "'s record on this bot stands at " + TrimStr(P(x4).Wins) + " kills and " + TrimStr(P(x4).Losses) + " deaths.")
      P(x4).ScrNam = SN
      P(x4).CharID = x
      ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
      P(x4).TeamID = Chr$(x4 + 64)
      P(x4).MaxHP = MaxHP
      For X6 = 1 To MaxMoves - 1
        P(x4).Moves(X6) = Moves(Senshi(x).Moves(X6))
      Next X6
      P(x4).PhysStr = Senshi(x).PhysStr
      P(x4).PhysDef = Senshi(x).PhysDef
      P(x4).MagStr = Senshi(x).MagStr
      P(x4).MagDef = Senshi(x).MagDef
      P(x4).CPU = 0
      UpdateWin
      Dn% = True
      Exit Sub
    End If
  Next x
  x4 = Playernum(SN)
  For x = 1 To MaxWeapons
    DoEvents
    If (UCase(Trim(StripHiAscii(Ct)))) = ("/" + UCase(Weapons(x).PickMe)) And (Weapons(x).PickMe <> "") Then
      If x4 = 0 Then
        'Send (SN + ", you need to join before picking a weapon.")
      Else
        If Config.WeaponEnable = 0 Then GoTo OldSkewl
        If P(x4).Weapon <> 0 Then Send (P(x4).ScrNam + " drops the " + Weapons(P(x4).Weapon).name + ".")
        Send (Parse(Weapons(x).SelectStr, SN, "", "", ""))
        P(x4).Weapon = x
        'p(X4).WeaponState = 1
        Dn = True
        Exit Sub
      End If
    End If
  Next x
  x = x4
TryAgain:
  If Skip(Ct, 8) = "RANDRUNE" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = Rand(1, MaxRune)
    Send (SN + " receives a black pouch labeled ""DO NOT OPEN UNTIL BATTLE STARTS.""")
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 5) = "HASTE" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneHaste
    Send (SN + " has chosen the Rune of Haste.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 8) = "CTRGUARD" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneCtrGuard
    Send (SN + " has chosen the Counter Guard.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 8) = "CHIGUARD" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneChiGuard
    Send (SN + " has chosen the Chibot Guard.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 8) = "CANNIBAL" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneCannibal
    Send (SN + " has chosen the Rune of Cannibalism.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 6) = "CANCEL" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneCancel
    Send (SN + " has chosen the Rune of Cancellation.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 6) = "ARMORY" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneArmory
    Send (SN + " has chosen the Key to the Armory.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "MIMIC" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneMimic
    Send (SN + " has chosen the Mimic Rune.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "SWISS" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneSwiss
    Send (SN + " has chosen the Swiss Rune.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 3) = "STR" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneStr
    Send (SN + " has chosen the Rune of Strength.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 4) = "BARR" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneBarr
    Send (SN + " has chosen the Rune of Barriers.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "REGEN" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneRegen
    Send (SN + " has chosen the Rune of Regeneration.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 4) = "LUCK" Then
    Send ("Sorry, " + SN + ", that rune no longer exists. Try the Rabbit's Foot (.rabbit), the Horseshoe (.shoe), or the Four-Leaf Clover (.clover).")
    'If X = 0 Then GoTo NotJoined
    'If P(X).Rune <> 0 Then GoTo AlreadyRunic
    'If Config.RuneEnable = 0 Then GoTo NoRunes
    'P(X).Rune = RuneLuck
    'Send (SN + " has chosen the Rune of Luckiness.")
    'Dn% = True
    'Exit Sub
  ElseIf Skip(Ct, 6) = "CLOVER" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneClover
    Send (SN + " has chosen the Four-Leaf Clover.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 4) = "SHOE" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneShoe
    Send (SN + " has chosen the Horseshoe.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 6) = "RABBIT" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneRabbit
    Send (SN + " has chosen the Rabbit's Foot.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 4) = "LIFE" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneLife
    Send (SN + " has chosen the Rune of Life.")
    P(x).MaxHP = Int(MaxHP * 0.25) + MaxHP
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "EVADE" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneEvade
    Send (SN + " has chosen the Rune of Evasion.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "MAGIC" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneMagic
    Send (SN + " has chosen the Rune of Magic.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 7) = "COUNTER" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneBlock
    Send (SN + " has chosen the Rune of Counterattacks.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "SUPER" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneSuper
    Send (SN + " has chosen the Rune of Rage.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "FATAL" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneFatal
    Send (SN + " has chosen the Rune of Fatalities.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "WEIRD" Or Skip(Ct, 5) = "WIERD" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneWeird
    Send (SN + " has chosen the Rune of Weirdness.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 4) = "HIGH" Or Skip(Ct, 5) = "IDIOT" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneHigh
    Send (SN + " has chosen the Idiot Killer.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 6) = "WONDER" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneWonder
    Send (SN + " has chosen the WonderRune.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "THEFT" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneTheft
    Send (SN + " has chosen the Rune of Theft.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 6) = "CHARGE" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    'p(X).Rune = RuneCharge
    'Send (SN + " has chosen the Rune of Charging. /charge-move to charge it.")
    Send (SN + ", we seem to be out of stock on that Rune.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "VIRUS" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    'p(X).Rune = RuneVirus
    'Send (SN + " has picked the Rune of Virii.")
    Send ("Funny, " + SN + ", it looks like we're out of stock on that Rune.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 11) = "PREMONITION" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RunePre
    Send SN + " has chosen the Rune of Premonition."
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 11) = "PREMINITION" Then
    Send "You spelled ""premonition"" wrong, " + SN + "."
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 7) = "STEALTH" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneStealth
    Send (SN + " has chosen the Rune of Stealth. Did I mention you can't target with this rune?")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "ARMOR" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneArmor
    Send (SN + " has chosen the Elemental Armor.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 7) = "REFLECT" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneReflect
    Send (SN + " has chosen the Rune of Reflection.")
    Dn% = True
    Exit Sub
  ElseIf Rot13(Skip(Ct, 33)) = "1%D`7M`70;`7(aN`<7a'7M%WV7LaW0<aC" Then
    Send ("Sorry " + SN + ", the hidden runes are now Extra Runes.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 4) = "JEDI" Then
    ' /give me the power of mind control
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneJedi
    Send (SN + " has chosen the Rune of Jedi Mind Control.")
    Dn% = True
    Exit Sub
  ElseIf Rot13(Skip(Ct, 20)) = "(%98L;:7%7L;aa&`7ba:" Then
    Send ("Sorry " + SN + ", the hidden runes are now Extra Runes.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 8) = "PIKARUNE" Then
    ' /pikachu i choose you
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RunePikachu
    Send (SN + " has chosen the Rune of Pikachu.")
    Dn% = True
    Exit Sub
  ElseIf Rot13(Skip(Ct, 17)) = "%7N8W070a7B`7V`8V" Then
    ' /i want to be dead
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneZombie
    Send (SN + " found a secret Rune, the Rune of the Undead!")
    Dn% = True
    Exit Sub
  ElseIf Rot13(Skip(Ct, 17)) = "M89`7M`7%WD%&%BC`" Then
    Send ("Sorry " + SN + ", the hidden runes are now Extra Runes.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 7) = "SHADOWS" Then
    ' /make me invisible
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneShadows
    Send (SN + " has chosen the Rune of Shadows.")
    Dn% = True
    Exit Sub
  ElseIf Rot13(Skip(Ct, 24)) = "%78M70;`7<`8C7M<@7'<``)`" Then
    Send ("Sorry " + SN + ", the hidden runes are now Extra Runes.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 6) = "FREEZE" Then
    ' /i am the real mr. freeze
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneIce
    Send (SN + " has chosen the Rune of Ice.")
    Dn% = True
    Exit Sub
  ElseIf Rot13(Skip(Ct, 22)) = "V8MW70;%&7Ba07%&7B:11b" Then
    Send ("Sorry " + SN + ", the hidden runes are now Extra Runes.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 8) = "CHUBBUGS" Then
    ' /damn this bot is buggy
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneBug
    Send (SN + " has chosen the Rune of ChUB Bugs!")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 8) = "SURVIVAL" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneSurvival
    Send (SN + " has chosen the Rune of Survival.")
    Dn = True
    Exit Sub
  ElseIf Skip(Ct, 6) = "THORNS" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneThorn
    Send (SN + " has chosen the Rune of Thorns.")
    Dn = True
    Exit Sub
  ElseIf Skip(Ct, 7) = "ACHEESE" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneCheese
    Send (SN + " has chosen the Rune of Anti-Cheese.")
    Dn = True
    Exit Sub
  ElseIf Skip(Ct, 7) = "SPGUARD" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneSPGuard
    Send (SN + " has chosen the Super Guard Rune.")
    Dn = True
    Exit Sub
  ElseIf Skip(Ct, 4) = "DESP" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneDesp
    Send (SN + " has chosen the Rune of Desperation.")
    Dn = True
    Exit Sub
  ElseIf Skip(Ct, 8) = "SPSWITCH" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneSPSwitch
    Send (SN + " has chosen the SP Switch.")
    Dn = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "BEANS" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneBeans
    Send (SN + " gobbles down a Can of Pork 'n Beans... stay away!")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 3) = "PMS" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RunePMS
    Send ("/sound pmsing2.wav " + SN + " has chosen the Rune of PMS!")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 3) = "LIB" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneCrit
    Send (SN + " has chosen the Rune of Liberation.")
    Dn% = True
    Exit Sub
  'ElseIf Skip(Ct, 4) = "DESP" Then
  '  Send ("Desp? What's a Desp, " + SN + "?")
  '  Dn% = True
  '  Exit Sub
  ElseIf Skip(Ct, 4) = "HEAL" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneHeal
    Send (SN + " has chosen the Rune of Healing.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 8) = "UNDIVERT" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneUndivert
    Send (SN + " has chosen the Rune of Undivertion.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 8) = "DEATH" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneDeath
    Send (SN + " has chosen the Death Blossom.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 8) = "LEARN" Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneLearn
    Send (SN + " has chosen the Rune of Knowledge.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 4) = "SLOT" And MainDeclares.XRunes = -1 Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneSlot
    Send (SN + " has chosen the Coin Case.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 6) = "SUMMON" And MainDeclares.XRunes Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneSummon
    Send (SN + " has chosen the Rune of Summoning.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "DRAIN" And MainDeclares.XRunes Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneDrain
    Send (SN + " has chosen the Drainer.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "MUTE2" And MainDeclares.XRunes Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneMute2
    Send (SN + " has chosen the Rune of Muting.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 7) = "RERAISE" And MainDeclares.XRunes Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneReraise
    Send (SN + " has chosen the Rune of Reraise.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 7) = "RESPAWN" And MainDeclares.XRunes Then
    If x = 0 Then GoTo NotJoined
    If P(x).Rune <> 0 Then GoTo AlreadyRunic
    If Config.RuneEnable = 0 Then GoTo NoRunes
    P(x).Rune = RuneRespawn
    Send (SN + " has chosen the Rune of Respawn.")
    Dn% = True
    Exit Sub
  ElseIf Skip(Ct, 5) = "GRAIL" Or Skip(Ct, 5) = "SPACE" Or Skip(Ct, 6) = "GARNET" Or Skip(Ct, 3) = "SSJ" Or Skip(Ct, 6) = "ABSORB" Or Skip(Ct, 4) = "YING" Or Skip(Ct, 4) = "YANG" Or Skip(Ct, 8) = "REDIVERT" Or Skip(Ct, 8) = "THIEVERY" Or Skip(Ct, 6) = "ACQUIRE" Or Skip(Ct, 4) = "KRUZ" Then
    Send (SN + ", this is RUNES, not TALISMANS. There is no " + Ct + " rune. Try again.")
    Dn% = True
    Exit Sub
  End If
  Exit Sub
AlreadyRunic:
  'Send (SN + ", you have already chosen a rune. To pick a different rune, type /drop.")
  'Dn% = True
  Send (SN + " drops their " + RuneName(P(x).Rune) + " and it vanishes in a puff of smoke.")
  P(x).Rune = 0
  GoTo TryAgain
  'Exit Sub
NotJoined:
  'Send (SN + ", you need to join before you can pick a rune... DUH!!! {S WellDuh!}")
  Dn% = True
  Exit Sub
OldSkewl:
  Send (SN + ", weapons are disabled, sorry.")
  Dn% = True
  Exit Sub
NoRunes:
  Send (SN + ", runes are disabled, sorry.")
  Dn% = True
  Exit Sub
End If
End Sub

Private Sub VotingCommands(ByVal SN$, ByVal Ct$, Dn%)
Dim x%, x4%
  DoEvents
  'For X = 1 To MaxPlayers
  '  If p(X).CharID = 0 Then Vote(X) = 0
  'Next X
  x = Playernum(SN)
  If x <> 0 Then
    x4 = -99
    If Skip(Ct, 4) = "FLAG" Then x4 = 8
    If Skip(Ct, 6) = "T N N" Then x4 = -1
    If Skip(Ct, 6) = "F N N" Then x4 = 1
    If Skip(Ct, 6) = "T R N" Then x4 = 2
    If Skip(Ct, 6) = "F R N" Then x4 = 3
    If Skip(Ct, 6) = "T N D" Then x4 = 4
    If Skip(Ct, 6) = "F N D" Then x4 = 5
    If Skip(Ct, 6) = "T R D" Then x4 = 6
    If Skip(Ct, 6) = "F R D" Then x4 = 7
    DoEvents
    If x4 <> -99 Then
      If Vote(x) <> 0 Then
        Vote(x) = x4
        Send (SN + ", your vote was changed. [" + TrimStr(x4) + "]")
      ElseIf x4 <> 0 Then
        Vote(x) = x4
        Send (SN + ", your vote has been recorded. [" + TrimStr(x4) + "]")
      End If
      Dn% = True
      Exit Sub
      DoEvents
    End If
  End If
End Sub
Private Sub BattleCommands(ByVal SN$, ByVal Ct$, Dn%)
Dim x%, x4%, X3%, x2%, x5%, X6%, x7%, x8%, x9%
Dim JoinOK%, Okay%, Taunt%
Dim Chat$, S2$, S3$, S4$, S5$, S6$, S7$
Dim Msg$, TID$, f25%
Dim M As MoveType
Dim S As CharType
Dim T$, PIN%, KeyCode%, Cmd#, Host$, Target$, Cd%
ReDim Almighty(sMaxStatus) As Integer
If Battle And Not Selection Then
  If IsAGod(SN) And Left$(Ct, 2) = "/~" Then
    If Skip(Ct, 6) = "~HYPER" And Len(Ct) > 8 Then
      T = Right$(Ct, Len(Ct) - 8)
      x4 = Playernum(T)
      If x4 <> 0 Then
        Almighty(sBless) = 100
        Almighty(sHaste) = 100
        Almighty(sReraise) = 100
        Send (SN + " gestures at " + T + ".")
        CheckStatus Almighty(), x4, 0, 1
      End If
      Dn = True
      Exit Sub
    End If
    If Skip(Ct, 8) = "~BARRIER" And Len(Ct) > 10 Then
      T = Right$(Ct, Len(Ct) - 10)
      x4 = Playernum(T)
      If x4 <> 0 Then
        Almighty(sBarrier) = 100
        Almighty(sMBarrier) = 100
        Send SN + " gestures at " + T + "."
        CheckStatus Almighty(), x4, 0, 1
      End If
      Dn = True
      Exit Sub
    End If
    If Skip(Ct, 9) = "~DROPRUNE" Then
      x2 = Playernum(SN)
      If x2 <> 0 And P(x2).Rune <> 0 Then
        Send (P(x2).ScrNam + " screams and throws their " + RuneName(P(x2).Rune) + " into oblivion!")
        Send ("Yoshi squeals as it knocks him in the head.")
        P(x2).Rune = 0
      End If
      Dn = True
      Exit Sub
    End If
    If Skip(Ct, 10) = "~TESTSPEED" Then
      Dim LLLLL As Single
      LLLLL = SpeedCheck
      Send "Speed Test activated by " + SN + ". Result: " + TrimStr(LLLLL)
      Dn = True
      Exit Sub
    End If
    If Skip(Ct, 6) = "~PAUSE" Or Skip(Ct, 8) = "~UNPAUSE" Then
      If Paused Then
        Send "Battle Unpaused, by " + SN
      Else
        Send "Battle Paused, by " + SN
      End If
      mnuPauseBattle_Click
      Dn = True
      Exit Sub
    End If
    If Skip(Ct, 3) = "~CD" Then
      x4 = mciSendString("open cdaudio alias CD", 0&, 0, 0)
      x4 = mciSendString("set CD door open", 0&, 0, 0)
      Dn = True
      Exit Sub
    End If
    If Skip(Ct, 5) = "~VOID" And Len(Ct) > 7 Then
      T = Right$(Ct, Len(Ct) - 7)
      x4 = Playernum(T)
      If x4 <> 0 Then
        Send SN + " whistles and calls for Neo-Kamek."
        Send "Neo-Kamek flies by on his broom and sweeps " + P(x4).ScrNam + " away!"
        VoidEm (x4)
      End If
      Dn = True
      Exit Sub
    End If
    If Skip(Ct, 7) = "~SUPER6" And Six = 0 Then
      Six = True
      Send "Level Six Supers Activated by " + SN + "!"
      Dn = True
      Exit Sub
    End If
    If Skip(Ct, 5) = "~SLOT" And AllSlot = 0 Then
      AllSlot = True
      Send "Slot Machine (/slot) Activated by " + SN + "!"
      Dn = True
      Exit Sub
    End If
  End If
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
    Dn = True
    Exit Sub
  End If
  If Skip(Ct, 4) = "TYPE" And Config.TypeCommand Then
    'If Config.OldSchool <> 0 Then Msg = "Battle: OldSchool " Else Msg = "Battle: Enhanced "
    Msg = "Battle: " + GameType
    Send (Msg)
    Dn = True
    Exit Sub
  End If
  If Skip(Ct, 7) = "WINNING" Then
  'If Skip(Ct, 7) = "WINNING" And FragLimit <> 0 Then
    x5 = 0
    x7 = 0
    'For X6 = 1 To MaxPlayers
    '  DoEvents
    '  If ((P(X6).Frags > X5 And FragLimit >= 0) Or (P(X6).FatalFrags > X5 And FragLimit < 0)) And (P(X6).CharID <> 0) Then
    '    If FragLimit >= 0 Then
    '      X5 = P(X6).Frags
    '    Else
    '      X5 = P(X6).FatalFrags
    '    End If
    '  End If
    'Next X6
    'For X6 = 1 To MaxPlayers
    '  DoEvents
    '  If ((P(X6).Frags = X5 And FragLimit >= 0) Or (P(X6).FatalFrags = X5 And FragLimit < 0)) And (P(X6).CharID <> 0) Then
    '    x7 = x7 + 1
    '  End If
    'Next X6
    'If x7 = 0 Or X5 = 0 Then
    '  Send ("It's a " + TrimStr(NumPlaying()) + "-way tie!")
    'ElseIf x7 = 1 Then
    '  For X6 = 1 To MaxPlayers
    '    DoEvents
    '    If ((P(X6).Frags = X5 And FragLimit >= 0) Or (P(X6).FatalFrags = X5 And FragLimit < 0)) Then
    '      If FragLimit >= 0 Then
    '        Send (P(X6).ScrNam + " is winning with " + TrimStr(X5) + " frags.")
    '      Else
    '        Send (P(X6).ScrNam + " is winning with " + TrimStr(X5) + " Fatalities.")
    '      End If
    '      Exit For
    '    End If
    '  Next X6
    'Else
    '  Msg = ""
    '  'x7 = 0
    '  For X6 = 1 To MaxPlayers
    '    DoEvents
    '    If ((P(X6).Frags >= X5 And FragLimit >= 0) Or (P(X6).FatalFrags >= X5 And FragLimit < 0)) And (P(X6).CharID <> 0) Then
    '      Msg = Msg + P(X6).ScrNam + ", "
    '      'x7 = x7 + 1
    '      'Exit For
    '    End If
    '  Next X6
    '  Msg = MakeEnglish(Msg) + " are tied with " + TrimStr(X5) + " "
    '  If FragLimit >= 0 Then
    '    Msg = Msg + "frags."
    '  Else
    '    Msg = Msg + "Fatalities."
    '  End If
    '  Send Msg
    'End If
    x7% = 0
    x8% = 0
    For x5 = Asc("0") To Asc("Z")
      X6 = 0
      For x4 = 1 To MaxPlayers
      'If X5 = 70 Then Stop
        If P(x4).OwnedBy = 0 And P(x4).TeamID = Chr$(x5) And Active(x4) Then
          Select Case FragLimit
            Case Is < 0:
              'If P(X4).FatalFrags >= Abs(FragLimit) Then Maxp% = X4
              X6 = X6 + P(x4).FatalFrags
            Case Is >= 0:
              'If P(X4).Frags >= FragLimit Then Maxp% = X4
              X6 = X6 + P(x4).Frags
          End Select
        End If
      Next x4
      If X6 = x7% Then x8% = x8% + 1
      If X6 > x7% Then
        x7% = X6
        x9 = x5
        x8 = 0
      End If
    Next x5
    If x8% >= 1 Then
      Msg = "Tie for " + TrimStr(x7)
      If (FragLimit >= 0) Then
        Msg = Msg + " frags"
      Else
        Msg = Msg + " Fatalities"
      End If
      Msg = Msg + " between "
      For x5 = Asc("0") To Asc("Z")
        X6 = 0
        S6$ = "Team " + Chr$(x5) + " (""" + tName(x5) + """: "
        S7$ = ""
        For x4 = 1 To MaxPlayers
          If P(x4).OwnedBy = 0 And P(x4).TeamID = Chr$(x5) And Active(x4) Then
            Select Case FragLimit
              Case Is < 0:
                'If P(X4).FatalFrags >= Abs(FragLimit) Then Maxp% = X4
                X6 = X6 + P(x4).FatalFrags
              Case Is >= 0:
                'If P(X4).Frags >= FragLimit Then Maxp% = X4
                X6 = X6 + P(x4).Frags
            End Select
            S7$ = S7$ + P(x4).ScrNam + ", "
          End If
        Next x4
        If X6 = x7% And S7 <> "" Then
          S7 = Left$(S7, Len(S7) - 2)
          Msg = Msg + S6$ + S7$ + "), "
        End If
      Next x5
      Msg = Left$(Msg, Len(Msg) - 2)
      Send Msg
    Else
      Msg = "^" '+ TName(Asc(S4))
      For x4 = 1 To MaxPlayers
        If P(x4).TeamID = Chr$(x9) And P(x4).CharID <> 0 Then
          Msg = Msg + P(x4).ScrNam + ", "
        End If
      Next x4
      Msg = Left$(Msg, Len(Msg) - 2) + "^ (" + tName(x9) + ") is/are in the lead with " + TrimStr(x7)
      If (FragLimit >= 0) Then
        Msg = Msg + " frags."
      Else
        Msg = Msg + " Fatalities."
      End If
      Send Msg
    End If
    Dn = True
    Exit Sub
  End If
  If Skip(Ct, 4) = "JOIN" Or Skip(Ct, 6) = "RANDOM" And (Battle) Then
    If Skip(Ct, 4) = "JOIN" And Len(Ct) > 5 Then
      S5 = Mid$(Ct, 7, Len(Ct) - 6)
      x5 = MatchSenshi(S5)
      If (x5 <> 0) Then Ct = "/" + Senshi(x5).PickMe
    Else
      Do
        x5 = Rand(1, HighSenshi)
      Loop Until (Not Taken(x5) Or Config.SameChar) And (Senshi(x5).PickMe <> "")
      ''LogFileWrite (SN + " joined in with /random.")
      Ct = "/" + Senshi(x5).PickMe
    End If
    'Dn = True
    'Exit Sub
  End If
  x = Playernum1(SN)
  If (x = 0 Or P(x).CharID = 0) Then
    If Config.NoJoin = 0 Then
      For X3 = 1 To HighSenshi
        DoEvents
        If (UCase(Trim(StripHiAscii(Ct)))) = ("/" + UCase(Senshi(X3).PickMe)) And Senshi(X3).PickMe <> "" Then
          JoinOK = True
          x = Taken(X3)
          If (Config.NoJoin <> 0) Then
            Taunt% = Rand(1, 3)
            Select Case Taunt%
              Case 1: Send (ccBold + "<!> " + SN + ", the host has disabled joining.")
              Case 2: Send (ccBold + "<!> Can't join, " + SN + ". Sorry.")
              Case 3: Send (ccBold + "<!> Join has been disabled, " + SN + ". Sorry.")
            End Select
            Dn = True
            Exit Sub
          End If
          If (x <> 0) And (Config.SameChar = 0) Then
            Send (ccBold + "<!> " + Senshi(X3).FullName + " is being used by " + P(x).ScrNam + ". Duplicate characters are disabled.")
            Dn = True
            Exit Sub
          End If
          For x5 = 1 To MaxPlayers
            DoEvents
            If (P(x5).ScrNam = SN) And (P(x5).CharID <> 0) Then
              JoinOK = False
              Exit For
            End If
          Next x5
          If JoinOK Then
            For x = 1 To MaxPlayers
              If (P(x).CharID = 0) Then
                Call SelectJoin(x, X3, SN)
                GoTo Joined21
              End If
            Next x
            Taunt% = Rand(1, 3)
            Select Case Taunt%
              Case 1: Send (ccBold + "<!> The game's full, " + SN + " -- " + TrimStr(MaxPlayers) + " are already playing. Sorry :(")
              Case 2: Send (ccBold + "<!> Can't join, " + SN + ". Game's full.")
              Case 3: Send (ccBold + "<!> Sorry, " + SN + ", the game is full.")
            End Select
Joined21:
            Dn = True
            Exit Sub
          End If
          Exit For
        End If
      Next X3
    ElseIf Config.Flag Then
      'Send ("<b><!> Sorry " + SN + ", you cannot join in Capture the Flag mode.")
    End If
  End If
  x2 = Playernum(SN)
  If (Battle) And (x2 <> 0) Then
    If Skip(Ct, 9) = "FRAGCOUNT" And Config.FragCount Then
      If Len(Ct) > 11 Then
        X6 = Playernum(Right$(Ct, Len(Ct) - 11))
      Else
        X6 = 0
      End If
      If X6 = 0 Then X6 = x2
      Send (P(X6).ScrNam + " has " + TrimStr(P(X6).Frags) + " kills and " + TrimStr(P(X6).FatalFrags) + " Fatalities.")
      Dn = True
      Exit Sub
    End If
    If Skip(Ct, 11) = "LEARNEDMOVE" Then
      If Len(Ct) > 13 Then
        X6 = Playernum(Right$(Ct, Len(Ct) - 13))
      Else
        X6 = 0
      End If
      If X6 = 0 Then X6 = x2
      If P(X6).Moves(14).Cmdkey <> "" Then
        Send (P(X6).ScrNam + " currently has " + P(X6).Moves(14).name + " available (." + P(X6).Moves(14).Cmdkey + ")")
      Else
        Send (P(X6).ScrNam + " has no learned move available.")
      End If
      Dn = True
      Exit Sub
    End If
    If Skip(Ct, 8) = "NEWTEAM" And Config.Defect Then
      If (x2 > 0) And (X6 <= MaxPlayers) Then
        For X3 = Asc("0") To Asc("Z")
          x4 = 0
          For x5 = 1 To MaxPlayers
            If TeamID(x5) = Chr$(X3) Then x4 = True
          Next x5
          If (x4 = 0) Then
            Send (P(x2).ScrNam + " creates Team " + Chr$(X3) + ". (Use .teamname to name it)")
            'LogFileWrite (SN + " created Team " + Chr$(x3) + ".")
            P(x2).TeamID = Chr$(X3)
            TCaptain(X3) = x2
            tName(X3) = ""
            x4 = 999
            Exit For
          End If
        Next X3
        If (x4 <> 999) Then
          Send ("Error: Unable to create new team.")
          'LogFileWrite (SN + " attempted to create a team but an error occured.")
        End If
      End If
      Dn = True
      Exit Sub
    End If
    If Skip(Ct, 7) = "CAPTAIN" And Config.Defect And Len(Ct) > 9 Then
      If TCaptain(Asc(P(x2).TeamID)) = x2 Then
        x7 = Playernum(Right$(Ct, Len(Ct) - 9))
        If x7 > 0 And P(x7).CharID <> 0 Then
          TCaptain(Asc(P(x2).TeamID)) = x7
          Send (SN + " names " + P(x7).ScrNam + " the new Captain of " + tName(Asc(P(x2).TeamID)) + "!")
        End If
      Else
        Send ("Only the team captain may delegate captaining authorities, " + SN + ".")
      End If
      Dn = True
      Exit Sub
    End If
    If Skip(Ct, 4) = "KICK" And Config.Defect And Len(Ct) > 7 Then
      x7 = Playernum(Right$(Ct, Len(Ct) - 6))
      If (x2 > 0) And (x2 <= MaxPlayers) And (x7 > 0) And (x7 <= MaxPlayers) And (TeamID(x2) = TeamID(x7)) Then
        If TCaptain(Asc(P(x2).TeamID)) = x2 Then
          Send (P(x2).ScrNam + " kicks " + P(x7).ScrNam + " off their team!")
          'LogFileWrite (SN + " kicked " + p(x7).ScrNam + " off of Team " + TeamID(x2) + ".")
          For X3 = Asc("0") To Asc("Z")
            x4 = 0
            For x5 = 1 To MaxPlayers
              If TeamID(x5) = Chr$(X3) Then x4 = True
            Next x5
            If (x4 = 0) Then
              Send (P(x7).ScrNam + " is now on Team " + Chr$(X3) + ". (Use .teamname to name it)")
              If tName(X3) = "" Then tName(X3) = "Team " + Chr$(X3)
              'LogFileWrite (p(x7).ScrNam + " was placed on Team " + Chr$(x3) + ".")
              P(x7).TeamID = Chr$(X3)
              TCaptain(X3) = x2
              tName(X3) = ""
              x4 = 999
              Exit For
            End If
          Next X3
          If (x4 <> 999) Then
            Send ("Error: Unable to create new team.")
            'LogFileWrite (p(x7).ScrNam + " could not be placed on a new team due to an error.")
          End If
        Else
          Send ("Only the team captain, " + P(TCaptain(Asc(P(x2).TeamID))).ScrNam + ", may .kick people.")
        End If
      End If
      Dn = True
    End If
    If Skip(Ct, 8) = "TEAMNAME" And Len(Ct) > 10 Then
      If (x2 > 0) Then
        If TCaptain(Asc(P(x2).TeamID)) = x2 Then
          S4 = Right$(Ct, Len(Ct) - 10)
          tName(Asc(P(x2).TeamID)) = S4
          Send ("Team " + P(x2).TeamID + " is now known as """ + S4 + """.")
          'LogFileWrite ("Team " + p(x2).TeamID + " was renamed to """ + S4 + """.")
        Else
          Send ("Only the team captain, " + P(TCaptain(Asc(P(x2).TeamID))).ScrNam + ", may change the team name.")
        End If
      End If
      Dn = True
      Exit Sub
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
          'Msg = Msg + TrimStr(P(X6).MP) + "M/"
        'End If
        Msg = Msg + TrimStr(P(X6).Super) + "S/" + TrimStr(P(X6).Cheese) + "C) [" + TextStatus(X6) + "] "
        If P(X6).HP <= (MaxHP / 6) Then
          Msg = ccColor + "04" + Msg
          If P(X6).HP > 0 Then Msg = Msg + " <DANGER>"
        End If
        Msg = Msg + " [" + GBStatus(X6) + "]"
        Send (Msg)
      End If
      Dn = True
      Exit Sub
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
      End If
      Dn = True
      Exit Sub
    End If
    If Skip(Ct, 7) = "RESPAWN" And P(x2).HP <= 0 And Config.Respawn Then
      Send (P(x2).ScrNam + "'s impatience causes a longer respawn delay for them!")
      P(x2).GotKilled = P(x2).GotKilled + 30
    End If
  End If
  x2 = Playernum(SN)
  If (Battle And x2 <> 0) Then
    If P(x2).ScrNam = SN Then
      If (Skip(Ct, 8) = "NODEFECT") And Config.Defect And Config.Defect Then
        P(x2).Defect = Not P(x2).Defect
        If Not P(x2).Defect Then
          Send (Parse(DATASET.AcceptDefects, SN, "", "", ""))
          'LogFileWrite (SN + " began to accept /defects.")
        Else
          Send (Parse(DATASET.DeclineDefects, SN, "", "", ""))
          'LogFileWrite (SN + " stopped accepting /defects.")
        End If
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 13) = "I WANT A DRAW") Then
        P(x2).Draw = Not P(x2).Draw
        If P(x2).Draw Then
          Send5 (Parse(DATASET.WantDraw, SN, "", "", ""))
          'LogFileWrite (SN + " called for a draw.")
        Else
          Send5 (Parse(DATASET.DontWantDraw, SN, "", "", ""))
          'LogFileWrite (SN + " decided not to draw.")
        End If
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 6) = "DEFECT") And (Config.Defect) And (Len(Ct) > 8) And Config.Defect Then
        P(x2).Target = Playernum(Mid$(Ct, 9, Len(Ct) - 8))
        Call DOMOVE(x2, P(x2).Target, pDefect, "")
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 3) = "CPU") And Config.CPU Then
        If (P(x2).CPU = 0) Then
          P(x2).CPU = 1
          P(x2).Goodwill = 50
          P(x2).Greed = 50
          P(x2).Wrath = 50
          P(x2).Arrogance = 0
          Send5 (SN + " is now controlled by the computer.")
          'LogFileWrite ("Control of " + SN + " was switched over to the CPU.")
        Else
          P(x2).CPU = 0
          Send5 (SN + " is no longer controlled by the computer.")
          'LogFileWrite (SN + " took control of their character.")
        End If
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 6) = "RCLAIM") And (Config.Flag <> 0) And RedFlag = 0 Then
        Send (P(x2).ScrNam + " claims the Red Flag.")
        RedFlag = x2
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 6) = "BCLAIM") And (Config.Flag <> 0) And BlueFlag = 0 Then
        Send (P(x2).ScrNam + " claims the Blue Flag.")
        BlueFlag = x2
        Dn = True
        Exit Sub
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
          If X6 = 0 Then X6 = x2
          If P(X6).Rune = 0 Then
            If X6 = x2 Then
              If Config.GetRune Then
                Do
                  P(x2).Rune = Rand(1, MaxRune)
                Loop Until P(x2).Rune <> RuneZombie
                Send ("The " + RuneName(P(x2).Rune) + " falls into " + P(x2).ScrNam + "'s hands.")
                'If P(x2).Rune = RuneLife Then P(X).MaxHP = Int(MaxHP * 0.25) + MaxHP
              Else
                Send (P(x2).ScrNam + ", you have no Rune. The host has disabled random rune selection for midgame joiners.")
              End If
            ElseIf Config.RuneCmd Then
              Send (P(X6).ScrNam + " has no Rune.")
            End If
          ElseIf Config.GetRune Then
            Send (P(X6).ScrNam + " has the " + RuneName(P(X6).Rune) + ".")
          End If
        End If
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 8) = "TALISMAN") Then
        Send ("Sorry, " + P(x2).ScrNam + ", we only deal with " + ccBold + ".RUNE" + ccBold + "s here, not Talismans.")
        Dn = True
        Exit Sub
      End If
      If Skip(Ct, 6) = "WEAPON" And Config.Weapon And Config.WeaponEnable Then
        If Len(Ct) > 8 Then
          X6 = Playernum(Right$(Ct, Len(Ct) - 8))
        End If
        If X6 = 0 Then X6 = x2
        If P(X6).Weapon <> 0 Then
          Send (P(X6).ScrNam + " has the """ + Weapons(P(X6).Weapon).name + """ with " + TrimStr(P(X6).WpnUsesLeft) + " charges remaining.")
        Else
          Send (P(X6).ScrNam + " has no weapon. Wait for one to appear!")
        End If
        Dn = True
        Exit Sub
      End If
      If Skip(Ct, 7) = "WPNLIST" Then
        If Config.WeaponEnable = 0 Then
          Send (SN + ", weapons are disabled, sorry.")
        ElseIf Config.WpnList Then
          Okay = False
          For x = 1 To MaxWeapons
            x5 = 0
            X6 = 0
            For x4 = 1 To MaxDropped
              If Dropped(x4) = x Then
                x5 = x5 + 1
                If DropUses(x4) > X6 Then X6 = DropUses(x4)
              End If
            Next x4
            If x5 <> 0 Then
              Okay = True
              If X6 = 0 Then
                Send ("x" + TrimStr(x5) + ": " + Weapons(x).name + " (.wget-" + Weapons(x).PickMe + ")")
              Else
                If x5 = 1 Then
                  Send ("x" + TrimStr(x5) + ": " + Weapons(x).name + " with " + TrimStr(X6) + " charges left (.wget-" + Weapons(x).PickMe + ")")
                Else
                  Send ("x" + TrimStr(x5) + ": " + Weapons(x).name + " with at most " + TrimStr(X6) + " charges left (.wget-" + Weapons(x).PickMe + ")")
                End If
              End If
            End If
          Next x
          If Okay = False Then Send ("No weapons are available to grab right now.")
        End If
        Dn = True
        Exit Sub
      End If
    End If
  End If
  If (Battle) And (Not Paused) Then
    If (P(x2).CharID <> 0) And (P(x2).HP > 0) And (P(x2).CurMove <> 0) And (P(x2).Status(sChaos) = 0) And P(x2).Status(sMIA) = 0 And P(x2).Status(sBerserk) = 0 And P(x2).Status(sMushroom) = 0 And P(x2).Status(sStop) = 0 And P(x2).Status(sSleep) = 0 Then
      If (Skip(Ct, 4) = "STOP") Or (Skip(Ct, 4) = "HALT") And (P(x2).CurMove <> pTaunt) And (P(x2).CurMove > 0) Then
        If (P(x2).CurMove < MaxMoves) And (P(x2).CurMove > 1) Then
          M = P(x2).Moves(P(x2).CurMove)
          If M.Element = Invin Then
            P(x2).Status(sInvin) = 0
            If M.Target = Allfriend Then
              For x4 = 1 To MaxPlayers
                If TeamID(x4) = TeamID(x2) Then P(x4).Status(sInvin) = 0
              Next x4
            End If
          End If
        End If
        If P(x2).CurMove <> pRest Then P(x2).CurMove = -255
        If P(x2).CurMove <> pTaunt Then P(x2).CurMove = -255
        Dn = True
        Exit Sub
      End If
    End If
    If (P(x2).CharID = 0) Or (P(x2).HP <= 0) Or (P(x2).CurMove <> 0) Or P(x2).Status(sMIA) <> 0 Or P(x2).Status(sMushroom) <> 0 Or P(x2).Status(sStop) <> 0 Or P(x2).Status(sSleep) <> 0 Then
      If P(x2).CurMove > 0 And P(x2).CurMove < MaxMoves Then Msg = "you are doing " + P(x2).Moves(P(x2).CurMove).name + "."
      If P(x2).CurMove > 0 And P(x2).CurMove < MaxMoves And P(x2).SuperNum <> 0 Then Msg = "you are Superering " + P(x2).Moves(P(x2).CurMove).name + "."
      If P(x2).CurMove = pRest Then Msg = "you are resting."
      If P(x2).CurMove = pFlee Then Msg = "you are running like a coward!"
      If P(x2).CurMove = pBlock Then Msg = "you are already blocking."
      If P(x2).CurMove = pFatal Then Msg = "you are doing a Fatality."
      If P(x2).CurMove = pSlot Then Msg = "you are playing the slots."
      If P(x2).CurMove = -255 Then Msg = "you are not ready."
      If P(x2).Status(sFreeze) <> 0 Then Msg = "you are frozen."
      If P(x2).Status(sStun) <> 0 Then Msg = "you are unconsious."
      If P(x2).Status(sSleep) <> 0 Then Msg = "you are sound asleep."
      If P(x2).Status(sMIA) <> 0 Then Msg = "you are Missing in Action!"
      If P(x2).Status(sStop) <> 0 Then Msg = "you are frozen in time."
      If P(x2).Status(sMushroom) <> 0 Then Msg = "you are in mushroom form."
      If P(x2).HP <= 0 Then Msg = "you are dead!"
      If P(x2).CharID = 0 Then
        Msg = "you are not playing in the game."
        'mIRCChatSend "Warning! Error! Program halted so Kamek can debug it!"
        'Stop
      End If
      For x4 = MaxMoves To 1 Step -1
        If (P(x2).Moves(x4).Cmdkey <> "") Then
          M = P(x2).Moves(x4)
          If (Skip(Ct, Len(M.Cmdkey)) = UCase(M.Cmdkey)) Then
            Send (SN + ", you can't do " + M.name + " while " + Msg)
            P(x2).Scroller = P(x2).Scroller + 1
            Dn = True
            Exit Sub
          End If
        End If
      Next x4
      If (Skip(Ct, 1) = "1") Or (Skip(Ct, 1) = "2") Or (Skip(Ct, 1) = "3") Or (Skip(Ct, 1) = "4" And Config.Fours) Or (Skip(Ct, 1) = "5" And Config.Fours) Or (Skip(Ct, 1) = "6" And Six) Then
        Send (SN + ", you can't do a Super while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 3) = "EAT") And P(x2).Rune = RuneCannibal Then
        Send (SN + ", you can't eat a corpse while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 6) = "ARMORY") And P(x2).Rune = RuneArmory Then
        Send (SN + ", you can't go to the armory while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 8) = "MIMERUNE") And P(x2).Rune = RuneMimic Then
        Send (SN + ", you can't use Rune Mime while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 5) = "BLOCK") Then
        Send (SN + ", you can't block while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
      If Skip(Ct, 3) = "GET" Then
        Send (SN + ", you can't get an item while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
      If Skip(Ct, 4) = "WGET" Then
        Send (SN + ", you can't get a weapon while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
      If Skip(Ct, 5) = "WDROP" Or Skip(Ct, 5) = "WJUNK" Then
        Send (SN + ", you can't drop your weapon while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
      If Skip(Ct, 7) = "UNMORPH" Then
        Send (SN + ", you can't unmorph while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
      If Skip(Ct, 6) = "CHICTR" Then
        Send (SN + ", you can't Chibot Counter while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
      If Skip(Ct, 4) = "FLEE" Then
        Send (SN + ", you can't run for the hills while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
      If Skip(Ct, 6) = "DIVERT" Then
        Send (SN + ", you can't Divert HP while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
      If Skip(Ct, 8) = "UNDIVERT" And P(x2).Rune = RuneUndivert Then
        Send (SN + ", you can't Un-divert HP while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, Len(S.Fatality.Cmdkey)) = UCase(S.Fatality.Cmdkey)) And S.Fatality.Cmdkey <> "" Then
        Send (SN + ", you can't Fatalize anyone while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
      If Skip(Ct, 4) = "SLOT" And (P(x2).Rune = RuneSlot Or AllSlot) Then
        Send (SN + ", you can't pull the slot machine while " + Msg)
        P(x2).Scroller = P(x2).Scroller + 1
        Dn = True
        Exit Sub
      End If
    End If
    If (P(x2).CharID <> 0) And (P(x2).HP > 0) And (P(x2).CurMove = 0) And (P(x2).Status(sStop) = 0 And P(x2).Status(sMIA) = 0 And P(x2).Status(sMushroom) = 0) Then
      InitMoves (x2)
      If (Skip(Ct, 8) = "MIMERUNE") And (P(x2).Rune = RuneMimic) Then
        If XTimer - P(x2).RuneTemp < MimicCharge * (Config.Lag / 1000) Then
          x5 = (MimicCharge * (Config.Lag / 1000)) - (XTimer - P(x2).RuneTemp)
          Send ("The Mimic Rune is discharged, " + P(x2).ScrNam + ". Wait for it to recharge before mimicing another rune. [" + TrimStr(x5) + " sec]")
        Else
          S7 = Mid$(Ct, 11, Len(Ct) - 1)
          x5 = Playernum(S7)
          P(x2).RuneTemp2 = RuneMimic
          P(x2).Rune = P(x5).Rune
          Send (P(x2).ScrNam + "'s Mimic Rune starts mimicing " + P(x5).ScrNam + "'s rune, the " + RuneName(P(x2).Rune)) + ". Type .unmime to stop miming."
        End If
      End If
      If (Skip(Ct, 6) = "UNMIME") And (P(x2).RuneTemp2 = RuneMimic) Then
        P(x2).RuneTemp2 = 0
        Send P(x2).ScrNam + "'s Mimic Rune stops mimicing the " + RuneName(P(x2).Rune) + "."
        P(x2).Rune = RuneMimic
        P(x2).RuneTemp = XTimer
      End If
      If (Skip(Ct, 6) = "ARMORY") And (P(x2).Rune = RuneArmory) Then
        If XTimer - P(x2).RuneTemp < ArmoryWait * (Config.Lag / 1000) Then
          x5 = (ArmoryWait * (Config.Lag / 1000)) - (XTimer - P(x2).RuneTemp)
          Send ("The security guard won't let " + P(x2).ScrNam + " in! Wait awhile before trying to go back to the armory. [" + TrimStr(x5) + " sec]")
        ElseIf P(x2).Weapon <> 0 Then
          Send (P(x2).ScrNam + " already has a weapon. Type .wdrop to drop your weapon before using the armory.")
        Else
          Send (P(x2).ScrNam + " goes to the armory, reaches into the Huge Box o'Weapons, and pulls one out.")
          x4 = Rand(1, MaxWeapons)
          P(x2).Weapon = x4
          InitMoves (x2)
          P(x2).WpnUsesLeft = Weapons(x4).NumUses
          Send (Parse(Weapons(P(x2).Weapon).SelectStr, P(x2).ScrNam, "", "", ""))
          P(x2).RuneTemp = XTimer
        End If
      End If
      If (Skip(Ct, 3) = "EAT") And (P(x2).Rune = RuneCannibal) Then
        S7 = Mid$(Ct, 6, Len(Ct) - 1)
        x5 = Playernum(S7)
        If x5 > 0 And x5 < MaxPlayers Then
          If P(x5).HP = -9999 Then
            Send (P(x5).ScrNam + "'s corpse has been desecrated and is unsuitable for eating.")
          ElseIf P(x5).HP = -777 Then
            Send (P(x5).ScrNam + " escaped from the battle.")
          ElseIf P(x5).HP < 0 Then
            P(x5).HP = -9999
            Send (P(x2).ScrNam + " messily devours " + P(x5).ScrNam + "'s corpse!!!")
            Send (P(x2).ScrNam + " lets out a disgusting burp. [+" + TrimStr(Int(P(x5).MaxHP / 6)) + " HP]")
            P(x2).HP = P(x2).HP + Int(P(x5).MaxHP / 6)
            Select Case Rand(1, 5)
              Case 1: Almighty(sBless) = 100
              Case 2: Almighty(sHaste) = 100
              Case 3: Almighty(sReraise) = 100
              Case 4: Almighty(sBarrier) = 100
              Case 5: Almighty(sMBarrier) = 100
              Case 6: Almighty(sRegen) = 100
            End Select
            CheckStatus Almighty(), x4, 0, 1
          End If
          Dn = True
          Exit Sub
        End If
      End If
      If (Skip(Ct, 7) = "UNMORPH") And (P(x2).Status(sMorph)) Then
        Send (Parse(DATASET.UnMorphMsg, P(x2).ScrNam, Senshi(P(x2).OldCharID).FullName, "", ""))
        'LogFileWrite (p(x2).ScrNam + " reverted back to " + Senshi(p(x2).OldCharID).FullName + ".")
        UnmorphMe (x2)
      End If
      If (Skip(Ct, 4) = "WGET") And Len(Ct) > 6 Then
        If Config.WeaponEnable = 0 Then
          Send SN + ", weapons are disabled , sorry."
        Else
          If P(x2).Weapon <> 0 Then
            Send (P(x2).ScrNam + " is already holding a weapon.")
          Else
            S2 = Right$(Ct, Len(Ct) - 6)
            x4 = MatchDropped(S2)
            If x4 = 0 Then
              Send (P(x2).ScrNam + " looks around for that weapon but can't find it.")
            Else
              'p(x2).WeaponState = 1
              P(x2).Weapon = Dropped(x4)
              Dropped(x4) = 0
              InitMoves (x2)
              P(x2).WpnUsesLeft = DropUses(x4)
              DropUses(x4) = 0
              Send (Parse(Weapons(P(x2).Weapon).SelectStr, P(x2).ScrNam, "", "", ""))
            End If
          End If
        End If
        Dn = True
        Exit Sub
      End If
      If Skip(Ct, 5) = "WDROP" And P(x2).Weapon <> 0 Then
        If Left$(Weapons(P(x2).Weapon).PickMe, 1) = "~" Then
          Send (P(x2).ScrNam + " drops their " + Weapons(P(x2).Weapon).name + " and it vanishes into oblivion!")
          P(x2).Weapon = 0
        Else
          DropWeapon x2, "%SN drops their %T."
        End If
        Dn = True
        Exit Sub
      End If
      If Skip(Ct, 5) = "WJUNK" And P(x2).Weapon <> 0 Then
        Send (P(x2).ScrNam + " drops their " + Weapons(P(x2).Weapon).name + " and it vanishes into oblivion!")
        P(x2).Weapon = 0
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 3) = "GET") Then
        If (Item2Get <> 0) Then
          Call DOMOVE(x2, 0, pGet, "")
        Else
          Send (Parse(DATASET.NoGetItem, P(x2).ScrNam, "", "", ""))
        End If
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 6) = "CHICTR") And (Len(Ct) > 8) Then
        If (P(x2).Super >= 200) Or P(x2).Rune = RuneBlock Then
          S3$ = Right$(Ct, Len(Ct) - 8)
          For x4 = MaxMoves To 1 Step -1
            If (P(x2).Moves(x4).Cmdkey <> "") Then
              Okay = False
              M = P(x2).Moves(x4)
              If (UCase(S3$)) = UCase(M.Cmdkey) Then Okay = True
              If (P(x2).Status(sMute) <> 0) And (M.Element <> Phys) Then Okay = False
              If (P(x2).Rune = RuneBlock And P(x2).Super < 100) And Okay Then
                Okay = False
                Send (P(x2).ScrNam + " still needs the 100 Super Point cost up front to do a Chibot Counter.")
              End If
              If Okay Then
                'Send (Parse(Dataset.ChiCtr, p(x2).ScrNam, p(x2).Moves(x4).Name, "", ""))
                P(x2).Status(sHamedo) = -1
                P(x2).Super = P(x2).Super - 100
                'LogFileWrite (SN + " Chibot Countered with """ + p(x2).moves(x4).Name + """")
                Call DOMOVE(x2, x4, pBlock, "")
              End If
            End If
          Next x4
        Else
          Send (P(x2).ScrNam + " needs 200 Super Points to do a Chibot Counter.")
        End If
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 5) = "BLOCK") Then
        P(x2).Status(sHamedo) = 0
        If (Len(Ct) > 7) And (P(x2).Super >= 100 Or P(x2).Rune = RuneBlock) Then
          S3$ = Right$(Ct, Len(Ct) - 7)
          For x4 = MaxMoves To 1 Step -1
            If (P(x2).Moves(x4).Cmdkey <> "") Then
              Okay = False
              M = P(x2).Moves(x4)
              If (UCase(S3$)) = UCase(M.Cmdkey) Then Okay = True
              If (P(x2).Status(sMute) <> 0) And (M.Element <> Phys) Then Okay = False
              If Okay Then
                'Send (Parse(Dataset.Counter, p(x2).ScrNam, p(x2).moves(x4).Name, "", ""))
                'LogFileWrite (SN + " counterattacked with """ + p(x2).moves(x4).Name + """")
                Call DOMOVE(x2, x4, pBlock, "")
              End If
            End If
          Next x4
        Else
          If (Len(Ct) > 7) And (P(x2).Super < 100) Then Send ("Not enough Super Point for " + SN + " to counter.")
          Call DOMOVE(x2, 0, pBlock, "")
        End If
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 5) = "TAUNT") Then
        Call DOMOVE(x2, 0, pTaunt, "")
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 4) = "FLEE") Then
        If (P(x2).HP >= 150) Then
          Call DOMOVE(x2, 0, pFlee, "")
        Else
          If (P(x2).HP > 10) Then P(x2).HP = P(x2).HP - 10
          Send5 (Parse(DATASET.FleeFail, SN, "", "", ""))
          'LogFileWrite (SN + " tried to flee but panicked.")
          ShowStatus x2
        End If
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 4) = "REST") Then
        Call DOMOVE(x2, 0, pRest, "")
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 8) = "UNDIVERT") And Len(Ct) > 10 And P(x2).Rune = RuneUndivert Then
        On Error GoTo NoOvr
        S5 = Right$(Ct, Len(Ct) - 10)
        x5 = Val(S5)
        If (x5 > 0) Then
          If P(x2).Super - x5 < 0 Then
            If P(x2).Super = 0 Then
              Send (SN + " cannot Un-divert any SP at this time.")
            Else
              If P(x2).Super > (P(x2).MaxHP * 1.5 - P(x2).HP) Then
                Send (SN + " cannot Un-divert any more SP than " + TrimStr(P(x2).Super) + ".")
              Else
                Send (SN + " cannot Un-divert any more SP than " + TrimStr(Int(P(x2).MaxHP * 1.5 - P(x2).HP)) + ".")
              End If
            End If
          ElseIf P(x2).HP + x5 > P(x2).MaxHP * 1.5 Then
            If P(x2).HP >= P(x2).MaxHP * 1.5 Then
              Send (SN + " cannont Un-divert any more SP at this time.")
            Else
              If P(x2).Super > (P(x2).MaxHP * 1.5 - P(x2).HP) Then
                Send (SN + " cannot Un-divert any more SP than " + TrimStr(P(x2).Super) + ".")
              Else
                Send (SN + " cannot Un-divert any more SP than " + TrimStr(Int(P(x2).MaxHP * 1.5 - P(x2).HP)) + ".")
              End If
            End If
          Else
            P(x2).HP = P(x2).HP + x5
            P(x2).Super = P(x2).Super - x5
            Send (SN + " Un-diverted " + TrimStr(x5) + " Super Points to their HP Meter.")
          End If
        Else
          GoTo NoOvr
        End If
        Dn = True
        Exit Sub
      End If
      If (Skip(Ct, 6) = "DIVERT" And Len(Ct) > 8) Then
        'Send ("Sorry, " + SN + ", but <b>/divert</b> no longer works.")
        On Error GoTo NoOvr
        S5 = Right$(Ct, Len(Ct) - 8)
        x5 = Val(S5)
        If (x5 > 0) Then
          If P(x2).HP - x5 < Int(MaxHP / 2) Then
            If P(x2).HP - Int(MaxHP / 2) > 0 Then
              Send (P(x2).ScrNam + " cannot divert any more than " + TrimStr(P(x2).HP - Int(MaxHP / 2)) + " HP at this time.")
            Else
              Send (P(x2).ScrNam + " cannot divert any more HP at this time.")
            End If
          Else
            P(x2).HP = P(x2).HP - x5
            P(x2).Super = P(x2).Super + x5
            Send (Parse(DATASET.HPDivert, P(x2).ScrNam, TrimStr(x5), "", ""))
            'LogFileWrite (SN + " diverted " + TrimStr(X5) + " HP to their Super Meter.")
            ShowStatus x2
            P(x2).CurMove = 0
          End If
        Else
NoOvr:
          Send (P(x2).ScrNam + ", I wasn't born yesterday foo'!")
          Resume ResMe
        End If
ResMe:
        On Error GoTo 0
        Dn = True
        Exit Sub
      End If
      S = Senshi(P(x2).CharID)
      If (Skip(Ct, Len(S.Fatality.Cmdkey)) = UCase(S.Fatality.Cmdkey)) And S.Fatality.Cmdkey <> "" Then
        If Len(Ct) > Len(S.Fatality.Cmdkey) + 1 Then
          P(x2).Target = Playernum(Mid$(Ct, Len(S.Fatality.Cmdkey) + 3, Len(Ct) - Len(S.Fatality.Cmdkey) - 2))
        Else
          P(x2).Target = 0
        End If
        Okay = True
        If (P(x2).Target = 0) Then
          For x4 = 1 To MaxPlayers
            If P(x4).HP <= (MaxHP / 6) And P(x4).HP > 0 And P(x4).CharID <> 0 Then
              P(x2).Target = x4
              Exit For
            End If
          Next x4
          If P(x2).Target = 0 Then Okay = False
        End If
        x4 = P(P(x2).Target).HP
        If P(x2).Status(sQuick) <> 0 Then
          Send ("Fatalitying while Quick? That's a low blow, isn't it " + P(x2).ScrNam + "??")
          Dn = True
          Exit Sub
        ElseIf (x4 <= (MaxHP / 6)) Then
          Call DOMOVE(x2, P(x2).Target, pFatal, "")
          Dn = True
          Exit Sub
        End If
      End If
      DoEvents
      S = Senshi(P(x2).CharID)
      If (Skip(Ct, 1) = "1") Or (Skip(Ct, 1) = "2") Or (Skip(Ct, 1) = "3") Or (Skip(Ct, 1) = "4" And Config.Fours) Or (Skip(Ct, 1) = "5" And Config.Fours) Or (Skip(Ct, 1) = "6" And Six) Then
        x4 = Val(Skip(Ct, 1))
        If (P(x2).Super >= 100 * x4) And (Len(Ct) > 3) Then
          S2 = Right$(Ct, Len(Ct) - 3)
          For x5 = MaxMoves To 1 Step -1
            If (UCase(Left$(S2, Len(P(x2).Moves(x5).Cmdkey))) = UCase(P(x2).Moves(x5).Cmdkey)) Then
              If (P(x2).Moves(x5).CanSuper = 1) Or (x4 >= P(x2).Moves(x5).CanSuper) Then Okay = True
              If (P(x2).Moves(x5).CanSuper > 4) Then Okay = False
              'Okay = P(X2).Moves(X5).CanSuper
            End If
            If Okay Then Exit For
          Next x5
          If P(x2).Status(sQuick) <> 0 Then
            Send ("Supering while Quick? That's quite a low blow, isn't it, " + SN + "?")
            Okay = False
            Dn = True
            Exit Sub
          End If
          If P(x2).Charging Then
            Send ("Sorry, you cannot Charge a Super Move.")
            Okay = False
          End If
          If Okay Then
            P(x2).SuperNum = x4
            Ct = "/" + S2
          End If
        ElseIf P(x2).Super < 100 * x4 Then
          mIRCStatusSend "/notice " + SN + " You don't have enough Super Points to do " + Ct + "!"
        End If
      Else
        P(x2).SuperNum = 0
      End If
      If Skip(Ct, 4) = "SLOT" And (P(x2).Rune = RuneSlot Or AllSlot) Then
        P(x2).Target = 0
        If (Len(Ct) > 6) Then
          If (P(x2).Rune <> RuneStealth) Or M.Element = Morph Then
            S7 = Mid$(Ct, x5 + 3, Len(Ct) - x5 - 2)
            P(x2).Target = Playernum(S7)
          Else
            Send ("Whoops! " + SN + ", did I mention that you can't target with the Rune of Stealth?")
          End If
        Else
          S7 = ""
        End If
        'If (P(X2).Target <> 0 And P(X2).ScrNam = YourSN And P(X2).Rune <> RuneStealth) Then Send (YourSN + " targeted " + P(P(X2).Target).ScrNam + "!!!")
        If P(x2).Target <> 0 Then
          If P(P(x2).Target).CPU = 0 And P(P(x2).Target).Rune = RuneSwiss Then
            Send "/notice " + P(P(x2).Target).ScrNam + " " + P(x2).ScrNam + " targeted you with the Slot command!"
          End If
        End If
        If (UCase(S7) = "FOE") And (P(x2).Rune <> RuneStealth) Then P(x2).Target = GetTarget(P(x2).TeamID)
        If ((UCase(S7) = "FRIEND") Or (UCase(S7) = "ALLFRIEND")) And (P(x2).Rune <> RuneStealth) Then P(x2).Target = x2
        If ((Left$(UCase(S7), 4) = "TEAM") Or (Left$(UCase(S7), 3) = "ALL")) And (P(x2).Rune <> RuneStealth) Then
          S6 = UCase(Right$(S7, 1))
          For X6 = 1 To MaxPlayers
            If P(X6).TeamID = UCase(S6) And P(X6).HP > 0 And Active(X6) <> 0 Then
              P(x2).Target = X6
              Exit For
            End If
          Next X6
        End If
        If (P(x2).Target = 0) Then
          Select Case M.Target
            Case Allfriend, Ally, OnlySelf: P(x2).Target = x2
            Case Else: P(x2).Target = GetTarget(P(x2).TeamID)
          End Select
        End If
        Call DOMOVE(x2, P(x2).Target, pSlot, "")
        Dn = True
        Exit Sub
      End If
      For x4 = MaxMoves To 1 Step -1
        If (P(x2).Moves(x4).Cmdkey <> "") Then
          Okay = False
          M = P(x2).Moves(x4)
          If (Skip(Strip32(Ct), Len(Strip32(M.Cmdkey))) = Strip32(UCase(M.Cmdkey))) Then Okay = True
          If (P(x2).Status(sMute) <> 0) And (M.Element <> Phys) Then
            If (Skip(Ct, Len(M.Cmdkey)) = UCase(M.Cmdkey)) Then Send (P(x2).ScrNam + ", you cannot do this move while Mute.")
            Okay = False
          End If
          If Okay Then
            x5 = Len(M.Cmdkey)
            P(x2).Target = 0
            If (Len(Ct) > x5 + 1) Then
              If (P(x2).Rune <> RuneStealth) Or M.Element = Morph Then
                S7 = Mid$(Ct, x5 + 3, Len(Ct) - x5 - 2)
                P(x2).Target = Playernum(S7)
              Else
                Send ("Whoops! " + SN + ", did I mention that you can't target with the Rune of Stealth?")
              End If
            Else
              S7 = ""
            End If
            'If (P(X2).Target <> 0 And P(X2).ScrNam = YourSN And P(X2).Rune <> RuneStealth) Then Send (YourSN + " targeted " + P(P(X2).Target).ScrNam + "!!!")
            If P(x2).Target <> 0 Then
              If P(P(x2).Target).CPU = 0 And P(P(x2).Target).Rune = RuneSwiss Then
                Send "/notice " + P(P(x2).Target).ScrNam + " " + P(x2).ScrNam + " targeted you with " + M.name + "!"
              End If
            End If
            If (UCase(S7) = "FOE") And (P(x2).Rune <> RuneStealth) Then P(x2).Target = GetTarget(P(x2).TeamID)
            If ((UCase(S7) = "FRIEND") Or (UCase(S7) = "ALLFRIEND")) And (P(x2).Rune <> RuneStealth) Then P(x2).Target = x2
            If ((Left$(UCase(S7), 4) = "TEAM") Or (Left$(UCase(S7), 3) = "ALL")) And (P(x2).Rune <> RuneStealth) Then
              S6 = UCase(Right$(S7, 1))
              For X6 = 1 To MaxPlayers
                If P(X6).TeamID = UCase(S6) And P(X6).HP > 0 And Active(X6) <> 0 Then
                  P(x2).Target = X6
                  Exit For
                End If
              Next X6
            End If
            If (P(x2).Target = 0) Then
              Select Case M.Target
                Case Allfriend, Ally, OnlySelf: P(x2).Target = x2
                Case Else: P(x2).Target = GetTarget(P(x2).TeamID)
              End Select
            End If
            Call DOMOVE(x2, P(x2).Target, x4, S7)
            Dn = True
            Exit Sub
          End If
        End If
      Next x4
      For x4 = 1 To MaxWeapons
        DoEvents
        If Skip(Ct, Len(Weapons(x4).PickMe)) = UCase(Weapons(x4).PickMe) Then
          Send (SN + ", you can't pick a weapon during battle. You may only pick up dropped weapons (/wpnlist)")
          Dn = True
          Exit Sub
        End If
      Next x4
    End If
  End If
End If
End Sub

Private Sub EndBattle()
Dim OK As Integer
Dim x%
  If (Not Auto) Then OK = (kDlgBoxfn("Really end the battle?", 36, "End Battle") = 6)
  If (Auto Or OK) Then
    Playwav ("end")
    EndIt DATASET.GameAborted
    For x = 1 To 255
      TCaptain(x) = 0
      tName(x) = ""
    Next x
    For x = 1 To MaxPlayers
      If P(x).CPU <> 0 Then P(x).Rune = 0
    Next x
  End If
End Sub

Private Sub EndIt(ByVal Reason$)
Dim x4%
  Unload fMidi
  Battle = False
  Paused = False
  'mBeginBattle.Enabled = True
  mnuBeginBattle.Caption = "&Begin"
  mnuPauseBattle.Caption = "&Pause"
  Send (DATASET.BattleEnd)
  Send (Reason$)
  DispFrags
  For x4 = 1 To MaxPlayers
    If P(x4).Status(sMorph) Then
      P(x4).CharID = P(x4).OldCharID
    End If
  Next x4
  sbChUBMain.SimpleText = "Dataset " + DATASET.LoadStr + " loaded"
  If (Auto) Then mnuExit_Click
End Sub

Private Sub FlagTeams()
Dim x%, x4%
Dim Td As String
Dim Hole%
  Hole = False
  For x = 1 To MaxPlayers
    P(x).TeamID = ""
    If P(x).CharID = 0 Then
      If Hole = 0 Then Hole = x
      If Hole > 0 Then Hole = -1
    End If
  Next x
  If Hole = -1 Then
    kDlgBox "There is a hole. Please remove it.", 16, "???"
    Exit Sub
  ElseIf Hole = x Then
    MaxPlayers = x
    ReDim Preserve P(MaxPlayers)
    ReDim Preserve Vote(MaxPlayers)
    ToChUBBot ("CHUBMAXP " + TrimStr(MaxPlayers))
  End If
  For x = 1 To Int(MaxPlayers / 2)
    Do
      x4 = Rand(1, MaxPlayers)
    Loop Until P(x4).TeamID = ""
    P(x4).TeamID = "R"
  Next x
  For x = 1 To MaxPlayers - Int(MaxPlayers / 2)
    Do
      x4 = Rand(1, MaxPlayers)
    Loop Until P(x4).TeamID = ""
    P(x4).TeamID = "B"
  Next x
  If (NumPlaying() < 4) Then Exit Sub
  tName(Asc("B")) = ccColor + "12Blue Team"
  tName(Asc("R")) = ccColor + "04Red Team"
  Config.Defect = 0
  TLimit = 0
  'y(1).Enabled = False
  'y(2).Enabled = False
  'y(3).Enabled = False
  'y(4).Enabled = False
  'y(5).Enabled = False
  Do
    RedFlag = Rand(1, MaxPlayers)
    DoEvents
  Loop Until (P(RedFlag).CharID <> 0) And P(RedFlag).TeamID = "R"
  FoundRedFlag = 0
  Do
    BlueFlag = Rand(1, MaxPlayers)
    DoEvents
  Loop Until (P(BlueFlag).CharID <> 0) And P(BlueFlag).TeamID = "B"
  FoundBlueFlag = 0
End Sub

Private Sub SneakAttack()
Dim x%, X6%
  If (Rand(1, 20) = 1) Then
    Send ("~~~*) SNEAK ATTACK (*~~~")
Start:
    Do
      x = Rand(1, MaxPlayers)
    Loop Until Active(x) <> 0
    Do
      P(x).CurMove = Rand(1, MaxMoves)
      X6 = X6 + 1
    Loop Until (P(x).Moves(P(x).CurMove).Cmdkey <> "") Or (X6 > 150) And (P(x).Moves(P(x).CurMove).Element <> Morph)
    If X6 > 150 Then
      GoTo Start
    Else
     X6 = P(x).Moves(P(x).CurMove).Target
     If (X6 = Allfriend) Or (X6 = OnlySelf) Or (X6 = Ally) Then
        P(x).Target = x
      Else
        P(x).Target = GetTarget(P(x).TeamID)
      End If
      P(x).MoveStart = -999
    End If
    Check2MoveDmg x, -1
  End If
End Sub

Private Sub tiMoveDmg_Timer()
Dim x2 As Integer, X3 As Integer, y2 As Integer, Okay As Integer
Dim x4 As Integer, x7 As Integer, X6%, AlreadyShown%
Dim MPR%, x%, Cnt%, Tt%
Dim M As MoveType
Dim Msg As String
Dim Tix As Long
  On Error Resume Next
  If (Battle) And (Not Paused) Then
    For x2 = 1 To MaxPlayers
      If (P(x2).HP > 0) Then
        Check2RemoveStatus (x2)
        If P(x2).CurMove = 0 And (P(x2).Status(sBerserk) <> 0 Or P(x2).Status(sChaos) <> 0 Or P(x2).Status(sPMS) <> 0) Then P(x2).CurMove = -255
        If (P(x2).CurMove = -255) And (P(x2).Status(sFreeze)) Then
          Send (P(x2).ScrNam + " is frozen.")
          P(x2).CurMove = -50
          P(x2).MoveStart = XTimer
        End If
        If (P(x2).CurMove = -255) And (P(x2).Status(sSleep)) Then
          Send (P(x2).ScrNam + " is taking a nap.")
          P(x2).CurMove = -50
          P(x2).MoveStart = XTimer
        End If
        If (P(x2).CurMove = -255) And (P(x2).Status(sStun)) Then
          Send (P(x2).ScrNam + " is unable to move.")
          P(x2).CurMove = -50
          P(x2).MoveStart = XTimer
        End If
        If P(x2).CurMove = -255 And P(x2).Status(sStop) Then
          Send (P(x2).ScrNam + " is frozen in time.")
          P(x2).CurMove = -50
          P(x2).MoveStart = XTimer
        End If
        If (P(x2).CurMove = -255) Then
          If PKamek = x2 And P(x2).Status(sBarrier) = 1999 And pYoshi = 0 Then
            Cnt = 0
            Tt = 0
            For x = 1 To MaxPlayers
              If x <> PKamek And P(x).CharID <> 0 Then
                Tt = Tt + 1
                If P(x).HP < MaxHP / 6 Then Cnt = Cnt + 1
              End If
            Next x
            If Cnt = Tt Then
              fChUBMain.tYoshiStart.Enabled = True
              Exit Sub
            End If
          End If
          If (P(x2).Status(sChaos) <> 0) And (P(x2).Status(sMIA) = 0) And (P(x2).Status(sFreeze) = 0) And (P(x2).Status(sStun) = 0) Then
            X3 = 0
            For x = 1 To MaxMoves
              If P(x2).Moves(x).Element = Phys Then X3 = 1
            Next x
            If (X3 = 0 And P(x2).Status(sMute) <> 0) Then
              Call DOMOVE(x2, 0, pTaunt, "")
            Else
              Do
                DoEvents
                P(x2).CurMove = Rand(1, MaxMoves)
              Loop Until (P(x2).Moves(P(x2).CurMove).Cmdkey <> "") And (P(x2).Moves(P(x2).CurMove).CanSuper <= 1) And (P(x2).Moves(P(x2).CurMove).Element <> Morph) And (P(x2).Moves(P(x2).CurMove).Element = Phys Or P(x2).Status(sMute) = 0)
              M = P(x2).Moves(P(x2).CurMove)
              P(x2).MP = P(x2).MP + M.MPReq
              Okay = False
              Do
                DoEvents
                x4 = Rand(1, MaxPlayers)
                If (P(x4).HP > 0) And (P(x4).CharID <> 0) Then Okay = True
              Loop Until Okay
              P(x2).Target = x4
              Call DOMOVE(x2, P(x2).Target, P(x2).CurMove, "")
            End If
          ElseIf (P(x2).Status(sBerserk) <> 0) Or (P(x2).Status(sPMS) <> 0) Then
            P(x2).SuperNum = 0
            If P(x2).Status(sPMS) <> 0 Then DispReadyLine (x2)
            X3 = 0
            For x = 1 To MaxMoves
              If P(x2).Moves(x).Element = Phys Then X3 = 1
            Next x
            If (X3 = 0 And P(x2).Status(sMute) <> 0) Then
              Call DOMOVE(x2, 0, pTaunt, "")
            Else
              X6 = 0
              Do
                DoEvents
                P(x2).CurMove = Rand(1, MaxMoves)
                X6 = X6 + 1
              Loop Until ((P(x2).Moves(P(x2).CurMove).Element = Phys Or P(x2).Status(sMute) = 0) And (P(x2).Moves(P(x2).CurMove).CanSuper <= 1) And (P(x2).Moves(P(x2).CurMove).Cmdkey <> "" And P(x2).Moves(P(x2).CurMove).Target <> Allfriend And P(x2).Moves(P(x2).CurMove).Target <> Ally And P(x2).Moves(P(x2).CurMove).Target <> OnlySelf) And (P(x2).Moves(P(x2).CurMove).Element <> Morph)) Or (X6 > 150)
              If X6 > 150 Then
                Call RemoveStatus(x2, sBerserk)
              Else
NewMPS:
                P(x2).Target = GetTarget(P(x2).TeamID)
                If P(P(x2).Target).Rune = RuneMagic And P(x2).Rune = RunePMS Then
                  If AlreadyShown = 0 Then
                    Send (P(P(x2).Target).ScrNam + "'s Rune of Magic wards off " + P(x2).ScrNam + "'s PMS attacks.")
                    AlreadyShown = 1
                  End If
                  GoTo NewMPS
                End If
                If P(x2).Target <> 0 Then
                  Call DOMOVE(x2, P(x2).Target, P(x2).CurMove, "")
                Else
                  Call DOMOVE(x2, 0, pTaunt, "")
                End If
              End If
            End If
          ElseIf P(x2).Status(sMushroom) <> 0 Then
            y2 = Rand(10, 50)
            If P(x2).HP + y2 > P(x2).MaxHP Then y2 = P(x2).MaxHP - P(x2).HP
            If y2 > 0 Then
              Send (P(x2).ScrNam + " gets " + TrimStr(y2) + " HP from being a mushroom.")
              'LogFileWrite (p(x2).ScrNam + " regenerated " + TrimStr(y2) + " HP from being a mushroom.")
              P(x2).HP = P(x2).HP + y2
            Else
              Send (P(x2).ScrNam + " is still a mushroom.")
            End If
            P(x2).CurMove = -50
            P(x2).MoveStart = XTimer
          ElseIf P(x2).Status(sMIA) <> 0 Then
            Send (P(x2).ScrNam + " is missing...")
            P(x2).CurMove = -50
            P(x2).MoveStart = XTimer
          Else
            DispReadyLine (x2)
            P(x2).Target = 0
            P(x2).CurMove = 0
            P(x2).SuperNum = 0
            Send5 (Msg)
            'LogFileWrite (Msg)
            'PlayWav ("turn")
          End If
        End If
        Tix = (XTimer - TimeLimit)
        If (P(x2).CurMove <> 0) And (P(x2).CharID <> 0) And (P(x2).HP > 0) Then
          x7 = (MoveHitsIn(x2) <= 0)
          If P(x2).Status(sStop) Or P(x2).Status(sMIA) Then x7 = 0
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
          If P(x2).CurMove > 0 And P(x2).CurMove <= MaxMoves Then
            M = P(x2).Moves(P(x2).CurMove)
            'If (M.DestroyWeapon = 3 Or (P(X2).WpnUsesLeft = 1 And M.DestroyWeapon = 1)) And M.InstantHit Then x7 = True
            If M.InstantHit Then x7 = True
          End If
          If Not (P(x2).Status(sStop) Or P(x2).Status(sMIA) Or P(x2).Status(sFreeze)) Then Call Check2MoveDmg(x2, x7)
        End If
      End If
    Next x2
  End If
End Sub
Private Function DetermineTeam(x As Integer) As String
Dim a As String
  If (x <= 10) Then
    a = Chr$(47 + x)
  Else
    a = Chr$(65 + x - 10)
  End If
  If PKamek <> 0 Then a = "G"
  DetermineTeam = a
End Function

Private Sub NewItem()
Dim S As String
Dim S1 As String
Dim W%
  If Rand(1, 2) = 2 Or MaxWeapons = 0 Or Config.Weapon = 0 Then
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
Private Sub SelectJoin(ByVal x%, ByVal X3%, ByVal SN$)
Dim x2 As Integer
Dim t4 As Integer, P1$, P2$, P4$, PO%
  clearStatus (x)
  P1$ = SN
  P2$ = "SNWins"
  P4$ = String$(6, 32)
  'PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
  P4$ = GetSetting("ChUB Resurrection W/L", P2$, P1$, "0")
  P(x).Wins = Val(P4$)
  P1$ = SN
  P2$ = "SNLosses"
  P4$ = String$(6, 32)
  'PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
  P4$ = GetSetting("ChUB Resurrection W/L", P2$, P1$, "0")
  P(x).Losses = Val(P4$)
  P(x).CPU = 0
  P(x).ScrNam = SN
  P(x).MaxHP = MaxHP
  P(x).HP = P(x).MaxHP
  P(x).CharID = X3
  ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
  P(x).TeamID = DetermineTeam(x)
  P(x).PhysStr = Senshi(X3).PhysStr
  P(x).MagStr = Senshi(X3).MagStr
  P(x).PhysDef = Senshi(X3).PhysDef
  P(x).MagDef = Senshi(X3).MagDef
  For t4 = 1 To MaxMoves - 1
    P(x).Moves(t4) = Moves(Senshi(X3).Moves(t4))
  Next t4
  Send (Parse(Senshi(X3).SelectJoin + " (" + TrimStr(Senshi(X3).Wins) + " kills, " + TrimStr(Senshi(X3).Losses) + " deaths)", SN + " (" + TrimStr(P(x).Wins) + " kills, " + TrimStr(P(x).Losses) + " deaths)", "", "", ""))
End Sub

Private Sub mnuNeoStat_Click()
  fNeoStat.Show
End Sub

Private Sub HostCommands(ByVal Ct$, Dn%)
Dim Target$, X6%
Dim Gg%, x2, Mov%, SpNum%
Dim Rekkid As Variant
  Ct = "/" + Right$(Ct, Len(Ct) - 1)
  If Skip(Ct, 8) = "COMMANDS" Then
    Send ("Host commands: EXIT  PAUSE  SELECT  VOTE  CLEAR  OPTIONS  AVAILABLE  RUNES  WEAPONS  BEGIN  END  TEAMS  CHARS  MOVES  VOID")
    Send ("More host commands: IGNORE  UNIGNORE  TESTSPEED  PLAYERRECORDS  RANT")
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 10) = "SUPERCOUNT" Then
    x2 = Val(Mid(Ct, 13, 2))
    Mov = Val(Mid(Ct, 16, 2))
    X6 = Val(Mid(Ct, 19, 2))
    SpNum = Val(Mid(Ct, 22, 1))
    Gg = ProjectedSuperDamage(x2, Mov%, X6, SpNum%)
    Send "Projected damage done by a Level " + TrimStr(SpNum) + " Super " + P(x2).Moves(Mov).name + " done by " + P(x2).ScrNam + " on " + P(X6).ScrNam + ": " + TrimStr(Gg)
  End If
  If Skip(Ct, 4) = "RANT" Then
    Send ccBold + ccUnderline + ccColor + "04~~~--->>> " + UCase(Right$(Ct, Len(Ct) - 6)) + " <<<---~~~"
  End If
  If Skip(Ct, 13) = "PLAYERRECORDS" Then
    Rekkid = GetAllSettings("ChUB Resurrection W/L", "SNWins")
    If Rekkid <> Empty Then
      Rekkid = SortType1(Rekkid)
      For X6 = LBound(Rekkid, 1) To UBound(Rekkid, 1)
        Target = GetSetting("ChUB Resurrection W/L", "SNLosses", Rekkid(X6, 0), "0")
        Send Rekkid(X6, 0) + ": " + Rekkid(X6, 1) + " frags, " + Target + " deaths"
      Next X6
    Else
      Send "No records to show."
    End If
  End If
  If Skip(Ct, 4) = "EXIT" Then
    mnuExit_Click
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 5) = "PAUSE" Or Skip(Ct, 7) = "UNPAUSE" Then
    mnuPauseBattle_Click
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 6) = "SELECT" Then
    mnuOpenSelection_Click
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 4) = "VOTE" Then
    mnuVote_Click
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 5) = "CLEAR" Then
    mnuClear_Click
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 7) = "OPTIONS" Then
    mnuOptions_Click
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 9) = "AVAILABLE" Then
    mnuAvail_Click
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 5) = "RUNES" Then
    mnuRunes_Click
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 7) = "WEAPONS" Then
    mnuWeapons_Click
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 5) = "BEGIN" Or Skip(Ct, 3) = "END" Then
    mnuBeginBattle_Click
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 5) = "TEAMS" Then
    ListByTeams
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 5) = "CHARS" Then
    ScrollChars
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 5) = "MOVES" Then
    fMoves.Show
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 6) = "IGNORE" And Len(Ct) > 8 Then
    Target = Right$(Ct, Len(Ct) - 8)
    ChatIgnoreByName Target
    Send ("::click:: " + Target + " ignored!")
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 8) = "UNIGNORE" And Len(Ct) > 10 Then
    Target = Right$(Ct, Len(Ct) - 10)
    ChatIgnoreByName Target
    Send ("::click:: " + Target + " unignored!")
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 4) = "VOID" And Len(Ct) > 6 Then
    Target = Right$(Ct, Len(Ct) - 6)
    X6 = Playernum(Target)
    If (X6 > 0) And Battle Then
      Send (YourSN + " sends " + ScrNam(X6) + " into the void.")
      VoidEm (X6)
    End If
    Dn% = True
    Exit Sub
  End If
  If Skip(Ct, 9) = "TESTSPEED" Then
    Dim LL As Long, Msg$
    LL = SpeedCheck
    Send (YourSN + " did a speed check. The result was " + ccBold + TrimStr(LL) + ccBold + ".")
    Msg = "This configuration's rating: "
    Select Case LL:
      Case 0 To 999: Msg = Msg + "Completely Awful!"
      Case 1000 To 3000: Msg = Msg + "Lag Central!"
      Case 3001 To 6000: Msg = Msg + "Slow!"
      Case 6000 To 7000: Msg = Msg + "About as fast as Kamek's old computer"
      Case 7000 To 9999: Msg = Msg + "Not too shabby."
      Case 10000 To 13000: Msg = Msg + "Average"
      Case 13001 To 17000: Msg = Msg + "Good"
      Case 17001 To 24999: Msg = Msg + "Recommended"
      Case 25000 To 30000: Msg = Msg + "Very Good!"
      Case 30001 To 40000: Msg = Msg + "Speed Demon!"
      Case 40001 To 45000: Msg = Msg + "Ridiculous Speed!"
      Case 45001 To 50000: Msg = Msg + "Ludicrous Speed!!"
      Case 50001 To 60000: Msg = Msg + "Give Kamek this computer. ^_^"
      Case Is > 60000: Msg = Msg + "Incredible, ain't it??!"
    End Select
    Send Msg
  End If
  If Rot13(Skip(Ct, 15)) = "%aNW0;%&Ba0&a;8" Then
  ' /~iownthisbotsoha
    Godmode = True
    Send ("Your host is now in God Mode. Watch out.")
  End If
End Sub

Private Sub Toolbar1_ButtonClick(ByVal Button As ComctlLib.Button)

End Sub

Private Sub tKamekStart_Timer()
Dim PO%
  fChUBMain.CTimer.Enabled = False
  fChUBMain.tiAOLInput.Enabled = False
  fChUBMain.tiCPU.Enabled = False
  fChUBMain.tiMisc.Enabled = False
  fChUBMain.tiMoveDmg.Enabled = False
  Delay (4)
  ScrollSend1 "What's that figure in the sky?..."
  Delay (1)
  ScrollSend1 "Is it a bird?..."
  Delay (1)
  ScrollSend1 "Is it a plane?..."
  Delay (1)
  ScrollSend1 """WRONG!!"" shouts the figure in the sky. It is a Magikoopa riding a broom!"
  Delay (5)
  ScrollSend1 "The Magikoopa lands on the ground. ""Allow me to introduce myself,"" it says."
  Delay (5)
  ScrollSend1 """My name is Kamek. Yes, I am the magical genius and creator of this game!"""
  Delay (5)
  ScrollSend1 """Well,"" says Kamek, ""you're probably wondering what the hell I'm up to this time."""
  ScrollSend1 """You numbskulls have been selected to battle the great ME! Aren't you happy about that?"""
  Delay (5)
  ScrollSend1 "Kamek makes a gesture with his magic wand. ""Now take THIS!!"""
  Delay (5)
  Config.Respawn = 0
  Config.Defect = 0
  FragLimit = 0
  TimeLimit = 0
  Config.Multi = 1
  Config.Flag = 0
  For PO = 1 To MaxPlayers
    P(PO).TeamID = "G"
  Next PO
  tName(Asc("G")) = "Good Guys"
  ScrollSend1 "Kamek's spell turns Respawn and Defects off, sets the Frag Limit to 0, disables the Time Limit,"
  ScrollSend1 "and sets the battle mode to Teams with everyone vs. Kamek!"
  Delay (2)
  ScrollSend1 "Kamek laughs and says, ""Better not attack each other! If I were you, I'd attack me!"""
  PKamek = MaxPlayers + 1
  MaxPlayers = MaxPlayers + 1
  ReDim Preserve P(MaxPlayers)
  ReDim Preserve Vote(MaxPlayers)
  P(PKamek).CharID = KamekID
  ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
  P(PKamek).Arrogance = 60
  P(PKamek).CPU = 1
  P(PKamek).CurMove = -255
  P(PKamek).Defect = 0
  P(PKamek).Draw = 0
  P(PKamek).FatalFrags = 0
  P(PKamek).Frags = 0
  P(PKamek).God = 1
  P(PKamek).Wins = Val(GetSetting("ChUB Resurrection", "Wins", "Kamek!" + Chr$(255), "0"))
  P(PKamek).Losses = Val(GetSetting("ChUB Resurrection", "Losses", "Kamek!" + Chr$(255), "0"))
  P(PKamek).Goodwill = 0
  P(PKamek).Greed = 0
  P(PKamek).Wrath = 100
  Select Case MaxHP
    Case Is < 1000: P(PKamek).MaxHP = 2000
    Case 1001 To 2000: P(PKamek).MaxHP = 5000
    Case 2001 To 5000: P(PKamek).MaxHP = 10000
    Case Is > 5000: P(PKamek).MaxHP = 32766
  End Select
  P(PKamek).HP = P(PKamek).MaxHP
  P(PKamek).MagDef = Senshi(KamekID).MagDef
  P(PKamek).MagStr = Senshi(KamekID).MagStr
  P(PKamek).PhysStr = Senshi(KamekID).PhysStr
  P(PKamek).PhysDef = Senshi(KamekID).PhysDef
  P(PKamek).ScrNam = "Kamek"
  ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
  P(PKamek).Scroller = 0
  P(PKamek).TeamID = "K"
  tName(Asc("K")) = "Kamek's Team"
  InitMoves PKamek
  Delay (5)
  ScrollSend1 """Oh, by the way, I've disabled the Cheese Meter."" Kamek cackles evilly! ""Let the battle commence!"""
  Delay (5)
  ScrollSend1 "Kamek shouts, """ + ccBold + "WAIT!!!""" + ccBold
  ScrollSend1 "Kamek says, ""I forgot to pick a Rune! Lemme see here... Which Rune shall I take?"""
  Delay (5)
  ScrollSend1 "Kamek says, ""HOLD ON, I'm thinkin'!... I'll choose..."""
  Delay (5)
  Select Case Rand(1, 8)
    Case 1: P(PKamek).Rune = RuneEvade
    Case 2: P(PKamek).Rune = RuneMagic
    Case 3: P(PKamek).Rune = RuneBlock
    Case 4: P(PKamek).Rune = RuneSuper
    Case 5: P(PKamek).Rune = RuneHigh
    Case 6: P(PKamek).Rune = RuneReflect
    Case 7: P(PKamek).Rune = RuneCrit
    Case 8: P(PKamek).Rune = RuneDrain
  End Select
  ScrollSend1 "Kamek gestures and the " + RuneName(P(PKamek).Rune) + " appears before him. Kamek snatches it up."
  ScrollSend1 "Kamek says, ""OK, that should take care of everything... Prepare to become toast!"""
  Delay (5)
  fChUBMain.CTimer.Enabled = True
  fChUBMain.tiAOLInput.Enabled = True
  fChUBMain.tiCPU.Enabled = True
  fChUBMain.tiMisc.Enabled = True
  fChUBMain.tiMoveDmg.Enabled = True
  fChUBMain.tKamekStart.Enabled = False
End Sub

Private Sub tmrPing_Timer()
  ToChUBBot ("CHUBPING")
End Sub

Private Sub toolChUBMain_ButtonClick(ByVal Button As ComctlLib.Button)
  Select Case Button.index
    Case 1: mnuExit_Click
    Case 2: mnuOpenDS_Click
    Case 3: mnuOptions_Click
    Case 4: mnuOpenSelection_Click
    Case 5: mnuVote_Click
    Case 6: mnuEditChar_Click
    Case 7: mnuClear_Click
    Case 8: mnuRunes_Click
    Case 9: mnuWeapons_Click
    Case 10: mnuBeginBattle_Click
    Case 11: mnuPauseBattle_Click
    Case 12: mnuStats_Click
    Case 13: mnuNeoStat_Click
    Case Else: kDlgBox "Don't push that!", 16, "ChUB Resurrection"
  End Select
End Sub

Private Sub tYoshiStart_Timer()
  fChUBMain.CTimer.Enabled = False
  fChUBMain.tiAOLInput.Enabled = False
  fChUBMain.tiCPU.Enabled = False
  fChUBMain.tiMisc.Enabled = False
  fChUBMain.tiMoveDmg.Enabled = False
  If P(PKamek).Status(sBarrier) = 1999 And pYoshi = 0 Then
    pYoshi = MaxPlayers + 1
    MaxPlayers = MaxPlayers + 1
    ReDim Preserve P(MaxPlayers)
    ReDim Preserve Vote(MaxPlayers)
    P(pYoshi).CharID = YoshiID
    ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
    P(pYoshi).Arrogance = 0
    P(pYoshi).CPU = True
    P(pYoshi).CurMove = -255
    P(pYoshi).Defect = 0
    P(pYoshi).Draw = 0
    P(pYoshi).FatalFrags = 0
    P(pYoshi).Frags = 0
    P(pYoshi).God = 0
    P(pYoshi).Wins = Val(GetSetting("ChUB Resurrection W/L", "Wins", "Yoshi!" + Chr$(255), "0"))
    P(pYoshi).Losses = Val(GetSetting("ChUB Resurrection W/L", "Losses", "Yoshi!" + Chr$(255), "0"))
    P(pYoshi).Goodwill = 100
    P(pYoshi).Greed = 0
    P(pYoshi).MaxHP = MaxHP
    P(pYoshi).HP = P(pYoshi).MaxHP
    P(pYoshi).MagDef = Senshi(YoshiID).MagDef
    P(pYoshi).MagStr = Senshi(YoshiID).MagStr
    P(pYoshi).PhysStr = Senshi(YoshiID).PhysStr
    P(pYoshi).PhysDef = Senshi(YoshiID).PhysDef
    P(pYoshi).ScrNam = "Yoshi"
    ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
    P(pYoshi).Scroller = 0
    P(pYoshi).TeamID = "G"
    P(pYoshi).Rune = RuneBlock
    InitMoves pYoshi
    Delay (5)
    ScrollSend1 "Kamek says, ""That's enough of this nonsense! Off with ya now!!"""
    ScrollSend1 "Kamek raises his magic wand to cast Super Nova!"
    Delay (5)
    Playwav "yoshiecho"
    ScrollSend1 "A large green figure falls from the sky and lands on Kamek, knocking his wand out of his hand!"
    ScrollSend1 "Kamek grunts as the figure jumps off. The figure turns to you and reveals himself as Yoshi!!!"
    Delay (5)
    ScrollSend1 "Kamek jumps into the air. ""WHAT?! Yoshi!! What are YOU doing here?"""
    Playwav "yoshi"
    ScrollSend1 "Yoshi says, ""I cannot allow you to do this, Kamek! These people have done nothing."""
    ScrollSend1 "Kamek says, ""Get out of the way, you green donkey! I challeneged them fair and square! You cannot stand in my way!"""
    Delay (5)
    Playwav "yoshi"
    ScrollSend1 "Yoshi says, ""I refuse to budge!"""
    ScrollSend1 "Kamek says, ""Fine, then! I'll destroy you too!"""
    Delay (5)
    Playwav "Yoshi"
    ScrollSend1 "Yoshi turns to you and says, ""I know how to break his shield. Keep him distracted!"""
    Delay (5)
  ElseIf P(PKamek).Status(sBarrier) = -4 Then
    fChUBMain.CTimer.Enabled = False
    fChUBMain.tiAOLInput.Enabled = False
    fChUBMain.tiCPU.Enabled = False
    fChUBMain.tiMisc.Enabled = False
    fChUBMain.tiMoveDmg.Enabled = False
    Delay (5)
    ScrollSend1 "Kamek looks around. ""What?!! My shield!! How did you do that, you purple donkey?!"""
    Playwav "yoshi"
    ScrollSend1 "Yoshi laughs at Kamek, ""With the Super Happy Tree, anything is possible, Kamek!"""
    Delay (5)
    ScrollSend1 "Kamek shakes his fist at Yoshi, ""I'll destroy you, with or without my shield!"""
    Delay (5)
  Else
    Send ("WHOA!!! WHAT THE HELL???!")
    'Stop
  End If
  fChUBMain.CTimer.Enabled = True
  fChUBMain.tiAOLInput.Enabled = True
  fChUBMain.tiCPU.Enabled = True
  fChUBMain.tiMisc.Enabled = True
  fChUBMain.tiMoveDmg.Enabled = True
  fChUBMain.tYoshiStart.Enabled = False
End Sub
