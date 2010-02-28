Option Explicit
Option Compare Text

Dim CurCh%, CurIt%, CurAr%, CurWpn%, CurMov%

Sub DupeTest ()
Dim x1%, X2%, C$
  For x1 = 1 To NumSenshi
    For X2 = x1 + 1 To NumSenshi
      If UCase(Senshi(x1).PickMe) = UCase(Senshi(X2).PickMe) And Len(Senshi(x1).PickMe) > 0 Then
	C$ = Right$(Senshi(X2).PickMe, 1)
	If Asc(C$) >= 48 And Asc(C$) <= 57 Then
	  C$ = Chr$(Asc(C$) + 1)
	  If Asc(C$) > 57 Then C$ = "-0"
	Else
	  C$ = "0"
	End If
	Senshi(X2).PickMe = Left$(Senshi(X2).PickMe, Len(Senshi(X2).PickMe) - 1) + C$
      End If
    Next X2
  Next x1
End Sub

Sub InitFromDisk (ByVal FileName As String)
Dim X%, x1%
Dim S$
Dim St$, Ch$, It$, Ar$, Yma$
Dim Dunnit%
what:
  Dunnit = 0
  ReDim Senshi(1)
  ReDim Items(1)
  ReDim Arena(1)
  ReDim Moves(1)
  ReDim Weapons(1)
  Weapons(0).Name = "No Weapon"
  Dataset.ClearChars = ">> Characters CLEARED {S Drop} <<"
  Dataset.CharsNotCleared = ">> Characters *NOT* Cleared {S Drop} <<"
  Dataset.GameAborted = ">>> Game aborted."
  Dataset.AcceptDefects = "%SN is now allowing defects."
  Dataset.DeclineDefects = "%SN is no longer allowing defects."
  Dataset.WantDraw = "%SN votes for a draw."
  Dataset.DontWantDraw = "%SN votes for no draw."
  Dataset.UnMorphMsg = "%SN reverts back to their original form as %S2."
  Dataset.ChiCtr = "~~ Ignore this line ~~"
  Dataset.Counter = "~~ Ignore this Line ~~"
  Dataset.FleeFail = "%SN tries to escape, but slams against a strange barrier."
  Dataset.NoGetItem = "There's nothing to get, %SN, so knock it off."
  Dataset.HPDivert = "~~ %SN diverts %S2 HP to their Super Meter."
  'Dataset.HPDivert = "~~ This line has no effect ~~"
  'Dataset.NotEnoughMP = "Sorry, %SN, but %T requires %S2 MP, and you only have %Y."
  Dataset.NotEnoughMP = "~~ Ignore this Line ~~"
  Dataset.AllDead = ">>> Drawn Game by mutual annihilation {S Goodbye}"
  Dataset.Draw = ">>> Drawn Game by Agreement"
  Dataset.X1HrLeft = "You have one hour to go."
  Dataset.X30MinsLeft = "You now have 30 minutes."
  Dataset.X20MinsLeft = "You now have 20 minutes."
  Dataset.X10MinsLeft = "__+ 10 Minutes To Go +__"
  Dataset.X5MinsLeft = ">> 5 Minutes Remaining On The Clock <<"
  Dataset.X2MinsLeft = ">>> TWO-MINUTE WARNING! <<<"
  Dataset.SuddenDeath = "--== Now Entering SUDDEN DEATH, 1 Minute to Go ==--"
  Dataset.x1MinsLeft = "!! 1 MINUTE TO GO !!"
  Dataset.x30SecsLeft = "You only have 30 seconds left to kick their butts in!"
  Dataset.x15SecsLeft = "15 seconds remaining"
  Dataset.x5SecsLeft = "5 seconds to go"
  Dataset.TimeExpired = ">>> Oh my God! You ran out of time! You bastards!"
  Dataset.BeatYouma = ">>> You have won!!"
  Dataset.YouLose = ">>> Sorry. You Lose."
  Dataset.Respawn = "%SN respawns back into the fragfest."
  Dataset.BeginVote = "(!) Millenium Voting Start (!)"
  Dataset.CommieVote = "~~ This line has no effect ~~"
  Dataset.Remove = "%SN wusses out after seeing the competition."
  Dataset.Random = "%SN randomly selects %S2."
  Dataset.Taken = "Guess what, %SN? %S2 is already taken -- by %T!"
  Dataset.DefectSucc = "%SN successfully defects to '%S2' (Team %T)"
  Dataset.DefectFail = "%T is not accepting your defects, %SN."
  Dataset.FleeAttempt = "%SN turns around and starts running."
  Dataset.SuperKill = "===>>> %S2-HIT SUPER %Y! <<<==="
  CurMov% = 1
  On Error GoTo erDataSet
  Open FileName For Input As #1
    Input #1, St$
    If FileExists(St$) Then
      kDlgBox "Sorry, old KUB datasets are no longer supported... Please read C2KDATA.TXT to see how to make a new dataset", 16, "ChUB 2000"
      End
    Else
      Dataset.LoadStr = St$
      Input #1, Dataset.BeginSelect
      Input #1, Dataset.EndSelect
      Input #1, Dataset.BattleBegin
      Input #1, Dataset.BattlePause
      Input #1, Dataset.BattleUnPause
      Input #1, Dataset.BattleEnd
      CurCh% = 1
      CurIt% = 1
      CurAr% = 1
      CurWpn% = 1
      Do
	Input #1, S$
	If (S$ = "[more]") Then
	  Dunnit = -1
	  Input #1, Dataset.ClearChars
	  Input #1, Dataset.CharsNotCleared
	  Input #1, Dataset.GameAborted
	  Input #1, Dataset.AcceptDefects
	  Input #1, Dataset.DeclineDefects
	  Input #1, Dataset.WantDraw
	  Input #1, Dataset.DontWantDraw
	  Input #1, Dataset.UnMorphMsg
	  Input #1, Dataset.ChiCtr
	  Input #1, Dataset.Counter
	  Input #1, Dataset.FleeFail
	  Input #1, Dataset.NoGetItem
	  Input #1, Dataset.HPDivert
	  Input #1, Dataset.NotEnoughMP
	  Input #1, Dataset.AllDead
	  Input #1, Dataset.Draw
	  Input #1, Dataset.X1HrLeft
	  Input #1, Dataset.X30MinsLeft
	  Input #1, Dataset.X20MinsLeft
	  Input #1, Dataset.X10MinsLeft
	  Input #1, Dataset.X5MinsLeft
	  Input #1, Dataset.X2MinsLeft
	  Input #1, Dataset.SuddenDeath
	  Input #1, Dataset.x1MinsLeft
	  Input #1, Dataset.x30SecsLeft
	  Input #1, Dataset.x15SecsLeft
	  Input #1, Dataset.x5SecsLeft
	  Input #1, Dataset.TimeExpired
	  Input #1, Dataset.BeatYouma
	  Input #1, Dataset.YouLose
	  Input #1, Dataset.Respawn
	  Input #1, Dataset.BeginVote
	  Input #1, Dataset.CommieVote
	  Input #1, Dataset.Remove
	  Input #1, Dataset.Random
	  Input #1, Dataset.Taken
	  Input #1, Dataset.DefectSucc
	  Input #1, Dataset.DefectFail
	  Input #1, Dataset.FleeAttempt
	  Input #1, Dataset.SuperKill
	  Input #1, S$
	End If
	If KWnStr("*.", S$) > 0 Then
	  If Mid$(S$, 2, 1) <> ":" Then
	    If KWnStr("*.", S$) > 1 Then
	      fWhat!Huh.Path = App.Path + "\" + Left$(S$, KWnStr("*.", S$) - 2)
	    Else
	      fWhat!Huh.Path = App.Path
	    End If
	  Else
	    fWhat!Huh.Path = Left$(S$, KWnStr("*", S$) - 2)
	  End If
	  fWhat!Huh.Pattern = "*." + FilExt(S$)
	  For X = 0 To fWhat!Huh.ListCount - 1
	    LoadIt (fWhat!Huh.Path + "\" + fWhat!Huh.List(X))
	  Next X
	Else
	  LoadIt (S$)
	End If
      Loop Until EOF(1)
      HighSenshi = CurCh% - 1
      NumSenshi = CurCh% - 1
      MaxItem = CurIt% - 1
      MaxArena = CurAr% - 1
      MaxWeapons = CurWpn% - 1
      Send ("Loaded Dataset " + Dataset.LoadStr + ": " + TrimStr(HighSenshi) + " characters (" + TrimStr(CurMov%) + " moves), " + TrimStr(MaxItem) + " items, " + TrimStr(MaxArena) + " arenas, " + TrimStr(MaxWeapons) + " weapons")
      Close #1
      Unload fWhat
      On Error GoTo 0
      DupeTest
      Exit Sub
    End If
erDataSet:
  kDlgBox S$ + " " + Error$(Err) + " reading dataset init file " + FileName + ".", 16, "Initialize Failed"
  Stop
End Sub

Sub LoadChar (ByVal FileName As String, ByVal x1 As Integer)
Dim X As Integer
Dim X2 As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim Rx As Integer
Dim Nu$, C$
Dim P1$, P2$, P4$, PO%, Msg$
Dim MovAvg%, NumMovs%
  On Error GoTo NotOk11
    Open FileName For Input As #3
      Input #3, Senshi(x1).FullName
      C$ = "Encrypted ChUB Character"
      If Left$(Senshi(x1).FullName, Len(C$)) = "Encrypted ChUB Character" Then
	'MsgBox "You cannot edit an encrypted character.", 16, "ChUB CharEdit"
	'ClearChar (x1)
	Close #3
	LoadEncryptedChar FileName$, x1%
	Exit Sub
      End If
      Input #3, Senshi(x1).CharID
      On Error GoTo Whoops91
      Input #3, Rx
      Senshi(x1).Weakness = Rx Mod 100
      Senshi(x1).Resist = (Rx - Senshi(x1).Weakness) / 100
      GoTo Whoops92
Whoops91:
      Senshi(x1).Weakness = Shadow
      Senshi(x1).Resist = Shadow
      Resume Whoops92
Whoops92:
      On Error GoTo NotOk11
      Input #3, Senshi(x1).PickMe
      Input #3, Senshi(x1).SelectStr
      Input #3, Senshi(x1).SelectJoin
      Input #3, Senshi(x1).PhysStr
      Input #3, Senshi(x1).PhysDef
      Input #3, Senshi(x1).MagStr
      Input #3, Senshi(x1).MagDef
      On Error GoTo Whoops93
      Input #3, Senshi(x1).BlockYes
      GoTo Whoops94
Whoops93:
      Senshi(x1).BlockYes = ""
      Resume Whoops94
Whoops94:
      On Error GoTo NotOk11
      If Senshi(x1).BlockYes = "0" Then Senshi(x1).BlockYes = ""
      'Input #3, X2
      Input #3, Senshi(x1).Rest
      Input #3, Senshi(x1).Block
      Input #3, Senshi(x1).BlockFail
      For X = 1 To 5
	Input #3, Senshi(x1).Taunt(X)
      Next X
      Input #3, Senshi(x1).Fatality.Cmdkey
      Input #3, Senshi(x1).Fatality.PreFatal
      Input #3, Senshi(x1).Fatality.FatalMove
      MovAvg = 0
      NumMovs = 0
      For X = 1 To 12
	Senshi(x1).Moves(X) = CurMov%
	Input #3, Moves(CurMov%).Name
	Input #3, Moves(CurMov%).Cmdkey
	Input #3, Moves(CurMov%).CanSuper
	Input #3, Moves(CurMov%).Begin2Attack
	Input #3, Moves(CurMov%).Begin2SuperAttack
	Input #3, Moves(CurMov%).Begin2HealSelf
	Input #3, Moves(CurMov%).Hit
	Input #3, Moves(CurMov%).SuperHit
	Input #3, Rx
	Input #3, Moves(CurMov%).HealSelf
	Input #3, Moves(CurMov%).CritHit
	Input #3, Moves(CurMov%).HealMeld
	Input #3, Moves(CurMov%).Miss
	Input #3, Moves(CurMov%).status(sMute)
	Input #3, Moves(CurMov%).status(schaos)
	Input #3, Moves(CurMov%).status(sfreeze)
	Input #3, Moves(CurMov%).status(ssleep)
	Input #3, Moves(CurMov%).status(sPoison)
	Input #3, Moves(CurMov%).status(sblind)
	Input #3, Rx
	Input #3, Moves(CurMov%).status(shaste)
	Input #3, Rx
	Input #3, Rx
	Input #3, Moves(CurMov%).SuperMiss
	If Moves(CurMov%).SuperMiss = "0" Then Moves(CurMov%).SuperMiss = ""
	Input #3, Moves(CurMov%).status(sslow)
	Input #3, Moves(CurMov%).status(sstun)
	Input #3, Moves(CurMov%).status(sLife3)
	Input #3, Moves(CurMov%).status(sRegen)
	Input #3, Moves(CurMov%).status(sStop)
	Input #3, Moves(CurMov%).status(sMushroom)
	Input #3, Moves(CurMov%).status(sMIA)
	Input #3, Moves(CurMov%).status(sQuick)
	Input #3, Moves(CurMov%).status(sBerserk)
	Input #3, Moves(CurMov%).status(sBarrier)
	Input #3, Moves(CurMov%).status(sMBarrier)
	Input #3, Moves(CurMov%).status(sCurse)
	Input #3, Moves(CurMov%).status(sBless)
	Input #3, Rx
	Input #3, Rx
	Input #3, Rx
	Input #3, Rx
	Input #3, Moves(CurMov%).Element
	If Moves(CurMov%).Element = 10 Then Moves(CurMov%).Element = 66
	If Moves(CurMov%).Element = Poison Then Moves(CurMov%).status(sPoison) = 100
	Input #3, Moves(CurMov%).MoveStr
	Input #3, Moves(CurMov%).Target
	'Moves(CurMov%).MPReq = CalcMP(CurMov%)
	If Moves(CurMov).Cmdkey <> "" Then
	  CurMov = CurMov + 1
	Else
	  Senshi(x1).Moves(X) = 0
	End If
	ReDim Preserve Moves(CurMov%)
      Next X
      On Error Resume Next
      For X = 1 To 5
	Input #3, Senshi(x1).DeathStr(X)
      Next X
      For X = 1 To 5
	Input #3, Senshi(x1).KillStr(X)
      Next X
      Input #3, Senshi(x1).Desc(1)
      Input #3, Senshi(x1).Desc(2)
      Input #3, Senshi(x1).Desc(3)
      Input #3, Senshi(x1).Desc(4)
      P1$ = Senshi(x1).FullName
      P2$ = "Wins"
      P4$ = String$(6, 32)
      PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
      Senshi(x1).Wins = Val(P4$)
      P2$ = "Losses"
      P4$ = String$(6, 32)
      PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
      Senshi(x1).Losses = Val(P4$)
    Close #3
    Exit Sub
  On Error GoTo 0
NotOk11:
  ScrollSend1 (Error$(Err) + " in character " + FileName)
  kDlgBox Error$(Err) + " in character " + FileName, 16, "Character Error"
  Close #3
  Exit Sub
End Sub

Sub LoadEncryptedChar (ByVal FileName$, ByVal x1%)
Dim X As Integer
Dim X2 As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim Rx%, C$, P1$, P2$, P4$, PO%
  Open FileName$ For Binary Access Read Lock Read Write As #84
    C$ = String$(24, 0)
    'For x = 1 To Len("Encrypted ChUB Character")
      Get #84, , C$
    'Next x
    Get #84, , Senshi(x1).PhysStr
    Get #84, , Senshi(x1).PhysDef
    Get #84, , Senshi(x1).MagStr
    Get #84, , Senshi(x1).MagDef
    Get #84, , Senshi(x1).Weakness
    Get #84, , Senshi(x1).Resist
    Senshi(x1).FullName = GetStr(84)
    Senshi(x1).CharID = GetStr(84)
    Senshi(x1).PickMe = GetStr(84)
    Senshi(x1).SelectStr = GetStr(84)
    Senshi(x1).SelectJoin = GetStr(84)
    Senshi(x1).BlockYes = GetStr(84)
    Senshi(x1).Rest = GetStr(84)
    Senshi(x1).Block = GetStr(84)
    Senshi(x1).BlockFail = GetStr(84)
    Senshi(x1).Fatality.Cmdkey = GetStr(84)
    Senshi(x1).Fatality.PreFatal = GetStr(84)
    Senshi(x1).Fatality.FatalMove = GetStr(84)
    For X = 1 To 5
      Senshi(x1).Taunt(X) = GetStr(84)
    Next X
    For X = 1 To 5
      Senshi(x1).DeathStr(X) = GetStr(84)
    Next X
    For X = 1 To 5
      Senshi(x1).KillStr(X) = GetStr(84)
    Next X
    For X = 1 To 4
      Senshi(x1).Desc(X) = GetStr(84)
    Next X
    For X = 1 To MaxMoves
      Senshi(x1).Moves(X) = CurMov%
      Get #84, , Moves(CurMov%).CanSuper
      Get #84, , Moves(CurMov%).Element
      Get #84, , Moves(CurMov%).ElementStr
      Get #84, , Moves(CurMov%).Target
      Get #84, , Moves(CurMov%).status(sMute)
      Get #84, , Moves(CurMov%).status(sMute)
      Get #84, , Moves(CurMov%).status(schaos)
      Get #84, , Moves(CurMov%).status(sfreeze)
      Get #84, , Moves(CurMov%).status(ssleep)
      Get #84, , Moves(CurMov%).status(sPoison)
      Get #84, , Moves(CurMov%).status(sblind)
      Get #84, , Moves(CurMov%).status(shaste)
      Get #84, , Moves(CurMov%).status(sslow)
      Get #84, , Moves(CurMov%).status(sstun)
      Get #84, , Moves(CurMov%).status(sLife3)
      Get #84, , Moves(CurMov%).status(sRegen)
      Get #84, , Moves(CurMov%).status(sStop)
      Get #84, , Moves(CurMov%).status(sMushroom)
      Get #84, , Moves(CurMov%).status(sMIA)
      Get #84, , Moves(CurMov%).status(sQuick)
      Get #84, , Moves(CurMov%).status(sBerserk)
      Get #84, , Moves(CurMov%).status(sBarrier)
      Get #84, , Moves(CurMov%).status(sMBarrier)
      Get #84, , Moves(CurMov%).status(sCurse)
      Get #84, , Moves(CurMov%).status(sBless)
      Moves(CurMov%).Name = GetStr(84)
      Moves(CurMov%).Cmdkey = GetStr(84)
      Moves(CurMov%).Begin2Attack = GetStr(84)
      Moves(CurMov%).Begin2SuperAttack = GetStr(84)
      Moves(CurMov%).Begin2HealSelf = GetStr(84)
      Moves(CurMov%).Hit = GetStr(84)
      Moves(CurMov%).SuperHit = GetStr(84)
      Moves(CurMov%).HealSelf = GetStr(84)
      Moves(CurMov%).CritHit = GetStr(84)
      Moves(CurMov%).HealMeld = GetStr(84)
      Moves(CurMov%).Miss = GetStr(84)
      Moves(CurMov%).SuperMiss = GetStr(84)
      If Moves(CurMov%).Element = 10 Then Moves(CurMov%).Element = 66
      If Moves(CurMov%).Element = Poison Then Moves(CurMov%).status(sPoison) = 100
      If Moves(CurMov).Cmdkey <> "" Then
	CurMov = CurMov + 1
      Else
	Senshi(x1).Moves(X) = 0
      End If
      ReDim Preserve Moves(CurMov%)
    Next X
    P1$ = Senshi(x1).FullName
    P2$ = "Wins"
    P4$ = String$(6, 32)
    PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
    Senshi(x1).Wins = Val(P4$)
    P2$ = "Losses"
    P4$ = String$(6, 32)
    PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
    Senshi(x1).Losses = Val(P4$)
  Close #84
End Sub

Sub LoadIt (ByVal S$)
  fWhat!Files.AddItem (S$)
  DoEvents
	Select Case FilExt(S$)
	  Case "CH2", "CHE":
	    ReDim Preserve Senshi(CurCh%)
	    LoadChar S$, CurCh%
	    CurCh% = CurCh% + 1
	  Case "CHR":
	    kDlgBox "KUB FiNAL .CHR files are no longer supported: " + S$, 16, "ChUB 2000"
	  Case "ANA":
	    ReDim Preserve Arena(CurAr%)
	    LoadArena S$, CurAr%
	    CurAr% = CurAr% + 1
	  Case "AN2":
	    ReDim Preserve Arena(CurAr%)
	    LoadNewArena S$, CurAr%
	    CurAr% = CurAr% + 1
	  Case "ITM":
	    ReDim Preserve Items(CurIt%)
	    LoadItem S$, CurIt%
	    CurIt% = CurIt% + 1
	  Case "WPN":
	    kDlgBox "ChUB Talisman .WPN files are not supported (" + S$ + ") If you have ChUB 2000 .WPN files, please change the extension to .W2K.", 16, "ChUB 2000"
	  Case "W2K":
	    ReDim Preserve Weapons(CurWpn%)
	    LoadWeapon S$, CurWpn%
	    CurWpn% = CurWpn% + 1
	  Case Else:
	    'kDlgBox "Invalid filename: " + S$, 16, "ChUB"
	End Select
End Sub

Sub LoadItem (ByVal FileName As String, ByVal x1 As Integer)
Dim Rx%
  On Error GoTo eLoadItem
  Open FileName For Input As #5
    Input #5, Items(x1).Name
    Input #5, Items(x1).Spawn
    Input #5, Items(x1).Telefrag
    Input #5, Items(x1).PlayerGet
    Input #5, Items(x1).YoumaGet
    Input #5, Items(x1).PlayerHP
    Input #5, Rx
    Input #5, Items(x1).PlayerStat(sMute)
    Input #5, Items(x1).PlayerStat(schaos)
    Input #5, Items(x1).PlayerStat(sfreeze)
    Input #5, Items(x1).PlayerStat(ssleep)
    Input #5, Items(x1).PlayerStat(sPoison)
    Input #5, Items(x1).PlayerStat(sblind)
    Input #5, Items(x1).PlayerStat(sCurse)
    Input #5, Items(x1).PlayerStat(shaste)
    Input #5, Items(x1).PlayerStat(sBarrier)
    Input #5, Items(x1).PlayerStat(sBless)
    Input #5, Items(x1).PlayerStat(sslow)
    Input #5, Items(x1).PlayerStat(sstun)
    Input #5, Rx
    Input #5, Rx
    Input #5, Items(x1).PlayerStat(sStop)
    Input #5, Items(x1).PlayerStat(sMBarrier)
    Input #5, Items(x1).PlayerStat(sLife3)
    Input #5, Items(x1).PlayerStat(sQuick)
    Input #5, Items(x1).PlayerStat(sRegen)
    Input #5, Items(x1).PlayerStat(sMushroom)
    Input #5, Items(x1).PlayerStat(sMIA)
    Input #5, Items(x1).PlayerStat(sBerserk)
  Close #5
  Exit Sub
eLoadItem:
  kDlgBox Error$(Err) + " in item " + FileName, 16, "Item Error"
  Exit Sub
End Sub

Sub LoadNewArena (ByVal FileName As String, ByVal X As Integer)
Dim CD As Integer
Dim X2 As Integer, Xx%
  On Error GoTo erLoad2
  Open FileName For Input As #2
    Input #2, Arena(X).Name
    For CD = 1 To 10
      Input #2, Arena(X).Desc(CD)
    Next CD
    On Error GoTo erTooManyDescLines2
    Input #2, Arena(X).AllAttacks
    On Error GoTo erOther2
    For X2 = 1 To MaxEle
      Input #2, Arena(X).Effect(X2)
    Next X2
    Input #2, Arena(X).RestLowHP
    Input #2, Arena(X).RestHighHP
    Input #2, Xx
    Input #2, Xx
    Input #2, Arena(X).GradualHP
    Input #2, Xx
    On Error GoTo erEvents2
    For CD = 1 To 10
      Input #2, Arena(X).Event(CD).Name
      Input #2, Arena(X).Event(CD).Frequency
      Input #2, Arena(X).Event(CD).ElementStr
      Input #2, Arena(X).Event(CD).Hit
      Input #2, Arena(X).Event(CD).Miss
      Input #2, Arena(X).Event(CD).HitsAll
    Next CD
    On Error Resume Next
    'If Not EOF(2) Then kDlgBox "Warning: Arena " + FileName + " has too many Events. Only the first 10 were loaded.", 16, "Arena Warning"
  Close #2
  Exit Sub
erLoad2:
  kDlgBox Error$(Err) + " in arena " + FileName, 16, "Arena Error"
  Stop
erTooManyDescLines2:
  kDlgBox "Error: Arena " + FileName + " has too many Description lines. Only 10 are allowed.", 16, "Arena Error"
  Stop
erOther2:
  kDlgBox Error$(Err) + " in arena " + FileName, 16, "Arena Error"
  Stop
erEvents2:
  kDlgBox "Error loading events for arena " + FileName, 16, "Arena Error"
  Stop
End Sub

Sub LoadWeapon (ByVal FileName$, ByVal x1%)
Dim X As Integer
Dim X2 As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim Rx As Integer
  On Error GoTo NotOk41
    Open FileName For Input As #43
      Input #43, Weapons(x1).Name
      Input #43, Weapons(x1).PickMe
      Input #43, Weapons(x1).SelectStr
      Input #43, Weapons(x1).Desc(1)
      Input #43, Weapons(x1).Desc(2)
      Input #43, Weapons(x1).NumUses
      For X = 1 To 5
	Input #43, Weapons(x1).Moves(X).Name
	Input #43, Weapons(x1).Moves(X).Cmdkey
	Input #43, Weapons(x1).Moves(X).CanSuper
	Input #43, Weapons(x1).Moves(X).Begin2Attack
	Input #43, Weapons(x1).Moves(X).Begin2SuperAttack
	Input #43, Weapons(x1).Moves(X).Begin2HealSelf
	Input #43, Weapons(x1).Moves(X).Hit
	Input #43, Weapons(x1).Moves(X).SuperHit
	Input #43, Weapons(x1).Moves(X).DestroyWeapon
	Input #43, Weapons(x1).Moves(X).HealSelf
	Input #43, Weapons(x1).Moves(X).CritHit
	Input #43, Weapons(x1).Moves(X).HealMeld
	Input #43, Weapons(x1).Moves(X).Miss
	Input #43, Weapons(x1).Moves(X).status(sMute)
	Input #43, Weapons(x1).Moves(X).status(schaos)
	Input #43, Weapons(x1).Moves(X).status(sfreeze)
	Input #43, Weapons(x1).Moves(X).status(ssleep)
	Input #43, Weapons(x1).Moves(X).status(sPoison)
	Input #43, Weapons(x1).Moves(X).status(sblind)
	Input #43, Weapons(x1).Moves(X).InstantHit
	Input #43, Weapons(x1).Moves(X).status(shaste)
	Input #43, Weapons(x1).Moves(X).ReqAllUses
	Input #43, X2
	Input #43, Weapons(x1).Moves(X).SuperMiss
	If Weapons(x1).Moves(X).SuperMiss = "0" Then Weapons(x1).Moves(X).SuperMiss = ""
	Input #43, Weapons(x1).Moves(X).status(sslow)
	Input #43, Weapons(x1).Moves(X).status(sstun)
	Input #43, Weapons(x1).Moves(X).status(sLife3)
	Input #43, Weapons(x1).Moves(X).status(sRegen)
	Input #43, Weapons(x1).Moves(X).status(sStop)
	Input #43, Weapons(x1).Moves(X).status(sMushroom)
	Input #43, Weapons(x1).Moves(X).status(sMIA)
	Input #43, Weapons(x1).Moves(X).status(sQuick)
	Input #43, Weapons(x1).Moves(X).status(sBerserk)
	Input #43, Weapons(x1).Moves(X).status(sBarrier)
	Input #43, Weapons(x1).Moves(X).status(sMBarrier)
	Input #43, Weapons(x1).Moves(X).status(sCurse)
	Input #43, Weapons(x1).Moves(X).status(sBless)
	Input #43, Rx
	Input #43, Rx
	Input #43, Rx
	Input #43, Rx
	Input #43, Weapons(x1).Moves(X).Element
	Input #43, Weapons(x1).Moves(X).ElementStr
	Input #43, Weapons(x1).Moves(X).Target
	'Weapons(x1).Moves(X).MPReq = 0
      Next X
    Close #43
    Exit Sub
  On Error GoTo 0
NotOk41:
  MsgBox Error$(Err) + " in weapon " + FileName, 16, "ChUB 2000 Weapon Error"
  Close #43
  Stop
End Sub

Sub LoadArena (ByVal FileName As String, ByVal X As Integer)
Dim CD As Integer
Dim X2 As Integer, Xx%
  On Error GoTo erLoad
  Open FileName For Input As #2
    Input #2, Arena(X).Name
    For CD = 1 To 10
      Input #2, Arena(X).Desc(CD)
    Next CD
    Input #2, Arena(X).AllAttacks
    For X2 = 1 To MaxEle
      Input #2, Arena(X).Effect(X2)
    Next X2
    Input #2, Arena(X).RestLowHP
    Input #2, Arena(X).RestHighHP
    Input #2, Xx
    Input #2, Xx
    Input #2, Arena(X).GradualHP
    Input #2, Xx
    For CD = 1 To 10
      Input #2, Arena(X).Event(CD).Name
      Input #2, Arena(X).Event(CD).Frequency
      Input #2, Arena(X).Event(CD).ElementStr
      Input #2, Arena(X).Event(CD).Hit
      Input #2, Arena(X).Event(CD).Miss
    Next CD
    On Error Resume Next
  Close #2
  Exit Sub
erLoad:
  kDlgBox Error$(Err) + " in arena " + FileName, 16, "Arena Error"
  Exit Sub
End Sub

