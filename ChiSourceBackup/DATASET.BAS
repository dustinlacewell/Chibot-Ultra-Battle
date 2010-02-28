Attribute VB_Name = "LoadDataset"
Option Explicit
Option Compare Text

Dim CurCh%, CurIt%, CurAr%, CurWpn%, CurMov%

Sub DupeTest()
Dim x1%, x2%, C$
  For x1 = 1 To NumSenshi
    For x2 = x1 + 1 To NumSenshi
      If UCase(Senshi(x1).PickMe) = UCase(Senshi(x2).PickMe) And Len(Senshi(x1).PickMe) > 0 Then
        C$ = Right$(Senshi(x2).PickMe, 1)
        If Asc(C$) >= 48 And Asc(C$) <= 57 Then
          C$ = Chr$(Asc(C$) + 1)
          If Asc(C$) > 57 Then C$ = "-0"
        Else
          C$ = "0"
        End If
        Senshi(x2).PickMe = Left$(Senshi(x2).PickMe, Len(Senshi(x2).PickMe) - 1) + C$
      End If
    Next x2
  Next x1
End Sub

Sub InitFromDisk(ByVal FileName As String)
Dim x%, x1%
Dim S$
Dim ST$, Ch$, It$, Ar$, Yma$
Dim Dunnit%
What:
  Dunnit = 0
  ReDim Senshi(1)
  ReDim Items(1)
  ReDim Arena(1)
  ReDim Moves(1)
  ReDim Weapons(1)
  Weapons(0).name = "No Weapon"
  DATASET.ClearChars = ">> Characters CLEARED <<"
  DATASET.CharsNotCleared = ">> Characters *NOT* Cleared <<"
  DATASET.GameAborted = ">>> Game aborted."
  DATASET.AcceptDefects = "%SN is now allowing defects."
  DATASET.DeclineDefects = "%SN is no longer allowing defects."
  DATASET.WantDraw = "%SN votes for a draw."
  DATASET.DontWantDraw = "%SN votes for no draw."
  DATASET.UnMorphMsg = "%SN reverts back to their original form as %S2."
  DATASET.GoCharge = "%SN continues to charge."
  DATASET.Counter = "~~ Ignore this Line ~~"
  DATASET.FleeFail = "%SN tries to escape, but slams against a strange barrier."
  DATASET.NoGetItem = "There's nothing to get, %SN, so knock it off."
  DATASET.HPDivert = "~~ %SN diverts %S2 HP to their Super Meter."
  'Dataset.HPDivert = "~~ This line has no effect ~~"
  'Dataset.NotEnoughMP = "Sorry, %SN, but %T requires %S2 MP, and you only have %Y."
  DATASET.NotEnoughMP = "~~ Ignore this Line ~~"
  DATASET.AllDead = ">>> Drawn Game by mutual annihilation"
  DATASET.Draw = ">>> Drawn Game by Agreement"
  DATASET.x1HrLeft = "You have one hour to go."
  DATASET.x30MinsLeft = "You now have 30 minutes."
  DATASET.x20MinsLeft = "You now have 20 minutes."
  DATASET.x10MinsLeft = "__+ 10 Minutes To Go +__"
  DATASET.x5MinsLeft = ">> 5 Minutes Remaining On The Clock <<"
  DATASET.x2MinsLeft = ">>> TWO-MINUTE WARNING! <<<"
  DATASET.SuddenDeath = "--== Now Entering SUDDEN DEATH, 1 Minute to Go ==--"
  DATASET.x1MinsLeft = "!! 1 MINUTE TO GO !!"
  DATASET.x30SecsLeft = "You only have 30 seconds left to kick their butts in!"
  DATASET.x15SecsLeft = "15 seconds remaining"
  DATASET.x5SecsLeft = "5 seconds to go"
  DATASET.TimeExpired = ">>> Oh my God! You ran out of time! You bastards!"
  DATASET.BeatYouma = ">>> You have won!!"
  DATASET.YouLose = ">>> Sorry. You Lose."
  DATASET.Respawn = "%SN respawns back into the fragfest."
  DATASET.BeginVote = "(!) Millenium Voting Start (!)"
  DATASET.CommieVote = "~~ This line has no effect ~~"
  DATASET.Remove = "%SN wusses out after seeing the competition."
  DATASET.Random = "%SN randomly selects %S2."
  DATASET.Taken = "Guess what, %SN? %S2 is already taken -- by %T!"
  DATASET.DefectSucc = "%SN successfully defects to '%S2' (Team %T)"
  DATASET.DefectFail = "%T is not accepting your defects, %SN."
  DATASET.FleeAttempt = "%SN turns around and starts running."
  DATASET.SuperKill = "===>>> %S2-HIT SUPER %Y! <<<==="
  CurMov% = 1
  'On Error GoTo erDataSet
  Open FileName For Input As #1
    Input #1, ST$
    If FileExists(ST$) Then
      kDlgBox "Sorry, old KUB datasets are no longer supported... Please read C2KDATA.TXT to see how to make a new dataset", 16, "ChUB Resurrection"
      End
    Else
      DATASET.LoadStr = ST$
      fChUBMain.sbChUBMain.SimpleText = "Loading dataset " + ST$
      Input #1, DATASET.BeginSelect
      Input #1, DATASET.EndSelect
      Input #1, DATASET.BattleBegin
      Input #1, DATASET.BattlePause
      Input #1, DATASET.BattleUnPause
      Input #1, DATASET.BattleEnd
      CurCh% = 1
      CurIt% = 1
      CurAr% = 1
      CurWpn% = 1
      MakeDefaultAna (CurAr%)
      Do
        Input #1, S$
        If (S$ = "[more]") Then
          Dunnit = -1
          Input #1, DATASET.ClearChars
          Input #1, DATASET.CharsNotCleared
          Input #1, DATASET.GameAborted
          Input #1, DATASET.AcceptDefects
          Input #1, DATASET.DeclineDefects
          Input #1, DATASET.WantDraw
          Input #1, DATASET.DontWantDraw
          Input #1, DATASET.UnMorphMsg
          Input #1, DATASET.GoCharge
          Input #1, DATASET.Counter
          Input #1, DATASET.FleeFail
          Input #1, DATASET.NoGetItem
          Input #1, DATASET.HPDivert
          Input #1, DATASET.NotEnoughMP
          Input #1, DATASET.AllDead
          Input #1, DATASET.Draw
          Input #1, DATASET.x1HrLeft
          Input #1, DATASET.x30MinsLeft
          Input #1, DATASET.x20MinsLeft
          Input #1, DATASET.x10MinsLeft
          Input #1, DATASET.x5MinsLeft
          Input #1, DATASET.x2MinsLeft
          Input #1, DATASET.SuddenDeath
          Input #1, DATASET.x1MinsLeft
          Input #1, DATASET.x30SecsLeft
          Input #1, DATASET.x15SecsLeft
          Input #1, DATASET.x5SecsLeft
          Input #1, DATASET.TimeExpired
          Input #1, DATASET.BeatYouma
          Input #1, DATASET.YouLose
          Input #1, DATASET.Respawn
          Input #1, DATASET.BeginVote
          Input #1, DATASET.CommieVote
          Input #1, DATASET.Remove
          Input #1, DATASET.Random
          Input #1, DATASET.Taken
          Input #1, DATASET.DefectSucc
          Input #1, DATASET.DefectFail
          Input #1, DATASET.FleeAttempt
          Input #1, DATASET.SuperKill
          Input #1, S$
        End If
        If UCase(S$) = "[DEATH]" Then GoTo DoDeath
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
          For x = 0 To fWhat!Huh.ListCount - 1
            LoadIt (fWhat!Huh.Path + "\" + fWhat!Huh.List(x))
          Next x
        Else
          LoadIt (S$)
        End If
      Loop Until EOF(1)
DoDeath:
      HighSenshi = CurCh% - 1
      MakeKamek (CurCh%)
      NumSenshi = CurCh%
      MaxItem = CurIt% - 1
      MaxArena = CurAr% - 1
      MaxWeapons = CurWpn% - 1
      MaxMoves_ = CurMov% - 1
      MaxDeath = 0
      MaxFatality = 0
      While Not EOF(1)
        Input #1, S$
        If UCase(S$) = "[FATALITY]" Then GoTo DoFatal
        MaxDeath = MaxDeath + 1
        ReDim Preserve Death(MaxDeath)
        Death(MaxDeath) = S$
      Wend
      If MaxDeath = 0 Then
        ReDim Death(7)
        MaxDeath = 7
        Death(1) = "Kyle says, ''Oh my God! They killed %SN! YOU BASTARDS!''"
        Death(2) = "Death slaps a tag on %SN reading ''KAPUT''."
        Death(3) = "%SN is quite dead."
        Death(4) = "%SN has been killed! Well R.I.P."
        Death(5) = "%SN is outta here!"
        Death(6) = "%SN is now living-impaired."
        Death(7) = "%SN has been defeated."
      End If
DoFatal:
      While Not EOF(1)
        Input #1, S$
        MaxFatality = MaxFatality + 1
        ReDim Preserve Fatality(MaxFatality)
        Fatality(MaxFatality) = S$
      Wend
      If MaxFatality = 0 Then
        MaxFatality = 1
        ReDim Fatality(1)
        Fatality(1) = ccBold + ccUnderline + ccColor + "04FATALITY!!!"
      End If
      Close #1
      Senshi(0).FullName = "(empty slot)"
      Unload fWhat
      On Error GoTo 0
      fChUBMain.sbChUBMain.SimpleText = "Dupe-testing " + DATASET.LoadStr
      ScrollSend1 ("Dupe-testing Dataset " + DATASET.LoadStr + "...")
      DupeTest
      ScrollSend1 ("Loaded Dataset " + DATASET.LoadStr + ": " + TrimStr(HighSenshi) + " characters (" + TrimStr(CurMov%) + " moves), " + TrimStr(MaxItem) + " items, " + TrimStr(MaxArena) + " arenas, " + TrimStr(MaxWeapons) + " weapons")
      fChUBMain.sbChUBMain.SimpleText = "Dataset " + DATASET.LoadStr + " loaded"
      Exit Sub
    End If
erDataSet:
  kDlgBox S$ + " " + Error$(Err) + " reading dataset init file " + FileName + ".", 16, "Initialize Failed"
  Stop
End Sub

Function LoadChar(ByVal FileName As String, ByVal x1 As Integer) As CharType
Dim x As Integer
Dim x2 As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim Rx As Integer
Dim Nu$, C$
Dim P1$, P2$, P4$, PO%, Msg$
Dim MovAvg%, NumMovs%
Dim Ch As CharType
  On Error GoTo NotOk11
    Open FileName For Input As #3
      Input #3, Ch.FullName
      C$ = "Encrypted ChUB Character"
      If Left$(Ch.FullName, Len(C$)) = "Encrypted ChUB Character" Then
        Close #3
        If x1 = 0 Then
          MsgBox "You cannot edit an encrypted character.", 16, "ChUB CharEdit"
        Else
          LoadChar = LoadEncryptedChar(FileName$)
        End If
        Exit Function
      End If
      Input #3, Ch.CharID
      On Error GoTo Whoops91
      Input #3, Rx
      Ch.weakness = Rx Mod 100
      Ch.Resist = (Rx - Ch.weakness) / 100
      If Ch.weakness = 0 Or Ch.Resist = 0 Then
        Ch.weakness = 0
        Ch.Resist = 0
      End If
      GoTo Whoops92
Whoops91:
      Ch.weakness = Shadow
      Ch.Resist = Shadow
      Resume Whoops92
Whoops92:
      On Error GoTo NotOk11
      Input #3, Ch.PickMe
      Input #3, Ch.SelectStr
      Input #3, Ch.SelectJoin
      Input #3, Ch.PhysStr
      Input #3, Ch.PhysDef
      Input #3, Ch.MagStr
      Input #3, Ch.MagDef
      If Ch.PhysStr + Ch.PhysDef + Ch.MagStr + Ch.MagDef > 220 Then
        Send FileName$ + " violates the 220 rule..."
        kDlgBox FileName$ + " violates the 220 rule... fix it!", 16, "Cheater!"
        End
      End If
      On Error GoTo Whoops93
      Input #3, Ch.BlockYes
      GoTo Whoops94
Whoops93:
      Ch.BlockYes = ""
      Resume Whoops94
Whoops94:
      On Error GoTo NotOk11
      If Ch.BlockYes = "0" Then Ch.BlockYes = ""
      'Input #3, X2
      Input #3, Ch.Rest
      Input #3, Ch.Block
      Input #3, Ch.BlockFail
      For x = 1 To 5
        Input #3, Ch.Taunt(x)
      Next x
      Input #3, Ch.Fatality.Cmdkey
      Input #3, Ch.Fatality.PreFatal
      Input #3, Ch.Fatality.FatalMove
      MovAvg = 0
      NumMovs = 0
      For x = 1 To 12
        Ch.Moves(x) = CurMov%
        Input #3, Moves(CurMov%).name
        Input #3, Moves(CurMov%).Cmdkey
        Input #3, Moves(CurMov%).CanSuper
        Input #3, Moves(CurMov%).Begin2Attack
        Input #3, Moves(CurMov%).Begin2SuperAttack
        Input #3, Moves(CurMov%).Begin2HealSelf
        Input #3, Moves(CurMov%).Hit
        Input #3, Moves(CurMov%).SuperHit
        Input #3, Rx
        Input #3, Moves(CurMov%).HealSelf
        Input #3, Moves(CurMov%).Crithit
        Input #3, Moves(CurMov%).HealMeld
        Input #3, Moves(CurMov%).Miss
        Input #3, Moves(CurMov%).Status(sMute)
        Input #3, Moves(CurMov%).Status(sChaos)
        Input #3, Moves(CurMov%).Status(sFreeze)
        Input #3, Moves(CurMov%).Status(sSleep)
        Input #3, Moves(CurMov%).Status(sPoison)
        Input #3, Moves(CurMov%).Status(sBlind)
        Input #3, Rx
        Input #3, Moves(CurMov%).Status(sHaste)
        Input #3, Rx
        Input #3, Rx
        Input #3, Moves(CurMov%).SuperMiss
        If Moves(CurMov%).SuperMiss = "0" Then Moves(CurMov%).SuperMiss = ""
        Input #3, Moves(CurMov%).Status(sSlow)
        Input #3, Moves(CurMov%).Status(sStun)
        Input #3, Moves(CurMov%).Status(sReraise)
        Input #3, Moves(CurMov%).Status(sRegen)
        Input #3, Moves(CurMov%).Status(sStop)
        Input #3, Moves(CurMov%).Status(sMushroom)
        Input #3, Moves(CurMov%).Status(sMIA)
        Input #3, Moves(CurMov%).Status(sQuick)
        Input #3, Moves(CurMov%).Status(sBerserk)
        Input #3, Moves(CurMov%).Status(sBarrier)
        Input #3, Moves(CurMov%).Status(sMBarrier)
        Input #3, Moves(CurMov%).Status(sCurse)
        Input #3, Moves(CurMov%).Status(sBless)
        Input #3, Moves(CurMov%).Status(sScarecrow)
        Input #3, Moves(CurMov%).Status(sCharm)
        Input #3, Moves(CurMov%).Status(sCPUWait)
        Input #3, Moves(CurMov%).Status(HitRate)
        If Moves(CurMov%).Status(HitRate) = 0 Then Moves(CurMov%).Status(HitRate) = 100
        Input #3, Moves(CurMov%).Element
        If Moves(CurMov%).Element = 10 Then Moves(CurMov%).Element = 66
        If Moves(CurMov%).Element = Poison Then Moves(CurMov%).Status(sPoison) = 100
        If Moves(CurMov%).Status(sScarecrow) <> 0 And Moves(CurMov%).Status(sMute) <> 0 Then
          Send FileName$ + " has a move that does Scarecrow and Mute in the same move. This is illegal."
          kDlgBox FileName$ + " has a move that does Scarecrow and Mute in the same move. This is illegal.", 16, "Cheater!"
          End
        End If
        Input #3, Moves(CurMov%).ElementStr
        'Moves(CurMov%).ElementStr = Moves(CurMov%).ElementStr
        Input #3, Moves(CurMov%).Target
        Moves(CurMov%).MPReq = CalcMP(CurMov%)
        If Moves(CurMov).Cmdkey <> "" Then
          CurMov = CurMov + 1
        Else
          Ch.Moves(x) = 0
        End If
        ReDim Preserve Moves(CurMov%)
      Next x
      On Error Resume Next
      For x = 1 To 5
        Input #3, Ch.DeathStr(x)
      Next x
      For x = 1 To 5
        Input #3, Ch.KillStr(x)
      Next x
      Input #3, Ch.Desc(1)
      Input #3, Ch.Desc(2)
      Input #3, Ch.Desc(3)
      Input #3, Ch.Desc(4)
      P1$ = Ch.FullName
      P2$ = "Wins"
      P4$ = String$(6, 32)
      'PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
      P4$ = GetSetting("ChUB Resurrection W/L", P2$, P1$, "0")
      Ch.Wins = Val(P4$)
      P2$ = "Losses"
      P4$ = String$(6, 32)
      'PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
      P4$ = GetSetting("ChUB Resurrection W/L", P2$, P1$, "0")
      Ch.Losses = Val(P4$)
    Close #3
    LoadChar = Ch
    Exit Function
  On Error GoTo 0
NotOk11:
  ScrollSend1 (Error$(Err) + " in character " + FileName)
  kDlgBox Error$(Err) + " in character " + FileName, 16, "Character Error"
  Close #3
  Exit Function
End Function

Function LoadEncryptedChar(ByVal FileName$) As CharType
Dim x As Integer
Dim x2 As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim Rx%, C$, P1$, P2$, P4$, PO%
Dim Ch As CharType
  Open FileName$ For Binary Access Read Lock Read Write As #84
    C$ = String$(24, 0)
    'For x = 1 To Len("Encrypted ChUB Character")
      Get #84, , C$
    'Next x
    Get #84, , Ch.PhysStr
    Get #84, , Ch.PhysDef
    Get #84, , Ch.MagStr
    Get #84, , Ch.MagDef
    If Ch.PhysStr + Ch.PhysDef + Ch.MagStr + Ch.MagDef > 220 Then
      kDlgBox FileName$ + " violates the 220 rule... fix it!", 16, "Cheater!"
      End
    End If
    Get #84, , Ch.weakness
    Get #84, , Ch.Resist
    Ch.FullName = GetStr(84)
    Ch.CharID = GetStr(84)
    Ch.PickMe = GetStr(84)
    Ch.SelectStr = GetStr(84)
    Ch.SelectJoin = GetStr(84)
    Ch.BlockYes = GetStr(84)
    Ch.Rest = GetStr(84)
    Ch.Block = GetStr(84)
    Ch.BlockFail = GetStr(84)
    Ch.Fatality.Cmdkey = GetStr(84)
    Ch.Fatality.PreFatal = GetStr(84)
    Ch.Fatality.FatalMove = GetStr(84)
    For x = 1 To 5
      Ch.Taunt(x) = GetStr(84)
    Next x
    For x = 1 To 5
      Ch.DeathStr(x) = GetStr(84)
    Next x
    For x = 1 To 5
      Ch.KillStr(x) = GetStr(84)
    Next x
    For x = 1 To 4
      Ch.Desc(x) = GetStr(84)
    Next x
    For x = 1 To MaxMoves
      Ch.Moves(x) = CurMov%
      Get #84, , Moves(CurMov%).CanSuper
      Get #84, , Moves(CurMov%).Element
      Get #84, , Moves(CurMov%).ElementStr
      'Moves(CurMov%).ElementStr = Moves(CurMov%).ElementStr
      Get #84, , Moves(CurMov%).Target
      Get #84, , Moves(CurMov%).Status(sMute)
      Get #84, , Moves(CurMov%).Status(sMute)
      Get #84, , Moves(CurMov%).Status(sChaos)
      Get #84, , Moves(CurMov%).Status(sFreeze)
      Get #84, , Moves(CurMov%).Status(sSleep)
      Get #84, , Moves(CurMov%).Status(sPoison)
      Get #84, , Moves(CurMov%).Status(sBlind)
      Get #84, , Moves(CurMov%).Status(sHaste)
      Get #84, , Moves(CurMov%).Status(sSlow)
      Get #84, , Moves(CurMov%).Status(sStun)
      Get #84, , Moves(CurMov%).Status(sReraise)
      Get #84, , Moves(CurMov%).Status(sRegen)
      Get #84, , Moves(CurMov%).Status(sStop)
      Get #84, , Moves(CurMov%).Status(sMushroom)
      Get #84, , Moves(CurMov%).Status(sMIA)
      Get #84, , Moves(CurMov%).Status(sQuick)
      Get #84, , Moves(CurMov%).Status(sBerserk)
      Get #84, , Moves(CurMov%).Status(sBarrier)
      Get #84, , Moves(CurMov%).Status(sMBarrier)
      Get #84, , Moves(CurMov%).Status(sCurse)
      Get #84, , Moves(CurMov%).Status(sBless)
      Moves(CurMov%).name = GetStr(84)
      If (Moves(CurMov%).name = "morestat") Then
        Get #84, , Moves(CurMov%).Status(sScarecrow)
        Get #84, , Moves(CurMov%).Status(sCharm)
        Get #84, , Moves(CurMov%).Status(sCPUWait)
        Get #84, , Moves(CurMov%).Status(HitRate)
        If Moves(CurMov%).Status(HitRate) = 0 Then Moves(CurMov%).Status(HitRate) = 100
        Moves(CurMov%).name = GetStr(84)
      End If
      Moves(CurMov%).Cmdkey = GetStr(84)
      Moves(CurMov%).Begin2Attack = GetStr(84)
      Moves(CurMov%).Begin2SuperAttack = GetStr(84)
      Moves(CurMov%).Begin2HealSelf = GetStr(84)
      Moves(CurMov%).Hit = GetStr(84)
      Moves(CurMov%).SuperHit = GetStr(84)
      Moves(CurMov%).HealSelf = GetStr(84)
      Moves(CurMov%).Crithit = GetStr(84)
      Moves(CurMov%).HealMeld = GetStr(84)
      Moves(CurMov%).Miss = GetStr(84)
      Moves(CurMov%).SuperMiss = GetStr(84)
      If Moves(CurMov%).Element = 10 Then Moves(CurMov%).Element = 66
      If Moves(CurMov%).Element = Poison Then Moves(CurMov%).Status(sPoison) = 100
      If Moves(CurMov).Cmdkey <> "" Then
        CurMov = CurMov + 1
      Else
        Ch.Moves(x) = 0
      End If
      ReDim Preserve Moves(CurMov%)
    Next x
    P1$ = Ch.FullName
    P2$ = "Wins"
    P4$ = String$(6, 32)
    'PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
    P4$ = GetSetting("ChUB Resurrection", P2$, P1$, "0")
    Ch.Wins = Val(P4$)
    P2$ = "Losses"
    P4$ = String$(6, 32)
    'PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
    P4$ = GetSetting("ChUB Resurrection", P2$, P1$, "0")
    Ch.Losses = Val(P4$)
  Close #84
  LoadEncryptedChar = Ch
End Function

Sub LoadIt(ByVal S$)
  fWhat!Files.AddItem (S$)
  DoEvents
        Select Case FilExt(S$)
          Case "CH2", "CHE":
            ReDim Preserve Senshi(CurCh%)
            Senshi(CurCh%) = LoadChar(S$, -1)
            CurCh% = CurCh% + 1
          Case "CHR":
            kDlgBox "KUB FiNAL .CHR files are no longer supported: " + S$, 16, "ChUB Resurrection"
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
            kDlgBox "ChUB Talisman .WPN files are not supported (" + S$ + ") If you have ChUB Resurrection .WPN files, please change the extension to .W2K.", 16, "ChUB Resurrection"
          Case "W2K":
            ReDim Preserve Weapons(CurWpn%)
            LoadWeapon S$, CurWpn%
            CurWpn% = CurWpn% + 1
          Case Else:
            'kDlgBox "Invalid filename: " + S$, 16, "ChUB"
        End Select
End Sub

Sub LoadItem(ByVal FileName As String, ByVal x1 As Integer)
Dim Rx%
  On Error GoTo eLoadItem
  Open FileName For Input As #5
    Input #5, Items(x1).name
    Input #5, Items(x1).Spawn
    Input #5, Items(x1).Telefrag
    Input #5, Items(x1).PlayerGet
    Input #5, Items(x1).YoumaGet
    Input #5, Items(x1).PlayerHP
    Input #5, Rx
    Input #5, Items(x1).PlayerStat(sMute)
    Input #5, Items(x1).PlayerStat(sChaos)
    Input #5, Items(x1).PlayerStat(sFreeze)
    Input #5, Items(x1).PlayerStat(sSleep)
    Input #5, Items(x1).PlayerStat(sPoison)
    Input #5, Items(x1).PlayerStat(sBlind)
    Input #5, Items(x1).PlayerStat(sCurse)
    Input #5, Items(x1).PlayerStat(sHaste)
    Input #5, Items(x1).PlayerStat(sBarrier)
    Input #5, Items(x1).PlayerStat(sBless)
    Input #5, Items(x1).PlayerStat(sSlow)
    Input #5, Items(x1).PlayerStat(sStun)
    Input #5, Rx
    Input #5, Rx
    Input #5, Items(x1).PlayerStat(sStop)
    Input #5, Items(x1).PlayerStat(sMBarrier)
    Input #5, Items(x1).PlayerStat(sReraise)
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

Sub LoadNewArena(ByVal FileName As String, ByVal x As Integer)
Dim Cd As Integer
Dim x2 As Integer, XX%
  On Error GoTo erLoad2
  Open FileName For Input As #2
    Input #2, Arena(x).name
    For Cd = 1 To 10
      Input #2, Arena(x).Desc(Cd)
    Next Cd
    On Error GoTo erTooManyDescLines2
    Input #2, Arena(x).AllAttacks
    On Error GoTo erOther2
    For x2 = 1 To 30
      Input #2, Arena(x).Effect(x2)
    Next x2
    Arena(x).Effect(Poison) = Arena(x).Effect(2)
    Arena(x).Effect(Grass) = Arena(x).Effect(3)
    Arena(x).Effect(Rock) = Arena(x).Effect(4)
    Arena(x).Effect(Dirt) = Arena(x).Effect(5)
    Arena(x).Effect(Psychic) = Arena(x).Effect(6)
    Arena(x).Effect(Ghost) = Arena(x).Effect(7)
    Arena(x).Effect(Demi) = Arena(x).Effect(8)
    Input #2, Arena(x).RestLowHP
    Input #2, Arena(x).RestHighHP
    Input #2, XX
    Input #2, XX
    Input #2, Arena(x).GradualHP
    Input #2, XX
    On Error GoTo erEvents2
    For Cd = 1 To 10
      Input #2, Arena(x).Happening(Cd).name
      Input #2, Arena(x).Happening(Cd).Frequency
      Input #2, Arena(x).Happening(Cd).ElementStr
      Input #2, Arena(x).Happening(Cd).Hit
      Input #2, Arena(x).Happening(Cd).Miss
      Input #2, Arena(x).Happening(Cd).HitsAll
    Next Cd
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

Sub LoadWeapon(ByVal FileName$, ByVal x1%)
Dim x As Integer
Dim x2 As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim Rx As Integer
  On Error GoTo NotOk41
    Open FileName For Input As #43
      Input #43, Weapons(x1).name
      Input #43, Weapons(x1).PickMe
      Input #43, Weapons(x1).SelectStr
      Input #43, Weapons(x1).Desc(1)
      Input #43, Weapons(x1).Desc(2)
      Input #43, Weapons(x1).NumUses
      For x = 1 To 5
        Input #43, Weapons(x1).Moves(x).name
        Input #43, Weapons(x1).Moves(x).Cmdkey
        Input #43, Weapons(x1).Moves(x).CanSuper
        Input #43, Weapons(x1).Moves(x).Begin2Attack
        Input #43, Weapons(x1).Moves(x).Begin2SuperAttack
        Input #43, Weapons(x1).Moves(x).Begin2HealSelf
        Input #43, Weapons(x1).Moves(x).Hit
        Input #43, Weapons(x1).Moves(x).SuperHit
        Input #43, Weapons(x1).Moves(x).DestroyWeapon
        Input #43, Weapons(x1).Moves(x).HealSelf
        Input #43, Weapons(x1).Moves(x).Crithit
        Input #43, Weapons(x1).Moves(x).HealMeld
        Input #43, Weapons(x1).Moves(x).Miss
        Input #43, Weapons(x1).Moves(x).Status(sMute)
        Input #43, Weapons(x1).Moves(x).Status(sChaos)
        Input #43, Weapons(x1).Moves(x).Status(sFreeze)
        Input #43, Weapons(x1).Moves(x).Status(sSleep)
        Input #43, Weapons(x1).Moves(x).Status(sPoison)
        Input #43, Weapons(x1).Moves(x).Status(sBlind)
        Input #43, Weapons(x1).Moves(x).InstantHit
        Input #43, Weapons(x1).Moves(x).Status(sHaste)
        Input #43, Weapons(x1).Moves(x).ReqAllUses
        Input #43, x2
        Input #43, Weapons(x1).Moves(x).SuperMiss
        If Weapons(x1).Moves(x).SuperMiss = "0" Then Weapons(x1).Moves(x).SuperMiss = ""
        Input #43, Weapons(x1).Moves(x).Status(sSlow)
        Input #43, Weapons(x1).Moves(x).Status(sStun)
        Input #43, Weapons(x1).Moves(x).Status(sReraise)
        Input #43, Weapons(x1).Moves(x).Status(sRegen)
        Input #43, Weapons(x1).Moves(x).Status(sStop)
        Input #43, Weapons(x1).Moves(x).Status(sMushroom)
        Input #43, Weapons(x1).Moves(x).Status(sMIA)
        Input #43, Weapons(x1).Moves(x).Status(sQuick)
        Input #43, Weapons(x1).Moves(x).Status(sBerserk)
        Input #43, Weapons(x1).Moves(x).Status(sBarrier)
        Input #43, Weapons(x1).Moves(x).Status(sMBarrier)
        Input #43, Weapons(x1).Moves(x).Status(sCurse)
        Input #43, Weapons(x1).Moves(x).Status(sBless)
        Input #43, Rx
        Input #43, Rx
        Input #43, Rx
        Input #43, Weapons(x1).Moves(x).Status(HitRate)
        If Weapons(x1).Moves(x).Status(HitRate) = 0 Then Weapons(x1).Moves(x).Status(HitRate) = 100
        Input #43, Weapons(x1).Moves(x).Element
        Input #43, Weapons(x1).Moves(x).ElementStr
        Input #43, Weapons(x1).Moves(x).Target
        Weapons(x1).Moves(x).MPReq = 0
      Next x
    Close #43
    Exit Sub
  On Error GoTo 0
NotOk41:
  MsgBox Error$(Err) + " in weapon " + FileName, 16, "ChUB Resurrection Weapon Error"
  Close #43
  Stop
End Sub

Sub LoadArena(ByVal FileName As String, ByVal x As Integer)
Dim Cd As Integer
Dim x2 As Integer, XX%
  On Error GoTo erLoad
  Open FileName For Input As #2
    Input #2, Arena(x).name
    For Cd = 1 To 10
      Input #2, Arena(x).Desc(Cd)
    Next Cd
    Input #2, Arena(x).AllAttacks
    For x2 = 1 To MaxEle
      Input #2, Arena(x).Effect(x2)
    Next x2
    Input #2, Arena(x).RestLowHP
    Input #2, Arena(x).RestHighHP
    Input #2, XX
    Input #2, XX
    Input #2, Arena(x).GradualHP
    Input #2, XX
    For Cd = 1 To 10
      Input #2, Arena(x).Happening(Cd).name
      Input #2, Arena(x).Happening(Cd).Frequency
      Input #2, Arena(x).Happening(Cd).ElementStr
      Input #2, Arena(x).Happening(Cd).Hit
      Input #2, Arena(x).Happening(Cd).Miss
    Next Cd
    On Error Resume Next
  Close #2
  Exit Sub
erLoad:
  kDlgBox Error$(Err) + " in arena " + FileName, 16, "Arena Error"
  Exit Sub
End Sub

Sub MakeKamek(ByVal KID%)
Dim x%
  KamekID = KID%
  ReDim Preserve Senshi(KID)
  Senshi(KamekID).Block = "%SN raises his magic wand and creates a shield."
  Senshi(KamekID).BlockFail = "%SN's shield shatters!"
  Senshi(KamekID).BlockYes = "%T's attack bounces off %SN's shield."
  Senshi(KamekID).CharID = "You Can't Be(at) Kamek"
  Senshi(KamekID).DeathStr(1) = "Impossible... The great %SN has fallen!"
  Senshi(KamekID).Fatality.Cmdkey = "kenny"
  Senshi(KamekID).Fatality.PreFatal = "%SN gestures and %T turns into Kenny!"
  Senshi(KamekID).Fatality.FatalMove = "%T is shot, hung, decapitated, microwaved, axed, and smashed into a wall by The Fonz."
  Senshi(KamekID).FullName = "Kamek"
  Senshi(KamekID).KillStr(1) = "%SN laughs and asks, ""Who's next?"""
  Senshi(KamekID).KillStr(2) = "%SN sighs. ""Too bad... Next!"""
  Senshi(KamekID).KillStr(3) = "%SN gets pissed off and shouts, ""What?! You can't be dead already! I barely touched you!"""
  Senshi(KamekID).KillStr(4) = "%SN jumps up and down. ""YES!! THEY'RE DEAD THEY'RE DEAD %T IS DEAD!!"""
  Senshi(KamekID).Losses = 0
  Senshi(KamekID).Wins = 666
  Senshi(KamekID).MagDef = 150
  Senshi(KamekID).MagStr = 150
  Senshi(KamekID).PhysStr = 100
  Senshi(KamekID).PhysDef = 0
  Senshi(KamekID).Resist = Shadow
  Senshi(KamekID).Rest = "%SN sits down and regenerates."
  Senshi(KamekID).SelectJoin = "%SN is now Kamek?"
  Senshi(KamekID).PickMe = "~magikoopa?"
  Senshi(KamekID).SelectStr = "%SN teleports in as Kamek!"
  Senshi(KamekID).Taunt(1) = "%SN laughs, ""Can't you do any better?"""
  Senshi(KamekID).Taunt(2) = "%SN grumbles, ""Why'd I have to get stuck with a bunch of weak losers?"""
  Senshi(KamekID).Taunt(3) = "%SN says, ""Well, I'm waiting! Can't you hit me?"""
  Senshi(KamekID).weakness = -4
  YoshiID = KID + 1
  ReDim Preserve Senshi(KID + 1)
  NumSenshi = NumSenshi + 1
  Senshi(YoshiID).Block = "%SN spits an egg at %T."
  Senshi(YoshiID).BlockFail = "%T laughs as an egg hits them."
  Senshi(YoshiID).BlockYes = "%SN's egg hits %T, disorienting them."
  Senshi(YoshiID).CharID = "You Can't Be Yoshi, Either!"
  Senshi(YoshiID).DeathStr(0) = "Yoshi has been defeated! Now Kamek will surely win..."
  Senshi(YoshiID).Fatality.Cmdkey = "swallow"
  Senshi(YoshiID).Fatality.PreFatal = "Yoshi sticks out his tongue at %T."
  Senshi(YoshiID).Fatality.FatalMove = "Yoshi swallows up %T. Yum yum!"
  Senshi(YoshiID).FullName = "Yoshi"
  Senshi(YoshiID).PickMe = "~yoshi?"
  Senshi(YoshiID).SelectStr = "%SN is now Yoshi?"
  Senshi(YoshiID).SelectJoin = "%SN joins in as Yoshi, hero of Yoshi's Island!"
  Senshi(YoshiID).MagDef = 80
  Senshi(YoshiID).MagStr = 80
  Senshi(YoshiID).PhysStr = 80
  Senshi(YoshiID).PhysDef = 80
  Senshi(YoshiID).Resist = 0
  Senshi(YoshiID).weakness = 0
  Senshi(YoshiID).Rest = "%SN eats some berries off the Super Happy Tree."
  For x = 1 To MaxMoves
    If KM(x).Cmdkey <> "" Then
      Moves(CurMov) = KM(x)
      Senshi(KamekID).Moves(x) = CurMov
      CurMov = CurMov + 1
      ReDim Preserve Moves(CurMov%)
    End If
    If YM(x).Cmdkey <> "" Then
      Moves(CurMov) = YM(x)
      Senshi(YoshiID).Moves(x) = CurMov
      CurMov = CurMov + 1
      ReDim Preserve Moves(CurMov%)
    End If
  Next x
End Sub

Sub MakeDefaultAna(ByVal An%)
Dim x%
  Arena(An).AllAttacks = 1
  Arena(An).Desc(1) = "There is nothing but grass as far as the eye can see. This arena is"
  Arena(An).Desc(2) = "perfect for fighting. There are no distractions in sight, and all"
  Arena(An).Desc(3) = "elements are well-balanced here."
  Arena(An).GradualHP = 0
  Arena(An).Happening(1).Frequency = 2
  Arena(An).Happening(1).Miss = "The sun beats high overhead."
  Arena(An).Happening(2).Frequency = 2
  Arena(An).Happening(2).Miss = "A solitary cloud can be seen in the sky."
  Arena(An).Happening(3).Frequency = 2
  Arena(An).Happening(3).Miss = "A bird flies by overhead."
  Arena(An).name = "Grassy Field"
  Arena(An).RestHighHP = 10
  Arena(An).RestLowHP = 3
  Arena(An).AllAttacks = 1
  For x = 1 To MaxEle
    Arena(An).Effect(x) = 1
  Next x
End Sub
