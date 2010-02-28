' CHARS.BAS
' Defines Arenas, Group Attacks, and some of the Characters.

Option Explicit

Sub InitVars ()
' Initializes variables, loads the Youma and Config files, and calls the
' subs to init the Characters.
Dim X, x2 As Integer
Dim Tw%, S$
Dim P1$, P2$, P4$, PO%, Msg$
  Config.Arena = 1
  CurArena = 1
  P1$ = "Millenium"
  P2$ = "Join"
  P4$ = String$(1, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "1", P4$, 5, "ChUB2000.ini")
  If P4$ <> "0" Then
    Config.NoJoin = 0
  Else
    Config.NoJoin = 1
  End If
  P2$ = "SameChar"
  P4$ = String$(1, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "1", P4$, 5, "ChUB2000.ini")
  If P4$ <> "0" Then
    Config.SameChar = 1
  Else
    Config.SameChar = 0
  End If
  'P2$ = "InfMP"
  'P4$ = String$(1, 32)
  'PO% = GetPrivateProfileString(P1$, P2$, "1", P4$, 5, "ChUB2000.ini")
  'If P4$ <> "0" Then
  '  Config.InfMP = 1
  'Else
  '  Config.InfMP = 0
  'End If
  P2$ = "Flag"
  P4$ = String$(1, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
  If P4$ <> "0" Then
    Config.Flag = 1
  Else
    Config.Flag = 0
  End If
  P2$ = "Fours"
  P4$ = String$(1, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
  If P4$ <> "0" Then
    Config.Fours = 1
  Else
    Config.Fours = 0
  End If
  P2$ = "Respawn"
  P4$ = String$(1, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
  If P4$ <> "0" Then
    Config.Respawn = 1
  Else
    Config.Respawn = 0
  End If
  P2$ = "Defect"
  P4$ = String$(1, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
  If P4$ <> "0" Then
    Config.Defect = 1
  Else
    Config.Defect = 0
  End If
  P2$ = "Zombie"
  P4$ = String$(1, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 5, "ChUB2000.ini")
  If P4$ <> "0" Then
    ZombieM = 1
  Else
    ZombieM = 0
  End If
  P2$ = "Multitarget"
  P4$ = String$(1, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "1", P4$, 5, "ChUB2000.ini")
  If P4$ <> "0" Then
    Config.Multi = 1
  Else
    Config.Multi = 0
  End If
  P2$ = "OldSchool"
  P4$ = String$(1, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 81, "ChUB2000.ini")
  If P4$ <> "0" Then
    Config.OldSchool = 1
  Else
    Config.OldSchool = 0
  End If
  P2$ = "FontName"
  P4$ = String$(50, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "Arial", P4$, 51, "ChUB2000.ini")
  Config.FontName = Trim(P4$)
  P2$ = "FontColor"
  P4$ = String$(6, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "000000", P4$, 7, "ChUB2000.ini")
  Config.FontColor = Trim(P4$)
  P2$ = "WhyNoJoin"
  P4$ = String$(80, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "none", P4$, 81, "ChUB2000.ini")
  Config.Reason = Trim(P4$)
  P2$ = "FlaCon"
  P4$ = String$(6, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "60", P4$, 81, "ChUB2000.ini")
  Config.FlaCon = Val(P4$)
  Config.WLog = ""
  P2$ = "DMult"
  P4$ = String$(6, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "100", P4$, 81, "ChUB2000.ini")
  DMult = Val(P4$)
  P2$ = "MaxHP"
  P4$ = String$(6, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "600", P4$, 81, "ChUB2000.ini")
  MaxHP = Val(P4$)
  P2$ = "MaxSP"
  P4$ = String$(6, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "1000", P4$, 81, "ChUB2000.ini")
  MaxSP = Val(P4$)
  P2$ = "GetRate"
  P4$ = String$(6, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "1000", P4$, 81, "ChUB2000.ini")
  GetRate = Val(P4$)
  P2$ = "FragLimit"
  P4$ = String$(6, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 81, "ChUB2000.ini")
  FragLimit = Val(P4$)
  P2$ = "GameNum"
  P4$ = String$(6, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "0", P4$, 81, "ChUB2000.ini")
  GameNum = Val(P4$)
  P2$ = "MaxPlayers"
  P4$ = String$(6, 32)
  PO% = GetPrivateProfileString(P1$, P2$, "10", P4$, 81, "ChUB2000.ini")
  MaxPlayers = Val(P4$)
  On Error GoTo eTwit
  MaxTwit = 0
  MaxBackTwit = 0
  Open "TWIT.TXT" For Input As #1
  Tw% = 1
  Do
    Do
      Input #1, S$
    Loop Until Mid$(S$, 1, 1) <> ";" Or EOF(1)
    If Left$(S$, 1) <> ";" Then
      ReDim Preserve Twit(Tw%)
      Twit(Tw%) = S$
      Tw% = Tw% + 1
    End If
  Loop Until EOF(1)
  MaxTwit = Tw% - 1
  Close #1
  GoTo ContinueOn
eTwit:
  Close #1
  Resume ContinueOn
ContinueOn:
  On Error GoTo NoBackTwit
  Open "BACKTWIT.TXT" For Input As #33
  Tw% = 1
  Msg = ""
  Do
    Do
      Input #33, S$
    Loop Until Left$(S$, 1) <> ";" Or EOF(33)
    If Left$(S$, 1) <> ";" Then
      ReDim Preserve BackTwit(Tw%)
      BackTwit(Tw%) = S$
      If Len(Msg + S$ + " ") > 130 Then
        Send Msg
        Msg = ""
      End If
      Msg = Msg + S$ + " "
      Tw = Tw + 1
    End If
  Loop Until EOF(33)
  MaxBackTwit = Tw - 1
  If MaxBackTwit > 0 Then
    Send ("BackTwit.TXT found! Only these people will be allowed to play:")
    Send Msg
  End If
  GoTo NextCont
NoBackTwit:
  Resume NextCont
NextCont:
  Close #33
  Lang = -1
  'For X = 2 To MaxYouma
  '  y(X).Enabled = 0
  'Next X
  'LoadYouma
  X = Len(Config)
  On Error GoTo 0
  CurArena = 1
  GetRate = 100
  On Error GoTo 0' We don't need error-trapping anymore!
  ReDim Preserve Senshi(0)
  Senshi(0).FullName = "(Empty Slot)"
  ReDim P(MaxPlayers)
  ReDim Vote(MaxPlayers)
  For X = 1 To MaxPlayers
    P(X).ScrNam = ""
    P(X).TeamID = "S"
    P(X).Goodwill = 50
    P(X).Greed = 50
    P(X).Wrath = 50
    P(X).Arrogance = 50
    'p(x).Darkness = 50
    'p(X).Smart = 1
  Next X
  InitXMoves
End Sub

Sub InitXMoves ()
Dim X%
  Langolier.Name = "The Langoliers."
  Langolier.Desc(1) = "= Langoliers Arena based on Steven King novella ="
  Langolier.Desc(2) = "You are one of few survivors of a terrible"
  Langolier.Desc(3) = "mishap. Your plane has crash-landed in the past."
  Langolier.Desc(4) = "This world is dying, and the keepers of time,"
  Langolier.Desc(5) = "the langoliers, will soon come to devour the past,"
  Langolier.Desc(6) = "and anything in it... including you."
  Langolier.Desc(7) = "You have 10 minutes... As time passes your attacks"
  Langolier.Desc(8) = "will gradually lose power, and resting will prove"
  Langolier.Desc(9) = "less refreshing. When time is up, the langoliers will"
  Langolier.Desc(10) = "have their main course... YOU!!"
  Langolier.RestLowHP = 35
  Langolier.RestHighHP = 50
  'Langolier.RestLowMP = 35
  'Langolier.RestHighMP = 50
  'Langolier.GradualMP = 1
  Langolier.AllAttacks = 1
  For X = 1 To MaxEle
    Langolier.Effect(X) = 1
  Next X
  InitHid
  'hmoves(1).cmdkey = "freebie"
  'hmoves(1).Name = "Freebie Attack"
  'hmoves(1).begin2attack = "%SN starts to do a freebie attack on %T."
  'hmoves(1).hit = "%SN hits %T!"
  'hmoves(1).crithit = "%SN massacres %T!"
  'hmoves(1).miss = "%SN misses %T."
  'hmoves(1).element = phys
  'hmoves(1).elementstr = 45
End Sub

