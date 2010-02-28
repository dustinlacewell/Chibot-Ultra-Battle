Option Explicit

Sub CounterAttack (ByVal d!, A!)
Dim x4!, x6!, x9!, q!, Okay!
  If (p(d).Target <> 0) Then
    x6 = p(d).Target
    x9 = p(d).Moves(x6).Target
    If (x9 = Friend Or x9 = AllFriend Or x9 = OnlySelf) Then
      p(d).Target = d
    Else
      p(d).Target = A
    End If
    If p(d).Moves(x6).Element <> Phys And p(d).Status(sMute) Then Exit Sub
    p(d).MoveStart = XTimer - 999
    p(d).CurMove = x6
    If p(d).Rune <> RuneBlock Then
      p(d).Super = p(d).Super - 100
    Else
      For x4 = 1 To MaxPlayers
	If p(x4).CurMove > 0 And p(x4).CurMove <= MaxMoves And p(x4).HP > 0 And p(x4).CharID <> 0 Then
	  q = p(x4).Moves(p(x4).CurMove).Target
	  If (p(x4).Target = A) And (q <> 1) And (q <> 3) And (q <> 8) And (p(x4).Rune <> RuneStealth) And p(x4).Rune = RuneMagic Then
	    Send (p(x4).ScrNam + "'s Rune of Magic prevents " + p(A).ScrNam + " from getting a free counterattack.")
	    If p(d).Super >= 100 Then
	      p(d).Super = p(d).Super - 100
	      Okay = True
	    Else
	      Send (p(d).ScrNam + " was unable to counterattack due to lack of SP.")
	      Okay = False
	    End If
	  End If
	End If
      Next x4
    End If
    TSP = TSP + 100
    If p(d).Status(sHamedo) Then
      Check2MoveDmg d, -1
      p(d).Status(sHamedo) = 0
    End If
  End If
End Sub

Function DamageCount (ByVal x2!, ByVal x6!, M As MoveType, s!) As Long
Dim Df As Integer
Dim St As Integer
Dim y2 As Integer, Scro!, C!
Dim y3!, Barrier!, MBarrier!
  If M.Element = Phys Then
    Df = p(x6).PhysDef
    St = p(x2).PhysStr
  Else
    Df = p(x6).MagDef
    St = p(x2).MagStr
  End If
  If p(x6).Rune <> RuneMagic Then
    Scro = p(x6).Scroller
  Else
    Scro = 0
  End If
  If p(x2).Rune = RuneHigh Then St = St + (30 * Scro)
  If p(x2).Rune <> RuneMagic Then
    Barrier = p(x6).Status(sBarrier)
    MBarrier = p(x6).Status(sMBarrier)
  End If
  If p(x2).Status(sBlind) Then St = 0
  On Error GoTo Whoops
  'y2 = Int(25 * M.ElementStr * Log(St + 1) / (Df + 10)) + Rand(-25, 25)
  y2 = Int((M.ElementStr * St / 45) / (.01 * (Df + 50))) + Rand(-MaxHP * .05, MaxHP * .05)
  'If p(x2).Rune = RuneCharge And p(x2).Charging = True Then
  '  y2 = Int(y2 * (XTimer - p(x2).RuneTemp) / 13)
  'End If
  y2 = Int(DMult / 100 * y2)
  If p(x2).Rune = RuneDesp And p(x6).Rune <> RuneMagic Then
    y2 = Int(y2 * (p(x2).MaxHP * .5 / p(x2).HP))
  End If
  If p(x2).Rune = RuneWonder And p(x6).Rune <> RuneMagic Then
    Select Case Rand(1, 4)
      Case 1: y2 = Rand((MaxHP / 4), (MaxHP * 3 / 4))
      Case 2: y2 = Rand(1, 30)
      Case 3: y2 = Rand(Int(MaxHP / 2), MaxHP)
      Case 4: y2 = Rand(MaxHP / 6, MaxHP / 2)
    End Select
  End If
  If Rand(1, 200) <= Df Then y2 = 0
  y2 = ProcessDmg(y2, M.Element)
  If Barrier > 0 And M.Element = Phys And p(x2).Rune <> RuneMagic Then
    y3 = Int(y2 / 2)
    If C = 0 Then
      p(x6).Status(sBarrier) = p(x6).Status(sBarrier) - Int(y3 / 2)
    End If
    y2 = y2 - y3
  End If
  If MBarrier > 0 And M.Element <> Phys And M.Element <> Heal And p(x2).Rune <> RuneMagic Then
    y3 = Int(y2 / 2)
    If C = 0 Then
      p(x6).Status(sMBarrier) = p(x6).Status(sMBarrier) - Int(y3 / 2)
    End If
    y2 = y2 - y3
  End If
  If p(x2).Status(sBless) Then y2 = Int(y2 * 1.25)
  If p(x2).Status(sCurse) Then y2 = Int(y2 / 1.25)
  If p(x6).Status(sZombie) Then y2 = 0
  'If p(x2).ScrNam = "KamEkSaLLy" Then Stop
  If Senshi(p(x6).CharID).weakness <> Senshi(p(x6).CharID).Resist And p(x2).CurMove <> 0 Then
    If Senshi(p(x6).CharID).weakness = M.Element Then
      Send (p(x2).ScrNam + "'s attack is very effective against " + p(x6).ScrNam + "!")
      y2 = Int(y2 * Rand(150, 300) / 100)
    End If
    If Senshi(p(x6).CharID).Resist = M.Element Then
      Send (p(x2).ScrNam + "'s attack isn't very effective against " + p(x6).ScrNam + "...")
      y2 = Int(y2 * Rand(25, 80) / 100)
    End If
  End If
  If y2 < 0 Then y2 = 0
  If p(x6).Rune = RuneArmor And p(x6).RuneTemp = M.Element Then
    If p(x2).Rune = RuneMagic Then
      Send (p(x2).ScrNam + "'s Rune of Magic bypasses " + p(x6).ScrNam + "'s " + EName(p(x6).RuneTemp) + " armor.")
    Else
      y2 = -y2
    End If
  End If
  DamageCount = y2
  Exit Function
Whoops:
  ScrollSend ("Whoops! " + p(x2).ScrNam + "'s character is too cheap!")
  'kDlgBox "Whoops! You have an ultra-cheap character!", 16, "Overflow"
  'Unload fMidi
  'End
  y2 = -500
  DamageCount = y2
  Exit Function
End Function

Sub LearnMove (ByVal x2!, M As MoveType)
Dim X!
  If p(x2).Rune = RuneLearn And p(x2).CurMove <> 14 Then
    p(x2).Moves(14) = M
    p(x2).Moves(14).Cmdkey = "!" + M.Cmdkey
    Send (p(x2).ScrNam + " learned the move they were attacked with, " + M.Name + ".")
    Send (p(x2).ScrNam + " can use this move by typing /!" + M.Cmdkey)
  End If
End Sub

Function PredictDamageCount (ByVal x2!, ByVal x6!, M As MoveType, s!) As Long
Dim Df As Integer
Dim St As Integer
Dim y2 As Integer, Scro!, C!
Dim y3!, Barrier!, MBarrier!
  If M.Element = Phys Then
    Df = p(x6).PhysDef
    St = p(x2).PhysStr
  Else
    Df = p(x6).MagDef
    St = p(x2).MagStr
  End If
  'If p(x6).Rune <> RuneMagic Then
  '  Scro = p(x6).Scroller
  'Else
    Scro = 0
  'End If
  If p(x2).Rune = RuneHigh Then St = St + (30 * Scro)
  If p(x2).Rune <> RuneMagic Then
    Barrier = p(x6).Status(sBarrier)
    MBarrier = p(x6).Status(sMBarrier)
  End If
  If p(x2).Status(sBlind) Then St = 0
  On Error GoTo Whoops5
  'y2 = Int(25 * M.ElementStr * Log(St + 1) / (Df + 10)) + Rand(-25, 25)
  y2 = Int((M.ElementStr * St / 45) / (.01 * (Df + 50))) + Rand(-MaxHP * .05, MaxHP * .05)
  'If p(x2).Rune = RuneCharge And p(x2).Charging = True Then
  '  y2 = Int(y2 * (XTimer - p(x2).RuneTemp) / 13)
  'End If
  y2 = Int(DMult / 100 * y2)
  If p(x2).Rune = RuneDesp And p(x6).Rune <> RuneMagic Then
    y2 = Int(y2 * (p(x2).MaxHP * .5 / p(x2).HP))
  End If
  If p(x2).Rune = RuneWonder And p(x6).Rune <> RuneMagic Then
    Select Case Rand(1, 4)
      Case 1: y2 = Rand((MaxHP / 4), (MaxHP * 3 / 4))
      Case 2: y2 = Rand(1, 30)
      Case 3: y2 = Rand(Int(MaxHP / 2), MaxHP)
      Case 4: y2 = Rand(MaxHP / 6, MaxHP / 2)
    End Select
  End If
  If Rand(1, 200) <= Df Then y2 = 0
  y2 = ProcessDmg(y2, M.Element)
  If Barrier > 0 And M.Element = Phys And p(x2).Rune <> RuneMagic Then
    y3 = Int(y2 / 2)
    If C = 0 Then
      p(x6).Status(sBarrier) = p(x6).Status(sBarrier) - Int(y3 / 2)
    End If
    y2 = y2 - y3
  End If
  If MBarrier > 0 And M.Element <> Phys And M.Element <> Heal And p(x2).Rune <> RuneMagic Then
    y3 = Int(y2 / 2)
    If C = 0 Then
      p(x6).Status(sMBarrier) = p(x6).Status(sMBarrier) - Int(y3 / 2)
    End If
    y2 = y2 - y3
  End If
  If p(x2).Status(sBless) Then y2 = Int(y2 * 1.25)
  If p(x2).Status(sCurse) Then y2 = Int(y2 / 1.25)
  If p(x6).Status(sZombie) Then y2 = 0
  If Senshi(p(x6).CharID).weakness <> Senshi(p(x6).CharID).Resist And p(x2).CurMove <> 0 Then
    If Senshi(p(x6).CharID).weakness = M.Element Then
      'If M.Target = Enemy Then Send (p(x2).ScrNam + "'s attack is very effective against " + p(x6).ScrNam + "!")
      y2 = Int(y2 * Rand(150, 300) / 100)
    End If
    If Senshi(p(x6).CharID).Resist = M.Element Then
      'If M.Target = Enemy Then Send (p(x2).ScrNam + "'s attack isn't very effective against " + p(x6).ScrNam + "...")
      y2 = Int(y2 * Rand(25, 80) / 100)
    End If
  End If
  If y2 < 0 Then y2 = 0
  If p(x6).Rune = RuneArmor And p(x6).RuneTemp = M.Element Then
    If p(x2).Rune = RuneMagic Then
      'Send (p(x2).ScrNam + "'s Rune of Magic bypasses " + p(x6).ScrNam + "'s " + EName(p(x6).RuneTemp) + " armor.")
    Else
      y2 = -y2
    End If
  End If
  PredictDamageCount = y2
  Exit Function
Whoops5:
  'ScrollSend ("Whoops! " + p(x2).ScrNam + "'s character is too cheap!")
  'kDlgBox "Whoops! You have an ultra-cheap character!", 16, "Overflow"
  'Unload fMidi
  'End
  y2 = -500
  PredictDamageCount = y2
  Exit Function
End Function

Function ProcessDmg (ByVal y2!, ByVal e!) As Long
Dim x1 As Long
Dim Tix As Long
  Tix = (XTimer - TimeLimit)
  x1 = Int(y2 * 2 / 3)
  x1 = Int(x1 * Arena(CurArena).AllAttacks)
  If e <= MaxEle And e > 0 Then x1 = Int(x1 * Arena(CurArena).Effect(e))
  If (TLimit - Tix) <= 60 And (TLimit > 0) Then x1 = x1 * 2.5
  ProcessDmg = x1
End Function

Sub StealSub (ByVal x1!, ByVal x2!)
Dim x9!
  If x1 = x2 Then Exit Sub
  If Rand(1, 4) = 3 Then
    If p(x2).Rune = RuneMagic Then
      Send (p(x1).ScrNam + " was unable to steal from " + p(x2).ScrNam + ".")
    ElseIf p(x2).Status(sLife3) = True And p(x1).Status(sLife3) = 0 Then
      p(x2).Status(sLife3) = 0
      p(x1).Status(sLife3) = True
      Send (p(x1).ScrNam + " steals " + p(x2).ScrNam + "'s fairy of life!")
    ElseIf p(x2).Status(sRegen) <> 0 And p(x1).Status(sRegen) = 0 Then
      p(x2).Status(sRegen) = 0
      p(x1).Status(sRegen) = True
      Send (p(x1).ScrNam + " steals regeneration from " + p(x2).ScrNam + "!")
    ElseIf p(x2).Status(sBless) <> 0 And p(x1).Status(sBless) = 0 Then
      p(x2).Status(sBless) = 0
      p(x1).Status(sBless) = True
      Send (p(x1).ScrNam + " steals " + p(x2).ScrNam + "'s blessing! They needed that for cheezing!")
    ElseIf (p(x2).Status(sBarrier) <> 0 Or p(x2).Status(sMBarrier) <> 0) Then
      p(x1).Status(sBarrier) = p(x1).Status(sBarrier) + p(x2).Status(sBarrier)
      p(x2).Status(sBarrier) = 0
      p(x1).Status(sMBarrier) = p(x1).Status(sMBarrier) + p(x2).Status(sMBarrier)
      p(x2).Status(sMBarrier) = 0
      Send (p(x1).ScrNam + " pickpockets all of " + p(x2).ScrNam + "'s barriers away!")
    ElseIf p(x2).Super > 0 And p(x1).Super < MaxSP Then
      x9 = Rand(1, Int(p(x2).Super / 2))
      p(x1).Super = p(x1).Super + x9
      p(x2).Super = p(x2).Super - x9
      Send (p(x1).ScrNam + " stealthily takes " + TrimStr(x9) + " Super Points from " + p(x2).ScrNam + "!")
    ElseIf p(x1).HP < MaxHP Then
      x9 = Rand(1, Int(p(x2).HP / 2))
      p(x1).HP = p(x1).HP + x9
      p(x2).HP = p(x2).HP - x9
      Send (p(x1).ScrNam + " stole " + TrimStr(x9) + " health from " + p(x2).ScrNam + "!")
    Else
      Send (p(x1).ScrNam + " was unable to steal from " + p(x2).ScrNam + ".")
    End If
  End If
End Sub

Function SuperDmgCt (ByVal x2!, ByVal x6!, M As MoveType, SuperHits!) As Long
Dim Df As Integer
Dim St As Integer
Dim y2 As Double, Scro!, C!
Dim y3!, Barrier!, MBarrier!
  If M.Element = Phys Then
    Df = p(x6).PhysDef
    St = p(x2).PhysStr
  Else
    Df = p(x6).MagDef
    St = p(x2).MagStr
  End If
  Scro = p(x6).Scroller
  If p(x2).Rune = RuneHigh Then St = St + (30 * Scro)
  If p(x2).Rune <> RuneMagic Then
    Barrier = p(x6).Status(sBarrier)
    MBarrier = p(x6).Status(sMBarrier)
  End If
  If p(x2).Status(sBlind) Then St = 0
  On Error GoTo Whoops3
  y2 = Int(25 * M.ElementStr * Log(St + 1) / (Df + 10)) + Rand(-25, 25)
  y2 = Int((M.ElementStr * St / 45) / (.01 * (Df + 50))) + Rand(-MaxHP * .05, MaxHP * .05)
  Select Case p(x2).SuperNum
    Case 1: SuperHits = Rand(200, 200)
    Case 2: SuperHits = Rand(200, 250)
    Case 3: SuperHits = Rand(250, 300)
    Case 4: SuperHits = Rand(300, 400)
    Case 5: SuperHits = Rand(300, 500)
    Case 6: SuperHits = Rand(300, 600)
  End Select
  y2 = Int(y2 / 100 * SuperHits)
  y2 = Int(DMult / 100 * y2)
  y2 = ProcessDmg(y2, M.Element)
  If Barrier > 0 And M.Element = Phys And p(x2).Rune <> RuneMagic Then
    y3 = Int(y2 / 2)
    If C = 0 Then
      p(x6).Status(sBarrier) = p(x6).Status(sBarrier) - Int(y3 / 2)
    End If
    y2 = y2 - y3
  End If
  If MBarrier > 0 And M.Element <> Phys And M.Element <> Heal And p(x2).Rune <> RuneMagic Then
    y3 = Int(y2 / 2)
    If C = 0 Then
      p(x6).Status(sMBarrier) = p(x6).Status(sMBarrier) - Int(y3 / 2)
    End If
    y2 = y2 - y3
  End If
  If (x2 <= MaxPlayers) And (x2 >= 0) Then
    If p(x2).Status(sBless) Then y2 = Int(y2 * 1.25)
    If p(x2).Status(sCurse) Then y2 = Int(y2 / 1.25)
  End If
  If (x6 <= MaxPlayers) And (x6 >= 0) Then
    If p(x6).Status(sZombie) Then y2 = 0
  End If
  If Senshi(p(x6).CharID).weakness <> Senshi(p(x6).CharID).Resist And p(x2).CurMove <> 0 Then
    If Senshi(p(x6).CharID).weakness = M.Element Then
      Send (p(x2).ScrNam + "'s attack is very effective against " + p(x6).ScrNam + "!")
      y2 = Int(y2 * Rand(150, 300) / 100)
    End If
    If Senshi(p(x6).CharID).Resist = M.Element Then
      Send (p(x2).ScrNam + "'s attack isn't very effective against " + p(x6).ScrNam + "...")
      y2 = Int(y2 * Rand(25, 80) / 100)
    End If
  End If
  If y2 < 0 Then y2 = 0
  If p(x6).Rune = RuneArmor And p(x6).RuneTemp = M.Element Then
    y2 = -y2
  End If
  SuperDmgCt = y2
  Exit Function
Whoops3:
  ScrollSend ("Whoops! " + p(x2).ScrNam + "'s character is too cheap!")
  y2 = -500
  SuperDmgCt = y2
  Exit Function
End Function

Sub XRunes (ByVal x2!, ByVal x6!)
' XRunes (Attacker, Defender)
  If Rand(1, 8) = 2 Then
    If p(x6).Rune = RuneIce Then
      If p(x2).Rune <> RuneMagic And p(x2).Rune <> RuneLuck Then
	Send (p(x2).ScrNam + " is frozen after coming into contact with " + p(x6).ScrNam + "!")
	p(x2).Status(sFreeze) = XTimer
      Else
	Send (p(x2).ScrNam + " receives a blast of cold, but their Rune keeps them from being frozen.")
      End If
    End If
    If p(x6).Rune = RunePikachu Then
      If p(x2).Rune <> RuneMagic And p(x2).Rune <> RuneLuck Then
	Send (p(x2).ScrNam + " receives a burst of electricity after coming into contact with " + p(x6).ScrNam + "!")
	Send (p(x2).ScrNam + " is stunned!")
	p(x2).Status(sStun) = XTimer
      Else
	Send (p(x2).ScrNam + " receives an electrostatic charge, but their Rune won't let them get stunned.")
      End If
    End If
  End If
  If Rand(1, 5) = 3 Then
    If p(x6).Rune = RuneLearn And x6 <> x2 Then
      If p(x2).CurMove > 0 And p(x2).CurMove <= MaxMoves Then
	If p(x2).Rune = RuneMagic Then
	  Send (p(x6).ScrNam + " tried to learn " + p(x2).ScrNam + "'s move, but failed.")
	Else
	  LearnMove x6, p(x2).Moves(p(x2).CurMove)
	End If
      End If
    End If
  End If
End Sub

Sub Blossom (ByVal Dead!, ByVal Killer!)
Dim X!, T!
  If p(Dead).Rune = RuneDeath Then
    X = Int(Rand(150, 500) * DMult / 100)
    If (Dead = Killer) Then
      Send ("Death blossoms rise out of " + p(Dead).ScrNam + ".")
      Send ("They quickly realize that " + p(Dead).ScrNam + " committed suicide and float away without attacking anybody.")
      Exit Sub
    End If
    If p(Killer).Rune = RuneMagic Then
      Send ("Death blossoms rise out of " + p(Dead).ScrNam + "'s body and move toward " + p(Killer).ScrNam + ".")
      Send ("A force field creased by the Rune of Magic prevents the blossoms from getting near. They implode harmlessly.")
      Exit Sub
    End If
    If p(Killer).HP <= 0 Then
      Send ("Death blossoms rise out of " + p(Dead).ScrNam + "'s body and float over to " + p(Killer).ScrNam + "'s dead body.")
      T = GetTarget(p(Dead).TeamID)
      Send ("The death blossoms can't attack a corpse -- They become confused and attack " + p(T).ScrNam + " instead! [" + TrimStr(X) + "]")
    Else
      Send ("Death blossoms rise out of " + p(Dead).ScrNam + "'s body and attack " + p(Killer).ScrNam + "! [" + TrimStr(X) + "]")
      T = Killer
    End If
    p(T).HP = p(T).HP - X
    If p(T).HP <= 0 Then
      p(Dead).Frags = p(Dead).Frags + 1
      IncWins (Dead)
      SayKillStr Dead, p(T).ScrNam
      Blossom T, Dead
    End If
  End If
End Sub

Sub Check2MoveDmg (ByVal x2!, ByVal X7!)
Dim Okay As Integer
Dim x1!, x3!, x4!, x5!, x6!, x8!, x9!, x0!
Dim Temp!, CM!, HP!, BlockYes!
Dim y2!, y3!, y9!, Df!, St!
Dim S1$, S2$, S3$, S4$, S5$, S6$, S7$, S8$, S9$, S0$, SN$
Dim Msg$, T$, SNMiss$, SNHit$, SNCrit$
ReDim s(sMaxStatus) As Integer
Dim Sx As CharType
Dim S1x As CharType
Dim Px As PlayerType
Dim M As MoveType
Dim Sng1!, Sng2!, Sng3!
Dim Avg!, Count!

  On Error Resume Next
  Px = p(x2)
  x1 = Px.Target
  Sx = Senshi(p(x2).CharID)
  CM = p(x2).CurMove
  SN = p(x2).ScrNam
  DoEvents
  Select Case x1
    Case 1 To MaxPlayers: T = p(x1).ScrNam
    'Case MaxPlayers + 1 To MaxYouma + MaxPlayers: t = y(x1 - MaxPlayers).Name
  End Select
  If (CM = pFatal) Then
    'If x1 > MaxPlayers Then
    '  HP = y(x1 - MaxPlayers).HP
    '  S1 = y(x1 - MaxPlayers).Name
    'Else
      HP = p(x1).HP
      S1 = p(x1).ScrNam
    'End If
    If (HP > (MaxHP / 6)) Or (HP <= 0) Then
      If (HP > (MaxHP / 6)) Then Send (p(x2).ScrNam + " cannot do a fatality on " + S1 + " right now.")
      If (HP <= 0) Then Send (p(x2).ScrNam + " finds it difficult to do a fatality on someone who is already dead.")
      p(x2).CurMove = -255
      Exit Sub
    End If
  End If
  DoEvents
  If (CM <= MaxMoves) And (CM >= 1) Then
    M = p(x2).Moves(CM)
    If (M.Element <> Morph) Then
      x9 = p(x2).Target
      If x9 > MaxPlayers Then
	x8 = 0
      Else
	x8 = p(x9).Status(sMIA)
      End If
      If x8 And (M.Target = Friend Or M.Target = Enemy) Then
	Send ("But " + ScrNam(x9) + " seems to be missing, " + p(x2).ScrNam + "!")
	'p(x2).T(x9) = 0
	p(x2).CurMove = 0
	Exit Sub
      End If
      DoEvents
      If HPx(x9) <= 0 And M.Element <> Life And (M.Target = Friend Or M.Target = Enemy) Then
	If Not DoneKilled(x9) Then ShowStatus (x9)
	Send ("But " + ScrNam(x9) + " is dead, " + p(x2).ScrNam + "!")
	'p(x2).T(x9) = 0
	p(x2).CurMove = 0
	Exit Sub
      End If
      If Active(x9) = 0 And (M.Target = Friend Or M.Target = Enemy) Then
	Send ("But " + ScrNam(x9) + " doesn't exist, " + p(x2).ScrNam + ".")
	'p(x2).T(x9) = 0
	p(x2).CurMove = 0
	Exit Sub
      End If
    End If
  End If
  DoEvents
  If X7 Then
    Select Case CM
      Case pBlock:
	p(x2).CurMove = -255
      Case pTaunt:
	p(x2).CurMove = -255
      Case pRest:
	p(x2).CurMove = -255
	p(x2).HP = p(x2).HP + Rand(Arena(CurArena).RestLowHP, Arena(CurArena).RestHighHP)
	If p(x2).HP > p(x2).MaxHP Then p(x2).HP = p(x2).MaxHP
	p(x2).Cheese = Int(p(x2).Cheese - 400 * DMult / 100)
	If p(x2).Cheese < 0 Then p(x2).Cheese = 0
      Case -50:
	If (p(x2).Status(sStun) = 0) And (p(x2).Status(sFreeze) = 0) And (p(x2).Status(sSleep) = 0) Then p(x2).CurMove = -255
      Case pFatal:
	Send (Parse(Sx.Fatality.FatalMove, SN, "", T, ""))
	p(x1).HP = -9999
	p(x1).AttackedMe = x2
	Blossom x1, x2
	ShowStatus (x1)
	DoEvents
	If (x1 <= MaxPlayers) Then SayKillStr x2, p(x1).ScrNam
	p(x2).CurMove = -255
	p(x2).FatalFrags = p(x2).FatalFrags + 1
	p(x2).Frags = p(x2).Frags + 1
	IncWins (x2)
	TFatal = TFatal + 1
      Case pFlee:
	Send (p(x2).ScrNam + " quickly flees from the fight. Everyone point and call them a coward!")
	p(x2).CurMove = 0
	p(x2).MoveStart = 0
	p(x2).HP = -777
      Case 1 To MaxMoves:
	x4 = p(x2).CharID
	x6 = p(x2).Target
	If x6 <= MaxPlayers Then
	  If M.Target <> Friend And M.Target <> OnlySelf And M.Target <> AllFriend And Rand(1, 5) = 3 And p(x6).Rune = RuneJedi Then
	    If p(x2).Rune = RuneMagic Then
	      Send (p(x6).ScrNam + " tries Jedi Mind Control on " + p(x2).ScrNam + ", but it fails.")
	    Else
	      Send (p(x6).ScrNam + " says to " + p(x2).ScrNam + ", ""I'm not the one you want to attack.""")
	      p(x2).Target = GetTarget(p(x2).TeamID)
	      x6 = p(x2).Target
	      Send (p(x2).ScrNam + " turns toward " + ScrNam(x6) + ".")
	    End If
	  End If
	End If
	If x6 <= MaxPlayers Then
	  If M.Target <> Friend And M.Target <> OnlySelf And M.Target <> AllFriend And Rand(1, 5) = 3 And p(x6).Rune = RuneReflect And x6 <> x2 Then
	    If p(x2).Rune = RuneMagic Then
	      Send (p(x6).ScrNam + "'s Rune reflects the move back to " + p(x2).ScrNam + "...")
	      Send ("... but " + p(x2).ScrNam + "'s Rune of Magic reflects it right back!")
	    Else
	      Send (p(x6).ScrNam + "'s Rune reflects the move back to " + p(x2).ScrNam + "!")
	      p(x2).Target = x2
	      x6 = p(x2).Target
	    End If
	  End If
	End If
	If p(x2).Moves(p(x2).CurMove).Element <> Morph Then
	  If TeamID(x1) <> TeamID(x2) And x1 <= MaxPlayers And M.Element <> Morph Then p(x1).AttackedMe = x2
	End If
	M = p(x2).Moves(CM)
	DoEvents
	If (p(x2).CurMove <> -255) Then
	  If M.Target = 2 Or M.Target = 4 Or M.Target = 5 Or M.Target = 6 Or M.Target = 7 Then
	    For x0 = 1 To MaxPlayers
	      If IsThisATarget(x2, x0) And p(x0).CurMove = pBlock And p(x0).Status(sHamedo) Then
		CounterAttack x0, x2
	      End If
	    Next x0
	  End If
	  If p(x2).Status(sStop) Or p(x2).Status(sMIA) Or p(x2).Status(sStun) Or p(x2).Status(sFreeze) Or p(x2).Status(sSleep) Or p(x2).Status(sMushroom) Or (p(x2).Status(sMute) And M.Element <> Phys) Or p(x2).HP <= 0 Then Exit Sub
	  Select Case M.Element
	   Case Invin:
	     Send (Parse(M.Hit, SN, "", T, ""))
	     For x0 = 1 To MaxPlayers
	       DoEvents
	       If p(x0).Status(sInvin) = x2 Then
		 p(x0).Status(sInvin) = False
		 CheckStatus M.Status(), x0, x2, 0
		 XRunes x2, x0
	       End If
	     Next x0
	     'p(x2).CurMove = -255
	   Case Heal:
	     For x0 = 1 To MaxPlayers
	       DoEvents
	       If IsThisATarget(x2, x0) Then
		 Call CheckStatus(M.Status(), x0, x2, 0)
		 y2 = Int((M.ElementStr + Int(p(x2).MagStr / 4)) / 2) + Rand(-10, 20)
		 y2 = Int(y2 * 100 / DMult)
		 If p(x0).Status(sZombie) Then
		   Send ("The cure harms the zombie!")
		   p(x0).HP = p(x0).HP - y2
		   Count = Count + 1
		   Avg = Avg + y2
		 Else
		   If p(x0).HP + y2 > p(x0).MaxHP Then y2 = p(x0).MaxHP - p(x0).HP
		   If (y2 >= 0) Then
		     Count = Count + 1
		     Avg = Avg + y2
		     p(x0).HP = p(x0).HP + y2
		   End If
		 End If
		 XRunes x2, x0
	       End If
	     Next x0
	     On Error Resume Next
	     Avg = Int(Avg / Count)
	     If (M.Target <> AllFriend) And (p(x2).Target = x2) Then
	       Send (Parse(M.HealSelf + " [" + TrimStr(Avg) + "]", SN, "", SN, ""))
	     Else
	       Send (Parse(M.Hit + " [" + TrimStr(Avg) + "]", SN, "", T, ""))
	     End If
	     'p(x2).CurMove = -255
	   Case Poison, Grass, Rock, Dirt, Psychic, Ghost, Phys, MoonE, Shadow, Water, Fire, Lit, Heart, Earth, Wind, Ki, Lum:
	     If M.Element = Poison Then M.Status(sPoison) = 100
	     If (p(x2).SuperNum <> 0) Then
	       y2 = SuperDmgCt(x2, x1, M, y3)
	       If x1 <= MaxPlayers Then
		 If p(x1).Rune = RuneEvade And Rand(1, 100) <= 15 Then
		   Send (p(x1).ScrNam + " teleports out of the way and laughs!")
		   If p(x2).Rune = RuneMagic Then
		     Send ("But " + p(x2).ScrNam + "'s Rune of Magic automatically redirects the attack!")
		   Else
		     y2 = 0
		   End If
		 End If
	       End If
	       If (y2 = 0) Then
		 If M.SuperMiss <> "" Then
		   Send (Parse(M.SuperMiss, SN, "", T, ""))
		 Else
		   Send (Parse(M.Miss, SN, "", T, ""))
		 End If
	       Else
		 Playwav ("superhit")
		 DoEvents
		 If p(x1).CurMove = pBlock Then y2 = Int(y2 / 2)
		 If p(x1).Status(sInvin) Then
		   If p(x2).Rune = RuneMagic Then
		     Send (p(x2).ScrNam + "'s Rune of Magic ignores " + p(x1).ScrNam + "'s shield!")
		   Else
		     y2 = Int(y2 / 3)
		   End If
		 End If
		 Send (Parse(M.SuperHit + " [" + TrimStr(y2) + "]", SN, TrimStr(y3), T, ""))
		 DoEvents
		 p(x1).HP = p(x1).HP - y2
		 If p(x1).CurMove <> pBlock And p(x1).Status(sInvin) = 0 Then Call CheckStatus(M.Status(), x1, x2, 1)
		 If y2 > 0 Then
		   If p(x1).Rune = RuneSuper And p(x2).Rune <> RuneMagic Then
		     p(x1).Super = p(x1).Super + Abs(Int(y2))
		   Else
		     p(x1).Super = p(x1).Super + Abs(Int(y2 * 2 / 3))
		   End If
		 End If
		 THP = THP + y2
		 If p(x1).HP <= 0 Then
		   Send (Parse(Dataset.SuperKill, p(x2).ScrNam, TrimStr(y3), p(x1).ScrNam, UCase(M.Name)))
		   p(x2).Frags = p(x2).Frags + 1
		   IncWins (x2)
		   SayKillStr x2, p(x1).ScrNam
		   Blossom x1, x2
		   If p(x2).Rune = RuneFatal And p(x1).HP > -(MaxHP / 6) And p(x1).Rune <> RuneMagic Then
		     p(x1).HP = 35
		     Send (p(x2).ScrNam + " moves in for the kill!!!")
		     p(x2).CurMove = pFatal
		     p(x2).MoveStart = -999
		     Check2MoveDmg x2, 1
		   End If
		 End If
		 If (p(x1).Status(sSleep) Or p(x1).Status(sStun)) And M.Element = Phys Then
		   RemoveStatus x1, sSleep
		   RemoveStatus x1, sStun
		   p(x1).CurMove = -255
		   Send (p(x1).ScrNam + " is jolted awake!")
		 End If
	       End If
	       XRunes x2, x1
	       If x1 <= MaxPlayers Then
		 If p(x1).Rune = RuneTheft And p(x1).HP > 0 Then
		   StealSub x1, x2
		 End If
	       End If
	       p(x2).Super = p(x2).Super - (p(x2).SuperNum * 100)
	       TSP = TSP + (p(x2).SuperNum * 100)
	       'p(x2).CurMove = -255
	     Else
	       If MultiHit(M.Target) Then
		 If M.HealSelf = "" Then
		   M.HealSelf = M.Begin2HealSelf
		   M.Begin2HealSelf = ""
		 End If
		 Send (Parse(M.Begin2HealSelf, SN, "", T, ""))
		 SNMiss = ""
		 SNHit = ""
		 SNCrit = ""
	       End If
	       For x0 = 1 To MaxPlayers
		 DoEvents
		 Okay = IsThisATarget(x2, x0)
		 If Okay Then
		   DoEvents
		   y2 = DamageCount(x2, x0, M, -1)
		   If p(x0).Rune = RuneEvade And Rand(1, 100) <= 25 And M.Target <> Friend And M.Target <> OnlySelf And M.Target <> AllFriend Then
		     Send (p(x0).ScrNam + " teleports out of the way and laughs!")
		     If p(x2).Rune = RuneMagic Then
		       Send ("But " + p(x2).ScrNam + "'s Rune of Magic automatically redirects the attack!")
		     Else
		       y2 = 0
		     End If
		   End If
		   BlockYes = 0
		   If p(x0).CurMove = pBlock Then
		     y3 = Rand(1, 3)
		     If (y3 = 1) Then
		       If Senshi(p(x0).CharID).BlockYes <> "" Then
			 BlockYes = True
			 Send (Parse(Senshi(p(x0).CharID).BlockYes, p(x0).ScrNam, "", SN, ""))
		       End If
		       y2 = 0
		     ElseIf (y3 = 2) Or (y3 = 3) Then
		       y2 = Int(y2 / 2)
		     Else
		       Send (Parse(Senshi(p(x0).CharID).BlockFail, p(x0).ScrNam, "", SN, ""))
		     End If
		     DoEvents
		   End If
		   If p(x0).Status(sInvin) Then
		     y3 = Rand(1, 5)
		     If (y3 = 1) Then
		       Send (p(x0).ScrNam + "'s shield collapses!")
		       p(x0).Status(sInvin) = 0
		     ElseIf p(x2).Rune = RuneMagic Then
		       Send (p(x2).ScrNam + "'s Rune of Magic ignores " + p(x0).ScrNam + "'s shield!")
		     Else
		       y2 = 0
		     End If
		   End If
		   DoEvents
		   If (y2 <> 0 Or p(x0).Status(sZombie) <> 0) Then
		     Select Case y2
		       Case Is > 0: Msg = " [" + TrimStr(y2) + "]"
		       Case 0: Msg = " [miss]"
		       Case Is < 0: Msg = " [Absorbed " + TrimStr(Abs(y2)) + "]"
		     End Select
		     If (Rand(1, 20) = 10) Or (Rand(1, 5) = 3 And p(x2).Rune = RuneCrit And p(x0).Rune <> RuneMagic) Then
		       y2 = y2 * 2
		       If (M.CritHit <> "") Then
			 If MultiHit(M.Target) Then
			   SNCrit = SNCrit + p(x0).ScrNam + Msg + ", "
			 Else
			   Msg = Parse(M.CritHit, SN, "", p(x0).ScrNam, "") + Msg
			   Send Msg
			 End If
		       ElseIf M.Hit <> "" Then
			 If MultiHit(M.Target) Then
			   SNHit = SNHit + p(x0).ScrNam + Msg + ", "
			 Else
			   Msg = Parse(M.Hit, SN, "", p(x0).ScrNam, "") + Msg
			   Send Msg
			 End If
		       End If
		     ElseIf M.Hit <> "" Then
		       If MultiHit(M.Target) Then
			 SNHit = SNHit + p(x0).ScrNam + Msg + ", "
		       Else
			 Msg = Parse(M.Hit, SN, "", p(x0).ScrNam, "") + Msg
			 Send Msg
		       End If
		     End If
		     DoEvents
		     DoEvents
		     If p(x0).HP - y2 <= 0 Then
		       p(x2).Frags = p(x2).Frags + 1
		       IncWins (x2)
		       SayKillStr x2, p(x0).ScrNam
		       Blossom x0, x2
		       If p(x2).Rune = RuneFatal And p(x0).HP > -(MaxHP / 6) And p(x0).Rune <> RuneMagic Then
			 p(x0).HP = 35
			 Send (p(x2).ScrNam + " moves in for the kill!!!")
			 p(x2).CurMove = pFatal
			 p(x2).MoveStart = -999
			 Check2MoveDmg x2, 1
		       End If
		     End If
		     Avg = Avg + y2
		     Count = Count + 1
		     p(x0).HP = p(x0).HP - y2
		     THP = THP + y2
		     If p(x0).Rune = RuneSuper And p(x2).Rune <> RuneMagic Then
		       p(x0).Super = p(x0).Super + Abs(Int(y2))
		     Else
		       p(x0).Super = p(x0).Super + Abs(Int(y2 * 2 / 3))
		     End If
		     If (y2 = 0) Then
		       If p(x0).Rune = RuneSuper And p(x2).Rune <> RuneMagic Then
			 p(x0).Super = p(x0).Super + Abs(Int(M.ElementStr / 1.5))
		       Else
			 p(x0).Super = p(x0).Super + Abs(Int(M.ElementStr / 3))
		       End If
		     End If
		     If p(x0).Super > MaxSP Then p(x0).Super = MaxSP
		     If (y2 > 0) And p(x0).CurMove <> pBlock Then
		       If p(x2).Rune = RuneMagic Then
			 p(x0).God = -666
		       End If
		       Call CheckStatus(M.Status(), x0, x2, 0)
		     End If
		     DoEvents
		   ElseIf BlockYes = 0 Then
		     If MultiHit(M.Target) Then
		       SNMiss = SNMiss + p(x0).ScrNam + ", "
		     Else
		       Send (Parse(M.Miss, SN, "", p(x0).ScrNam, ""))
		     End If
		   End If
		   If (p(x0).Status(sSleep) Or p(x0).Status(sStun)) And M.Element = Phys And M.Status(sSleep) = 0 And M.Status(sStun) = 0 Then
		     RemoveStatus x0, sSleep
		     RemoveStatus x0, sStun
		     p(x0).CurMove = -255
		     Send (p(x0).ScrNam + " is jolted awake!")
		   End If
		   XRunes x2, x0
		   If x0 <= MaxPlayers Then
		     If p(x0).Rune = RuneTheft And p(x0).HP > 0 Then
		       StealSub x0, x2
		     End If
		   End If
		 End If
	       Next x0
	       On Error Resume Next
	       x5 = Avg
	       If Count = 0 Then
		 Avg = 0
	       Else
		 Avg = Int(Avg / Count)
	       End If
	       On Error GoTo 0
	       If MultiHit(M.Target) Then
		 If SNHit <> "" Then
		   SNHit = MakeEnglish(SNHit)
		   Send (Parse(M.Hit + " [" + TrimStr(Avg) + "]", SN, "", SNHit, ""))
		 End If
		 If SNCrit <> "" Then
		   SNCrit = MakeEnglish(SNCrit)
		   Send (Parse(M.CritHit, SN, "", SNCrit, ""))
		 End If
		 If SNMiss <> "" Then
		   SNMiss = MakeEnglish(SNMiss)
		   Send (Parse(M.Miss, SN, "", SNMiss, ""))
		 End If
		 If M.HealSelf = "" Then M.HealSelf = "Damage caused by !SN's " + M.Name + " :"
		 Send (Parse(M.HealSelf + " [" + TrimStr(Avg) + " avg, " + TrimStr(x5) + " tot]", SN, "", T, ""))
	       End If
	       'p(x2).CurMove = -255
	       If Avg > 0 Then
		 p(x2).Super = p(x2).Super + Abs(Int(Avg * 2 / 3))
	       Else
		 p(x2).Super = p(x2).Super + Abs(Int(M.ElementStr / 3))
	       End If
	       If (p(x2).Super > MaxSP) Then p(x2).Super = MaxSP
	       p(x2).Cheese = p(x2).Cheese + x5
	     End If
	   Case NoDmg:
	     x8 = 0
	     If (M.Target = Friend Or M.Target = AllFriend Or M.Target = OnlySelf) And (p(x2).Target = x2) And (M.HealSelf <> "") Then
	       Send (Parse(M.HealSelf, SN, "", SN, ""))
	       x8 = 1
	     Else
	       If (M.Begin2HealSelf <> "") Then Send (Parse(M.Begin2HealSelf, SN, "", T, ""))
	     End If
	     For x0 = 1 To MaxPlayers
	       DoEvents
	       Okay = IsThisATarget(x2, x0)
	       If Okay Then
		 y2 = True
		 If (M.Target <> Friend And M.Target <> AllFriend And M.Target <> OnlySelf) Then
		   If p(x0).Rune = RuneEvade And Rand(1, 100) <= 25 Then
		     Send (p(x0).ScrNam + " teleports out of the way and laughs!")
		     If p(x2).Rune = RuneMagic Then
		       Send ("But " + p(x2).ScrNam + "'s Rune of Magic automatically redirects the attack!")
		     Else
		       y2 = 0
		     End If
		   End If
		 End If
		 BlockYes = 0
		 If p(x0).CurMove = pBlock And (M.Target <> Friend And M.Target <> AllFriend) Then
		   y3 = Rand(1, 3)
		   If (y3 <> 1) Then
		     If Senshi(p(x0).CharID).BlockYes <> "" Then
		       Send (Parse(Senshi(p(x0).CharID).BlockYes, p(x0).ScrNam, "", SN, ""))
		       BlockYes = True
		     End If
		     y2 = 0
		   Else
		     Send (Parse(Senshi(p(x0).CharID).BlockFail, p(x0).ScrNam, "", SN, ""))
		   End If
		 End If
		 If p(x0).Status(sInvin) And (M.Target <> Friend And M.Target <> AllFriend) Then
		   y3 = Rand(1, 5)
		   If (y3 = 1) Then
		     Send (p(x0).ScrNam + "'s shield collapses!")
		     p(x0).Status(sInvin) = 0
		   ElseIf p(x2).Rune = RuneMagic Then
		     Send (p(x2).ScrNam + "'s Rune of Magic ignores " + p(x0).ScrNam + "'s shield!")
		   Else
		     'Send (p(x0).ScrNam + "'s shield cannot be penetrated!")
		     y2 = 0
		   End If
		 End If
		 If Rand(1, 200) < p(x0).MagDef And M.Target <> Friend And M.Target <> AllFriend And M.Target <> OnlySelf Then y2 = False
		 If (y2 = True) Then
		   If (M.Target = Enemy Or M.Target = Friend Or M.Target = OnlySelf) And (x8 = 0) Then Send (Parse(M.Hit, SN, "", p(x0).ScrNam, ""))
		   If p(x2).Rune = RuneMagic Then
		     p(x0).God = -666
		   End If
		   Call CheckStatus(M.Status(), x0, x2, 0)
		 ElseIf BlockYes = 0 Then
		   Send (Parse(M.Miss, SN, "", p(x0).ScrNam, ""))
		 End If
		 XRunes x2, x0
		 If x0 <= MaxPlayers And (M.Target = Enemy Or M.Target = Everybody Or M.Target = AllFoe Or M.Target = AllTeam) Then
		   If p(x0).Rune = RuneTheft And p(x0).HP > 0 Then
		     StealSub x0, x2
		   End If
		 End If
	       End If
	     Next x0
	     'p(x2).CurMove = -255
	   Case Morph:
	     If M.Hit <> "" Then
	       Send (Parse(M.Hit, p(x2).ScrNam, "", Senshi(p(x2).Target).FullName, ""))
	     ElseIf M.HealSelf <> "" Then
	       Send (Parse(M.HealSelf, p(x2).ScrNam, "", Senshi(p(x2).Target).FullName, ""))
	     Else
	       Send (Parse("!SN successfully morphed into !T.", p(x2).ScrNam, "", Senshi(p(x2).Target).FullName, ""))
	     End If
	     p(x2).Status(sMorph) = XTimer
	     'p(x2).CurMove = -255
	     Temp = True
	     If (p(x2).OldCharID = 0) Then p(x2).OldCharID = p(x2).CharID
	     p(x2).CharID = p(x2).Target
	     X7 = p(x2).CharID
	     x8 = p(x2).OldCharID
	     p(x2).PhysStr = Senshi(X7).PhysStr
	     p(x2).PhysDef = Senshi(X7).PhysDef
	     p(x2).MagDef = Senshi(X7).MagStr
	     p(x2).MagStr = Senshi(X7).MagDef
	     For X7 = 1 To MaxMoves - 1
	       DoEvents
	       p(x2).Moves(X7) = Moves(Senshi(p(x2).CharID).Moves(X7))
	     Next X7
	     Call CheckStatus(M.Status(), x2, x2, 0)
	     'p(x2).CurMove = -255
	   Case MPTheft:
	     Send ("MP Theft is obsolete and no longer supported.")
	   Case HPTheft, SPTheft:
	     x3 = p(x1).HP
	     'x4 = p(x1).MP
	     x5 = p(x1).Super
	     If M.Element = SPTheft Then
	       y2 = Rand(x5 / 4, x5 / 1.5)
	     Else
	       y2 = Rand(Int(M.ElementStr / 3), M.ElementStr)
	     End If
	     If p(x1).Rune = RuneEvade And Rand(1, 100) <= 25 Then
	       Send (p(x1).ScrNam + " teleports out of the way and laughs!")
	       If p(x2).Rune = RuneMagic Then
		 Send ("But " + p(x2).ScrNam + "'s Rune of Magic automatically redirects the attack!")
	       Else
		 y2 = 0
	       End If
	     End If
	     BlockYes = 0
	     If p(x1).CurMove = pBlock Then
	       y3 = Rand(1, 3)
	       If (y3 = 1) Then
		 If Senshi(p(x1).CharID).BlockYes <> "" Then
		   Send (Parse(Senshi(p(x1).CharID).BlockYes, p(x1).ScrNam, "", SN, ""))
		   BlockYes = True
		 End If
		 y2 = 0
	       ElseIf (y3 = 2) Or (y3 = 3) Then
		 y2 = Int(y2 / 2)
	       Else
		 Send (Parse(Senshi(p(x1).CharID).BlockFail, p(x1).ScrNam, "", SN, ""))
	       End If
	     End If
	     If p(x1).Status(sInvin) Then
	       y3 = Rand(1, 5)
	       If (y3 = 1) Then
		 Send (p(x1).ScrNam + "'s shield collapses!")
		 p(x1).Status(sInvin) = 0
	       ElseIf p(x2).Rune = RuneMagic Then
		 Send (p(x2).ScrNam + "'s Rune of Magic ignores " + p(x1).ScrNam + "'s shield!")
	       Else
		 y2 = 0
	       End If
	     End If
	     If (M.Element = HPTheft) Then
	       If (p(x2).HP + y2 > p(x2).MaxHP) And (p(x2).HP <= MaxHP) Then y2 = p(x2).MaxHP - p(x2).HP
	       If (x3 - y2 <= 0) Then y2 = x3
	       x3 = x3 - y2
	       THP = THP + y2
	       p(x2).HP = p(x2).HP + y2
	     'ElseIf (M.Element = MPTheft) Then
	     '  If (p(x2).MP + y2 > MaxMP) And (p(x2).MP <= MaxMP) Then y2 = MaxMP - p(x2).MP
	     '  If (x4 - y2 <= 0) Then y2 = x4
	     '  x4 = x4 - y2
	     '  p(x2).MP = p(x2).MP + y2
	     Else
	       'If (p(x2).MP + y2 > MaxSP) And (p(x2).MP <= MaxSP) Then y2 = MaxSP - p(x2).Super
	       'If (x5 - y2 <= 0) Then y2 = x5
	       x5 = x5 - y2
	       p(x2).Super = p(x2).Super + Abs(y2)
	     End If
	     p(x1).HP = x3
	     If (p(x1).HP <= 0) Then
	       p(x2).Frags = p(x2).Frags + 1
	       IncWins (x2)
	       SayKillStr x2, p(x1).ScrNam
	       Blossom x1, x2
	       If p(x2).Rune = RuneFatal And p(x1).HP > -(MaxHP / 6) And p(x1).Rune <> RuneMagic Then
		 p(x1).HP = 35
		 Send (Parse(M.Hit, SN, "", T, ""))
		 Send (p(x2).ScrNam + " moves in for the kill!!!")
		 p(x2).CurMove = pFatal
		 p(x2).MoveStart = -999
		 Check2MoveDmg x2, 1
	       End If
	     End If
	     'p(x1).MP = x4
	     p(x1).Super = x5
	     If y2 <= 0 Then
	       If BlockYes = 0 Then Send (Parse(M.Miss, SN, "", T, ""))
	     Else
	       If M.Element = HPTheft Then
		 Send (Parse(M.Hit + " [" + TrimStr(y2) + " HP]", SN, "", T, ""))
	       'ElseIf M.Element = MPTheft Then
	       ' Send (Parse(M.Hit + " [" + TrimStr(y2) + " MP]", SN, "", T, ""))
	       ElseIf M.Element = SPTheft Then
		 Send (Parse(M.Hit + " [" + TrimStr(y2) + " SP]", SN, "", T, ""))
	       End If
	     End If
	     XRunes x2, x1
	     If x1 <= MaxPlayers Then
	       If p(x1).Rune = RuneTheft And p(x1).HP > 0 Then
		 StealSub x1, x2
	       End If
	     End If
	     p(x2).Cheese = p(x2).Cheese + y2
	     'p(x2).CurMove = -255
	   Case Life:
	     Select Case p(x1).HP
	       Case Is > 0:
		 If p(x1).Status(sZombie) Then
		   Send (T + " is brought back to life, killing them!")
		   p(x1).HP = 0
		 Else
		   Send5 ("Whoops! " + T + " is already alive!")
		   CheckStatus M.Status(), x1, x2, 0
		 End If
	       Case -666: Send5 (T + " sacrificed their life and cannot be revived.")
	       Case -777: Send5 (T + " has fled from battle.")
	       Case Else:
		 Send (Parse(M.Hit, SN, "", T, ""))
		 p(x1).HP = Int(p(x2).MaxHP / 1.7)
		 CheckStatus M.Status(), x1, x2, 0
	     End Select
	     XRunes x2, x1
	     'p(x2).CurMove = -255
	   Case Demi:
	     y2 = 2
	     If (x1 <= MaxPlayers) Then
	       If p(x1).Rune = RuneEvade And Rand(1, 100) <= 25 Then
		 Send (p(x1).ScrNam + " teleports out of the way and laughs!")
		 If p(x2).Rune = RuneMagic Then
		   Send ("But " + p(x2).ScrNam + "'s Rune of Magic automatically redirects the attack!")
		 Else
		   y2 = 0
		 End If
	       End If
	     Else
	       GoTo nodemihere
	     End If
	     BlockYes = 0
	     If p(x1).CurMove = pBlock Then
	       y3 = Rand(1, 3)
	       If (y3 = 1) Then
		 If Senshi(p(x0).CharID).BlockYes <> "" Then
		   Send (Parse(Senshi(p(x0).CharID).BlockYes, p(x0).ScrNam, "", SN, ""))
		   BlockYes = True
		 End If
	       Else
		 Send (Parse(Senshi(p(x1).CharID).BlockFail, p(x1).ScrNam, "", SN, ""))
	       End If
	     End If
	     If p(x1).Status(sInvin) Then
	       y3 = Rand(1, 5)
	       If (y3 = 1) Then
		 Send (p(x1).ScrNam + "'s shield collapses!")
		 p(x1).Status(sInvin) = 0
	       ElseIf p(x2).Rune = RuneMagic Then
		 Send (p(x2).ScrNam + "'s Rune of Magic ignores " + p(x0).ScrNam + "'s shield!")
	       Else
		 y2 = 0
	       End If
	     End If
	     If (x1 <= MaxPlayers) Then If p(x1).God Then y2 = 0
	     If (y2 = 2) And (x1 <= MaxPlayers) Then
	       y2 = Int(p(x1).HP / 2)
	       p(x1).HP = Int(p(x1).HP / 2)
	       Send (Parse(M.Hit + " [" + TrimStr(y2) + "]", SN, "", T, ""))
	       THP = THP + y2
	       p(x1).Super = p(x1).Super + Abs(y2)
	     ElseIf BlockYes = 0 Then
	       Send (Parse(M.Miss, SN, "", T, ""))
	     End If
	     XRunes x2, x1
	     If x1 <= MaxPlayers Then
	       If p(x1).Rune = RuneTheft And p(x1).HP > 0 Then
		 StealSub x1, x2
	       End If
	     End If
nodemihere:
	     'p(x2).CurMove = -255
	   Case Reveal:
	     y2 = Rand(1, 5)
	     If (y2 <> 2) Then
	       Send (Parse(M.Hit, SN, "", T, ""))
	       ShowReveal (x1)
	     Else
	       Send (Parse(M.Miss, SN, "", T, ""))
	     End If
	     XRunes x2, x1
	     'p(x2).CurMove = -255
	   Case 42:
	     y2 = Rand(1, 5)
	     'x0 = x1
	     If (y2 <> 2) And (x1 <= MaxPlayers) Then
	       Send (Parse(M.Hit, SN, "", T, ""))
	       S1x = Senshi(p(x1).CharID)
	       If (x1 = x2) Then
		 y3 = Rand(1, MaxHidMoves)
		 If HM(y3).Cmdkey <> "" Then
		   p(x2).Moves(14) = HM(y3)
		   Msg = SN + " found a secret move, " + HM(y3).Name + "! (" + p(x1).Moves(14).Cmdkey + ")"
		   If p(x1).Moves(14).CanSuper Then Msg = Msg + "*"
		   Msg = Msg + "!"
		   Send5 (Msg)
		 Else
		   Send ("But why steal your own move...??")
		 End If
	       Else
		 X7 = 0
		 Do
		   y3 = Rand(1, 14)
		   X7 = X7 + 1
		   DoEvents
		 Loop Until (X7 > 100) Or (p(x1).Moves(y3).Cmdkey <> "")
		 p(x2).Moves(14) = p(x1).Moves(y3)
		 p(x2).Moves(14).Cmdkey = "!" + p(x2).Moves(14).Cmdkey
		 Msg = SN + " learned their move, " + p(x1).Moves(y3).Name + "! (" + p(x1).Moves(y3).Cmdkey + ")"
		 If p(x1).Moves(y3).CanSuper Then Msg = Msg + "*"
		 Msg = Msg + " To use type /" + p(x2).Moves(14).Cmdkey
		 Send5 (Msg)
	       End If
	     Else
	       Send (Parse(M.Miss, SN, "", T, ""))
	     End If
	     XRunes x2, x1
	     'p(x2).CurMove = -255
	   Case Else:
	     Send ("Error -- Element #" + TrimStr(M.Element) + " not defined. Notify the creator of this character")
	     p(x2).CurMove = -255
	  End Select
	  If (M.Target = 2 Or M.Target = 4 Or M.Target = 5 Or M.Target = 6 Or M.Target = 7) And M.Element <> Morph Then
	    For x0 = 1 To MaxPlayers
	      If IsThisATarget(x2, x0) And p(x0).CurMove = pBlock And p(x0).Status(sHamedo) = 0 Then
		CounterAttack x0, x2
	      End If
	    Next x0
	  End If
	  For x0 = 1 To sMaxStatus
	    If PeepzStats(x0) <> "" Then
	      x3 = 0
	      For x5 = 1 To Len(PeepzStats(x0))
		If Mid$(PeepzStats(x0), x5, 2) = ", " Then x3 = x3 + 1
	      Next x5
	      'Msg = "(" + Left$(PeepzStats(x0), Len(PeepzStats(x0)) - 1) + ") "
	      If x3 > 1 Then
		Msg = MakeEnglish(PeepzStats(x0)) + " are "
	      Else
		Msg = MakeEnglish(PeepzStats(x0)) + " is "
	      End If
	      Select Case x0
		Case sMute: Msg = Msg + "muted (phys attacks only!)"
		Case sChaos: Msg = Msg + "confused (uncontrollable)"
		Case sFreeze: Msg = Msg + "frozen (cannot move)"
		Case sPoison: Msg = Msg + "poisoned (lose HP)"
		Case sBlind: Msg = Msg + "blinded (attack goes to 0)"
		Case sInvin: Msg = Msg + "shielded"
		Case sHaste: Msg = Msg + "hasted (speed up)"
		Case sMorph: Msg = Msg + "morphed"
		Case sSlow: Msg = Msg + "slowed"
		Case sStun: Msg = Msg + "paralyzed (cannot move)"
		Case sLife3: Msg = Msg + "blessed with a fairy from above"
		Case sRegen: Msg = Msg + "regenerating HP"
		Case sStop: Msg = Msg + "frozen in time (cannot move)"
		Case sMushroom: Msg = Msg + "mushroomied! (cannot do anything)"
		Case sMIA: Msg = Msg + "missing in action (cannot do anything)"
		Case sQuick: Msg = Msg + "glowing with a strange white light!"
		Case sBerserk: Msg = Msg + "going insane (uncontrolled attacks)"
		Case sSleep: Msg = Msg + "snoozing quietly (cannot move)"
		Case sVirus: Msg = Msg + "virused"
		Case sBarrier: Msg = Msg + "protected from attacks"
		Case sMBarrier: Msg = Msg + "protected from magic"
		Case sBless: Msg = Msg + "blessed"
		Case sCurse: Msg = Msg + "cursed (attack goes down)"
		Case sPMS: Msg = Msg + "really pissed off"
		Case sZombie: Msg = Msg + "zombified"
		Case sHamedo: Msg = Msg + "Chibot Countering"
		Case Else: Msg = Msg + "causing this bot to error"
	      End Select
	      Send Msg
	    End If
	    PeepzStats(x0) = ""
	  Next x0
	  For x0 = 1 To sMaxStatus
	    Select Case x0
	      Case sMute, sChaos, sFreeze, sStun, sMIA, sBerserk, sSleep, sStop, sMushroom:
		If M.Status(x0) > 0 Then p(x2).Cheese = p(x2).Cheese + (1 * M.Status(x0))
	      Case sPoison, sBlind, sSlow, sCurse:
		If M.Status(x0) > 0 Then p(x2).Cheese = p(x2).Cheese + (1 * M.Status(x0))
	      Case sHaste, sLife3, sRegen, sQuick, sBarrier, sMBarrier, sBless:
		If M.Status(x0) = -1 Then p(x2).Cheese = Int(p(x2).Cheese + (100 * DMult / 100))
	    End Select
	  Next x0
	  p(x2).CurMove = -255
	  For x3 = 1 To MaxPlayers
	    ShowStatus (x3)
	    If p(x3).Status(sStun) Or p(x3).Status(sFreeze) Or p(x3).Status(sSleep) Then p(x3).CurMove = -50
	  Next x3
	End If
	If M.DestroyWeapon = 1 Then
	  p(x2).WpnUsesLeft = p(x2).WpnUsesLeft - 1
	  If p(x2).WpnUsesLeft <= 0 Then DestroyWeapon (x2)
	End If
	If M.DestroyWeapon = 2 Then
	  If Left$(Weapons(p(x2).Weapon).PickMe, 1) = "~" Then
	    DestroyWeapon (x2)
	  Else
	    DropWeapon x2, "!SN's !T lands on the ground."
	  End If
	End If
	If M.DestroyWeapon = 3 Then DestroyWeapon (x2)
    End Select
  End If
  p(x2).CurMove = p(x2).CurMove
  Exit Sub
End Sub

