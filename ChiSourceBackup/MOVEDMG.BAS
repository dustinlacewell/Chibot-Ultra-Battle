Attribute VB_Name = "MoveDmgModule"
Option Explicit

Sub CounterAttack(ByVal D!, a!)
Dim x4!, X6!, x9!, Q!, Okay!
  If (P(D).Target <> 0) Then
    If P(D).Status(sHamedo) And P(a).Rune = RuneChiGuard And P(D).Rune <> RuneMagic Then
      Exit Sub
    End If
    X6 = P(D).Target
    x9 = P(D).Moves(X6).Target
    If (x9 = Ally Or x9 = Allfriend Or x9 = OnlySelf) Then
      P(D).Target = D
    Else
      P(D).Target = a
    End If
    If P(D).Moves(X6).Element = Morph Then
      If Left$(P(D).Moves(X6).name, 1) = "~" Then P(D).Target = MatchSenshi(Right$(P(D).Moves(X6).name, Len(P(D).Moves(X6).name) - 1))
    End If
    If P(D).Moves(X6).Element <> Phys And P(D).Status(sMute) Then Exit Sub
    P(D).MoveStart = XTimer - 999
    P(D).CurMove = X6
    If P(D).Rune <> RuneBlock Then
      P(D).Super = P(D).Super - 100
    Else
      For x4 = 1 To MaxPlayers
        If P(x4).CurMove > 0 And P(x4).CurMove <= MaxMoves And P(x4).HP > 0 And P(x4).CharID <> 0 Then
          Q = P(x4).Moves(P(x4).CurMove).Target
          If (P(x4).Target = a) And (Q <> 1) And (Q <> 3) And (Q <> 8) And (P(x4).Rune <> RuneStealth) And P(x4).Rune = RuneMagic Then
            Send (P(x4).ScrNam + "'s Rune of Magic prevents " + P(a).ScrNam + " from getting a free counterattack.")
            If P(D).Super >= 100 Then
              P(D).Super = P(D).Super - 100
              Okay = True
            Else
              Send (P(D).ScrNam + " was unable to counterattack due to lack of SP.")
              Okay = False
            End If
          End If
        End If
      Next x4
    End If
    tSP = tSP + 100
    If P(D).Status(sHamedo) Then
      'If P(X6).Rune <> RuneChiGuard Or P(D).Rune = RuneMagic Then
        Check2MoveDmg D, -1
        P(D).Status(sHamedo) = 0
      'Else
        'Send (P(X6).ScrNam + " doesn't set off Chibot Counters.")
      'End If
    End If
  End If
End Sub

Function DamageCount(ByVal x2!, ByVal X6!, M As MoveType, S!) As Long
Dim Df As Integer
Dim ST As Integer
Dim y2 As Integer, Scro!, C!
Dim y3!, Barrier!, MBarrier!
  'If M.Element = Phys Or M.Element = Shadow Then Stop
  If M.Element = Phys Then
    Df = P(X6).PhysDef
    ST = P(x2).PhysStr
  Else
    Df = P(X6).MagDef
    ST = P(x2).MagStr
  End If
  If M.Status(sCPUWait) <> 0 Then
    ST = M.Status(sCPUWait)
  End If
  If P(X6).Rune <> RuneMagic Then
    Scro = P(X6).Scroller
  Else
    Scro = 0
  End If
  If P(x2).Rune = RuneHigh Then ST = ST + (30 * Scro)
  If P(x2).Rune <> RuneMagic Then
    Barrier = P(X6).Status(sBarrier)
    MBarrier = P(X6).Status(sMBarrier)
  End If
  On Error GoTo Whoops
  'y2 = Int(25 * M.ElementStr * Log(St + 1) / (Df + 10)) + Rand(-25, 25)
  y2 = Int((M.ElementStr * ST / 45) / (0.01 * (Df + 50))) + Rand(-MaxHP * 0.05, MaxHP * 0.05)
  'If p(x2).Rune = RuneCharge And p(x2).Charging = True Then
  '  y2 = Int(y2 * (XTimer - p(x2).RuneTemp) / 13)
  'End If
  y2 = Int(DMult / 100 * y2)
  'If P(X2).Rune = RuneDesp And P(X6).Rune <> RuneMagic Then
  '  y2 = Int(y2 * (P(X2).MaxHP * 0.5 / P(X2).HP))
  'End If
  If P(x2).Rune = RuneWonder And P(X6).Rune <> RuneMagic Then
    Select Case Rand(1, 4)
      Case 1: y2 = Rand((MaxHP / 4), (MaxHP * 3 / 4))
      Case 2: y2 = Rand(1, 30)
      Case 3: y2 = Rand(Int(MaxHP / 2), MaxHP)
      Case 4: y2 = Rand(MaxHP / 6, MaxHP / 2)
    End Select
  End If
  'If Rand(1, 200) <= Df Then y2 = 0
  y2 = ProcessDmg(y2, M.Element)
  If Barrier = 1999 And M.Element = Phys Then
    y2 = 0
  ElseIf Barrier > 0 And M.Element = Phys And P(x2).Rune <> RuneMagic Then
    y3 = Int(y2 / 2)
    If C = 0 Then
      P(X6).Status(sBarrier) = P(X6).Status(sBarrier) - Int(y3 / 2)
    End If
    y2 = y2 - y3
  End If
  If MBarrier > 0 And M.Element <> Phys And M.Element <> Heal And P(x2).Rune <> RuneMagic Then
    y3 = Int(y2 / 2)
    If C = 0 Then
      P(X6).Status(sMBarrier) = P(X6).Status(sMBarrier) - Int(y3 / 2)
    End If
    y2 = y2 - y3
  End If
  If P(x2).Status(sBless) Then y2 = Int(y2 * 1.25)
  If P(x2).Status(sCurse) Then y2 = Int(y2 / 1.25)
  If P(X6).Status(sZombie) Then y2 = 0
  'If p(x2).ScrNam = "KamEkSaLLy" Then Stop
  If P(x2).Rune <> RuneSwiss And P(X6).Rune <> RuneSwiss Then
    If Senshi(P(X6).CharID).weakness <> Senshi(P(X6).CharID).Resist And P(x2).CurMove <> 0 And y2 > 0 Then
      If Senshi(P(X6).CharID).weakness = M.Element Then
        Send (P(x2).ScrNam + "'s attack is very effective against " + P(X6).ScrNam + "!")
        y2 = Int(y2 * Rand(150, 300) / 100)
      End If
      If Senshi(P(X6).CharID).Resist = M.Element Then
        Send (P(x2).ScrNam + "'s attack isn't very effective against " + P(X6).ScrNam + "...")
        y2 = Int(y2 * Rand(25, 80) / 100)
      End If
    End If
  End If
  If P(x2).Status(sBlind) And Rand(1, 4) <> 3 Then y2 = 0
  If y2 < 0 Then y2 = 0
  If P(X6).Rune = RuneArmor And P(X6).RuneTemp = M.Element Then
    If P(x2).Rune = RuneMagic Then
      Send (P(x2).ScrNam + "'s Rune of Magic bypasses " + P(X6).ScrNam + "'s " + EName(P(X6).RuneTemp) + " armor.")
    Else
      y2 = -y2
    End If
  End If
  If OnFire = x2 And FireKills >= 3 And PKamek = 0 Then y2 = Int(y2 * 1.25)
  'If y2 > 999 Then y2 = 999
  If Rand(0, 99) >= M.Status(HitRate) And M.Status(HitRate) <> -1 Then y2 = 0
  DamageCount = y2
  If y2 > MaxHP * (5 / 6) And PKamek = 0 Then Playwav "cheap"
  Exit Function
Whoops:
  'ScrollSend ("Overflow in calculating " + P(X2).ScrNam + "'s damage on " + P(X6).ScrNam + "...")
  'kDlgBox "Whoops! You have an ultra-cheap character!", 16, "Overflow"
  'Unload fMidi
  'End
  y2 = P(X6).HP * 2
  If Rand(0, 99) >= M.Status(HitRate) And M.Status(HitRate) <> -1 Then y2 = 0
  If PKamek = 0 And y2 > MaxHP * 5 / 6 Then Playwav "cheap"
  DamageCount = y2
  Exit Function
End Function

Sub LearnMove(ByVal x2!, M As MoveType)
Dim x!
  If P(x2).Rune = RuneLearn And P(x2).CurMove <> 14 Then
    P(x2).Moves(14) = M
    P(x2).Moves(14).Cmdkey = "!" + M.Cmdkey
    Send (P(x2).ScrNam + " learned the move they were attacked with, " + M.name + ".")
    Send (P(x2).ScrNam + " can use this move by typing /!" + M.Cmdkey)
  End If
End Sub

Function PredictDamageCount(ByVal x2!, ByVal X6!, M As MoveType, S!) As Long
Dim Df As Integer
Dim ST As Integer
Dim y2 As Integer, Scro!, C!
Dim y3!, Barrier!, MBarrier!
  If M.Element = Phys Then
    Df = P(X6).PhysDef
    ST = P(x2).PhysStr
  Else
    Df = P(X6).MagDef
    ST = P(x2).MagStr
  End If
  If M.Status(sCPUWait) <> 0 Then
    ST = M.Status(sCPUWait)
  End If
  'If p(x6).Rune <> RuneMagic Then
  '  Scro = p(x6).Scroller
  'Else
    Scro = 0
  'End If
  If P(x2).Rune = RuneHigh Then ST = ST + (30 * Scro)
  If P(x2).Rune <> RuneMagic Then
    Barrier = P(X6).Status(sBarrier)
    MBarrier = P(X6).Status(sMBarrier)
  End If
  'If P(x2).Status(sBlind) Then ST = 0
  On Error GoTo Whoops5
  'y2 = Int(25 * M.ElementStr * Log(St + 1) / (Df + 10)) + Rand(-25, 25)
  y2 = Int((M.ElementStr * ST / 45) / (0.01 * (Df + 50))) + 0
  'If p(x2).Rune = RuneCharge And p(x2).Charging = True Then
  '  y2 = Int(y2 * (XTimer - p(x2).RuneTemp) / 13)
  'End If
  y2 = Int(DMult / 100 * y2)
  'If P(X2).Rune = RuneDesp And P(X6).Rune <> RuneMagic Then
  '  y2 = Int(y2 * (P(X2).MaxHP * 0.5 / P(X2).HP))
  'End If
  'If P(X2).Rune = RuneWonder And P(X6).Rune <> RuneMagic Then
  '  Select Case Rand(1, 4)
  '    Case 1: y2 = Rand((MaxHP / 4), (MaxHP * 3 / 4))
  '    Case 2: y2 = Rand(1, 30)
  '    Case 3: y2 = Rand(Int(MaxHP / 2), MaxHP)
  '    Case 4: y2 = Rand(MaxHP / 6, MaxHP / 2)
  '  End Select
  'End If
  'If Rand(1, 200) <= Df And M.Status(HitRate) <> -1 Then y2 = 0
  y2 = ProcessDmg(y2, M.Element)
  If Barrier = 1999 And M.Element = Phys Then
    y2 = 0
  ElseIf Barrier > 0 And M.Element = Phys And P(x2).Rune <> RuneMagic Then
    y3 = Int(y2 / 2)
    y2 = y2 - y3
  End If
  If MBarrier > 0 And M.Element <> Phys And M.Element <> Heal And P(x2).Rune <> RuneMagic Then
    y3 = Int(y2 / 2)
    y2 = y2 - y3
  End If
  If P(x2).Status(sBless) Then y2 = Int(y2 * 1.25)
  If P(x2).Status(sCurse) Then y2 = Int(y2 / 1.25)
  If P(X6).Status(sZombie) Then y2 = 0
  If P(X6).Rune = RuneSwiss Or P(x2).Rune = RuneSwiss Then
    If Senshi(P(X6).CharID).weakness <> Senshi(P(X6).CharID).Resist And P(x2).CurMove <> 0 Then
      If Senshi(P(X6).CharID).weakness = M.Element Then
        'If M.Target = Enemy Then Send (p(x2).ScrNam + "'s attack is very effective against " + p(x6).ScrNam + "!")
        y2 = Int(y2 * 225 / 100)
      End If
      If Senshi(P(X6).CharID).Resist = M.Element Then
        'If M.Target = Enemy Then Send (p(x2).ScrNam + "'s attack isn't very effective against " + p(x6).ScrNam + "...")
        y2 = Int(y2 * 52.5 / 100)
      End If
    End If
  End If
  If y2 < 0 Then y2 = 0
  If P(X6).Rune = RuneArmor And P(X6).RuneTemp = M.Element Then
    If P(x2).Rune = RuneMagic Then
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
  y2 = 999
  PredictDamageCount = y2
  Exit Function
End Function

Function ProcessDmg(ByVal y2!, ByVal E!) As Long
Dim x1 As Long
Dim Tix As Long
  Tix = (XTimer - TimeLimit)
  x1 = Int(y2 * 2 / 3)
  x1 = Int(x1 * Arena(CurArena).AllAttacks)
  If E <= MaxEle And E > 0 Then x1 = Int(x1 * Arena(CurArena).Effect(E))
  If (TLimit - Tix) <= 60 And (TLimit > 0) Then x1 = x1 * 2.5
  ProcessDmg = x1
End Function

Sub StealSub(ByVal x1!, ByVal x2!)
Dim x9!
  If x1 = x2 Then Exit Sub
  If Rand(1, 4) = 3 Then
    If P(x2).Rune = RuneMagic Or x2 = PKamek Then
      Send (P(x1).ScrNam + " was unable to steal from " + P(x2).ScrNam + ".")
    ElseIf P(x2).Status(sReraise) = True And P(x1).Status(sReraise) = 0 Then
      P(x2).Status(sReraise) = 0
      P(x1).Status(sReraise) = True
      Send (P(x1).ScrNam + " steals " + P(x2).ScrNam + "'s fairy of life!")
    ElseIf P(x2).Status(sRegen) <> 0 And P(x1).Status(sRegen) = 0 Then
      P(x2).Status(sRegen) = 0
      P(x1).Status(sRegen) = True
      Send (P(x1).ScrNam + " steals regeneration from " + P(x2).ScrNam + "!")
    ElseIf P(x2).Status(sBless) <> 0 And P(x1).Status(sBless) = 0 Then
      P(x2).Status(sBless) = 0
      P(x1).Status(sBless) = True
      Send (P(x1).ScrNam + " steals " + P(x2).ScrNam + "'s blessing! They needed that for cheezing!")
    ElseIf (P(x2).Status(sBarrier) <> 0 Or P(x2).Status(sMBarrier) <> 0) And (P(x1).Status(sBarrier) < 100 Or P(x1).Status(sMBarrier) < 100) Then
      P(x1).Status(sBarrier) = P(x1).Status(sBarrier) + P(x2).Status(sBarrier)
      P(x2).Status(sBarrier) = 0
      P(x1).Status(sMBarrier) = P(x1).Status(sMBarrier) + P(x2).Status(sMBarrier)
      P(x2).Status(sMBarrier) = 0
      Send (P(x1).ScrNam + " pickpockets all of " + P(x2).ScrNam + "'s barriers away!")
    ElseIf P(x2).Super > 0 And P(x1).Super < MaxSP Then
      x9 = Rand(1, Int(P(x2).Super / 2))
      P(x1).Super = P(x1).Super + x9
      P(x2).Super = P(x2).Super - x9
      Send (P(x1).ScrNam + " stealthily takes " + TrimStr(x9) + " Super Points from " + P(x2).ScrNam + "!")
    ElseIf P(x1).HP < MaxHP Then
      x9 = Rand(1, Int(P(x2).HP / 2))
      P(x1).HP = P(x1).HP + x9
      P(x2).HP = P(x2).HP - x9
      Send (P(x1).ScrNam + " stole " + TrimStr(x9) + " health from " + P(x2).ScrNam + "!")
    Else
      Send (P(x1).ScrNam + " was unable to steal from " + P(x2).ScrNam + ".")
    End If
  End If
End Sub

Function SuperDmgCt(ByVal x2!, ByVal X6!, M As MoveType, SuperHits!) As Long
Dim Df As Integer
Dim ST As Integer
Dim y2 As Double, Scro!, C!
Dim y3!, Barrier!, MBarrier!
  If M.Element = Phys Then
    Df = P(X6).PhysDef
    ST = P(x2).PhysStr
  Else
    Df = P(X6).MagDef
    ST = P(x2).MagStr
  End If
  If M.Status(sCPUWait) <> 0 Then
    ST = M.Status(sCPUWait)
  End If
  Scro = P(X6).Scroller
  If P(x2).Rune = RuneHigh Then ST = ST + (30 * Scro)
  If P(x2).Rune <> RuneMagic Then
    Barrier = P(X6).Status(sBarrier)
    MBarrier = P(X6).Status(sMBarrier)
  End If
  If P(x2).Status(sBlind) Then ST = 0
  On Error GoTo Whoops3
  'y2 = Int(25 * M.ElementStr * Log(ST + 1) / (Df + 10)) + Rand(-25, 25)
  y2 = Int((M.ElementStr * ST / 45) / (0.01 * (Df + 50))) + Rand(-MaxHP * 0.05, MaxHP * 0.05)
  Select Case P(x2).SuperNum
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
  If Barrier = 1999 And M.Element = Phys Then
    y2 = 0
  ElseIf Barrier > 0 And M.Element = Phys And P(x2).Rune <> RuneMagic Then
    y3 = Int(y2 / 2)
    If C = 0 Then
      P(X6).Status(sBarrier) = P(X6).Status(sBarrier) - Int(y3 / 2)
    End If
    y2 = y2 - y3
  End If
  If MBarrier > 0 And M.Element <> Phys And M.Element <> Heal And P(x2).Rune <> RuneMagic Then
    y3 = Int(y2 / 2)
    If C = 0 Then
      P(X6).Status(sMBarrier) = P(X6).Status(sMBarrier) - Int(y3 / 2)
    End If
    y2 = y2 - y3
  End If
  If (x2 <= MaxPlayers) And (x2 >= 0) Then
    If P(x2).Status(sBless) Then y2 = Int(y2 * 1.25)
    If P(x2).Status(sCurse) Then y2 = Int(y2 / 1.25)
  End If
  If (X6 <= MaxPlayers) And (X6 >= 0) Then
    If P(X6).Status(sZombie) Then y2 = 0
  End If
  If P(x2).Rune <> RuneSwiss And P(X6).Rune <> RuneSwiss Then
    If Senshi(P(X6).CharID).weakness <> Senshi(P(X6).CharID).Resist And P(x2).CurMove <> 0 Then
      If Senshi(P(X6).CharID).weakness = M.Element Then
        Send (P(x2).ScrNam + "'s attack is very effective against " + P(X6).ScrNam + "!")
        y2 = Int(y2 * Rand(150, 300) / 100)
      End If
      If Senshi(P(X6).CharID).Resist = M.Element Then
        Send (P(x2).ScrNam + "'s attack isn't very effective against " + P(X6).ScrNam + "...")
        y2 = Int(y2 * Rand(25, 80) / 100)
      End If
    End If
  End If
  If y2 < 0 Then y2 = 0
  If P(X6).Rune = RuneArmor And P(X6).RuneTemp = M.Element Then
    y2 = -y2
  End If
  'If y2 > 999 Then y2 = 999
  SuperDmgCt = y2
  Exit Function
Whoops3:
  'ScrollSend ("Whoops! Overflow, " + P(X2).ScrNam + "!")
  y2 = 999
  SuperDmgCt = y2
  Exit Function
End Function
Sub Survival(ByVal x2!, ByVal X6!)
  If P(X6).Rune = RuneSurvival And Rand(1, 4) = 1 And P(X6).HP <= 0 Then
    If P(x2).Rune <> RuneMagic Then
      P(X6).HP = 1
      AddStatus X6, sQuick
      Send (P(X6).ScrNam + "'s Survival Rune keeps them alive with 1 HP!")
      Send (P(X6).ScrNam + " glows with a bright white light!")
    Else
      Send (P(X6).ScrNam + "'s Survival Rune keeps them alive with 1 HP!")
      Send ("But " + P(x2).ScrNam + "'s Rune of Magic blasts " + P(X6).ScrNam + " with a fatal blast of energy!")
    End If
  End If
End Sub
Sub XRunes(ByVal x2!, ByVal X6!)
' XRunes (Attacker, Defender)
Dim Y%
Dim Lemon(sMaxStatus) As Integer
  If P(X6).Rune = RuneThorn And X6 <> x2 Then
    Y% = Rand(MaxHP * 0.03, MaxHP * 0.06)
    If P(x2).Rune <> RuneMagic Then
      Send (P(x2).ScrNam + " took " + TrimStr(Y) + " damage from " + P(X6).ScrNam + "'s Rune of Thorns! Ouch!")
      P(x2).HP = P(x2).HP - Y
      Survival X6, x2
      If P(x2).HP <= 0 Then
        If X6 <> x2 Then P(X6).Frags = P(X6).Frags + 1
        IncWins (X6)
        SayKillStr X6, x2
        Blossom x2, X6
      End If
    Else
      Send (P(x2).ScrNam + "'s Rune of Magic shields them from " + P(X6).ScrNam + "'s thorns.")
    End If
  End If
  If Rand(1, 5) = 1 Then
    If P(X6).Rune = RuneCancel Then
      If P(x2).Rune <> RuneMagic And P(x2).Rune <> RuneClover And P(x2).Rune <> RuneLuck And x2 <> PKamek Then
        If x2 = X6 Then
          'Send (P(X2).ScrNam + "'s Rune backfires and freezes " + P(X2).ScrNam + "!")
        Else
          Send (P(x2).ScrNam + " got zapped by " + P(X6).ScrNam + "'s Rune of Cancellation!")
          If P(x2).Status(sReraise) = -1 Then
            Lemon(sReraise) = -1
          ElseIf P(x2).Status(sHaste) <> 0 Then
            Lemon(sHaste) = -1
          ElseIf P(x2).Status(sRegen) <> 0 Then
            Lemon(sRegen) = -1
          ElseIf P(x2).Status(sMBarrier) <> 0 Then
            Lemon(sMBarrier) = -1
          ElseIf P(x2).Status(sBarrier) <> 0 Then
            Lemon(sBarrier) = -1
          ElseIf P(x2).Status(sBless) <> 0 Then
            Lemon(sBless) = -1
          Else
            Send ("However, " + P(x2).ScrNam + " has nothing to be cancelled.")
          End If
          CheckStatus Lemon, x2, X6, 1
        End If
      Else
        Send (P(x2).ScrNam + " receives a jolt of energy, but their " + RuneName(P(x2).Rune) + " protects them from being dispelled.")
      End If
    End If
  End If
  If Rand(1, 8) = 2 Then
    If P(X6).Rune = RuneIce Then
      If P(x2).Rune <> RuneMagic And P(x2).Rune <> RuneShoe And P(x2).Rune <> RuneLuck And x2 <> PKamek Then
        If x2 = X6 Then
          'Send (P(X2).ScrNam + "'s Rune backfires and freezes " + P(X2).ScrNam + "!")
        Else
          Send (P(x2).ScrNam + " is frozen after coming into contact with " + P(X6).ScrNam + "!")
          P(x2).Status(sFreeze) = XTimer
        End If
      Else
        Send (P(x2).ScrNam + " receives a blast of cold, but their " + RuneName(P(x2).Rune) + " keeps them from being frozen.")
      End If
    End If
    If P(X6).Rune = RunePikachu Then
      If P(x2).Rune <> RuneMagic And P(x2).Rune <> RuneLuck And P(x2).Rune <> RuneRabbit And x2 <> PKamek Then
        If x2 = X6 Then
          'Send (P(X2).ScrNam + " is shocked by the Rune of Pikachu!")
        Else
          Send (P(x2).ScrNam + " receives a burst of electricity after coming into contact with " + P(X6).ScrNam + "!")
          Send (P(x2).ScrNam + " is stunned!")
          P(x2).Status(sStun) = XTimer
        End If
      Else
        Send (P(x2).ScrNam + " receives an electrostatic charge, but their " + RuneName(P(x2).Rune) + " won't let them get stunned.")
      End If
    End If
    If P(X6).Rune = RuneMute2 Then
      If P(x2).Rune <> RuneMagic And P(x2).Rune <> RuneLuck And P(x2).Rune <> RuneRabbit Then
        If x2 <> X6 Then
          Send (P(x2).ScrNam + " is muted by " + P(X6).ScrNam + "'s Rune of Muting!")
          P(x2).Status(sMute) = XTimer
        Else
          'Send (P(X2).ScrNam + "'s Rune backfires and mutes " + P(X2).ScrNam + ". Oops!")
        End If
      Else
        Send (P(x2).ScrNam + " is bombarded with mute waves, but their " + RuneName(P(x2).Rune) + " protects them.")
      End If
    End If
  End If
  If Rand(1, 5) = 3 Then
    If P(X6).Rune = RuneLearn And X6 <> x2 Then
      If P(x2).CurMove > 0 And P(x2).CurMove <= MaxMoves Then
        If P(x2).Rune = RuneMagic Or P(x2).CharID = KamekID Then
          Send (P(X6).ScrNam + " tried to learn " + P(x2).ScrNam + "'s move, but failed.")
        Else
          LearnMove X6, P(x2).Moves(P(x2).CurMove)
        End If
      End If
    End If
  End If
End Sub

Sub Blossom(ByVal Dead!, ByVal Killer!)
Dim x!, T!
  If P(Dead).Rune = RuneDeath Then
    x = Int(Rand(Int(MaxHP * 0.25), (MaxHP * 0.9)) * DMult / 100)
    If (Dead = Killer) Then
      Send ("Death blossoms rise out of " + P(Dead).ScrNam + ".")
      Send ("They quickly realize that " + P(Dead).ScrNam + " committed suicide and float away without attacking anybody.")
      Exit Sub
    End If
    If P(Killer).Rune = RuneMagic Then
      Send ("Death blossoms rise out of " + P(Dead).ScrNam + "'s body and move toward " + P(Killer).ScrNam + ".")
      Send ("A force field creased by the Rune of Magic prevents the blossoms from getting near. They implode harmlessly.")
      Exit Sub
    End If
    If P(Killer).HP <= 0 Then
      Send ("Death blossoms rise out of " + P(Dead).ScrNam + "'s body and float over to " + P(Killer).ScrNam + "'s dead body.")
      T = GetTarget(P(Dead).TeamID)
      Send ("The death blossoms can't attack a corpse -- They become confused and attack " + P(T).ScrNam + " instead! [" + TrimStr(x) + "]")
    Else
      Send ("Death blossoms rise out of " + P(Dead).ScrNam + "'s body and attack " + P(Killer).ScrNam + "! [" + TrimStr(x) + "]")
      T = Killer
    End If
    P(T).HP = P(T).HP - x
    Survival Dead, T
    If P(T).HP <= 0 Then
      If Dead <> T Then P(Dead).Frags = P(Dead).Frags + 1
      IncWins (Dead)
      SayKillStr Dead, T
      Blossom T, Dead
    End If
  End If
End Sub

Sub Check2MoveDmg(ByVal x2!, ByVal x7!)
Dim Okay As Integer
Dim x1!, X3!, x4!, x5!, X6!, x8!, x9!, x0!
Dim temp!, CM!, HP!, BlockYes!, SPWorked!
Dim y2!, y3!, y9!, Df!, ST!
Dim Slt%(3)
Dim S1$, S2$, S3$, S4$, S5$, S6$, S7$, S8$, S9$, S0$, SN$
Dim Msg$, T$, SNMiss$, SNHit$, SNCrit$
ReDim S(sMaxStatus) As Integer
Dim Sx As CharType
Dim S1x As CharType
Dim Px As PlayerType
Dim Lemon(sMaxStatus) As Integer
Dim M As MoveType
Dim Sng1!, Sng2!, Sng3!
Dim Avg!, Count!

  On Error Resume Next
  Px = P(x2)
  x1 = Px.Target
  Sx = Senshi(P(x2).CharID)
  CM = P(x2).CurMove
  SN = P(x2).ScrNam
  DoEvents
  T = P(x1).ScrNam
  If (CM = pFatal) Then
    'If x1 > MaxPlayers Then
    '  HP = y(x1 - MaxPlayers).HP
    '  S1 = y(x1 - MaxPlayers).Name
    'Else
      HP = P(x1).HP
      S1 = P(x1).ScrNam
    'End If
    If (HP > (MaxHP / 6)) Or (HP <= 0) Then
      If (HP > (MaxHP / 6)) Then Send (P(x2).ScrNam + " cannot do a fatality on " + S1 + " right now.")
      If (HP <= 0) Then Send (P(x2).ScrNam + " finds it difficult to do a fatality on someone who is already dead.")
      P(x2).CurMove = -255
      Exit Sub
    End If
  End If
  DoEvents
  If (CM <= MaxMoves) And (CM >= 1) Then
    M = P(x2).Moves(CM)
    If (M.Element <> Morph) Then
      x9 = P(x2).Target
      If x9 > MaxPlayers Then
        x8 = 0
      Else
        x8 = P(x9).Status(sMIA)
      End If
      If x8 And (M.Target = Ally Or M.Target = Enemy) Then
        Send ("But " + ScrNam(x9) + " seems to be missing, " + P(x2).ScrNam + "!")
        'p(x2).T(x9) = 0
        P(x2).CurMove = 0
        Exit Sub
      End If
      DoEvents
      If HPx(x9) <= 0 And M.Element <> Life And (M.Target = Ally Or M.Target = Enemy) Then
        If Not DoneKilled(x9) Then ShowStatus (x9)
        Send ("But " + ScrNam(x9) + " is dead, " + P(x2).ScrNam + "!")
        'p(x2).T(x9) = 0
        P(x2).CurMove = 0
        Exit Sub
      End If
      If Active(x9) = 0 And (M.Target = Ally Or M.Target = Enemy) Then
        Send ("But " + ScrNam(x9) + " doesn't exist, " + P(x2).ScrNam + ".")
        'p(x2).T(x9) = 0
        P(x2).CurMove = 0
        Exit Sub
      End If
    End If
  End If
  DoEvents
  If x7 Then
    Select Case CM
      Case pBlock:
        P(x2).CurMove = -255
      Case pTaunt:
        P(x2).CurMove = -255
      Case pCharge:
        P(x2).ChargeLevel = P(x2).ChargeLevel + 1
        P(x2).CurMove = -255
      Case pRest:
        P(x2).CurMove = -255
        P(x2).HP = P(x2).HP + Rand(Arena(CurArena).RestLowHP / 100 * MaxHP, Arena(CurArena).RestHighHP / 100 * MaxHP)
        If P(x2).HP > P(x2).MaxHP Then P(x2).HP = P(x2).MaxHP
        P(x2).Cheese = Int(P(x2).Cheese - 300 * DMult / 100)
        If P(x2).Cheese < 0 Then P(x2).Cheese = 0
      Case -50:
        If (P(x2).Status(sStun) = 0) And (P(x2).Status(sFreeze) = 0) And (P(x2).Status(sSleep) = 0) Then P(x2).CurMove = -255
      Case pFatal:
        Send (Parse(Sx.Fatality.FatalMove, SN, "", T, ""))
        P(x1).HP = -9999
        P(x1).AttackedMe = x2
        Blossom x1, x2
        ShowStatus (x1)
        DoEvents
        If (x1 <= MaxPlayers) Then SayKillStr x2, x1
        P(x2).CurMove = -255
        P(x2).FatalFrags = P(x2).FatalFrags + 1
        If x2 <> x1 Then P(x2).Frags = P(x2).Frags + 1
        IncWins (x2)
        TFatal = TFatal + 1
      Case pFlee:
        Send (P(x2).ScrNam + " quickly flees from the fight. Everyone point and call them a coward!")
        Playwav "flee"
        P(x2).CurMove = 0
        P(x2).MoveStart = 0
        P(x2).Status(sReraise) = 0
        P(x2).HP = -777
      Case pSlot:
        X6 = P(x2).Target
        If P(X6).Rune = RuneJedi Then
          If P(x2).Rune = RuneMagic Then
            Send (P(X6).ScrNam + " tries Jedi Mind Control on " + P(x2).ScrNam + ", but it fails.")
          Else
            Send (P(X6).ScrNam + " says to " + P(x2).ScrNam + ", ""I'm not the one you want to attack.""")
            P(x2).Target = GetTarget(P(x2).TeamID)
            X6 = P(x2).Target
            Send (P(x2).ScrNam + " turns toward " + ScrNam(X6) + ".")
          End If
        End If
        If P(X6).Rune = RuneReflect Then
          If P(x2).Rune = RuneMagic Then
            Send (P(X6).ScrNam + "'s Rune reflects the move back to " + P(x2).ScrNam + "...")
            Send ("... but " + P(x2).ScrNam + "'s Rune of Magic reflects it right back!")
          ElseIf x2 = PKamek Then
            Send (P(X6).ScrNam + "'s Rune reflects the move back to " + P(x2).ScrNam + "...")
            Send ("... but Kamek laughs as he reflects it back!")
          Else
            Send (P(X6).ScrNam + "'s Rune reflects the move back to " + P(x2).ScrNam + "!")
            P(x2).Target = x2
            X6 = P(x2).Target
          End If
        End If
        If P(X6).CurMove = pBlock And P(X6).Status(sHamedo) Then CounterAttack X6, x2
        Msg = "Slot: ["
        For x0 = 1 To 3
          If x0 = 1 Then Slt(x0) = Rand(1, 6)
          If x0 = 2 Then
            Select Case Rand(1, 10)
              Case 1 To 7: Slt(2) = Slt(1)
              Case Else: Slt(x0) = Rand(1, 6)
            End Select
          End If
          If x0 = 3 Then
            If Slt(1) = Slt(2) Then
              Select Case Slt(1):
                Case 1, 2, 3, 4: x9 = 2
                Case 5, 6: x9 = 10
              End Select
              Select Case Rand(1, x9)
                Case 1: Slt(3) = Slt(2)
                Case Else: Slt(3) = Rand(1, 6)
              End Select
            Else
              Slt(3) = Rand(1, 6)
            End If
          End If
          Select Case Slt(x0):
            Case 1: Msg = Msg + ccColor + "15anvil"
            Case 2: Msg = Msg + ccColor + "14cherry"
            Case 3: Msg = Msg + ccColor + "07coin"
            Case 4: Msg = Msg + ccColor + "08lemon"
            Case 5: Msg = Msg + ccColor + "12Kamek"
            Case 6: Msg = Msg + ccColor + "13cheese"
          End Select
          Msg = Msg + ccColor + "14"
          If x0 = 3 Then
            Msg = Msg + "]"
          Else
            Msg = Msg + "|"
          End If
        Next x0
        Send (P(x2).ScrNam + " watches as the reels slow down.")
        Send Msg
        If Slt(1) = Slt(2) And Slt(2) = Slt(3) Then
          Select Case Slt(1)
            Case 1: ' Anvil
              y2 = Rand(MaxHP / 10, MaxHP / 5)
              Send ("A large anvil falls on " + P(X6).ScrNam + ". [" + TrimStr(y2) + "]")
              If P(X6).CurMove = pBlock Then CounterAttack X6, x2
              XRunes x2, X6
            Case 2: ' Cherry Blossom
              y2 = Rand(MaxHP / 40, MaxHP / 2)
              Send ("Red cherry blossoms explode around " + P(X6).ScrNam + ". [" + TrimStr(y2) + "]")
              If P(X6).CurMove = pBlock Then CounterAttack X6, x2
              XRunes x2, X6
            Case 3: ' Coin Rain
              y2 = Rand(1, MaxSP)
              Send (P(x2).ScrNam + " receives a shower of coins labeled ""S""... for super! [+" + TrimStr(y2) + " SP]")
              P(x2).Super = P(x2).Super + y2
              y2 = 0
              If P(x2).Super > MaxSP Then P(x2).Super = MaxSP
            Case 4: ' Lemon
              y2 = 0
              Send ("Lemons rain down on " + P(X6).ScrNam + "... and these lemons are the kind you can't make lemonade out of.")
              Lemon(sCurse) = 100
              Lemon(sBlind) = 100
              Lemon(sMute) = 100
              Lemon(sSlow) = 100
              CheckStatus Lemon(), X6, x2, 1
              Lemon(sCurse) = 0
              Lemon(sBlind) = 0
              Lemon(sMute) = 0
              Lemon(sSlow) = 0
              If P(X6).CurMove = pBlock Then CounterAttack X6, x2
              XRunes x2, X6
            Case 5: ' Kamek
              y2 = 0
              If x2 = X6 Then
                Send ("Kamek cannot do much at this time.")
              Else
                Send ("Kamek waves his magic wand.")
                Lemon(sRegen) = -1
                Lemon(sBless) = -1
                Lemon(sHaste) = -1
                Lemon(sBarrier) = -1
                Lemon(sMBarrier) = -1
                Lemon(sReraise) = -1
                CheckStatus Lemon(), X6, x2, 1
                Lemon(sRegen) = 100
                Lemon(sBless) = 100
                Lemon(sHaste) = 100
                Lemon(sBarrier) = 100
                Lemon(sMBarrier) = 100
                Lemon(sReraise) = 100
                CheckStatus Lemon(), x2, x2, 1
                If P(X6).CurMove = pBlock Then CounterAttack X6, x2
                XRunes x2, X6
              End If
            Case 6: ' Cheese
              y2 = MaxHP - 1
              Send ("Cheese assaults " + P(X6).ScrNam + " from all sides. [" + TrimStr(y2) + "]")
              If P(X6).CurMove = pBlock Then CounterAttack X6, x2
              XRunes x2, X6
          End Select
        Else
          y2 = Rand(MaxHP / 20, MaxHP / 10)
          Send ("The slot machine spits out a few pebbles and small rocks at " + P(X6).ScrNam + ". [" + TrimStr(y2) + "]")
          If P(X6).CurMove = pBlock Then CounterAttack X6, x2
          XRunes x2, X6
        End If
        If y2 <> 0 Then
          P(X6).HP = P(X6).HP - y2
          Survival x2, X6
          If P(x2).CharID = KamekID And pYoshi = 0 Then P(X6).HP = 1
          If P(X6).HP <= 0 Then
            If x2 = PKamek Then Stop
            If x2 <> X6 Then P(x2).Frags = P(x2).Frags + 1
            IncWins (x2)
            SayKillStr x2, X6
            Blossom X6, x2
            If P(x2).Rune = RuneFatal And P(X6).HP > -(MaxHP / 6) And P(X6).Rune <> RuneMagic Then
              P(X6).HP = 35
              Send (P(x2).ScrNam + " moves in for the kill!!!")
              P(x2).CurMove = pFatal
              P(x2).MoveStart = -999
              Check2MoveDmg x2, 1
            End If
          End If
        End If
        P(x2).CurMove = -255
      Case 1 To MaxMoves:
        x4 = P(x2).CharID
        X6 = P(x2).Target
        If X6 <= MaxPlayers Then
          If M.Target <> Ally And M.Target <> OnlySelf And M.Target <> Allfriend And Rand(1, 5) = 3 And P(X6).Rune = RuneJedi Then
            If P(x2).Rune = RuneMagic Then
              Send (P(X6).ScrNam + " tries Jedi Mind Control on " + P(x2).ScrNam + ", but it fails.")
            Else
              Send (P(X6).ScrNam + " says to " + P(x2).ScrNam + ", ""I'm not the one you want to attack.""")
              P(x2).Target = GetTarget(P(x2).TeamID)
              X6 = P(x2).Target
              Send (P(x2).ScrNam + " turns toward " + ScrNam(X6) + ".")
            End If
          End If
        End If
        If X6 <= MaxPlayers Then
          If M.Target <> Ally And M.Target <> OnlySelf And M.Target <> Allfriend And Rand(1, 5) = 3 And P(X6).Rune = RuneReflect And X6 <> x2 Then
            If P(x2).Rune = RuneMagic Then
              Send (P(X6).ScrNam + "'s Rune reflects the move back to " + P(x2).ScrNam + "...")
              Send ("... but " + P(x2).ScrNam + "'s Rune of Magic reflects it right back!")
            Else
              Send (P(X6).ScrNam + "'s Rune reflects the move back to " + P(x2).ScrNam + "!")
              P(x2).Target = x2
              X6 = P(x2).Target
            End If
          End If
        End If
        If P(x2).Moves(P(x2).CurMove).Element <> Morph Then
          If TeamID(x1) <> TeamID(x2) And x1 <= MaxPlayers And M.Element <> Morph Then P(x1).AttackedMe = x2
        End If
        M = P(x2).Moves(CM)
        If M.Status(HitRate) = 0 Then M.Status(HitRate) = 100
        If M.MPReq > P(x2).MP Then
          Send (P(x2).ScrNam + " doesn't have enough MP to finish the move (has " + TrimStr(P(x2).MP)) + ", move requires " + TrimStr(M.MPReq) + ")"
          P(x2).CurMove = -255
          Exit Sub
        Else
          P(x2).MP = P(x2).MP - M.MPReq
        End If
        DoEvents
        If (P(x2).CurMove <> -255) Then
          If M.Target = 2 Or M.Target = 4 Or M.Target = 5 Or M.Target = 6 Or M.Target = 7 Then
            For x0 = 1 To MaxPlayers
              If IsThisATarget(x2, x0) And P(x0).CurMove = pBlock And P(x0).Status(sHamedo) Then
                CounterAttack x0, x2
                If P(x0).Status(sMIA) Then
                  Send P(x2).ScrNam + " blinks and sees that " + P(x0).ScrNam + " isn't there!"
                  P(x2).CurMove = -255
                  Exit Sub
                End If
              End If
            Next x0
          End If
          If P(x2).Status(sStop) Or P(x2).Status(sMIA) Or P(x2).Status(sStun) Or P(x2).Status(sFreeze) Or P(x2).Status(sSleep) Or P(x2).Status(sMushroom) Or (P(x2).Status(sMute) And M.Element <> Phys) Or P(x2).HP <= 0 Then Exit Sub
          If P(x2).CurMove > 20 Then P(x2).RuneTemp = XTimer
          Select Case M.Element
            Case eSlot:
              X6 = P(x2).Target
              If P(X6).CurMove = pBlock And P(X6).Status(sHamedo) Then CounterAttack X6, x2
              Msg = "Slot: ["
              For x0 = 1 To 3
                If x0 = 1 Then Slt(x0) = Rand(1, 6)
                If x0 = 2 Then
                  Select Case Rand(1, 10)
                    Case 1 To 7: Slt(2) = Slt(1)
                    Case Else: Slt(x0) = Rand(1, 6)
                  End Select
                End If
                If x0 = 3 Then
                  If Slt(1) = Slt(2) Then
                    Select Case Slt(1):
                      Case 1, 2, 3, 4: x9 = 2
                      Case 5, 6: x9 = 10
                    End Select
                    Select Case Rand(1, x9)
                      Case 1: Slt(3) = Slt(2)
                      Case Else: Slt(3) = Rand(1, 6)
                    End Select
                  Else
                    Slt(3) = Rand(1, 6)
                  End If
                End If
                Select Case Slt(x0):
                  Case 1: Msg = Msg + ccColor + "15anvil"
                  Case 2: Msg = Msg + ccColor + "14cherry"
                  Case 3: Msg = Msg + ccColor + "07coin"
                  Case 4: Msg = Msg + ccColor + "08lemon"
                  Case 5: Msg = Msg + ccColor + "12Kamek"
                  Case 6: Msg = Msg + ccColor + "13cheese"
                End Select
                Msg = Msg + ccColor + "14"
                If x0 = 3 Then
                  Msg = Msg + "]"
                Else
                  Msg = Msg + "|"
                End If
              Next x0
              Send (P(x2).ScrNam + " watches as the reels slow down.")
              Send Msg
              If Slt(1) = Slt(2) And Slt(2) = Slt(3) Then
                Select Case Slt(1)
                  Case 1: ' Anvil
                    y2 = Rand(MaxHP / 10, MaxHP / 5)
                    Send ("A large anvil falls on " + P(X6).ScrNam + ". [" + TrimStr(y2) + "]")
                    If P(X6).CurMove = pBlock Then CounterAttack X6, x2
                    XRunes x2, X6
                  Case 2: ' Cherry Blossom
                    y2 = Rand(MaxHP / 40, MaxHP / 2)
                    Send ("Red cherry blossoms explode around " + P(X6).ScrNam + ". [" + TrimStr(y2) + "]")
                    If P(X6).CurMove = pBlock Then CounterAttack X6, x2
                    XRunes x2, X6
                  Case 3: ' Coin Rain
                    y2 = Rand(1, MaxSP)
                    Send (P(x2).ScrNam + " receives a shower of coins labeled ""S""... for super! [+" + TrimStr(y2) + " SP]")
                    P(x2).Super = P(x2).Super + y2
                    y2 = 0
                    If P(x2).Super > MaxSP Then P(x2).Super = MaxSP
                  Case 4: ' Lemon
                    y2 = 0
                    Send ("Lemons rain down on " + P(X6).ScrNam + "... and these lemons are the kind you can't make lemonade out of.")
                    Lemon(sCurse) = 100
                    Lemon(sBlind) = 100
                    Lemon(sMute) = 100
                    Lemon(sSlow) = 100
                    CheckStatus Lemon(), X6, x2, 1
                    Lemon(sCurse) = 0
                    Lemon(sBlind) = 0
                    Lemon(sMute) = 0
                    Lemon(sSlow) = 0
                    If P(X6).CurMove = pBlock Then CounterAttack X6, x2
                    XRunes x2, X6
                  Case 5: ' Kamek
                    y2 = 0
                    If x2 = X6 Then
                      Send ("Kamek cannot do much at this time.")
                    Else
                      Send ("Kamek waves his magic wand.")
                      Lemon(sRegen) = -1
                      Lemon(sBless) = -1
                      Lemon(sHaste) = -1
                      Lemon(sBarrier) = -1
                      Lemon(sMBarrier) = -1
                      Lemon(sReraise) = -1
                      CheckStatus Lemon(), X6, x2, 1
                      Lemon(sRegen) = 100
                      Lemon(sBless) = 100
                      Lemon(sHaste) = 100
                      Lemon(sBarrier) = 100
                      Lemon(sMBarrier) = 100
                      Lemon(sReraise) = 100
                      CheckStatus Lemon(), x2, x2, 1
                      If P(X6).CurMove = pBlock Then CounterAttack X6, x2
                      XRunes x2, X6
                    End If
                  Case 6: ' Cheese
                    y2 = MaxHP - 1
                    Send ("Cheese assaults " + P(X6).ScrNam + " from all sides. [" + TrimStr(y2) + "]")
                    If P(X6).CurMove = pBlock Then CounterAttack X6, x2
                    XRunes x2, X6
                End Select
              Else
                y2 = Rand(MaxHP / 20, MaxHP / 10)
                Send ("The slot machine spits out a few pebbles and small rocks at " + P(X6).ScrNam + ". [" + TrimStr(y2) + "]")
                If P(X6).CurMove = pBlock Then CounterAttack X6, x2
                XRunes x2, X6
              End If
              If y2 <> 0 Then
                P(X6).HP = P(X6).HP - y2
                Survival x2, X6
                If P(x2).CharID = KamekID And pYoshi = 0 Then P(X6).HP = 1
                If P(X6).HP <= 0 Then
                  If x2 = PKamek Then Stop
                  If x2 <> X6 Then P(x2).Frags = P(x2).Frags + 1
                  IncWins (x2)
                  SayKillStr x2, X6
                  Blossom X6, x2
                  If P(x2).Rune = RuneFatal And P(X6).HP > -(MaxHP / 6) And P(X6).Rune <> RuneMagic Then
                    P(X6).HP = 35
                    Send (P(x2).ScrNam + " moves in for the kill!!!")
                    P(x2).CurMove = pFatal
                    P(x2).MoveStart = -999
                    Check2MoveDmg x2, 1
                  End If
                End If
              End If
              P(x2).CurMove = -255
            Case 250:
              If X6 = PKamek Then
                Send Parse(M.Hit, P(x2).ScrNam, "", P(X6).ScrNam, "")
                P(X6).Status(sBarrier) = -4
                P(X6).MagStr = 80
                For x0 = 1 To MaxPlayers
                  If P(x0).TeamID = "G" And P(x0).CharID <> 0 Then P(x0).HP = P(x0).MaxHP
                Next x0
                fChUBMain.tYoshiStart.Enabled = True
              Else
                Send Parse(M.Miss, P(x2).ScrNam, "", P(X6).ScrNam, "")
              End If
            Case 252:
              y2 = -1
              x0 = X6
              BlockYes = 0
              If P(x0).CurMove = pBlock And M.Status(HitRate) <> -1 Then
                y3 = Rand(1, 3)
                If (y3 = 1) Then
                  If Senshi(P(x0).CharID).BlockYes <> "" Then
                    BlockYes = True
                    Send (Parse(Senshi(P(x0).CharID).BlockYes, P(x0).ScrNam, "", SN, ""))
                  End If
                  y2 = 0
                ElseIf (y3 = 2) Or (y3 = 3) Then
                  'y2 = Int(y2 / 2)
                Else
                  Send (Parse(Senshi(P(x0).CharID).BlockFail, P(x0).ScrNam, "", SN, ""))
                End If
                DoEvents
              End If
              If P(x0).Status(sInvin) Then
                y3 = Rand(1, 5)
                If (y3 = 1) And x0 <> PKamek Then
                  Send (P(x0).ScrNam + "'s shield collapses!")
                  P(x0).Status(sInvin) = 0
                ElseIf P(x2).Rune = RuneMagic Then
                  Send (P(x2).ScrNam + "'s Rune of Magic ignores " + P(x0).ScrNam + "'s shield!")
                Else
                  y2 = 0
                End If
              End If
              If P(x0).Rune = RuneSwiss Then
                Send (P(x0).ScrNam + "'s Swiss Rune protects them from the attack!")
                y2 = 0
              End If
              If P(x0).CurMove = -255 And P(x0).Rune = RuneCtrGuard And P(x2).Rune <> RuneMagic And Rand(1, 3) = 1 Then
                Send (P(x0).ScrNam + " teleported away from " + P(x2).ScrNam + "'s counterattack!")
                y2 = 0
              End If
              If P(X6).Weapon = 0 Then y2 = 0
              If Rand(0, 99) > M.Status(HitRate) And M.Status(HitRate) <> -1 Then y2 = 0
              If y2 = 0 Then
                Send Parse(M.Miss, P(x2).ScrNam, "", P(X6).ScrNam, "")
              Else
                Send Parse(M.Hit, P(x2).ScrNam, Weapons(P(X6).Weapon).name, P(X6).ScrNam, "")
                DestroyWeapon X6
              End If
              XRunes x2, X6
            Case 251:
              Send Parse(M.Hit, P(x2).ScrNam, "", "", "")
              P(x2).Status(sBarrier) = 1999
            Case 255:
              y2 = -1
              x0 = X6
              BlockYes = 0
              If P(x0).CurMove = pBlock And M.Status(HitRate) <> -1 Then
                y3 = Rand(1, 3)
                If (y3 = 1) Then
                  If Senshi(P(x0).CharID).BlockYes <> "" Then
                    BlockYes = True
                    Send (Parse(Senshi(P(x0).CharID).BlockYes, P(x0).ScrNam, "", SN, ""))
                  End If
                  y2 = 0
                ElseIf (y3 = 2) Or (y3 = 3) Then
                  'y2 = Int(y2 / 2)
                Else
                  Send (Parse(Senshi(P(x0).CharID).BlockFail, P(x0).ScrNam, "", SN, ""))
                End If
                DoEvents
              End If
              If P(x0).Status(sInvin) Then
                y3 = Rand(1, 5)
                If (y3 = 1) And x0 <> PKamek Then
                  Send (P(x0).ScrNam + "'s shield collapses!")
                  P(x0).Status(sInvin) = 0
                ElseIf P(x2).Rune = RuneMagic Then
                  Send (P(x2).ScrNam + "'s Rune of Magic ignores " + P(x0).ScrNam + "'s shield!")
                Else
                  y2 = 0
                End If
              End If
              If P(x0).CurMove = -255 And P(x0).Rune = RuneCtrGuard And P(x2).Rune <> RuneMagic And Rand(1, 3) = 1 Then
                Send (P(x0).ScrNam + " teleported away from " + P(x2).ScrNam + "'s counterattack!")
                y2 = 0
              End If
              If Rand(0, 99) > M.Status(HitRate) And M.Status(HitRate) <> -1 Then y2 = 0
              If y2 Then
                Send Parse(M.Hit, SN, "", T, "")
                If P(x0).Status(sRegen) <> 0 Then
                  Lemon(sRegen) = -1
                  CheckStatus Lemon(), x0, x2, 1
                  Lemon(sRegen) = 100
                  CheckStatus Lemon(), x2, x2, 1
                  Lemon(sRegen) = 0
                End If
                If P(x0).Status(sReraise) <> 0 Then
                  Lemon(sReraise) = -1
                  CheckStatus Lemon(), x0, x2, 1
                  Lemon(sReraise) = 100
                  CheckStatus Lemon(), x2, x2, 1
                  Lemon(sReraise) = 0
                End If
                If P(x0).Status(sQuick) <> 0 Then
                  Lemon(sQuick) = -1
                  CheckStatus Lemon(), x0, x2, 1
                  Lemon(sQuick) = 100
                  CheckStatus Lemon(), x2, x2, 1
                  Lemon(sQuick) = 0
                End If
                If P(x0).Status(sBarrier) <> 0 Then
                  Lemon(sBarrier) = -1
                  CheckStatus Lemon(), x0, x2, 1
                  Lemon(sBarrier) = 100
                  CheckStatus Lemon(), x2, x2, 1
                  Lemon(sBarrier) = 0
                End If
                If P(x0).Status(sMBarrier) <> 0 Then
                  Lemon(sMBarrier) = -1
                  CheckStatus Lemon(), x0, x2, 1
                  Lemon(sMBarrier) = 100
                  CheckStatus Lemon(), x2, x2, 1
                  Lemon(sMBarrier) = 0
                End If
                If P(x0).Status(sBless) <> 0 Then
                  Lemon(sBless) = -1
                  CheckStatus Lemon(), x0, x2, 1
                  Lemon(sBless) = 100
                  CheckStatus Lemon(), x2, x2, 1
                  Lemon(sBless) = 0
                End If
                If P(x0).Status(sHaste) <> 0 Then
                  Lemon(sHaste) = -1
                  CheckStatus Lemon(), x0, x2, 1
                  Lemon(sHaste) = 100
                  CheckStatus Lemon(), x2, x2, 1
                  Lemon(sHaste) = 0
                End If
              Else
                Send Parse(M.Miss, SN, "", T, "")
              End If
              XRunes x2, X6
            Case 253:
              'Send ("Phew, this one's a toughie...!")
              Okay = True
              For x0 = 1 To MaxPlayers
                If P(x0).OwnedBy = x2 Then
                  Okay = False
                  Exit For
                End If
              Next x0
              BlockYes = 0
              If P(X6).CurMove = pBlock And M.Status(HitRate) <> -1 Then
                y3 = Rand(1, 5)
                If (y3 < 4) Then
                  If Senshi(P(X6).CharID).BlockYes <> "" Then
                    Okay = False
                    BlockYes = True
                    Send (Parse(Senshi(P(X6).CharID).BlockYes, P(x2).ScrNam, "", SN, ""))
                  End If
                  y2 = 0
                  Send (Parse(Senshi(P(X6).CharID).BlockFail, P(x2).ScrNam, "", SN, ""))
                End If
                DoEvents
              End If
              If P(X6).Status(sInvin) Then
                y3 = Rand(1, 5)
                If (y3 = 1) And x0 <> PKamek Then
                  Send (P(X6).ScrNam + "'s shield collapses!")
                  P(X6).Status(sInvin) = 0
                ElseIf P(X6).Rune = RuneMagic Then
                  Send (P(x2).ScrNam + "'s Rune of Magic ignores " + P(X6).ScrNam + "'s shield!")
                Else
                  Okay = False
                End If
              End If
              If P(X6).CurMove = -255 And P(X6).Rune = RuneCtrGuard And P(x2).Rune <> RuneMagic And Rand(1, 3) = 1 Then
                Send (P(X6).ScrNam + " teleported away from " + P(x2).ScrNam + "'s counterattack!")
                Okay = 0
              End If
              If P(X6).Rune = RuneSwiss Then
                Send (P(X6).ScrNam + "'s Swiss Rune protects them from the attack!")
                Okay = 0
              End If
              If Rand(0, 99) > M.Status(HitRate) And M.Status(HitRate) <> -1 Then Okay = 0
              If Okay Then
                Send (Parse(M.Hit, SN, "", T, ""))
                ReDim Preserve P(MaxPlayers + 1)
                MaxPlayers = MaxPlayers + 1
                x4 = MaxPlayers
                P(x4).CharID = P(X6).CharID
                ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
                P(x4).ScrNam = "CLONE " + TrimStr(Rand(1000, 9999))
                If Len(P(X6).ScrNam) < 4 Then
                  P(x4).ScrNam = P(x4).ScrNam + P(X6).ScrNam
                Else
                  P(x4).ScrNam = P(x4).ScrNam + Left$(P(X6).ScrNam, 4)
                End If
StartOver:
                For x0 = 1 To MaxPlayers
                  If x0 <> x4 And P(x0).ScrNam = P(x4).ScrNam Then
                    P(x4).ScrNam = P(x4).ScrNam + "0"
                    GoTo StartOver
                  End If
                Next x0
                'P(X4).PhysStr = 120
                'P(X4).PhysDef = 0
                'P(X4).MagStr = 120
                'P(X4).MagDef = 0
                P(x4).PhysStr = P(X6).PhysStr
                P(x4).PhysDef = P(X6).PhysDef
                P(x4).MagStr = P(X6).MagStr
                P(x4).MagDef = P(X6).MagDef
                P(x4).TeamID = P(x2).TeamID
                P(x4).CPU = 1
                P(x4).Wrath = 0
                P(x4).Arrogance = 0
                P(x4).Defect = 0
                P(x4).Goodwill = 100
                P(x4).Greed = 0
                P(x4).OwnedBy = x2
                P(x4).MaxHP = MaxHP
                P(x4).HP = MaxHP
                P(x4).CurMove = -255
                InitMoves (x4)
              Else
                Send Parse(M.Miss, SN, "", T, "")
              End If
              XRunes x2, X6
            Case Invin:
              Send (Parse(M.Hit, SN, "", T, ""))
              For x0 = 1 To MaxPlayers
                DoEvents
                If P(x0).Status(sInvin) = x2 Then
                  P(x0).Status(sInvin) = False
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
                  y2 = Int((M.ElementStr + Int(P(x2).MagStr / 4)) / 2) + Rand(-(MaxHP / 30), MaxHP / 30)
                  y2 = Int(y2 * DMult / 100)
                  If P(x2).Rune = RuneDesp And P(x2).HP < 0.25 * P(x2).MaxHP Then y2 = y2 + Int(y2 * Rand(0, 200) / 100)
                  If P(x2).Rune = RuneHeal Then y2 = Int(y2 * 1.5)
                  If P(x0).Status(sZombie) Then
                    Send ("The cure harms the zombie!")
                    P(x0).HP = P(x0).HP - y2
                    Survival x2, x0
                    Count = Count + 1
                    Avg = Avg + y2
                  Else
                    If P(x0).HP + y2 > P(x0).MaxHP Then y2 = P(x0).MaxHP - P(x0).HP
                    If (y2 >= 0) Then
                      Count = Count + 1
                      Avg = Avg + y2
                      P(x0).HP = P(x0).HP + y2
                    End If
                  End If
                  XRunes x2, x0
                End If
              Next x0
              On Error Resume Next
              Avg = Int(Avg / Count)
              If (M.Target <> Allfriend) And (P(x2).Target = x2) Then
                Send (Parse(M.HealSelf + " [" + TrimStr(Avg) + "]", SN, "", SN, ""))
              Else
                Send (Parse(M.Hit + " [" + TrimStr(Avg) + "]", SN, "", T, ""))
              End If
              'p(x2).CurMove = -255
            Case Poison, Grass, Rock, Dirt, Psychic, Ghost, Phys, MoonE, Shadow, Water, Fire, Lit, Heart, Earth, Wind, Ki, Lum:
              If M.Element = Poison Then M.Status(sPoison) = 100
              If (P(x2).SuperNum <> 0) Then
                y2 = SuperDmgCt(x2, x1, M, y3)
                If x1 <= MaxPlayers Then
                  If P(x1).Rune = RuneEvade And Rand(1, 100) <= 15 Then
                    Send (P(x1).ScrNam + " teleports out of the way and laughs!")
                    If P(x2).Rune = RuneMagic Then
                      Send ("But " + P(x2).ScrNam + "'s Rune of Magic automatically redirects the attack!")
                    Else
                      y2 = 0
                    End If
                  End If
                End If
                If P(x2).Super < P(x2).SuperNum * 100 Then
                  Send (P(x2).ScrNam + " doesn't have enough SP to finish the move! (has " + TrimStr(P(x2).Super) + ", requires " + TrimStr(P(x2).SuperNum) * 100) + ")"
                  y2 = 0
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
                  If P(x1).CurMove = pBlock Then y2 = Int(y2 / 2)
                  If P(x1).Status(sInvin) Then
                    If P(x2).Rune = RuneMagic Then
                      Send (P(x2).ScrNam + "'s Rune of Magic ignores " + P(x1).ScrNam + "'s shield!")
                    Else
                      y2 = Int(y2 / 3)
                    End If
                  End If
                  If P(x1).Rune = RuneSPGuard And P(x2).Rune <> RuneMagic Then
                    y2 = Int(y2 * 3 / 4)
                    'Send "Debug note: The Super Guard Rune worked."
                  End If
                  DoEvents
                  SPWorked = False
                  If P(x1).Rune = RuneSPSwitch And P(x2).Rune <> RuneMagic And Rand(1, 4) = 1 And P(x1).Super > 0 Then
                    Send (P(x1).ScrNam + " took the damage on their Super Meter instead!")
                    SPWorked = True
                    P(x1).Super = P(x1).Super - y2
                    If P(x1).Super < 0 Then P(x1).Super = 0
                  Else
                    P(x1).HP = P(x1).HP - y2
                  End If
                  Send (Parse(M.SuperHit + " [" + TrimStr(y2) + "]", SN, TrimStr(y3), T, ""))
                  y3 = 0
                  If P(x2).Rune = RuneDesp And P(x2).HP < 0.16 * P(x2).MaxHP And P(x1).Rune <> RuneMagic Then
                    y3 = Int(y2 * Rand(0, 200) / 100)
                    P(x1).HP = P(x1).HP - y3
                    Send (P(x2).ScrNam + " does an additional " + TrimStr(y3) + " damage in desperation!")
                  End If
                  If P(x2).Rune = RuneCheese And P(x1).Cheese > 350 And P(x1).Rune <> RuneMagic Then
                    y3 = Int((P(x1).Cheese - 350) * MaxHP / 650)
                    P(x1).HP = P(x1).HP - y3
                    Send ("Since " + P(x1).ScrNam + " cheeses so much, " + P(x2).ScrNam + " does an additional " + TrimStr(y3) + " damage!")
                  End If
                  Survival x2, x1
                  If P(x2).CharID = KamekID And pYoshi = 0 And P(x1).HP <= 0 Then P(x1).HP = 1
                  If P(x1).CurMove <> pBlock And P(x1).Status(sInvin) = 0 Then Call CheckStatus(M.Status(), x1, x2, 1)
                  If y2 > 0 And SPWorked = 0 Then
                    If P(x1).Rune = RuneSuper And P(x2).Rune <> RuneMagic Then
                      P(x1).Super = P(x1).Super + Abs(Int(y2))
                    ElseIf SPWorked = 0 Then
                      P(x1).Super = P(x1).Super + Abs(Int(y2 * 2 / 3))
                    End If
                  End If
                  tHP = tHP + y2
                  If P(x2).Rune = RuneDrain Then
                    If P(x2).HP + (y2 / 2) <= P(x2).MaxHP Then
                      P(x2).HP = P(x2).HP + y2
                    ElseIf P(x2).HP < P(x2).MaxHP Then
                      P(x2).HP = P(x2).MaxHP
                    End If
                  End If
                  If P(x1).HP <= 0 Then
                    'If X2 = PKamek Then Stop
                    Send (Parse(DATASET.SuperKill, P(x2).ScrNam, TrimStr(y3), P(x1).ScrNam, UCase(M.name)))
                    If x2 <> x1 Then P(x2).Frags = P(x2).Frags + 1
                    IncWins (x2)
                    SayKillStr x2, x1
                    Blossom x1, x2
                    If P(x2).Rune = RuneFatal And P(x1).HP > -(MaxHP / 6) And P(x1).Rune <> RuneMagic Then
                      P(x1).HP = 35
                      Send (P(x2).ScrNam + " moves in for the kill!!!")
                      P(x2).CurMove = pFatal
                      P(x2).MoveStart = -999
                      Check2MoveDmg x2, 1
                    End If
                  End If
                  If (P(x1).Status(sSleep) Or P(x1).Status(sStun)) And M.Element = Phys Then
                    RemoveStatus x1, sSleep
                    RemoveStatus x1, sStun
                    P(x1).CurMove = -255
                    Send (P(x1).ScrNam + " is jolted awake!")
                  End If
                End If
                XRunes x2, x1
                If x1 <= MaxPlayers Then
                  If P(x1).Rune = RuneTheft And P(x1).HP > 0 Then
                    StealSub x1, x2
                  End If
                End If
                P(x2).Super = P(x2).Super - (P(x2).SuperNum * 100)
                tSP = tSP + (P(x2).SuperNum * 100)
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
                    If P(x0).Rune = RuneEvade And Rand(1, 100) <= 25 And M.Target <> Ally And M.Target <> OnlySelf And M.Target <> Allfriend Then
                      Send (P(x0).ScrNam + " teleports out of the way and laughs!")
                      If P(x2).Rune = RuneMagic Then
                        Send ("But " + P(x2).ScrNam + "'s Rune of Magic automatically redirects the attack!")
                      Else
                        y2 = 0
                      End If
                    End If
                    BlockYes = 0
                    If P(x0).CurMove = pBlock And M.Status(HitRate) <> -1 Then
                      y3 = Rand(1, 3)
                      If (y3 = 1) Then
                        If Senshi(P(x0).CharID).BlockYes <> "" Then
                          BlockYes = True
                          Send (Parse(Senshi(P(x0).CharID).BlockYes, P(x0).ScrNam, "", SN, ""))
                        End If
                        y2 = 0
                      ElseIf (y3 = 2) Or (y3 = 3) Then
                        y2 = Int(y2 / 2)
                      Else
                        Send (Parse(Senshi(P(x0).CharID).BlockFail, P(x0).ScrNam, "", SN, ""))
                      End If
                      DoEvents
                    End If
                    If P(x0).Status(sInvin) Then
                      y3 = Rand(1, 5)
                      If (y3 = 1) And x0 <> PKamek Then
                        Send (P(x0).ScrNam + "'s shield collapses!")
                        P(x0).Status(sInvin) = 0
                      ElseIf P(x2).Rune = RuneMagic Then
                        Send (P(x2).ScrNam + "'s Rune of Magic ignores " + P(x0).ScrNam + "'s shield!")
                      Else
                        y2 = 0
                      End If
                    End If
                    DoEvents
                    If P(x0).CurMove = -255 And P(x0).Rune = RuneCtrGuard And P(x2).Rune <> RuneMagic And Rand(1, 3) = 1 Then
                      Send (P(x0).ScrNam + " teleported away from " + P(x2).ScrNam + "'s counterattack!")
                      y2 = 0
                    End If
                    
                    If (y2 <> 0 Or P(x0).Status(sZombie) <> 0) Then
                      Select Case y2
                        Case Is > 0: Msg = " [" + TrimStr(y2) + "]"
                        Case 0: Msg = " [miss]"
                        Case Is < 0: Msg = " [Absorbed " + TrimStr(Abs(y2)) + "]"
                      End Select
                      If (Rand(1, 20) = 10) Or (Rand(1, 5) = 3 And P(x2).Rune = RuneCrit And P(x0).Rune <> RuneMagic) Then
                        y2 = y2 * 2
                        If (M.Crithit <> "") Then
                          If MultiHit(M.Target) Then
                            SNCrit = SNCrit + P(x0).ScrNam + Msg + ", "
                          Else
                            Msg = Parse(M.Crithit, SN, "", P(x0).ScrNam, "") + Msg
                            Send Msg
                          End If
                        ElseIf M.Hit <> "" Then
                          If MultiHit(M.Target) Then
                            SNHit = SNHit + P(x0).ScrNam + Msg + ", "
                          Else
                            Msg = Parse(M.Hit, SN, "", P(x0).ScrNam, "") + Msg
                            Send Msg
                          End If
                        End If
                      ElseIf M.Hit <> "" Then
                        If MultiHit(M.Target) Then
                          SNHit = SNHit + P(x0).ScrNam + Msg + ", "
                        Else
                          Msg = Parse(M.Hit, SN, "", P(x0).ScrNam, "") + Msg
                          Send Msg
                        End If
                      End If
                      DoEvents
                      DoEvents
                      Avg = Avg + y2
                      Count = Count + 1
                      SPWorked = 0
                      If P(x0).Rune = RuneSPSwitch And P(x2).Rune <> RuneMagic And Rand(1, 4) = 1 And P(x0).Super > 0 Then
                        Send (P(x0).ScrNam + " took the damage on their Super Meter instead!")
                        SPWorked = True
                        P(x0).Super = P(x0).Super - y2
                        If P(x0).Super < 0 Then P(x0).Super = 0
                      Else
                        P(x0).HP = P(x0).HP - y2
                      End If
                      If P(x2).Rune = RuneDesp And P(x2).HP < 0.16 * P(x2).MaxHP And P(x1).Rune <> RuneMagic Then
                        y3 = Int(y2 * Rand(0, 200) / 100)
                        P(x1).HP = P(x1).HP - y3
                        Send (P(x2).ScrNam + " does an additional " + TrimStr(y3) + " damage in desperation!")
                      End If
                      If P(x2).Rune = RuneCheese And P(x0).Cheese > 350 And P(x0).Rune <> RuneMagic Then
                        y3 = Int((P(x0).Cheese - 350) * MaxHP / 650)
                        P(x0).HP = P(x0).HP - y3
                        Send ("Since " + P(x0).ScrNam + " cheeses so much, " + P(x2).ScrNam + " does an additional " + TrimStr(y3) + " damage!")
                      End If
                      Survival x2, x0
                      If P(x2).CharID = KamekID And pYoshi = 0 And P(x0).HP <= 0 Then P(x0).HP = 1
                      If P(x0).HP <= 0 Then
                        'If X2 = PKamek Then Stop
                        If x2 <> x0 Then P(x2).Frags = P(x2).Frags + 1
                        IncWins (x2)
                        SayKillStr x2, x0
                        Blossom x0, x2
                        If P(x2).Rune = RuneFatal And P(x0).HP + y2 > -(MaxHP / 6) And P(x0).Rune <> RuneMagic And P(x2).CharID <> KamekID Then
                          P(x0).HP = 35
                          Send (P(x2).ScrNam + " moves in for the kill!!!")
                          P(x2).CurMove = pFatal
                          P(x2).MoveStart = -999
                          Check2MoveDmg x2, 1
                        End If
                      End If
                      tHP = tHP + y2
                      If P(x2).Rune = RuneDrain Then
                        If P(x2).HP + (y2 / 2) <= P(x2).MaxHP Then
                          P(x2).HP = P(x2).HP + y2
                        ElseIf P(x2).HP < P(x2).MaxHP Then
                          P(x2).HP = P(x2).MaxHP
                        End If
                      End If
                      If P(x0).Rune = RuneSuper And P(x2).Rune <> RuneMagic And SPWorked = 0 Then
                        P(x0).Super = P(x0).Super + Abs(Int(y2))
                      ElseIf SPWorked = 0 Then
                        P(x0).Super = P(x0).Super + Abs(Int(y2 * 2 / 3))
                      End If
                      If (y2 = 0) And SPWorked = 0 Then
                        If P(x0).Rune = RuneSuper And P(x2).Rune <> RuneMagic Then
                          P(x0).Super = P(x0).Super + Abs(Int(M.ElementStr / 1.5))
                        ElseIf SPWorked = 0 Then
                          P(x0).Super = P(x0).Super + Abs(Int(M.ElementStr / 3))
                        End If
                      End If
                      If P(x0).Super > MaxSP Then P(x0).Super = MaxSP
                      If (y2 > 0) And P(x0).CurMove <> pBlock Then
                        'If P(X2).Rune = RuneMagic Then
                        '  P(x0).God = -666
                        'End If
                        Call CheckStatus(M.Status(), x0, x2, 0)
                      End If
                      DoEvents
                    ElseIf BlockYes = 0 Then
                      If MultiHit(M.Target) Then
                        SNMiss = SNMiss + P(x0).ScrNam + ", "
                      Else
                        Send (Parse(M.Miss, SN, "", P(x0).ScrNam, ""))
                      End If
                    End If
                    If (P(x0).Status(sSleep) Or P(x0).Status(sStun)) And M.Element = Phys And M.Status(sSleep) = 0 And M.Status(sStun) = 0 Then
                      RemoveStatus x0, sSleep
                      RemoveStatus x0, sStun
                      P(x0).CurMove = -255
                      Send (P(x0).ScrNam + " is jolted awake!")
                    End If
                    XRunes x2, x0
                    If x0 <= MaxPlayers Then
                      If P(x0).Rune = RuneTheft And P(x0).HP > 0 Then
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
                    'SNHit = MakeEnglish(SNHit)
                    Send (Parse2(M.Hit + " [" + TrimStr(Avg) + "]", SN, "", SNHit, ""))
                  End If
                  If SNCrit <> "" Then
                    'SNCrit = MakeEnglish(SNCrit)
                    Send (Parse2(M.Crithit, SN, "", SNCrit, ""))
                  End If
                  If SNMiss <> "" Then
                    'SNMiss = MakeEnglish(SNMiss)
                    Send (Parse2(M.Miss, SN, "", SNMiss, ""))
                  End If
                  If M.HealSelf = "" Then M.HealSelf = "Damage caused by %SN's " + M.name + " :"
                  Send (Parse(M.HealSelf + " [" + TrimStr(Avg) + " avg, " + TrimStr(x5) + " tot]", SN, "", T, ""))
                End If
                'p(x2).CurMove = -255
                If Avg > 0 Then
                  P(x2).Super = P(x2).Super + Abs(Int(Avg * 2 / 3))
                Else
                  P(x2).Super = P(x2).Super + Abs(Int(M.ElementStr / 3))
                End If
                If (P(x2).Super > MaxSP) Then P(x2).Super = MaxSP
                If PKamek = 0 Then
                  P(x2).Cheese = P(x2).Cheese + Int(x5 * M.Status(HitRate) / 100)
                End If
                'P(x2).ChargeLevel = P(x2).ChargeLevel - ChargesRequired(M)
              End If
            Case 254:
              Send (Parse(M.Begin2HealSelf, SN, "", T, ""))
              For x0 = 1 To MaxPlayers
                DoEvents
                Okay = (IsThisATarget(x2, x0)) And (P(x0).Status(sBarrier) <> 0 Or P(x0).Status(sMBarrier) <> 0)
                y2 = -1
                If P(x0).CurMove = pBlock Or P(x0).Status(sInvin) Then
                  Select Case Rand(1, 2)
                    Case 1: y2 = 0
                    Case 2: y2 = -1
                  End Select
                End If
                If P(x0).CurMove = -255 And P(x0).Rune = RuneCtrGuard And P(x2).Rune <> RuneMagic And Rand(1, 3) = 1 Then
                  Send (P(x0).ScrNam + " teleported away from " + P(x2).ScrNam + "'s counterattack!")
                  y2 = 0
                End If
                If Rand(0, 99) > M.Status(HitRate) And M.Status(HitRate) <> -1 Then y2 = 0
                If Okay And y2 Then
                  For x9 = 1 To sMaxStatus
                    Lemon(x9) = 0
                  Next x9
                  If M.Hit <> "" Then Send Parse(M.Hit, P(x2).ScrNam, "", P(x0).ScrNam, "")
                  Select Case Rand(1, 9)
                    Case 1: Lemon(sMute) = 100
                    Case 2: Lemon(sChaos) = 100
                    Case 3: Lemon(sFreeze) = 100
                    Case 4: Lemon(sBlind) = 100
                    Case 5: Lemon(sSlow) = 100
                    Case 6: Lemon(sStun) = 100
                    Case 7: Lemon(sStop) = 100
                    Case 8: Lemon(sSleep) = 100
                    Case 9: Lemon(sCurse) = 100
                  End Select
                  CheckStatus Lemon(), x0, x2, 0
                Else
                  Send Parse(M.Miss, P(x2).ScrNam, "", P(x0).ScrNam, "")
                End If
              Next x0
            Case NoDmg:
              x8 = 0
              If (M.Target = Ally Or M.Target = Allfriend Or M.Target = OnlySelf) And (P(x2).Target = x2) And (M.HealSelf <> "") Then
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
                  If (M.Target <> Ally And M.Target <> Allfriend And M.Target <> OnlySelf) Then
                    If P(x0).Rune = RuneEvade And Rand(1, 100) <= 25 Then
                      Send (P(x0).ScrNam + " teleports out of the way and laughs!")
                      If P(x2).Rune = RuneMagic Then
                        Send ("But " + P(x2).ScrNam + "'s Rune of Magic automatically redirects the attack!")
                      Else
                        y2 = 0
                      End If
                    End If
                  End If
                  BlockYes = 0
                  If P(x0).CurMove = pBlock And (M.Target <> Ally And M.Target <> Allfriend) And M.Status(HitRate) <> -1 Then
                    y3 = Rand(1, 3)
                    If (y3 <> 1) Then
                      If Senshi(P(x0).CharID).BlockYes <> "" Then
                        Send (Parse(Senshi(P(x0).CharID).BlockYes, P(x0).ScrNam, "", SN, ""))
                        BlockYes = True
                      End If
                      y2 = 0
                    Else
                      Send (Parse(Senshi(P(x0).CharID).BlockFail, P(x0).ScrNam, "", SN, ""))
                    End If
                  End If
                  If P(x0).Status(sInvin) And (M.Target <> Ally And M.Target <> Allfriend) Then
                    y3 = Rand(1, 5)
                    If (y3 = 1) And x0 <> PKamek Then
                      Send (P(x0).ScrNam + "'s shield collapses!")
                      P(x0).Status(sInvin) = 0
                    ElseIf P(x2).Rune = RuneMagic Then
                      Send (P(x2).ScrNam + "'s Rune of Magic ignores " + P(x0).ScrNam + "'s shield!")
                    Else
                      'Send (p(x0).ScrNam + "'s shield cannot be penetrated!")
                      y2 = 0
                    End If
                  End If
                  If P(x0).CurMove = -255 And P(x0).Rune = RuneCtrGuard And P(x2).Rune <> RuneMagic And Rand(1, 3) = 1 And M.Target <> Ally And M.Target <> Allfriend And M.Target <> OnlySelf Then
                    Send (P(x0).ScrNam + " teleported away from " + P(x2).ScrNam + "'s counterattack!")
                    y2 = 0
                  End If
                  If Rand(1, 200) < P(x0).MagDef And M.Target <> Ally And M.Target <> Allfriend And M.Target <> OnlySelf And M.Status(HitRate) <> -1 Then y2 = False
                  If Rand(0, 99) > M.Status(HitRate) And M.Status(HitRate) <> -1 Then y2 = 0
                  If (y2 = True) Then
                    If (M.Target = Enemy Or M.Target = Ally Or M.Target = OnlySelf) And (x8 = 0) Then Send (Parse(M.Hit, SN, "", P(x0).ScrNam, ""))
                    'If P(X2).Rune = RuneMagic Then
                    '  P(x0).God = -666
                    'End If
                    Call CheckStatus(M.Status(), x0, x2, 0)
                  ElseIf BlockYes = 0 Then
                    Send (Parse(M.Miss, SN, "", P(x0).ScrNam, ""))
                  End If
                  XRunes x2, x0
                  If x0 <= MaxPlayers And (M.Target = Enemy Or M.Target = Everybody Or M.Target = AllFoe Or M.Target = AllTeam) Then
                    If P(x0).Rune = RuneTheft And P(x0).HP > 0 Then
                      StealSub x0, x2
                    End If
                  End If
                End If
              Next x0
              'p(x2).CurMove = -255
            Case Morph:
              If M.Hit <> "" Then
                Send (Parse(M.Hit, P(x2).ScrNam, "", Senshi(P(x2).Target).FullName, ""))
              ElseIf M.HealSelf <> "" Then
                Send (Parse(M.HealSelf, P(x2).ScrNam, "", Senshi(P(x2).Target).FullName, ""))
              Else
                Send (Parse("%SN successfully morphed into %T.", P(x2).ScrNam, "", Senshi(P(x2).Target).FullName, ""))
              End If
              P(x2).Status(sMorph) = XTimer
              'p(x2).CurMove = -255
              temp = True
              If (P(x2).OldCharID = 0) Then P(x2).OldCharID = P(x2).CharID
              P(x2).CharID = P(x2).Target
              x7 = P(x2).CharID
              x8 = P(x2).OldCharID
              P(x2).PhysStr = Senshi(x7).PhysStr
              P(x2).PhysDef = Senshi(x7).PhysDef
              P(x2).MagDef = Senshi(x7).MagStr
              P(x2).MagStr = Senshi(x7).MagDef
              For x7 = 1 To MaxMoves - 1
                DoEvents
                P(x2).Moves(x7) = Moves(Senshi(P(x2).CharID).Moves(x7))
              Next x7
              Call CheckStatus(M.Status(), x2, x2, 0)
              'p(x2).CurMove = -255
            'Case MPTheft:
            '  Send ("MP Theft is obsolete and no longer supported.")
            Case HPTheft, SPTheft, MPTheft:
              X3 = P(x1).HP
              x4 = P(x1).MP
              x5 = P(x1).Super
              If M.Element = SPTheft Then
                y2 = Rand(x5 / 4, x5 / 1.5)
              Else
                y2 = Rand(Int(M.ElementStr / 3), M.ElementStr)
              End If
              If P(x1).Rune = RuneEvade And Rand(1, 100) <= 25 Then
                Send (P(x1).ScrNam + " teleports out of the way and laughs!")
                If P(x2).Rune = RuneMagic Then
                  Send ("But " + P(x2).ScrNam + "'s Rune of Magic automatically redirects the attack!")
                Else
                  y2 = 0
                End If
              End If
              If P(x1).Rune = RuneSwiss Then
                Send (P(x1).ScrNam + "'s Swiss Rune protects them from the attack!")
                y2 = 0
              End If
              BlockYes = 0
              If P(x1).CurMove = pBlock And M.Status(HitRate) <> -1 Then
                y3 = Rand(1, 3)
                If (y3 = 1) Then
                  If Senshi(P(x1).CharID).BlockYes <> "" Then
                    Send (Parse(Senshi(P(x1).CharID).BlockYes, P(x1).ScrNam, "", SN, ""))
                    BlockYes = True
                  End If
                  y2 = 0
                ElseIf (y3 = 2) Or (y3 = 3) Then
                  y2 = Int(y2 / 2)
                Else
                  Send (Parse(Senshi(P(x1).CharID).BlockFail, P(x1).ScrNam, "", SN, ""))
                End If
              End If
              If P(x1).Status(sInvin) Then
                y3 = Rand(1, 5)
                If (y3 = 1) And x0 <> PKamek Then
                  Send (P(x1).ScrNam + "'s shield collapses!")
                  P(x1).Status(sInvin) = 0
                ElseIf P(x2).Rune = RuneMagic Then
                  Send (P(x2).ScrNam + "'s Rune of Magic ignores " + P(x1).ScrNam + "'s shield!")
                Else
                  y2 = 0
                End If
              End If
              If Rand(0, 99) > M.Status(HitRate) And M.Status(HitRate) <> -1 Then y2 = 0
              If P(x1).CurMove = -255 And P(x1).Rune = RuneCtrGuard And P(x2).Rune <> RuneMagic And Rand(1, 3) = 1 Then
                Send (P(x1).ScrNam + " teleported away from " + P(x2).ScrNam + "'s counterattack!")
                y2 = 0
              End If
              If (M.Element = HPTheft) Then
                If (P(x2).HP + y2 > P(x2).MaxHP) And (P(x2).HP <= MaxHP) Then y2 = P(x2).MaxHP - P(x2).HP
                If (X3 - y2 <= 0) Then y2 = X3
                X3 = X3 - y2
                'If X3 = P(x1).HP And P(X2).CharID = KamekID Then X3 = X3 + 1
                tHP = tHP + y2
                P(x2).HP = P(x2).HP + y2
              ElseIf (M.Element = MPTheft) Then
                If (P(x2).MP + y2 > MaxMP) And (P(x2).MP <= MaxMP) Then y2 = MaxMP - P(x2).MP
                If (x4 - y2 <= 0) Then y2 = x4
                x4 = x4 - y2
                P(x2).MP = P(x2).MP + y2
              Else
                'If (p(x2).SP + y2 > MaxSP) And (p(x2).MP <= MaxSP) Then y2 = MaxSP - p(x2).Super
                'If (x5 - y2 <= 0) Then y2 = x5
                x5 = x5 - y2
                P(x2).Super = P(x2).Super + Abs(y2)
              End If
              P(x1).HP = X3
              Survival x2, x1
              If x2 = PKamek And pYoshi = 0 And P(x1).HP <= 0 Then P(x1).HP = 1
              If (P(x1).HP <= 0) Then
                If x2 = PKamek Then Stop
                If x2 <> x1 Then P(x2).Frags = P(x2).Frags + 1
                IncWins (x2)
                SayKillStr x2, x1
                Blossom x1, x2
                If P(x2).Rune = RuneFatal And P(x1).HP > -(MaxHP / 6) And P(x1).Rune <> RuneMagic Then
                  P(x1).HP = 35
                  Send (Parse(M.Hit, SN, "", T, ""))
                  Send (P(x2).ScrNam + " moves in for the kill!!!")
                  P(x2).CurMove = pFatal
                  P(x2).MoveStart = -999
                  Check2MoveDmg x2, 1
                End If
              End If
              P(x1).MP = x4
              P(x1).Super = x5
              If y2 <= 0 Then
                If BlockYes = 0 Then Send (Parse(M.Miss, SN, "", T, ""))
              Else
                If M.Element = HPTheft Then
                  Send (Parse(M.Hit + " [" + TrimStr(y2) + " HP]", SN, "", T, ""))
                ElseIf M.Element = MPTheft Then
                 Send (Parse(M.Hit + " [" + TrimStr(y2) + " MP]", SN, "", T, ""))
                ElseIf M.Element = SPTheft Then
                  Send (Parse(M.Hit + " [" + TrimStr(y2) + " SP]", SN, "", T, ""))
                End If
              End If
              If (y2 > 0) And P(x1).CurMove <> pBlock Then
                'If P(X2).Rune = RuneMagic Then
                '  P(x0).God = -666
                'End If
                Call CheckStatus(M.Status(), x1, x2, 0)
              End If
              XRunes x2, x1
              If x1 <= MaxPlayers Then
                If P(x1).Rune = RuneTheft And P(x1).HP > 0 Then
                  StealSub x1, x2
                End If
              End If
              If PKamek = 0 Then P(x2).Cheese = P(x2).Cheese + y2
              'p(x2).CurMove = -255
            Case Life:
              Select Case P(x1).HP
                Case Is > 0:
                  If P(x1).Status(sZombie) Then
                    Send (T + " is brought back to life, killing them!")
                    P(x1).HP = 0
                    Survival x2, x1
                  Else
                    Send5 ("Whoops! " + T + " is already alive!")
                    CheckStatus M.Status(), x1, x2, 0
                  End If
                Case -666: Send5 (T + " sacrificed their life and cannot be revived.")
                Case -777: Send5 (T + " has fled from battle.")
                Case Else:
                  Send (Parse(M.Hit, SN, "", T, ""))
                  P(x1).HP = Int(P(x2).MaxHP / 1.7)
                  CheckStatus M.Status(), x1, x2, 0
              End Select
              XRunes x2, x1
              'p(x2).CurMove = -255
            Case 103:
              y2 = 2
              If (x1 <= MaxPlayers) Then
                If P(x1).Rune = RuneEvade And Rand(1, 100) <= 25 Then
                  Send (P(x1).ScrNam + " teleports out of the way and laughs!")
                  If P(x2).Rune = RuneMagic Then
                    Send ("But " + P(x2).ScrNam + "'s Rune of Magic automatically redirects the attack!")
                  Else
                    y2 = 0
                  End If
                End If
              Else
                GoTo nodemihere
              End If
              BlockYes = 0
              If P(x1).CurMove = pBlock And M.Status(HitRate) <> -1 Then
                y3 = Rand(1, 3)
                If (y3 = 1) Then
                  If Senshi(P(x0).CharID).BlockYes <> "" Then
                    Send (Parse(Senshi(P(x0).CharID).BlockYes, P(x0).ScrNam, "", SN, ""))
                    BlockYes = True
                  End If
                Else
                  Send (Parse(Senshi(P(x1).CharID).BlockFail, P(x1).ScrNam, "", SN, ""))
                End If
              End If
              If P(x1).Status(sInvin) Then
                y3 = Rand(1, 5)
                If (y3 = 1) And x0 <> PKamek Then
                  Send (P(x1).ScrNam + "'s shield collapses!")
                  P(x1).Status(sInvin) = 0
                ElseIf P(x2).Rune = RuneMagic Then
                  Send (P(x2).ScrNam + "'s Rune of Magic ignores " + P(x0).ScrNam + "'s shield!")
                Else
                  y2 = 0
                End If
              End If
              If (x1 <= MaxPlayers) Then If P(x1).God Then y2 = 0
              If Rand(0, 99) > M.Status(HitRate) And M.Status(HitRate) <> -1 Then y2 = 0
              If (y2 = 2) And (x1 <= MaxPlayers) Then
                'y2 = Int(P(X1).HP / 2)
                'y2 = Int(y2 * Arena(CurArena).Effect(Demi))
                y2 = M.ElementStr
                'P(X1).HP = Int(P(X1).HP / 2)
                Send (Parse(M.Hit + " [" + TrimStr(y2) + "]", SN, "", T, ""))
                tHP = tHP + y2
                'If P(X2).Rune = RuneDrain Then
                  'If P(X2).HP + (y2 / 2) <= P(X2).MaxHP Then
                    P(x1).HP = P(x1).HP - y2
                    Survival x2, x1
                  'ElseIf P(X2).HP < P(X2).MaxHP Then
                    'P(X2).HP = P(X2).MaxHP
                  'End If
                'End If
                P(x1).Super = P(x1).Super + Abs(y2)
              ElseIf BlockYes = 0 Then
                Send (Parse(M.Miss, SN, "", T, ""))
              End If
              If (P(x1).HP <= 0) Then
                If x2 = PKamek Then Stop
                If x2 <> x1 Then P(x2).Frags = P(x2).Frags + 1
                IncWins (x2)
                SayKillStr x2, x1
                Blossom x1, x2
                If P(x2).Rune = RuneFatal And P(x1).HP > -(MaxHP / 6) And P(x1).Rune <> RuneMagic Then
                  P(x1).HP = 35
                  Send (Parse(M.Hit, SN, "", T, ""))
                  Send (P(x2).ScrNam + " moves in for the kill!!!")
                  P(x2).CurMove = pFatal
                  P(x2).MoveStart = -999
                  Check2MoveDmg x2, 1
                End If
              End If
              XRunes x2, x1
              If x1 <= MaxPlayers Then
                If P(x1).Rune = RuneTheft And P(x1).HP > 0 Then
                  StealSub x1, x2
                End If
              End If
            'Case HPBased:
            Case -2576:
              y2 = 2
              If (x1 <= MaxPlayers) Then
                If P(x1).Rune = RuneEvade And Rand(1, 100) <= 25 Then
                  Send (P(x1).ScrNam + " teleports out of the way and laughs!")
                  If P(x2).Rune = RuneMagic Then
                    Send ("But " + P(x2).ScrNam + "'s Rune of Magic automatically redirects the attack!")
                  Else
                    y2 = 0
                  End If
                End If
              Else
                GoTo nodemihere
              End If
              BlockYes = 0
              If P(x1).CurMove = pBlock And M.Status(HitRate) <> -1 Then
                y3 = Rand(1, 3)
                If (y3 = 1) Then
                  If Senshi(P(x0).CharID).BlockYes <> "" Then
                    Send (Parse(Senshi(P(x0).CharID).BlockYes, P(x0).ScrNam, "", SN, ""))
                    BlockYes = True
                  End If
                Else
                  Send (Parse(Senshi(P(x1).CharID).BlockFail, P(x1).ScrNam, "", SN, ""))
                End If
              End If
              If P(x1).Status(sInvin) Then
                y3 = Rand(1, 5)
                If (y3 = 1) And x0 <> PKamek Then
                  Send (P(x1).ScrNam + "'s shield collapses!")
                  P(x1).Status(sInvin) = 0
                ElseIf P(x2).Rune = RuneMagic Then
                  Send (P(x2).ScrNam + "'s Rune of Magic ignores " + P(x0).ScrNam + "'s shield!")
                Else
                  y2 = 0
                End If
              End If
              If (x1 <= MaxPlayers) Then If P(x1).God Then y2 = 0
              If Rand(0, 99) > M.Status(HitRate) And M.Status(HitRate) <> -1 Then y2 = 0
              If (y2 = 2) And (x1 <= MaxPlayers) Then
                'y2 = Int(P(X1).HP / 2)
                'y2 = Int(y2 * Arena(CurArena).Effect(Demi))
                y2 = Int((P(x2).MaxHP - P(x2).HP) * 1.25)
                'P(X1).HP = Int(P(X1).HP / 2)
                Send (Parse(M.Hit + " [" + TrimStr(y2) + "]", SN, "", T, ""))
                tHP = tHP + y2
                'If P(X2).Rune = RuneDrain Then
                  'If P(X2).HP + (y2 / 2) <= P(X2).MaxHP Then
                    P(x1).HP = P(x1).HP - y2
                    Survival x2, x1
                  'ElseIf P(X2).HP < P(X2).MaxHP Then
                    'P(X2).HP = P(X2).MaxHP
                  'End If
                'End If
                P(x1).Super = P(x1).Super + Abs(y2)
              ElseIf BlockYes = 0 Then
                Send (Parse(M.Miss, SN, "", T, ""))
              End If
              If (P(x1).HP <= 0) Then
                If x2 = PKamek Then Stop
                If x2 <> x1 Then P(x2).Frags = P(x2).Frags + 1
                IncWins (x2)
                SayKillStr x2, x1
                Blossom x1, x2
                If P(x2).Rune = RuneFatal And P(x1).HP > -(MaxHP / 6) And P(x1).Rune <> RuneMagic Then
                  P(x1).HP = 35
                  Send (Parse(M.Hit, SN, "", T, ""))
                  Send (P(x2).ScrNam + " moves in for the kill!!!")
                  P(x2).CurMove = pFatal
                  P(x2).MoveStart = -999
                  Check2MoveDmg x2, 1
                End If
              End If
              XRunes x2, x1
              If x1 <= MaxPlayers Then
                If P(x1).Rune = RuneTheft And P(x1).HP > 0 Then
                  StealSub x1, x2
                End If
              End If
            Case Demi:
              y2 = 2
              If (x1 <= MaxPlayers) Then
                If P(x1).Rune = RuneEvade And Rand(1, 100) <= 25 Then
                  Send (P(x1).ScrNam + " teleports out of the way and laughs!")
                  If P(x2).Rune = RuneMagic Then
                    Send ("But " + P(x2).ScrNam + "'s Rune of Magic automatically redirects the attack!")
                  Else
                    y2 = 0
                  End If
                End If
              Else
                GoTo nodemihere
              End If
              BlockYes = 0
              If P(x1).CurMove = pBlock And M.Status(HitRate) <> -1 Then
                y3 = Rand(1, 3)
                If (y3 = 1) Then
                  If Senshi(P(x0).CharID).BlockYes <> "" Then
                    Send (Parse(Senshi(P(x0).CharID).BlockYes, P(x0).ScrNam, "", SN, ""))
                    BlockYes = True
                  End If
                Else
                  Send (Parse(Senshi(P(x1).CharID).BlockFail, P(x1).ScrNam, "", SN, ""))
                End If
              End If
              If P(x1).Status(sInvin) Then
                y3 = Rand(1, 5)
                If (y3 = 1) And x0 <> PKamek Then
                  Send (P(x1).ScrNam + "'s shield collapses!")
                  P(x1).Status(sInvin) = 0
                ElseIf P(x2).Rune = RuneMagic Then
                  Send (P(x2).ScrNam + "'s Rune of Magic ignores " + P(x0).ScrNam + "'s shield!")
                Else
                  y2 = 0
                End If
              End If
              If (x1 <= MaxPlayers) Then If P(x1).God Then y2 = 0
              If Rand(0, 99) > M.Status(HitRate) And M.Status(HitRate) <> -1 Then y2 = 0
              If (y2 = 2) And (x1 <= MaxPlayers) Then
                y2 = Int(P(x1).HP / 2)
                y2 = Int(y2 * Arena(CurArena).Effect(Demi))
                'P(X1).HP = Int(P(X1).HP / 2)
                P(x1).HP = P(x1).HP - y2
                Survival x2, x1
                Send (Parse(M.Hit + " [" + TrimStr(y2) + "]", SN, "", T, ""))
                tHP = tHP + y2
                If P(x2).Rune = RuneDrain Then
                  If P(x2).HP + (y2 / 2) <= P(x2).MaxHP Then
                    P(x2).HP = P(x2).HP + y2
                  ElseIf P(x2).HP < P(x2).MaxHP Then
                    P(x2).HP = P(x2).MaxHP
                  End If
                End If
                P(x1).Super = P(x1).Super + Abs(y2)
              ElseIf BlockYes = 0 Then
                Send (Parse(M.Miss, SN, "", T, ""))
              End If
              XRunes x2, x1
              If x1 <= MaxPlayers Then
                If P(x1).Rune = RuneTheft And P(x1).HP > 0 Then
                  StealSub x1, x2
                End If
              End If
nodemihere:
              'p(x2).CurMove = -255
            Case Reveal:
              y2 = Rand(1, 5)
              If M.Status(HitRate) = -1 Then y2 = 0
              If P(x1).CurMove = -255 And P(x1).Rune = RuneCtrGuard And P(x2).Rune <> RuneMagic And Rand(1, 3) = 1 Then
                Send (P(x1).ScrNam + " teleported away from " + P(x2).ScrNam + "'s counterattack!")
                y2 = 2
              End If
              If P(x1).Rune = RuneSwiss Then
                Send (P(x1).ScrNam + "'s Swiss Rune protects them from the attack!")
                y2 = 2
              End If
              If Rand(0, 99) > M.Status(HitRate) And M.Status(HitRate) <> -1 Then y2 = 2
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
              If M.Status(HitRate) = -1 Then y2 = 0
              If P(x1).CurMove = -255 And P(x1).Rune = RuneCtrGuard And P(x2).Rune <> RuneMagic And Rand(1, 3) = 1 Then
                Send (P(x1).ScrNam + " teleported away from " + P(x2).ScrNam + "'s counterattack!")
                y2 = 2
              End If
              If P(x1).Rune = RuneSwiss Then
                Send (P(x1).ScrNam + "'s Swiss Rune protects them from the attack!")
                y2 = 2
              End If
              If Rand(0, 99) > M.Status(HitRate) And M.Status(HitRate) <> -1 Then y2 = 2
              If (y2 <> 2) And (x1 <= MaxPlayers) And P(x1).CharID <> KamekID Then
                Send (Parse(M.Hit, SN, "", T, ""))
                S1x = Senshi(P(x1).CharID)
                If (x1 = x2) Then
                  y3 = Rand(1, MaxHidMoves)
                  If hm(y3).Cmdkey <> "" Then
                    P(x2).Moves(14) = hm(y3)
                    Msg = SN + " found a secret move, " + hm(y3).name + "! (" + P(x1).Moves(14).Cmdkey + ")"
                    If P(x1).Moves(14).CanSuper Then Msg = Msg + "*"
                    Msg = Msg + "!"
                    Send5 (Msg)
                  Else
                    Send ("But why steal your own move...??")
                  End If
                Else
                  x7 = 0
                  Do
                    y3 = Rand(1, 14)
                    x7 = x7 + 1
                    DoEvents
                  Loop Until (x7 > 100) Or (P(x1).Moves(y3).Cmdkey <> "")
                  P(x2).Moves(14) = P(x1).Moves(y3)
                  P(x2).Moves(14).Cmdkey = "!" + P(x2).Moves(14).Cmdkey
                  Msg = SN + " learned their move, " + P(x1).Moves(y3).name + "! (" + P(x1).Moves(y3).Cmdkey + ")"
                  If P(x1).Moves(y3).CanSuper Then Msg = Msg + "*"
                  Msg = Msg + " To use type /" + P(x2).Moves(14).Cmdkey
                  Send5 (Msg)
                End If
              Else
                Send (Parse(M.Miss, SN, "", T, ""))
              End If
              XRunes x2, x1
              'p(x2).CurMove = -255
            Case Else:
              Send ("Error -- Element #" + TrimStr(M.Element) + " not defined. Notify the creator of this character")
              P(x2).CurMove = -255
          End Select
          If (M.Target = 2 Or M.Target = 4 Or M.Target = 5 Or M.Target = 6 Or M.Target = 7) And M.Element <> Morph Then
            For x0 = 1 To MaxPlayers
              If IsThisATarget(x2, x0) And P(x0).CurMove = pBlock And P(x0).Status(sHamedo) = 0 Then
                CounterAttack x0, x2
              End If
            Next x0
          End If
          For x0 = 1 To sMaxStatus
            If PeepzStats(x0) <> "" Then
              X3 = 0
              For x5 = 1 To Len(PeepzStats(x0))
                If Mid$(PeepzStats(x0), x5, 2) = ", " Then X3 = X3 + 1
              Next x5
              'Msg = "(" + Left$(PeepzStats(x0), Len(PeepzStats(x0)) - 1) + ") "
              If X3 > 1 Then
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
                Case sReraise: Msg = Msg + "blessed with a fairy from above"
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
                Case sScarecrow: Msg = Msg + "Scarecrowized"
                Case Else: Msg = Msg + "causing this bot to error"
              End Select
              Send Msg
            End If
            PeepzStats(x0) = ""
          Next x0
          If PKamek = 0 Then
            For x0 = 1 To sMaxStatus
              Select Case x0
                Case sMute, sChaos, sFreeze, sStun, sMIA, sBerserk, sSleep, sStop, sMushroom:
                  If M.Status(x0) > 0 Then P(x2).Cheese = P(x2).Cheese + Int(0.5 * M.Status(x0))
                Case sPoison, sBlind, sSlow, sCurse:
                  If M.Status(x0) > 0 Then P(x2).Cheese = P(x2).Cheese + (0.5 * M.Status(x0))
                Case sHaste, sReraise, sRegen, sQuick, sBarrier, sMBarrier, sBless:
                  If M.Status(x0) = -1 Then P(x2).Cheese = Int(P(x2).Cheese + (25 * DMult / 100))
              End Select
            Next x0
          End If
          P(x2).CurMove = -255
          For X3 = 1 To MaxPlayers
            ShowStatus (X3)
            If P(X3).Status(sStun) Or P(X3).Status(sFreeze) Or P(X3).Status(sSleep) Then P(X3).CurMove = -50
          Next X3
        End If
        If M.DestroyWeapon = 1 Then
          P(x2).WpnUsesLeft = P(x2).WpnUsesLeft - 1
          If P(x2).WpnUsesLeft <= 0 And Weapons(P(x2).Weapon).NumUses > 0 Then DestroyWeapon (x2)
        End If
        If M.DestroyWeapon = 2 Then
          If Left$(Weapons(P(x2).Weapon).PickMe, 1) = "~" Then
            DestroyWeapon (x2)
          Else
            DropWeapon x2, "%SN's %T lands on the ground."
          End If
        End If
        If M.DestroyWeapon = 3 Then DestroyWeapon (x2)
    End Select
  End If
  P(x2).CurMove = P(x2).CurMove
  Exit Sub
End Sub

