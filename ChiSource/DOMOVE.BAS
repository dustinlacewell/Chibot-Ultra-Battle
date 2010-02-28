Attribute VB_Name = "DoMoveSub"
' DOMOVE.BAS
' Contains the vital DoMove sub

Option Explicit
Option Compare Text

Sub DOMOVE(ByVal x2%, ByVal T%, ByVal x4%, ByVal S7$)
' This is an important sub. All moves done need to call this sub to be
' done properly.
Dim x%, x1%, X3%, x5%, X6%, x7%, x8%
Dim SNum%, SPly%, Q%, Atkr$
Dim Y1%, y2%, y3%, Y4%
Dim S4$, S6$, SN$, TS$
Dim sID%
Dim Msg$
Dim Okay%
Dim M As MoveType
  DoEvents
  P(x2).Target = T
  P(x2).MoveSpawn = 0
  SN = P(x2).ScrNam
  sID = P(x2).CharID
  Select Case x4
    Case pDefect:
    ' Defect to another team???
      P(x2).CurMove = 0
      DoEvents
      Select Case T
        Case 1 To MaxPlayers:
          S4 = P(T).TeamID
          SN = P(T).ScrNam
          Okay = Not P(T).Defect
        Case 0:
          Exit Sub
      End Select
      DoEvents
      If (Okay) Then
        Send (Parse(DATASET.DefectSucc, P(x2).ScrNam, tName(Asc(S4)), S4, SN))
        P(x2).TeamID = S4
      Else
        Send (Parse(DATASET.DefectFail, P(x2).ScrNam, "", SN, ""))
      End If
    Case pGet:
    ' The /get move... done instantly
      P(x2).CurMove = 0
      X6 = Item2Get
      Item2Get = 0
      S6 = P(x2).ScrNam
      DoEvents
      If (X6 = 0) Then
        Send (P(x2).ScrNam + ", ain't nuthin' ta get foo!")
      Else
        Send5 (Parse(Items(X6).PlayerGet + " [" + TrimStr(Items(X6).PlayerHP) + "HP]", S6, "", "", S6))
        P(x2).HP = P(x2).HP + Items(X6).PlayerHP
        'P(x2).MP = P(x2).MP + Items(X6).PlayerMP
        CheckStatus Items(X6).PlayerStat(), x2, 0, 1
        ShowStatus x2
      End If
    Case pCharge:
      Send (Parse(DATASET.GoCharge, P(x2).ScrNam, "", "", ""))
      P(x2).MoveStart = XTimer
      P(x2).CurMove = pCharge
    Case pBlock:
      Msg = ""
      P(x2).SuperNum = 0
      If (P(x2).Moves(T).CanSuper > 1) And (P(x2).Moves(T).CanSuper < 6) Then
        If P(x2).Moves(T).CanSuper = 2 Then
          Send P(x2).ScrNam + ", this move must be done as a Super."
        Else
          Send P(x2).ScrNam + ", this move must be Supered to a minimum of Level " + TrimStr(P(x2).Moves(T).CanSuper - 1) + "."
        End If
        Exit Sub
      End If
      If T > 20 And XTimer - P(x2).RuneTemp <= SummonCharge * (Config.Lag / 1000) Then
        x5 = (SummonCharge * (Config.Lag / 1000)) - (XTimer - P(x2).RuneTemp)
        Send (P(x2).ScrNam + "'s Rune of Summoning is discharged. Wait for it to recharge before trying another summon. [" + TrimStr(x5) + " sec]")
        Exit Sub
      End If
      If (T = 0) Then
        P(x2).Target = 0
      Else
        P(x2).Target = T
        If P(x2).Status(sHamedo) Then
          Msg = Msg + ccBold + "<Chi~" + P(x2).Moves(T).name + ">"
        Else
          Msg = Msg + ccUnderline + "<Ctr~" + P(x2).Moves(T).name + ">"
        End If
      End If
      Msg = Msg + " " + Senshi(P(x2).CharID).Block
      TS = AttackMe$(x2)
      If TS = "" Then TS = "No threat"
      Msg = Msg + " [" + TS + "]"
      SNum = 9999
      SPly = 0
      For x = 1 To MaxPlayers
        If P(x).CurMove > 0 And P(x).CurMove <= MaxMoves And P(x).HP > 0 And P(x).CharID <> 0 Then
          Q = P(x).Moves(P(x).CurMove).Target
          If (P(x).Target = x2) And (Q <> 1) And (Q <> 3) And (Q <> 8) Then
            If P(x).Rune = RuneStealth Then
              If P(x2).Rune = RuneMagic And MoveHitsIn(x) < SNum Then
                SPly = x
                SNum = MoveHitsIn(x)
              End If
            Else
              If MoveHitsIn(x) < SNum Then
                SPly = x
                SNum = MoveHitsIn(x)
              End If
            End If
          End If
        End If
      Next x
      If SPly <> 0 Then Atkr = P(SPly).ScrNam
      If Atkr = "" Then Atkr = "nobody in particular"
      Send (Parse(Msg, P(x2).ScrNam, "", Atkr$, ""))
      P(x2).MoveStart = XTimer
      P(x2).CurMove = pBlock
      DoEvents
    Case pFlee:
    ' Flee from Battle
      P(x2).CurMove = pFlee
      P(x2).MoveStart = XTimer
      Send (Parse(DATASET.FleeAttempt, P(x2).ScrNam, "", "", ""))
    Case pRest:
    ' Resting
      P(x2).CurMove = pRest
      P(x2).MoveStart = XTimer
      Send (Parse(Senshi(P(x2).CharID).Rest, P(x2).ScrNam, "", "", ""))
      If P(x2).Status(sQuick) <> 0 Then
        P(x2).MoveStart = XTimer - 999
        P(x2).Status(sQuick) = 0
      End If
      DoEvents
    Case pFatal:
      If P(x2).Target <> x2 Then
        If Active(P(x2).Target) Then
          P(x2).CurMove = pFatal
          P(x2).MoveStart = XTimer + Rand(-2, 2)
          'If (p(x2).Target > MaxPlayers) Then
          '  SN = y(p(x2).Target - MaxPlayers).Name
          'Else
            SN = P(P(x2).Target).ScrNam
          'End If
          DoEvents
          Send (Parse(Senshi(P(x2).CharID).Fatality.PreFatal + " (fatality)", P(x2).ScrNam, "", SN, ""))
          If P(x2).Status(sQuick) <> 0 Then
            P(x2).MoveStart = XTimer - 999
            P(x2).Status(sQuick) = 0
          End If
        End If
      End If
    Case pTaunt:
    ' Taunt... works only on Youma
      P(x2).MoveStart = XTimer
      If Senshi(sID).Taunt(1) = "" And Senshi(sID).Taunt(2) = "" And Senshi(sID).Taunt(3) = "" And Senshi(sID).Taunt(4) = "" And Senshi(sID).Taunt(5) = "" Then
        Send (Senshi(sID).FullName + " has no taunt. Stupid character!")
      Else
        Do
          DoEvents
          X3 = Int(Rnd * 5) + 1
        Loop Until (Senshi(P(x2).CharID).Taunt(X3) <> "")
        Send (Parse(Senshi(P(x2).CharID).Taunt(X3), P(x2).ScrNam, "", "", ""))
      End If
      P(x2).CurMove = pTaunt
    Case pSlot:
      If T = 0 Then
        Send (P(x2).ScrNam + " cannot find a formidable opponent to attack.")
        Exit Sub
      End If
      Send (P(x2).ScrNam + " pulls the lever on a slot machine.")
      P(x2).MoveStart = XTimer
      P(x2).CurMove = pSlot
    Case 1 To MaxMoves:
    ' Another move
      DoEvents
      y3 = P(x2).CharID
      If T = 0 Then
        Send (P(x2).ScrNam + " cannot find a formidable opponent to attack.")
        Exit Sub
      End If
      If x4 > 20 And XTimer - P(x2).RuneTemp <= SummonCharge * (Config.Lag / 1000) Then
        x5 = (SummonCharge * (Config.Lag / 1000)) - (XTimer - P(x2).RuneTemp)
        Send (P(x2).ScrNam + "'s Rune of Summoning is discharged. Wait for it to recharge before trying another summon. [" + TrimStr(x5) + " sec]")
        Exit Sub
      End If
      Dim Cnt%, Tt%
      Cnt = 0
      Tt = 0
      x5 = ProjectedTotalDamage(x2, x4, P(x2).Target)
      If Not ((TLimit - (XTimer - TimeLimit)) <= 60 And (TLimit > 0)) Then
        If (P(x2).Cheese + Int(x5 * (P(x2).Moves(x4).Status(HitRate) / 100)) > MaxCheeseLimit * DMult / 100 And P(x2).SuperNum = 0 And x5 > 0) And (P(x2).Status(sPMS) = 0) And (P(x2).Status(sBerserk) = 0) And (P(x2).Status(sChaos) = 0) And (PKamek = 0) And (OnFire <> x2 Or FireKills < 3) Then
          Send ("Slow down, " + P(x2).ScrNam + "! Don't be too cheezy! (Projected Damage by that move: " + TrimStr(x5) + ". Current Cheese: " + TrimStr(P(x2).Cheese) + ".)")
          P(x2).CurMove = 0
          Exit Sub
        End If
      End If
      If M.MPReq > P(x2).MP Then
        Send ccColor + "04WARNING: Not enough MP required to complete move. Better have enough before the move hits! (has " + TrimStr(P(x2).MP) + ", requires " + TrimStr(M.MPReq) + ")"
      End If
      P(x2).MoveStart = XTimer + Rand(-2, 2)
      M = P(x2).Moves(x4)
      If M.Element <> Phys And P(x2).Status(sMute) <> 0 Then
        Send (P(x2).ScrNam + ", you cannot do this move while Mute.")
        Exit Sub
      End If
      If M.Element = Phys And P(x2).Status(sScarecrow) <> 0 Then
        Send P(x2).ScrNam + ", you can't do physical attacks as a scarecrow."
        Exit Sub
      End If
      'X5 = ChargesRequired(M)
      'If (P(x2).ChargeLevel < X5 And P(x2).SuperNum = 0 And X4 < 15) Then
      '  Send ("This move requires a Level " + TrimStr(X5) + " on your Charge Meter, " + SN + ", and you are only at Level " + TrimStr(P(x2).ChargeLevel) + ".")
      '  Exit Sub
      'End If
      If M.CanSuper >= 6 Then
        Msg = P(x2).ScrNam + ", this move must be used as a "
        Select Case M.CanSuper
          Case 6: Msg = Msg + "counterattack."
          Case 7: Msg = Msg + "Chibot Counter."
        End Select
        Send Msg
        Exit Sub
      End If
      If (M.CanSuper > 1) And (P(x2).SuperNum < M.CanSuper - 1) And (M.CanSuper < 6) Then
        If M.CanSuper = 2 Then
          Send P(x2).ScrNam + ", this move must be done as a Super."
        Else
          Send P(x2).ScrNam + ", this move must be Supered to a minimum of Level " + TrimStr(M.CanSuper - 1) + "."
        End If
        Exit Sub
      End If
      If P(x2).SuperNum > 0 And M.CanSuper = 0 Then
        Send P(x2).ScrNam + ", that move cannot be Supered."
        Exit Sub
      End If
      If (P(x2).SuperNum > 0) And (M.CanSuper = 1 Or P(x2).SuperNum >= M.CanSuper - 1) And (M.CanSuper <= 5) Then
        'If p(x2).SuperNum = 4 Then Send ("4---4---4---4")
        'If P(X2).SuperNum = 5 Then Send ("<b>DISCLAIMER:</b> Level 5's can be really powerful, but they could also be weak.")
        If P(x2).Rune = RuneStealth And P(T).Rune <> RuneMagic Then
          M.Begin2Attack = "Lev" + TrimStr(P(x2).SuperNum) + ": " + M.Begin2SuperAttack
        Else
          M.Begin2Attack = "Lev" + TrimStr(P(x2).SuperNum) + "/" + ScrNam(T) + ": " + M.Begin2SuperAttack
        End If
        Playwav ("limit")
        'Send ("{S limit}")
      End If
      ' "t" is the Playernum of the target
      If M.ReqAllUses <> 0 And P(x2).WpnUsesLeft < Weapons(P(x2).Weapon).NumUses Then
        Send (P(x2).ScrNam + ", this move requires all " + TrimStr(Weapons(P(x2).Weapon).NumUses) + " charges on your weapon.")
        P(x2).CurMove = 0
        Exit Sub
      End If
      Select Case M.Element
        Case Heal:
          If M.Begin2HealSelf = "" Then M.Begin2HealSelf = M.Begin2Attack
          Select Case T
            Case x2, 0:
              TS = (Parse(M.Begin2HealSelf, P(x2).ScrNam, "", "", ""))
            Case Else:
              TS = (Parse(M.Begin2Attack, P(x2).ScrNam, "", P(T).ScrNam, ""))
          End Select
        Case Morph:
          If M.Begin2Attack <> "" Then
            TS = (Parse(M.Begin2Attack, P(x2).ScrNam, "", S7$, ""))
          ElseIf M.Begin2HealSelf <> "" Then
            TS = (Parse(M.Begin2HealSelf, P(x2).ScrNam, "", S7$, ""))
          Else
            TS = (Parse("%SN begins to morph into %T.", P(x2).ScrNam, "", S7$, ""))
          End If
        Case Else:
          If P(x2).Rune = RuneStealth And P(T).Rune <> RuneMagic Then
            TS = (Parse(M.Begin2Attack, P(x2).ScrNam, "", "someone", ""))
          ElseIf P(x2).Rune = RunePre Then
            Select Case M.Element
              Case Poison, Grass, Rock, Dirt, Psychic, Ghost, Phys, MoonE, Shadow, Water, Fire, Lit, Heart, Earth, Wind, Ki, Lum:
                Select Case M.Target
                  Case Enemy, OnlySelf, Ally:
                    TS = (Parse(M.Begin2Attack + " [est. " + TrimStr(x5) + " total]", P(x2).ScrNam, "", P(T).ScrNam, ""))
                  Case Else:
                    TS = (Parse(M.Begin2Attack + " [est. " + TrimStr(x5) + " total]", P(x2).ScrNam, "", P(T).ScrNam, ""))
                End Select
              Case Else:
                TS = (Parse(M.Begin2Attack + " [est. ???]", P(x2).ScrNam, "", P(T).ScrNam, ""))
            End Select
          Else
            TS = (Parse(M.Begin2Attack, P(x2).ScrNam, "", P(T).ScrNam, ""))
          End If
        End Select
        Send (TS)
        If (P(x2).CurMove <> -255) Then
        ' Okay, let's go on with the move...
          If M.Target > 3 And (Config.Multi = 0) Then
            Send ("Sorry, " + P(x2).ScrNam + ", moves that hit more than one person have been disabled by the host.")
            P(x2).CurMove = 0
            P(x2).MoveStart = 0
            Exit Sub
          End If
          P(x2).CurMove = x4
          DoEvents
          X6 = True
          If (X6 = False And M.Element <> Morph And M.Element <> Life) Then
            'Send ("Attack target invalid, move halted.")
            P(x2).CurMove = -255
            P(x2).MoveStart = 0
            Exit Sub
          End If
          DoEvents
          Select Case M.Element      ' Special stuff for certain moves
            Case Invin:
              If (Config.Flag <> 0) Then
                P(x2).CurMove = 0
                Send ("No shields allowed in Capture the Flag!")
              Else
                Playwav "shield"
                For x7 = 1 To MaxPlayers
                  If IsThisATarget(x2, x7) Then P(x7).Status(sInvin) = x2
                Next x7
              End If
            Case Morph:
              T = 0
              If Left$(M.name, 1) = "~" Then T = MatchSenshi(Right$(M.name, Len(M.name) - 1))
              If T = 0 Then T = MatchSenshi(S7)
              If (T = 0) Then
                Send (P(x2).ScrNam + ": invalid ""Morph"" target. Format is: /" + M.Cmdkey + "-<person to morph to>")
                P(x2).CurMove = 0
              End If
              If T = KamekID Then
                Send (P(x2).ScrNam + ": You WISH you could be Kamek!")
                P(x2).CurMove = 0
              End If
            Case Else:
              If P(x2).Status(sQuick) <> 0 And M.Status(sQuick) <= 0 Then
                P(x2).MoveStart = XTimer - 999
                P(x2).Status(sQuick) = 0
              End If
          End Select
        End If
    Case Else:
      P(x2).CurMove = -255
  End Select
  DoEvents
  If (P(x2).CurMove = -25) Then
    P(x2).Target = x4
  Else
    P(x2).Target = T
  End If
  DoEvents
  UpdateGameStat
End Sub

