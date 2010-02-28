' DOMOVE.BAS
' Contains the vital DoMove sub

Option Explicit
Option Compare Text

Sub DoMove (ByVal x2%, ByVal T%, ByVal x4%, ByVal S7$)
' This is an important sub. All moves done need to call this sub to be
' done properly.
Dim X%, X1%, X3%, x5%, X6%, X7%, X8%
Dim SNum%, SPly%, Q%, Atkr$
Dim Y1%, Y2%, Y3%, Y4%
Dim S4$, S6$, SN$, TS$
Dim Sid%
Dim Msg$
Dim Okay%
Dim M As MoveType
  DoEvents
  p(x2).Target = T
  p(x2).MoveSpawn = 0
  SN = p(x2).ScrNam
  Sid = p(x2).CharID
  Select Case x4
    Case pDefect:
    ' Defect to another team???
      p(x2).CurMove = 0
      DoEvents
      Select Case T
        'Case MaxPlayers + 1 To MaxPlayers + MaxYouma:
        '  S4 = y(T - MaxPlayers).TeamID
        '  SN = y(T - MaxPlayers).Name
        '  Okay = True
        Case 1 To MaxPlayers:
          S4 = p(T).TeamID
          SN = p(T).ScrNam
          Okay = Not p(T).Defect
        Case 0:
          Exit Sub
      End Select
      DoEvents
      If (Okay) Then
        Send (Parse(Dataset.DefectSucc, p(x2).ScrNam, TName(Asc(S4)), S4, SN))
        p(x2).TeamID = S4
      Else
        Send (Parse(Dataset.DefectFail, p(x2).ScrNam, "", SN, ""))
      End If
    Case pGet:
    ' The /get move... done instantly
      p(x2).CurMove = 0
      X6 = Item2Get
      Item2Get = 0
      S6 = p(x2).ScrNam
      DoEvents
      If (X6 = 0) Then
        Send (p(x2).ScrNam + ", ain't nuthin' ta get foo!")
      Else
        Send5 (Parse(Items(X6).PlayerGet + " [" + TrimStr(Items(X6).PlayerHP) + "HP]", S6, "", "", S6))
        p(x2).HP = p(x2).HP + Items(X6).PlayerHP
        'p(x2).MP = p(x2).MP + Items(X6).PlayerMP
        CheckStatus Items(X6).PlayerStat(), x2, 0, 1
        ShowStatus x2
      End If
    Case pBlock:
      Msg = ""
      If (T = 0) Then
        p(x2).Target = 0
      Else
        p(x2).Target = T
        If p(x2).Status(sHamedo) Then
          Msg = Msg + "<b><Chi~" + p(x2).Moves(T).Name + "></b>"
        Else
          Msg = Msg + "<u><Ctr~" + p(x2).Moves(T).Name + "></u>"
        End If
      End If
      Msg = Msg + " " + Senshi(p(x2).CharID).Block
      TS = AttackMe$(x2)
      If TS = "" Then TS = "No threat"
      Msg = Msg + " [" + TS + "]"
      SNum = 9999
      SPly = 0
      For X = 1 To MaxPlayers
        If p(X).CurMove > 0 And p(X).CurMove <= MaxMoves And p(X).HP > 0 And p(X).CharID <> 0 Then
          Q = p(X).Moves(p(X).CurMove).Target
          If (p(X).Target = x2) And (Q <> 1) And (Q <> 3) And (Q <> 8) Then
            If p(X).Rune = RuneStealth Then
              If p(x2).Rune = RuneMagic And MoveHitsIn(X) < SNum Then
                SPly = X
                SNum = MoveHitsIn(X)
              End If
            Else
              If MoveHitsIn(X) < SNum Then
                SPly = X
                SNum = MoveHitsIn(X)
              End If
            End If
          End If
        End If
      Next X
      If SPly <> 0 Then Atkr = p(SPly).ScrNam
      If Atkr = "" Then Atkr = "nobody in particular"
      Send (Parse(Msg, p(x2).ScrNam, "", Atkr$, ""))
      p(x2).MoveStart = XTimer
      p(x2).CurMove = pBlock
      DoEvents
    Case pFlee:
    ' Flee from Battle
      p(x2).CurMove = pFlee
      p(x2).MoveStart = XTimer
      Send (Parse(Dataset.FleeAttempt, p(x2).ScrNam, "", "", ""))
    Case pRest:
    ' Resting
      p(x2).CurMove = pRest
      p(x2).MoveStart = XTimer
      Send (Parse(Senshi(p(x2).CharID).Rest, p(x2).ScrNam, "", "", ""))
      If p(x2).Status(sQuick) <> 0 Then
        p(x2).MoveStart = XTimer - 999
        p(x2).Status(sQuick) = 0
      End If
      DoEvents
    Case pFatal:
      If p(x2).Target <> x2 Then
        If Active(p(x2).Target) Then
          p(x2).CurMove = pFatal
          p(x2).MoveStart = XTimer + Rand(-2, 2)
          'If (p(x2).Target > MaxPlayers) Then
          '  SN = y(p(x2).Target - MaxPlayers).Name
          'Else
            SN = p(p(x2).Target).ScrNam
          'End If
          DoEvents
          Send (Parse(Senshi(p(x2).CharID).Fatality.PreFatal + " (fatality)", p(x2).ScrNam, "", SN, ""))
          If p(x2).Status(sQuick) <> 0 Then
            p(x2).MoveStart = XTimer - 999
            p(x2).Status(sQuick) = 0
          End If
        End If
      End If
    Case pTaunt:
    ' Taunt... works only on Youma
      p(x2).MoveStart = XTimer
      If Senshi(Sid).Taunt(1) = "" And Senshi(Sid).Taunt(2) = "" And Senshi(Sid).Taunt(3) = "" And Senshi(Sid).Taunt(4) = "" And Senshi(Sid).Taunt(5) = "" Then
        Send (Senshi(Sid).FullName + " has no taunt. Stupid character!")
      Else
        Do
          DoEvents
          X3 = Int(Rnd * 5) + 1
        Loop Until (Senshi(p(x2).CharID).Taunt(X3) <> "")
        Send (Parse(Senshi(p(x2).CharID).Taunt(X3), p(x2).ScrNam, "", "", ""))
      End If
      p(x2).CurMove = pTaunt
    Case 1 To MaxMoves:
    ' Another move
      DoEvents
      Y3 = p(x2).CharID
      If T = 0 Then
        Send (p(x2).ScrNam + " cannot find a formidable opponent to attack.")
        Exit Sub
      End If
      x5 = ProjectedTotalDamage(x2, x4, p(x2).Target)
      If (p(x2).Cheese + x5 > MaxCheeseLimit * DMult / 100 And p(x2).SuperNum = 0 And x5 > 0) And (p(x2).Status(sPMS) = 0) And (p(x2).Status(sBerserk) = 0) And (p(x2).Status(sChaos) = 0) Then
        Send ("Slow down, " + p(x2).ScrNam + "! Don't be too cheezy! (Projected Damage by that move: " + TrimStr(x5) + ")")
        p(x2).CurMove = 0
        Exit Sub
      End If
      p(x2).MoveStart = XTimer + Rand(-2, 2)
      M = p(x2).Moves(x4)
      If (p(x2).SuperNum > 0) And (M.CanSuper) Then
        'If p(x2).SuperNum = 4 Then Send ("4---4---4---4")
        If p(x2).SuperNum = 5 Then Send ("<b>DISCLAIMER:</b> Level 5's can be really powerful, but they could also be weak.")
        If p(x2).Rune = RuneStealth And p(T).Rune <> RuneMagic Then
          M.Begin2Attack = "Lev" + TrimStr(p(x2).SuperNum) + ": " + M.Begin2SuperAttack
        Else
          M.Begin2Attack = "Lev" + TrimStr(p(x2).SuperNum) + "/" + ScrNam(T) + ": " + M.Begin2SuperAttack
        End If
        Playwav ("limit")
        'Send ("{S limit}")
      End If
      ' "t" is the Playernum of the target
      If M.ReqAllUses <> 0 And p(x2).WpnUsesLeft < Weapons(p(x2).Weapon).NumUses Then
        Send (p(x2).ScrNam + ", this move requires all " + TrimStr(Weapons(p(x2).Weapon).NumUses) + " charges on your weapon.")
        Exit Sub
      End If
      If M.Element <> Phys And p(x2).Status(sMute) <> 0 Then
        Send (p(x2).ScrNam + ", you cannot do this move while Mute.")
        Exit Sub
      End If
      Select Case M.Element
          Case Heal:
            If M.Begin2HealSelf = "" Then M.Begin2HealSelf = M.Begin2Attack
            Select Case T
              Case x2, 0:
                TS = (Parse(M.Begin2HealSelf, p(x2).ScrNam, "", "", ""))
              'Case Is > MaxPlayers:
              '  TS = (Parse(M.begin2attack, p(x2).ScrNam, "", y(T - MaxPlayers).Name, ""))
              '  'If (No_T(M.Begin2Attack)) Then TS = TS + "[on " + ScrNam(T) + "]"
              Case Is <= MaxPlayers
                TS = (Parse(M.Begin2Attack, p(x2).ScrNam, "", p(T).ScrNam, ""))
                'If No_T(M.Begin2Attack) Then TS = TS + "[on " + ScrNam(T) + "]"
            End Select
          Case Morph:
            If M.Begin2Attack <> "" Then
              TS = (Parse(M.Begin2Attack, p(x2).ScrNam, "", S7$, ""))
            ElseIf M.Begin2HealSelf <> "" Then
              TS = (Parse(M.Begin2HealSelf, p(x2).ScrNam, "", S7$, ""))
            Else
              TS = (Parse("%SN begins to morph into %T.", p(x2).ScrNam, "", S7$, ""))
            End If
          Case Else:
            Select Case T
              'Case Is > MaxPlayers:
              '  TS = (Parse(M.begin2attack, p(x2).ScrNam, "", y(T - MaxPlayers).Name, ""))
              '  'If No_T(M.Begin2Attack) Then TS = TS + "[on " + ScrNam(T) + "]"
              Case Is <= MaxPlayers:
                If p(x2).Rune = RuneStealth And p(T).Rune <> RuneMagic Then
                  TS = (Parse(M.Begin2Attack, p(x2).ScrNam, "", "someone", ""))
                Else
                  TS = (Parse(M.Begin2Attack, p(x2).ScrNam, "", p(T).ScrNam, ""))
                End If
                'If No_T(M.Begin2Attack) Then TS = TS + "[on " + ScrNam(T) + "]"
            End Select
        End Select
        'If ((M.Element >= MoonE And M.Element <= Lum) Or M.Element = Phys) And p(x2).Rune = RuneCharge And p(x2).Charging Then
        '  If p(T).Rune = RuneMagic Then
        '    Send (p(T).ScrNam + "'s Rune of Magic forbids " + p(x2).ScrNam + " to charge.")
        '    p(x2).Charging = False
        '  Else
        '    TS = "<i>((/release))</i> " + TS
        '    p(x2).RuneTemp = XTimer
        '  End If
        'Else
        '  p(x2).Charging = False
        'End If
        Send (TS)
        If (p(x2).CurMove <> -255) Then
        ' Okay, let's go on with the move...
          If M.Target > 3 And (Config.Multi = 0) Then
            Send ("Sorry, " + p(x2).ScrNam + ", moves that hit more than one person have been disabled by the host.")
            p(x2).CurMove = 0
            p(x2).MoveStart = 0
            Exit Sub
          End If
          p(x2).CurMove = x4
          DoEvents
          X6 = True
          If (X6 = False And M.Element <> Morph And M.Element <> Life) Then
            'Send ("Attack target invalid, move halted.")
            p(x2).CurMove = -255
            p(x2).MoveStart = 0
            Exit Sub
          End If
          DoEvents
          Select Case M.Element      ' Special stuff for certain moves
            Case Invin:
              If (Config.Flag <> 0) Then
                p(x2).CurMove = 0
                Send ("No shields allowed in Capture the Flag!")
              Else
                For X7 = 1 To MaxPlayers
                  If IsThisATarget(x2, X7) Then p(X7).Status(sInvin) = x2
                Next X7
              End If
            Case Morph:
              T = 0
              If Left$(M.Name, 1) = "~" Then T = MatchSenshi(Right$(M.Name, Len(M.Name) - 1))
              If T = 0 Then T = MatchSenshi(S7)
              If (T = 0) Then
                Send (p(x2).ScrNam + ": invald ""Morph"" target. Format is: /" + M.Cmdkey + "-<person to morph to>")
                p(x2).CurMove = 0
              End If
            Case Else:
              If p(x2).Status(sQuick) <> 0 And M.Status(sQuick) <= 0 Then
                p(x2).MoveStart = XTimer - 999
                p(x2).Status(sQuick) = 0
              End If
          End Select
        End If
    Case Else:
      p(x2).CurMove = -255
  End Select
  DoEvents
  If (p(x2).CurMove = -25) Then
    p(x2).Target = x4
  Else
    p(x2).Target = T
  End If
  DoEvents
  UpdateGameStat
End Sub

