Attribute VB_Name = "CPUStuff"
Option Explicit
Function WillThisHurt(ByVal x2%, ByVal x%)
Dim Ame$, Q%, Q2%
Dim MeHit%, YouHit%, MeCm%, YouCm%
Dim Threat%
  MeCm = P(x2).CurMove
  YouCm = P(x).Target
  Threat = False
  If P(x).CurMove = pBlock And P(x).Status(sHamedo) Then
    If YouCm > 0 And P(x).HP > 0 And P(x).CharID <> 0 Then
      If YouCm <= MaxMoves Then
        Q = P(x).Moves(YouCm).Target
      ElseIf YouCm = pSlot Then
        Q = Enemy
      Else
        GoTo Skipwho
      End If
      Q2 = 0
      If (Q = 5 Or Q = 6) And P(x2).TeamID <> P(x).TeamID Then Q2 = True
      If (Q = 7) Then Q2 = True
      If (Q = 2) Then Q2 = True
      If P(x).Moves(YouCm).Element <> Morph And Q = 4 Then Q2 = True
      If Q2 And (Q <> 1) And (Q <> 3) And (Q <> 8) Then
        If (P(x).Rune <> RuneStealth) Or (P(x2).Rune = RuneMagic) Then
          If P(x).Moves(YouCm).Status(sMute) >= 50 And P(x2).Moves(MeCm).Element <> Phys Then Threat = True
          If P(x).Moves(YouCm).Status(sChaos) >= 50 Then Threat = True
          If P(x).Moves(YouCm).Status(sFreeze) >= 50 Then Threat = True
          If P(x).Moves(YouCm).Status(sScarecrow) >= 50 And P(x2).Moves(MeCm).Element = Phys Then Threat = True
          If P(x).Moves(YouCm).Status(sStun) >= 50 Then Threat = True
          If P(x).Moves(YouCm).Status(sStop) >= 50 Then Threat = True
          If P(x).Moves(YouCm).Status(sMIA) >= 50 Then Threat = True
          If P(x).SuperNum = 0 Then
            If PredictDamageCount(x2, x, P(x).Moves(YouCm), 1) > P(x2).HP Then Threat = True
          Else
            If ProjectedSuperDamage(x, YouCm, x2, P(x).SuperNum) > P(x2).HP Then Threat = True
          End If
        End If
      End If
    End If
  End If
Skipwho:
  WillThisHurt = Threat
End Function

Function ShouldIStop(ByVal x2%)
Dim x%, Ame$, Q%, Q2%
Dim MeHit%, YouHit%, MeCm%, YouCm%
Dim Threat%
  MeCm = P(x2).CurMove
  YouCm = P(x).CurMove
  Threat = False
  For x = 1 To MaxPlayers
    DoEvents
    If P(x).CurMove > 0 And P(x).HP > 0 And P(x).CharID <> 0 Then
      If P(x).CurMove <= MaxMoves Then
        Q = P(x).Moves(P(x).CurMove).Target
      ElseIf P(x).CurMove = pSlot Then
        Q = Enemy
      Else
        GoTo Skipwho
      End If
      Q2 = 0
      If (Q = 5 Or Q = 6) And P(x2).TeamID <> P(x).TeamID Then Q2 = True
      If (Q = 7) Then Q2 = True
      If P(x).Moves(P(x).CurMove).Element <> Morph And Q = 4 Then
        If P(P(x).Target).TeamID = P(x2).TeamID Then Q2 = True
      End If
      If (P(x).Target = x2 Or Q2) And (Q <> 1) And (Q <> 3) And (Q <> 8) Then
        If (P(x).Rune <> RuneStealth) Or (P(x2).Rune = RuneMagic) Then
          MeHit = MoveHitsIn(x2)
          YouHit = MoveHitsIn(x)
          If MeHit >= YouHit Then
            If P(x).Moves(YouCm).Status(sMute) >= 50 And P(x2).Moves(MeCm).Element <> Phys Then Threat = True
            If P(x).Moves(YouCm).Status(sChaos) >= 50 Then Threat = True
            If P(x).Moves(YouCm).Status(sFreeze) >= 50 Then Threat = True
            If P(x).Moves(YouCm).Status(sScarecrow) >= 50 Then
              If P(x2).Moves(MeCm).Element = Phys Then Threat = True
            End If
            If P(x).Moves(YouCm).Status(sStun) >= 50 Then Threat = True
            If P(x).Moves(YouCm).Status(sStop) >= 50 Then Threat = True
            If P(x).Moves(YouCm).Status(sMIA) >= 50 Then Threat = True
            If P(x).SuperNum = 0 Then
              If PredictDamageCount(x2, x, P(x).Moves(YouCm), 1) > P(x2).HP Then Threat = True
            Else
              If ProjectedSuperDamage(x, YouCm, x2, P(x).SuperNum) > P(x2).HP Then Threat = True
            End If
          End If
        End If
      End If
Skipwho:
    End If
  Next x
  'If Threat = True Then Stop
  ShouldIStop = Threat
End Function
Sub BehNormal(ByVal x2 As Integer, MTD%, Tgt%, MT$)
Dim x%, X3%, x4%, x5%, X6%, x7%, x8%, x9%, x0%, Bla%
Dim WpnTk%, Goto1%
Beginning:
  On Error GoTo 0
  If Battle = 0 Then Exit Sub
  'If X2 = PKamek And P(X2).HP < P(X2).MaxHP * 2 / 5 Then Stop
  'If X2 = 11 Then Stop
  P(x2).SuperNum = 0
  WpnTk = 0
  X6% = False
  If Config.Flag <> 0 Then
    If RedFlag = 0 Then
      RedFlag = x2
      Send (P(x2).ScrNam + " claims the Red Flag.")
    End If
    If BlueFlag = 0 Then
      BlueFlag = x2
      Send (P(x2).ScrNam + " claims the Blue Flag.")
    End If
  End If
  For x% = 1 To MaxPlayers
    DoEvents
    If P(x).Target = x2 And P(x).CurMove < MaxMoves And P(x).CurMove > 0 Then
      If P(x).SuperNum > 0 Then
        X6 = x
        Exit For
      End If
    End If
  Next x%
  If ShouldIStop(x2) Then X6 = -1
  Goto1 = 0
DoItAgain:
  If X6% <> 0 Then
    x5% = FindMoveByElement(x2, Invin, x2)
    If P(x2).HP >= P(x2).MaxHP * 2 / 5 And x2 <> PKamek Then
      If (x5% <> 0) Then
        MTD% = x5%
        Tgt% = x2
        Exit Sub
      ElseIf Goto1 = 0 Then
        x5% = FindDroppedByElement(x2, Invin, x2)
        If x5 <> 0 Then
          If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
          CPUGetWeapon x2, x5
          WpnTk = 1
          Goto1 = 1
          GoTo DoItAgain
        End If
      End If
    End If
    'If (P(X2).Super >= 100 Or P(X2).HP <= MaxHP * 3 / 5) And P(X2).CharID <> KamekID Then
    If P(x2).Super < 200 Then
      Tgt = 200 - P(x2).Super
      If P(x2).HP - Tgt < P(x2).MaxHP / 2 Then
        Tgt = 100 - P(x2).Super
      End If
      If P(x2).HP - Tgt >= P(x2).MaxHP / 2 And Tgt > 0 Then
        P(x2).HP = P(x2).HP - Tgt
        P(x2).HP = P(x2).HP - Tgt
        P(x2).Super = P(x2).Super + Tgt
        Send (Parse(DATASET.HPDivert, P(x2).ScrNam, TrimStr(Tgt), "", ""))
      End If
    End If
    If (P(x2).Super >= 100 Or P(x2).HP <= P(x2).MaxHP * 3 / 5) And P(x2).Status(sQuick) = 0 Then
      If P(x2).Super >= 200 Then
        Tgt% = FindChiCtr(x2, X6)
        If Tgt% = 0 Then Tgt% = FindCounter(x2, X6)
      Else
        Tgt% = FindCounter(x2, X6)
      End If
      If Tgt% <> 0 Then
        Select Case P(x2).Super
          Case Is >= 200:
            'Send (p(x2).ScrNam + " is Chibot Countering with " + p(x2).Moves(Tgt%).Name + "!")
            P(x2).Super = P(x2).Super - 100
            P(x2).Status(sHamedo) = -1
          Case 100 To 199:
            'Send (p(x2).ScrNam + " is whoopin' up with " + p(x2).Moves(Tgt%).Name + "!")
        End Select
      End If
      MTD% = pBlock
      Exit Sub
    End If
  End If
  If (P(x2).Super >= 300 And Config.Fours = 0) Or (P(x2).Super >= 400 And Config.Fours) And P(x2).Status(sQuick) = 0 Then
    'X5% = StrongestPossibleSuper(X2,)
    Tgt% = CPUTarget(x2)
    If Tgt% <> 0 Then
      'Send (p(x2).ScrNam + " says, ""/3-" + p(x2).Moves(x5).Cmdkey + "-" + ScrNam(Tgt%) + """")
      'MTD% = X5%
      'If P(x2).Super >= 600 And fChUBMain.Six Then
      '  P(x2).SuperNum = 6
      If P(x2).Super >= 500 And Config.Fours Then
        P(x2).SuperNum = 5
      ElseIf P(x2).Super >= 400 And Config.Fours Then
        P(x2).SuperNum = 4
      ElseIf P(x2).Super >= 300 Then
        P(x2).SuperNum = 3
      ElseIf P(x2).Super >= 200 Then
        P(x2).SuperNum = 2
      ElseIf P(x2).Super >= 100 Then
        P(x2).SuperNum = 1
      Else
        P(x2).SuperNum = 0
        'kDlgBox "Error!", 16, "BehNormal"
        'Stop
      End If
      x5% = StrongestPossibleSuper(x2, P(x2).SuperNum, Tgt%, P(x2).SuperNum)
      If x5 <> 0 And P(x2).Moves(x5).CanSuper > 0 Then
        X6 = StrongestPossibleMove(x2, Tgt%)
        x7 = ProjectedTotalDamage(x2, X6, Tgt)
        If x7 < ProjectedSuperDamage(x2, x5, Tgt, P(x2).SuperNum) Then
          MTD% = x5%
          Exit Sub
        Else
          Tgt = 0
          P(x2).SuperNum = 0
        End If
      Else
        Tgt% = 0
        P(x2).SuperNum = 0
      End If
    End If
  End If
  If x2 = pYoshi And P(PKamek).Status(sBarrier) = 1999 Then
    x5 = FindMoveByElement(x2, 250, PKamek)
    If x5 <> 0 Then
      MTD = x5
      Tgt = PKamek
      Exit Sub
    End If
    Stop ' Error...!!!
  End If
  If P(x2).Status(sBarrier) = 0 Then
    x5 = FindMoveByElement(x2, 251, x2)
    If x5 <> 0 Then
      MTD = x5
      Tgt = x2
      Exit Sub
    End If
  End If
  If P(x2).Status(sBarrier) = -4 And HPx(pYoshi) <= 0 Then
    x5 = FindMoveByElement(x2, 251, x2)
    If x5 <> 0 Then
      MTD = x5
      Tgt = x2
      Exit Sub
    End If
  End If
  If P(x2).Status(sReraise) = 0 Then
    x5% = FindMoveByStatus(x2, x2, sReraise, 0)
    If x5% <> 0 Then
      MTD% = x5%
      Tgt% = x2
      Exit Sub
    ElseIf Goto1 = 0 Then
      x5 = FindDroppedByStatus(x2, x2, sReraise)
      If x5 <> 0 Then
        If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
        CPUGetWeapon x2, x5
        WpnTk = 1
        Goto1 = 1
        GoTo DoItAgain
      End If
    End If
  End If
  Bla = P(x2).OwnedBy
  Tgt = CPUTarget(x2)
  If Bla <> 0 And HPx(Bla) > 0 And Active(Bla) Then
    If P(Bla).Status(sReraise) = 0 Then
      x5% = FindMoveByStatus(x2, Bla, sReraise, 0)
      If x5% <> 0 Then
        MTD% = x5%
        Tgt% = Bla
        Exit Sub
      ElseIf Goto1 = 0 Then
        x5 = FindDroppedByStatus(x2, Bla, sReraise)
        If x5 <> 0 Then
          If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
          CPUGetWeapon x2, x5
          WpnTk = 1
          Goto1 = 1
          GoTo DoItAgain
        End If
      End If
    End If
    X6% = False
    For x% = 1 To MaxPlayers
      DoEvents
      If P(x).Target = Bla And P(x).CurMove < MaxMoves And P(x).CurMove > 0 Then If P(x).Moves(P(x).CurMove).Target = Enemy Then X6 = True
    Next x%
    If X6% Then
      x5% = FindMoveByElement(x2, Invin, Bla)
      If x5% <> 0 Then
        MTD% = x5%
        Tgt% = Bla
        Exit Sub
      ElseIf Goto1 = 0 Then
        x5 = FindDroppedByElement(x2, Invin, Bla)
        If x5 <> 0 Then
          If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
          CPUGetWeapon x2, x5
          WpnTk = 1
          Goto1 = 1
          GoTo DoItAgain
        End If
      End If
    End If
    If (P(Bla).HP < (MaxHP * 2 / 5)) Or (Tgt = 0 And P(Bla).HP < (MaxHP * 4 / 5)) Then
      x5% = FindMoveByElement(x2, Heal, Bla)
      If x5% <> 0 Then
        MTD% = x5%
        Tgt% = Bla
        Exit Sub
      ElseIf Goto1 = 0 Then
        x5 = FindDroppedByElement(x2, Heal, Bla)
        If x5 <> 0 Then
          If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
          CPUGetWeapon x2, x5
          WpnTk = 1
          Goto1 = 1
          GoTo DoItAgain
        End If
      End If
    End If
    If P(Bla).Status(sRegen) = 0 Then
      x5% = FindMoveByStatus(x2, Bla, sRegen, 0)
      If x5 <> 0 Then
        MTD% = x5
        Tgt% = Bla
        Exit Sub
      ElseIf Goto1 = 0 Then
        x5 = FindDroppedByStatus(x2, Bla, sRegen)
        If x5 <> 0 Then
          If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
          CPUGetWeapon x2, x5
          WpnTk = 1
          Goto1 = 1
          GoTo DoItAgain
        End If
      End If
    End If
    If P(Bla).Status(sHaste) = 0 Then
      x5% = FindMoveByStatus(x2, Bla, sHaste, 0)
      If x5 <> 0 Then
        MTD% = x5
        Tgt% = Bla
        Exit Sub
      ElseIf Goto1 = 0 Then
        x5 = FindDroppedByStatus(x2, Bla, sHaste)
        If x5 <> 0 Then
          If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
          CPUGetWeapon x2, x5
          WpnTk = 1
          Goto1 = 1
          GoTo DoItAgain
        End If
      End If
    End If
    If P(Bla).Status(sBarrier) = 0 Then
      x5 = FindMoveByStatus(x2, Bla, sBarrier, 0)
      If x5 <> 0 Then
        MTD% = x5
        Tgt% = Bla
        Exit Sub
      ElseIf Goto1 = 0 Then
        x5 = FindDroppedByStatus(x2, Bla, sBarrier)
        If x5 <> 0 Then
          If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
          CPUGetWeapon x2, x5
          WpnTk = 1
          Goto1 = 1
          GoTo DoItAgain
        End If
      End If
    End If
    If P(Bla).Status(sMBarrier) = 0 Then
      x5 = FindMoveByStatus(x2, Bla, sMBarrier, 0)
      If x5 <> 0 Then
        MTD% = x5
        Tgt% = Bla
        Exit Sub
      ElseIf Goto1 = 0 Then
        x5 = FindDroppedByStatus(x2, Bla, sMBarrier)
        If x5 <> 0 Then
          If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
          CPUGetWeapon x2, x5
          WpnTk = 1
          Goto1 = 1
          GoTo DoItAgain
        End If
      End If
    End If
    If (P(Bla).HP < (MaxHP * 2 / 5)) Or (Tgt = 0 And P(Bla).HP < (MaxHP * 4 / 5)) Then
      x5% = FindMoveByElement(x2, Heal, Bla)
      If x5% <> 0 Then
        MTD% = x5%
        Tgt% = Bla
        Exit Sub
      ElseIf Goto1 = 0 Then
        x5 = FindDroppedByElement(x2, Heal, Bla)
        If x5 <> 0 Then
          If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
          CPUGetWeapon x2, x5
          WpnTk = 1
          Goto1 = 1
          GoTo DoItAgain
        End If
      End If
    End If
  End If
  If Config.Flag <> 0 Then
    If P(x2).TeamID = "R" And P(BlueFlag).TeamID = "R" Then Bla = BlueFlag
    If P(x2).TeamID = "B" And P(RedFlag).TeamID = "B" Then Bla = RedFlag
    If P(x2).TeamID = "R" And P(RedFlag).TeamID = "R" And FoundRedFlag Then Bla = RedFlag
    If P(x2).TeamID = "B" And P(BlueFlag).TeamID = "B" And FoundBlueFlag Then Bla = BlueFlag
    If P(Bla).Status(sMIA) <> 0 Then Bla = 0
    If Bla <> 0 Then
      x5% = FindMoveByStatus(x2, Bla, sMIA, 0)
      If x5% <> 0 Then
        MTD% = x5%
        Tgt% = Bla
        Exit Sub
      ElseIf Goto1 = 0 Then
        x5 = FindDroppedByStatus(x2, x2, sMIA)
        If x5 <> 0 Then
          If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
          CPUGetWeapon x2, x5
          WpnTk = 1
          Goto1 = 1
          GoTo DoItAgain
        End If
      End If
      If P(Bla).Status(sReraise) = 0 Then
        x5% = FindMoveByStatus(x2, Bla, sReraise, 0)
        If x5% <> 0 Then
          MTD% = x5%
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByStatus(x2, Bla, sReraise)
          If x5 <> 0 Then
            If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
      X6% = False
      For x% = 1 To MaxPlayers
        DoEvents
        If P(x).Target = Bla And P(x).CurMove < MaxMoves And P(x).CurMove > 0 Then If P(x).Moves(P(x).CurMove).Target = Enemy Then X6 = True
      Next x%
      If X6% Then
        x5% = FindMoveByElement(x2, Invin, Bla)
        If x5% <> 0 Then
          MTD% = x5%
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByElement(x2, Invin, Bla)
          If x5 <> 0 Then
            If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
      If P(Bla).HP < (MaxHP * 2 / 5) Then
        x5% = FindMoveByElement(x2, Heal, Bla)
        If x5% <> 0 Then
          MTD% = x5%
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByElement(x2, Heal, Bla)
          If x5 <> 0 Then
            If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
      If P(Bla).Status(sRegen) = 0 Then
        x5% = FindMoveByStatus(x2, Bla, sRegen, 0)
        If x5 <> 0 Then
          MTD% = x5
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByStatus(x2, Bla, sRegen)
          If x5 <> 0 Then
            If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
      If P(Bla).Status(sHaste) = 0 Then
        x5% = FindMoveByStatus(x2, Bla, sHaste, 0)
        If x5 <> 0 Then
          MTD% = x5
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByStatus(x2, Bla, sHaste)
          If x5 <> 0 Then
            If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
      If P(Bla).Status(sBarrier) = 0 Then
        x5 = FindMoveByStatus(x2, Bla, sBarrier, 0)
        If x5 <> 0 Then
          MTD% = x5
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByStatus(x2, Bla, sBarrier)
          If x5 <> 0 Then
            If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
      If P(Bla).Status(sMBarrier) = 0 Then
        x5 = FindMoveByStatus(x2, Bla, sMBarrier, 0)
        If x5 <> 0 Then
          MTD% = x5
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByStatus(x2, Bla, sMBarrier)
          If x5 <> 0 Then
            If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
      If P(Bla).HP < (MaxHP * 4 / 5) Then
        x5% = FindMoveByElement(x2, Heal, Bla)
        If x5% <> 0 Then
          MTD% = x5%
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByElement(x2, Heal, Bla)
          If x5 <> 0 Then
            If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
    End If
  End If
  If (P(x2).HP < (MaxHP * 3 / 5)) Or (Tgt = 0 And P(x2).HP < MaxHP * 4 / 5) Then
    x5% = FindMoveByElement(x2, Heal, x2)
    If x5% <> 0 Then
      MTD% = x5%
      Tgt% = x2
      Exit Sub
    ElseIf Goto1 = 0 Then
      x5 = FindDroppedByElement(x2, Heal, x2)
      If x5 <> 0 Then
        If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
        CPUGetWeapon x2, x5
        WpnTk = 1
        Goto1 = 1
        GoTo DoItAgain
      End If
    End If
  End If
  If P(x2).Status(sRegen) = 0 Then
    x5% = FindMoveByStatus(x2, x2, sRegen, 0)
    If x5 <> 0 Then
      MTD% = x5
      Tgt% = x2
      Exit Sub
    ElseIf Goto1 = 0 Then
      x5 = FindDroppedByStatus(x2, x2, sRegen)
      If x5 <> 0 Then
        If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
        CPUGetWeapon x2, x5
        WpnTk = 1
        Goto1 = 1
        GoTo DoItAgain
      End If
    End If
  End If
  If P(x2).Status(sHaste) = 0 Then
    x5% = FindMoveByStatus(x2, x2, sHaste, 0)
    If x5 <> 0 Then
      MTD% = x5
      Tgt% = x2
      Exit Sub
    ElseIf Goto1 = 0 Then
      x5 = FindDroppedByStatus(x2, x2, sHaste)
      If x5 <> 0 Then
        If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
        CPUGetWeapon x2, x5
        WpnTk = 1
        Goto1 = 1
        GoTo DoItAgain
      End If
    End If
  End If
  If P(x2).Status(sBless) = 0 Then
    x5% = FindMoveByStatus(x2, x2, sBless, 0)
    If x5 <> 0 Then
      MTD% = x5
      Tgt% = x2
      Exit Sub
    ElseIf Goto1 = 0 Then
      x5 = FindDroppedByStatus(x2, x2, sBless)
      If x5 <> 0 Then
        If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
        CPUGetWeapon x2, x5
        WpnTk = 1
        Goto1 = 1
        GoTo DoItAgain
      End If
    End If
  End If
  If P(x2).Status(sBarrier) = 0 Then
    x5 = FindMoveByStatus(x2, x2, sBarrier, 0)
    If x5 <> 0 Then
      MTD% = x5
      Tgt% = x2
      Exit Sub
    ElseIf Goto1 = 0 Then
      x5 = FindDroppedByStatus(x2, x2, sBarrier)
      If x5 <> 0 Then
        If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
        CPUGetWeapon x2, x5
        WpnTk = 1
        Goto1 = 1
        GoTo DoItAgain
      End If
    End If
  End If
  If P(x2).Status(sMBarrier) = 0 Then
    x5 = FindMoveByStatus(x2, x2, sMBarrier, 0)
    If x5 <> 0 Then
      MTD% = x5
      Tgt% = x2
      Exit Sub
    ElseIf Goto1 = 0 Then
      x5 = FindDroppedByStatus(x2, x2, sMBarrier)
      If x5 <> 0 Then
        If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
        CPUGetWeapon x2, x5
        WpnTk = 1
        Goto1 = 1
        GoTo DoItAgain
      End If
    End If
  End If
  For x4% = 1 To MaxPlayers
    DoEvents
    If P(x4).TeamID = P(x2).TeamID And P(x4).CharID <> 0 And P(x4).HP > 0 And P(x4).Status(sMIA) = 0 Then
      If ((P(x4).HP < (MaxHP * 3 / 5)) Or (P(x4).HP < MaxHP * 4 / 5 And Tgt = 0)) And Rand(1, 100) <= P(x2).Goodwill Then
        x5% = FindMoveByElement(x2, Heal, x4)
        If x5% <> 0 Then
          MTD% = x5%
          Tgt% = x4%
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByElement(x2, Heal, x4)
          If x5 <> 0 Then
            If P(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        Else
          Exit For
        End If
      End If
    End If
  Next x4
  If Item2Get <> 0 And Rand(1, 100) <= P(x2).Greed Then
    MTD% = pGet
    Tgt% = 0
    Exit Sub
  End If
  If Rand(1, 100) <= P(x2).Arrogance And Rand(1, 5) = 3 And P(x2).HP >= P(x2).MaxHP * 3 / 4 Then
    MTD% = pTaunt
    Tgt% = 0
    Exit Sub
  End If
  If P(x2).Status(sMorph) <> 0 Then
    If (XTimer - P(x2).Status(sMorph) >= 120) And Rand(1, 3) = 1 Then
      Send (P(x2).ScrNam + " reverts back to " + Senshi(P(x2).OldCharID).FullName + ".")
      UnmorphMe (x2)
      GoTo Beginning
    End If
  End If
  If P(x2).Weapon = 0 Then
    For x4 = 1 To MaxDropped
      If Dropped(x4) <> 0 Then
        CPUGetWeapon x2, x4
        WpnTk = 1
        Exit For
      End If
    Next x4
  End If
  x5 = FindMoveByElement(x2, Morph, x2)
  If x5 <> 0 And (Rand(1, 10) = 2 Or (NumMoves(x2) <= 4 And Rand(1, 2) = 2) Or (NumMoves(x2) = 1)) Then
    Tgt% = x2
    MTD% = x5
    MT$ = Senshi(Rand(1, HighSenshi)).FullName
    Exit Sub
  End If
  Tgt% = CPUTarget(x2)
  If Tgt <> 0 Then MTD% = StrongestPossibleMove(x2, Tgt%)
  If MTD < 15 And WpnTk = 0 And P(x2).Weapon <> 0 Then
    P(x2).UselessWpn = P(x2).UselessWpn + 1
    If P(x2).UselessWpn >= 3 Then
      DropWeapon x2, "%SN decides that the %T is of no use and drops it."
    End If
  End If
  If HPx(Tgt%) <= (MaxHP / 6) And Senshi(P(x2).CharID).Fatality.Cmdkey <> "" And (PKamek <> x2 Or pYoshi <> 0) And P(x2).Status(sQuick) = 0 Then MTD% = pFatal
  'If ProjectedTotalDamage(x2, MTD, Tgt) < MaxHP / 10 And Rand(1, 3) <> 1 And IsStrongest(x2, MTD, Tgt) = 0 Then
  '  MTD% = pCharge
  '  Tgt% = x2
  'End If
  If (Tgt% <> 0) And (MTD% <> 0) Then Exit Sub
  If P(x2).HP > P(x2).MaxHP * 5 / 6 And P(x2).Super < MaxSP Then
    Tgt = P(x2).HP - P(x2).MaxHP * 4 / 6
    If Tgt > MaxSP - P(x2).Super Then Tgt = MaxSP - P(x2).Super
    If Tgt > 0 Then
      P(x2).HP = P(x2).HP - Tgt
      P(x2).Super = P(x2).Super + Tgt
      Send (Parse(DATASET.HPDivert, P(x2).ScrNam, TrimStr(Tgt), "", ""))
      GoTo DoItAgain
    End If
  End If
  MTD% = pRest
  Tgt% = x2
End Sub

Sub CPUGetWeapon(ByVal x2%, ByVal x4%)
  If P(x2).CharID <> KamekID Then
    P(x2).Weapon = Dropped(x4)
    Dropped(x4) = 0
    InitMoves (x2)
    P(x2).WpnUsesLeft = DropUses(x4)
    DropUses(x4) = 0
    Send (Parse(Weapons(P(x2).Weapon).SelectStr, P(x2).ScrNam, "", "", ""))
    P(x2).UselessWpn = 0
  End If
End Sub

Function FindChiCtr(ByVal x2%, ByVal Tg%)
Dim x%, Strn%, M%, Pj%, Pg%
  Strn% = -1
  M% = 0
  For x = 1 To MaxMoves
    Pj% = ProjectedTotalDamage(x2, x, Tg%)
    If P(x2).Moves(x).Cmdkey <> "" And DoesDamage(P(x2).Moves(x).Element) And P(x2).Moves(x).Element <> MPTheft And Pj% > Strn% And (P(x2).Moves(x).CanSuper <= 1 Or P(x2).Moves(x).CanSuper = 7) Then
      If Not (P(x2).Moves(x).Element <> Phys And P(x2).Status(sMute) <> 0) Then
        M% = x
        Strn% = Pj%
      End If
    End If
  Next x
  If Strn% < HPx(Tg) Then
    If P(Tg).Weapon <> 0 And P(Tg).CurMove > 15 Then Pg% = FindMoveByElement(x2, 252, Tg)
    If P(Tg).Moves(P(Tg).CurMove).Element <> Phys Then Pg% = FindMoveByStatus(x2, Tg, sMute, 7)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sFreeze, 7)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sStun, 7)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sSleep, 7)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sChaos, 7)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sBerserk, 7)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sMushroom, 7)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sMIA, 7)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sStop, 7)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sSlow, 7)
    If Pg% = 0 Then Pg% = M%
  Else
    Pg% = M%
  End If
  If P(x2).HP <= P(x2).MaxHP * 3 / 5 Then
    x = FindMoveByElement(x2, Heal, x2)
    If x <> 0 Then Pg% = x
  End If
  If x2 = PKamek And (HPx(pYoshi) < 0 Or pYoshi = 0) And P(x2).Status(sBarrier) <> 1999 Then
    x = FindMoveByElement(x2, 251, x2)
    If x <> 0 Then Pg% = x
  End If
  FindChiCtr = Pg%
End Function

Function FindCounter(ByVal x2 As Integer, ByVal Tg%)
Dim x%, Strn%, M%, Pj%
  Strn% = -1
  M% = 0
  If P(Tg).Weapon <> 0 Then
    M = FindMoveByElement(x2, 252, Tg)
    If M <> 0 Then
      FindCounter = M
      Exit Function
    End If
  End If
  For x = 1 To MaxMoves
    Pj% = ProjectedTotalDamage(x2, x, Tg%)
    If P(x2).Moves(x).Cmdkey <> "" And DoesDamage(P(x2).Moves(x).Element) And P(x2).Moves(x).Element <> MPTheft And ((P(x2).Moves(x).CanSuper <= 1) Or (P(x2).Moves(x).CanSuper >= 6)) And Pj% > Strn% Then
      If (P(x2).Moves(x).Element = Phys Or P(x2).Status(sMute) <> 0) And (P(x2).Moves(x).Element <> Phys Or P(x2).Status(sScarecrow) = 0) Then
        M% = x
        Strn% = Pj%
      End If
    End If
  Next x
  FindCounter = M%
End Function

Function FindDroppedByElement(ByVal x2 As Integer, ByVal Ele%, ByVal Q%)
Dim x%, Strn%, Sug%, W%
  If Ele% <> Phys And P(x2).Status(sMute) Then
    FindDroppedByElement = 0
    Exit Function
  End If
  If Ele% = Phys And P(x2).Status(sScarecrow) Then
    FindDroppedByElement = 0
    Exit Function
  End If
  If Ele = Invin And Config.Flag <> 0 Then
    FindDroppedByElement = 0
    Exit Function
  End If
  Strn = -1
  Sug = 0
  For W = 1 To MaxDropped
    For x% = 1 To 5
      If Weapons(Dropped(W)).Moves(x).Element = Ele% And (Config.Respawn = 0 Or Weapons(Dropped(W)).Moves(x).Target <= 3) And Kor(Q = x2, Weapons(Dropped(W)).Moves(x).Target = OnlySelf) Then
        If Weapons(Dropped(W)).Moves(x).ElementStr > Strn Then
          Strn = Weapons(Dropped(W)).Moves(x).ElementStr
          Sug = W
        End If
      End If
    Next x%
  Next W
  FindDroppedByElement = Sug
End Function

Function FindDroppedByStatus(ByVal x2%, ByVal Q%, ByVal Stat%)
Dim x%, Pct%, T%, W%
  Pct% = 75
  T% = 0
  For W = 1 To MaxDropped
    For x% = 1 To 5
      If Weapons(Dropped(W)).Moves(x).Status(Stat) > Pct% And (Config.Multi = 0 Or Weapons(Dropped(W)).Moves(x).Target <= 3 Or Weapons(Dropped(W)).Moves(x).Target = OnlySelf) And Kor(Q = x2, Weapons(Dropped(W)).Moves(x).Target = OnlySelf) And (P(x2).Status(sMute) = 0 Or Weapons(Dropped(W)).Moves(x).Element = Phys) And (P(x2).Status(sScarecrow) = 0 Or Weapons(Dropped(W)).Moves(x).Element <> Phys) Then
        T% = W%
        Pct% = Weapons(Dropped(W)).Moves(x).Status(Stat%)
      End If
    Next x%
  Next W
  If P(x2).Rune = RuneSwiss Or P(Q).Rune = RuneSwiss Then
    FindDroppedByStatus = 0
  Else
    FindDroppedByStatus = T%
  End If
End Function

Function FindMoveByElement(ByVal x2 As Integer, ByVal Ele%, ByVal Q%)
Dim x%, Strn%, Sug%, x5%
  If (Ele% <> Phys And P(x2).Status(sMute)) Or (Ele% = Phys And P(x2).Status(sScarecrow)) Or (Ele = Invin And Config.Flag <> 0) Then
    FindMoveByElement = 0
    Exit Function
  End If
  Strn = -1
  Sug = 0
  For x% = 1 To MaxMoves
    If P(x2).Moves(x).Cmdkey <> "" And P(x2).Moves(x).Element = Ele% And (Config.Multi = 1 Or P(x2).Moves(x).Target <= 3) And Kor(Q = x2, P(x2).Moves(x).Target = OnlySelf) And (P(x2).Moves(x).CanSuper <= 1) Then
      If (P(x2).Moves(x).ReqAllUses And P(x2).WpnUsesLeft = Weapons(P(x2).Weapon).NumUses) Or (P(x2).Moves(x).ReqAllUses = 0) Then
        x5 = ProjectedTotalDamage(x2, x, Q)
        If P(x2).Moves(x).Element = Heal Then x5 = P(x2).Moves(x).ElementStr
        If x5 > Strn Then
          If ((P(x2).Cheese + x5 <= MaxCheeseLimit * DMult / 100) Or PKamek <> 0) And (x <= 19 Or (P(x2).Moves(x).ReqAllUses And P(x2).WpnUsesLeft = Weapons(P(x2).Weapon).NumUses)) Then
            Strn = x5
            Sug = x
          End If
        End If
      End If
    End If
  Next x%
  FindMoveByElement = Sug
End Function

Function FindMoveByStatus(ByVal x2%, ByVal Q%, ByVal Stat%, ByVal SpN%)
Dim x%, Pct%, T%
  Pct% = 75
  T% = 0
  For x% = 1 To MaxMoves
    If P(x2).Moves(x).Cmdkey <> "" And P(x2).Moves(x).Status(Stat) > Pct% And (Config.Multi = 0 Or P(x2).Moves(x).Target <= 3 Or P(x2).Moves(x).Target = OnlySelf) And Kor(Q = x2, P(x2).Moves(x).Target = OnlySelf) And (P(x2).Status(sMute) = 0 Or P(x2).Moves(x).Element = Phys) And (P(x2).Status(sScarecrow) = 0 Or P(x2).Moves(x).Element <> Phys) And (x <= 19 Or (P(x2).Moves(x).ReqAllUses And P(x2).WpnUsesLeft = Weapons(P(x2).Weapon).NumUses)) And (P(x2).SuperNum = SpN Or P(x2).SuperNum <= 1) Then
      T% = x%
      Pct% = P(x2).Moves(x).Status(Stat%)
    End If
  Next x%
  If P(x2).Rune = RuneSwiss Or P(Q).Rune = RuneSwiss Then
    FindMoveByStatus = 0
  Else
    FindMoveByStatus = T%
  End If
End Function

Sub MoveToDo(ByVal x2 As Integer)
' The Artificial Stupidity Routines.
Dim x, X3, x4, x5, LowHP, LowTarget, DoAMove As Integer
Dim Sx As CharType
Dim SN As String
Dim Team, Msg As String
Dim ErCode, ErLine As Integer
  Sx = Senshi(P(x2).CharID)
  Team = P(x2).TeamID
  SN = P(x2).ScrNam
  DoEvents
  BehNormal x2, Mx, Tx, STX
End Sub

Function NumMoves(ByVal x2%)
Dim x%, Ty%
  Ty = 0
  For x = 1 To MaxMoves
    If P(x2).Moves(x).Cmdkey <> "" Then Ty = Ty + 1
  Next x
  NumMoves = Ty
End Function

Function PosesAThreat(M As MoveType) As Integer
  If M.Status(sStun) + M.Status(sMute) + M.Status(sFreeze) + M.Status(sMIA) + M.Status(sStop) + M.Status(sBerserk) >= 30 Then
    PosesAThreat = True
  Else
    PosesAThreat = False
  End If
End Function

Function ProjectedTotalDamage(ByVal x2%, ByVal mo%, ByVal Tg%) As Integer
Dim NumT%, x1%, Td%
  Td = 0
  x1 = P(x2).Moves(mo).Element
  If x1 = Morph Or x1 = Life Or x1 = NoDmg Then
    ProjectedTotalDamage = 0
    GoTo fukkinshit
  End If
  If x1 = -2576 Then
    ProjectedTotalDamage = Int((P(x2).MaxHP - P(x2).HP) * 1.25)
    GoTo fukkinshit
  End If
  If x1 = 103 Then
    ProjectedTotalDamage = P(x2).Moves(mo).ElementStr
    GoTo fukkinshit
  End If
  Select Case P(x2).Moves(mo).Target
    Case Enemy: Td = PredictDamageCount(x2, Tg%, P(x2).Moves(mo), -1)
    Case AllTeam:
      For x1 = 1 To MaxPlayers
        If P(x1).TeamID = TeamID(Tg%) And P(x1).HP > 0 And P(x1).CharID <> 0 And P(x1).Status(sMIA) = 0 Then Td = Td + PredictDamageCount(x2, x1, P(x2).Moves(mo), -1)
      Next x1
    Case AllFoe:
      For x1 = 1 To MaxPlayers
        If P(x1).TeamID <> TeamID(x2%) And P(x1).HP > 0 And P(x1).CharID <> 0 And P(x1).Status(sMIA) = 0 Then Td = Td + PredictDamageCount(x2, x1, P(x2).Moves(mo), -1)
      Next x1
    Case AllButSelf:
      For x1 = 1 To MaxPlayers
        If P(x1).TeamID <> TeamID(x2%) And P(x1).HP > 0 And P(x1).CharID <> 0 And P(x1).Status(sMIA) = 0 Then Td = Td + PredictDamageCount(x2, x1, P(x2).Moves(mo), -1)
        If P(x1).TeamID = TeamID(x2%) And P(x1).HP > 0 And P(x1).CharID <> 0 And P(x1).Status(sMIA) = 0 Then Td = Td - PredictDamageCount(x2, x1, P(x2).Moves(mo), -1)
      Next x1
    Case Everybody:
      For x1 = 1 To MaxPlayers
        If P(x1).TeamID <> TeamID(x2%) And P(x1).HP > 0 And P(x1).CharID <> 0 And P(x1).Status(sMIA) = 0 Then Td = Td + PredictDamageCount(x2, x1, P(x2).Moves(mo), -1)
        If P(x1).TeamID = TeamID(x2%) And P(x1).HP > 0 And P(x1).CharID <> 0 And P(x1).Status(sMIA) = 0 Then Td = Td - PredictDamageCount(x2, x1, P(x2).Moves(mo), -1)
      Next x1
    Case Else:
      Td = 0
  End Select
  ProjectedTotalDamage = Td
fukkinshit:
End Function

Function StrongestPossibleMove(ByVal x2 As Integer, ByVal Tg%)
Dim x%, Strn%, M%, Pj%
  Strn% = -1
  M% = 0
  On Error GoTo 0
  For x = 1 To MaxMoves
    Pj% = Int(ProjectedTotalDamage(x2, x, Tg%) * (P(x2).Moves(x).Status(HitRate) / 100))
    If P(x2).Moves(x).Cmdkey <> "" And DoesDamage(P(x2).Moves(x).Element) And P(x2).Moves(x).Element <> MPTheft And P(x2).Moves(x).Element <> HPTheft And P(x2).Moves(x).CanSuper <= 1 And Pj% > Strn% And (Config.Respawn = 0 Or P(x2).Moves(x).Target <= 3) Then
      If (P(x2).Moves(x).Element = Phys Or P(x2).Status(sMute) = 0) And (P(x2).Moves(x).Element <> Phys Or P(x2).Status(sScarecrow) = 0) Then
        If ((P(x2).Cheese + Pj <= MaxCheeseLimit * DMult / 100) Or PKamek <> 0) And (x <= 19 Or (P(x2).Moves(x).ReqAllUses And P(x2).WpnUsesLeft = Weapons(P(x2).Weapon).NumUses)) Then
          M% = x
          Strn% = Pj%
        End If
      End If
    End If
  Next x
  StrongestPossibleMove = M%
End Function

Function StrongestPossibleSuper(ByVal x2 As Integer, ByVal Lv%, ByVal Tg%, SpNum%)
Dim x%, Strn%, M%, Pj%
  Strn% = 0
  M% = 0
  For x = 1 To MaxMoves
    Pj% = ProjectedSuperDamage(x2, x, Tg%, SpNum%)
    If P(x2).Moves(x).Cmdkey <> "" And DoesDamage(P(x2).Moves(x).Element) And Pj% > Strn% And P(x2).Moves(x).CanSuper And (Config.Respawn = 0 Or P(x2).Moves(x).Target <= 3) And (Lv >= P(x2).Moves(x).CanSuper - 1) And (P(x2).Moves(x).CanSuper > 0) Then
      If (P(x2).Moves(x).Element = Phys) Or (P(x2).Status(sMute) = 0) Then
        If (x <= 19 Or (P(x2).Moves(x).ReqAllUses And P(x2).WpnUsesLeft = Weapons(P(x2).Weapon).NumUses)) Then
          If (P(x2).Cheese + Pj <= MaxCheeseLimit * DMult / 100) And (x <= 19 Or (P(x2).Moves(x).ReqAllUses And P(x2).WpnUsesLeft = Weapons(P(x2).Weapon).NumUses)) Then
            M% = x
            Strn% = Pj%
          End If
        End If
      End If
    End If
  Next x
  StrongestPossibleSuper = M%
End Function

Function IsStrongest(x2%, Mov%, Tg%)
Dim x%, Strn%, M%, Pj%
  Strn% = -1
  M% = 0
  For x = 1 To MaxMoves
    Pj% = ProjectedTotalDamage(x2, x, Tg%)
    If P(x2).Moves(x).Cmdkey <> "" And DoesDamage(P(x2).Moves(x).Element) And P(x2).Moves(x).Element <> MPTheft And Pj% > Strn% And (Config.Respawn = 0 Or P(x2).Moves(x).Target <= 3) Then
      If Not (P(x2).Moves(x).Element <> Phys And P(x2).Status(sMute) <> 0) Then
        M% = x
        Strn% = Pj%
      End If
    End If
  Next x
  IsStrongest = (M% = Mov%)
End Function

Function ProjectedSuperDamage(ByVal x2!, ByVal Mov%, ByVal X6, ByVal SpNum%) As Long
Dim Df As Integer
Dim ST As Integer
Dim y2 As Double, Scro!, C!, SuperHits%
Dim y3!, Barrier!, MBarrier!
  If P(x2).Moves(Mov).Element = Phys Then
    Df = P(X6).PhysDef
    ST = P(x2).PhysStr
  Else
    Df = P(X6).MagDef
    ST = P(x2).MagStr
  End If
  If P(x2).Moves(Mov).Status(sCPUWait) <> 0 Then
    ST = P(x2).Moves(Mov).Status(sCPUWait)
  End If
  'Scro = P(X6).Scroller
  Scro = 0
  If P(x2).Rune = RuneHigh Then ST = ST + (30 * Scro)
  If P(x2).Rune <> RuneMagic Then
    Barrier = P(X6).Status(sBarrier)
    MBarrier = P(X6).Status(sMBarrier)
  End If
  'If P(x2).Status(sBlind) Then ST = 0
  On Error GoTo Whoops3
  'y2 = Int(25 * M.ElementStr * Log(ST + 1) / (Df + 10)) + Rand(-25, 25)
  y2 = Int((P(x2).Moves(Mov).ElementStr * ST / 45) / (0.01 * (Df + 50))) + Rand(-MaxHP * 0.05, MaxHP * 0.05)
  Select Case P(x2).SuperNum
    Case 1: SuperHits = 200
    Case 2: SuperHits = 225
    Case 3: SuperHits = 275
    Case 4: SuperHits = 350
    Case 5: SuperHits = 400
    Case 6: SuperHits = 450
  End Select
  y2 = Int(y2 / 100 * SuperHits)
  y2 = Int(DMult / 100 * y2)
  y2 = ProcessDmg(y2, P(x2).Moves(Mov).Element)
  If Barrier = 1999 And P(x2).Moves(Mov).Element = Phys Then
    y2 = 0
  ElseIf Barrier > 0 And P(x2).Moves(Mov).Element = Phys And P(x2).Rune <> RuneMagic Then
    y3 = Int(y2 / 2)
    y2 = y2 - y3
  End If
  If MBarrier > 0 And P(x2).Moves(Mov).Element <> Phys And P(x2).Moves(Mov).Element <> Heal And P(x2).Rune <> RuneMagic Then
    y3 = Int(y2 / 2)
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
      If Senshi(P(X6).CharID).weakness = P(x2).Moves(Mov).Element Then
        'Send (P(x2).ScrNam + "'s attack is very effective against " + P(X6).ScrNam + "!")
        y2 = Int(y2 * 225 / 100)
      End If
      If Senshi(P(X6).CharID).Resist = P(x2).Moves(Mov).Element Then
        'Send (P(x2).ScrNam + "'s attack isn't very effective against " + P(X6).ScrNam + "...")
        y2 = Int(y2 * 52.5 / 100)
      End If
    End If
  End If
  If y2 < 0 Then y2 = 0
  If P(X6).Rune = RuneArmor And P(X6).RuneTemp = P(x2).Moves(Mov).Element Then
    y2 = -y2
  End If
  'If y2 > 999 Then y2 = 999
  ProjectedSuperDamage = y2
  Exit Function
Whoops3:
  'ScrollSend ("Whoops! Overflow, " + P(X2).ScrNam + "!")
  y2 = 999
  ProjectedSuperDamage = y2
  Exit Function
End Function

