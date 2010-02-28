Option Explicit

Sub BehNormal (ByVal x2 As Integer, MTD%, Tgt%, MT$)
Dim X%, X3%, x4%, x5%, x6%, x7%, x8%, x9%, x0%, Bla%
Dim WpnTk%, Goto1%
Beginning:
  If Battle = 0 Then Exit Sub
  WpnTk = 0
  x6% = False
  If Config.Flag <> 0 Then
    If RedFlag = 0 Then
      RedFlag = x2
      Send (p(x2).ScrNam + " claims the Red Flag.")
    End If
    If BlueFlag = 0 Then
      BlueFlag = x2
      Send (p(x2).ScrNam + " claims the Blue Flag.")
    End If
  End If
  For X% = 1 To MaxPlayers
    DoEvents
    If p(X).Target = x2 And p(X).CurMove < MaxMoves And p(X).CurMove > 0 Then
      If p(X).SuperNum > 0 Then
        x6 = X
        Exit For
      End If
    End If
  Next X%
  Goto1 = 0
DoItAgain:
  If x6% <> 0 Then
    x5% = FindMoveByElement(x2, Invin, x2)
    If p(x2).HP <= MaxHP * 2 / 5 Then
      If (x5% <> 0) Then
        MTD% = x5%
        Tgt% = x2
        Exit Sub
      ElseIf Goto1 = 0 Then
        x5% = FindDroppedByElement(x2, Invin, x2)
        If x5 <> 0 Then
          If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
          CPUGetWeapon x2, x5
          WpnTk = 1
          Goto1 = 1
          GoTo DoItAgain
        End If
      End If
    End If
    If p(x2).Super >= 100 Or p(x2).HP <= MaxHP * 3 / 5 Then
      If p(x2).Super >= 200 Then
        Tgt% = FindChiCtr(x2, x6)
        If Tgt% = 0 Then Tgt% = FindCounter(x2, x6)
      Else
        Tgt% = FindCounter(x2, x6)
      End If
      If Tgt% <> 0 Then
        Select Case p(x2).Super
          Case Is >= 200:
            'Send (p(x2).ScrNam + " is Chibot Countering with " + p(x2).Moves(Tgt%).Name + "!")
            p(x2).Super = p(x2).Super - 100
            p(x2).Status(sHamedo) = -1
          Case 100 To 199:
            'Send (p(x2).ScrNam + " is whoopin' up with " + p(x2).Moves(Tgt%).Name + "!")
        End Select
      End If
      MTD% = pBlock
      Exit Sub
    End If
  End If
  If (p(x2).Super >= 300 And Config.Fours = 0) Or (p(x2).Super >= 400 And Config.Fours) Then
    x5% = StrongestPossibleSuper(x2)
    Tgt% = CPUTarget(x2)
    If x5% <> 0 And Tgt% <> 0 Then
      'Send (p(x2).ScrNam + " says, ""/3-" + p(x2).Moves(x5).Cmdkey + "-" + ScrNam(Tgt%) + """")
      MTD% = x5%
      If p(x2).Super >= 400 And Config.Fours Then
        p(x2).SuperNum = 4
      ElseIf p(x2).SuperNum >= 300 Then
        p(x2).SuperNum = 3
      ElseIf p(x2).SuperNum >= 200 Then
        p(x2).SuperNum = 2
      ElseIf p(x2).SuperNum >= 100 Then
        p(x2).SuperNum = 1
      Else
        kDlgBox "Error!", 16, "BehNormal"
        Stop
      End If
      Exit Sub
    End If
  End If
  If p(x2).Status(sLife3) = 0 Then
    x5% = FindMoveByStatus(x2, x2, sLife3)
    If x5% <> 0 Then
      MTD% = x5%
      Tgt% = x2
      Exit Sub
    ElseIf Goto1 = 0 Then
      x5 = FindDroppedByStatus(x2, x2, sLife3)
      If x5 <> 0 Then
        If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
        CPUGetWeapon x2, x5
        WpnTk = 1
        Goto1 = 1
        GoTo DoItAgain
      End If
    End If
  End If
  If Config.Flag <> 0 Then
    If p(x2).TeamID = "R" And p(BlueFlag).TeamID = "R" Then Bla = BlueFlag
    If p(x2).TeamID = "B" And p(RedFlag).TeamID = "B" Then Bla = RedFlag
    If p(x2).TeamID = "R" And p(RedFlag).TeamID = "R" And FoundRedFlag Then Bla = RedFlag
    If p(x2).TeamID = "B" And p(BlueFlag).TeamID = "B" And FoundBlueFlag Then Bla = BlueFlag
    If p(Bla).Status(sMIA) <> 0 Then Bla = 0
    If Bla <> 0 Then
      x5% = FindMoveByStatus(x2, Bla, sMIA)
      If x5% <> 0 Then
        MTD% = x5%
        Tgt% = Bla
        Exit Sub
      ElseIf Goto1 = 0 Then
        x5 = FindDroppedByStatus(x2, x2, sMIA)
        If x5 <> 0 Then
          If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
          CPUGetWeapon x2, x5
          WpnTk = 1
          Goto1 = 1
          GoTo DoItAgain
        End If
      End If
      If p(Bla).Status(sLife3) = 0 Then
        x5% = FindMoveByStatus(x2, Bla, sLife3)
        If x5% <> 0 Then
          MTD% = x5%
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByStatus(x2, Bla, sLife3)
          If x5 <> 0 Then
            If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
      x6% = False
      For X% = 1 To MaxPlayers
        DoEvents
        If p(X).Target = Bla And p(X).CurMove < MaxMoves And p(X).CurMove > 0 Then If p(X).Moves(p(X).CurMove).Target = Enemy Then x6 = True
      Next X%
      If x6% Then
        x5% = FindMoveByElement(x2, Invin, Bla)
        If x5% <> 0 Then
          MTD% = x5%
          Tgt% = x2
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByElement(x2, Invin, Bla)
          If x5 <> 0 Then
            If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
      If p(Bla).HP < (MaxHP * 2 / 5) Then
        x5% = FindMoveByElement(x2, Heal, Bla)
        If x5% <> 0 Then
          MTD% = x5%
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByElement(x2, Heal, Bla)
          If x5 <> 0 Then
            If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
      If p(Bla).Status(sRegen) = 0 Then
        x5% = FindMoveByStatus(x2, Bla, sRegen)
        If x5 <> 0 Then
          MTD% = x5
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByStatus(x2, Bla, sRegen)
          If x5 <> 0 Then
            If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
      If p(Bla).Status(sHaste) = 0 Then
        x5% = FindMoveByStatus(x2, Bla, sHaste)
        If x5 <> 0 Then
          MTD% = x5
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByStatus(x2, Bla, sHaste)
          If x5 <> 0 Then
            If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
      If p(Bla).Status(sBarrier) = 0 Then
        x5 = FindMoveByStatus(x2, Bla, sBarrier)
        If x5 <> 0 Then
          MTD% = x5
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByStatus(x2, Bla, sBarrier)
          If x5 <> 0 Then
            If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
      If p(Bla).Status(sMBarrier) = 0 Then
        x5 = FindMoveByStatus(x2, Bla, sMBarrier)
        If x5 <> 0 Then
          MTD% = x5
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByStatus(x2, Bla, sMBarrier)
          If x5 <> 0 Then
            If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
      If p(Bla).HP < (MaxHP * 4 / 5) Then
        x5% = FindMoveByElement(x2, Heal, Bla)
        If x5% <> 0 Then
          MTD% = x5%
          Tgt% = Bla
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByElement(x2, Heal, Bla)
          If x5 <> 0 Then
            If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
            CPUGetWeapon x2, x5
            WpnTk = 1
            Goto1 = 1
            GoTo DoItAgain
          End If
        End If
      End If
    End If
  End If
  If p(x2).HP < (MaxHP * 3 / 5) Then
    x5% = FindMoveByElement(x2, Heal, x2)
    If x5% <> 0 Then
      MTD% = x5%
      Tgt% = x2
      Exit Sub
    ElseIf Goto1 = 0 Then
      x5 = FindDroppedByElement(x2, Heal, x2)
      If x5 <> 0 Then
        If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
        CPUGetWeapon x2, x5
        WpnTk = 1
        Goto1 = 1
        GoTo DoItAgain
      End If
    End If
  End If
  If p(x2).Status(sRegen) = 0 Then
    x5% = FindMoveByStatus(x2, x2, sRegen)
    If x5 <> 0 Then
      MTD% = x5
      Tgt% = x2
      Exit Sub
    ElseIf Goto1 = 0 Then
      x5 = FindDroppedByStatus(x2, x2, sRegen)
      If x5 <> 0 Then
        If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
        CPUGetWeapon x2, x5
        WpnTk = 1
        Goto1 = 1
        GoTo DoItAgain
      End If
    End If
  End If
  If p(x2).Status(sHaste) = 0 Then
    x5% = FindMoveByStatus(x2, x2, sHaste)
    If x5 <> 0 Then
      MTD% = x5
      Tgt% = x2
      Exit Sub
    ElseIf Goto1 = 0 Then
      x5 = FindDroppedByStatus(x2, x2, sHaste)
      If x5 <> 0 Then
        If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
        CPUGetWeapon x2, x5
        WpnTk = 1
        Goto1 = 1
        GoTo DoItAgain
      End If
    End If
  End If
  If p(x2).Status(sBless) = 0 Then
    x5% = FindMoveByStatus(x2, x2, sBless)
    If x5 <> 0 Then
      MTD% = x5
      Tgt% = x2
      Exit Sub
    ElseIf Goto1 = 0 Then
      x5 = FindDroppedByStatus(x2, x2, sBless)
      If x5 <> 0 Then
        If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
        CPUGetWeapon x2, x5
        WpnTk = 1
        Goto1 = 1
        GoTo DoItAgain
      End If
    End If
  End If
  If p(x2).Status(sBarrier) = 0 Then
    x5 = FindMoveByStatus(x2, x2, sBarrier)
    If x5 <> 0 Then
      MTD% = x5
      Tgt% = x2
      Exit Sub
    ElseIf Goto1 = 0 Then
      x5 = FindDroppedByStatus(x2, x2, sBarrier)
      If x5 <> 0 Then
        If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
        CPUGetWeapon x2, x5
        WpnTk = 1
        Goto1 = 1
        GoTo DoItAgain
      End If
    End If
  End If
  If p(x2).Status(sMBarrier) = 0 Then
    x5 = FindMoveByStatus(x2, x2, sMBarrier)
    If x5 <> 0 Then
      MTD% = x5
      Tgt% = x2
      Exit Sub
    ElseIf Goto1 = 0 Then
      x5 = FindDroppedByStatus(x2, x2, sMBarrier)
      If x5 <> 0 Then
        If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
        CPUGetWeapon x2, x5
        WpnTk = 1
        Goto1 = 1
        GoTo DoItAgain
      End If
    End If
  End If
  For x4% = 1 To MaxPlayers
    DoEvents
    If p(x4).TeamID = p(x2).TeamID And p(x4).CharID <> 0 And p(x4).HP > 0 And p(x4).Status(sMIA) = 0 Then
      If p(x4).HP < (MaxHP * 3 / 5) And Rand(1, 100) <= p(x2).Goodwill Then
        x5% = FindMoveByElement(x2, Heal, x4)
        If x5% <> 0 Then
          MTD% = x5%
          Tgt% = x4%
          Exit Sub
        ElseIf Goto1 = 0 Then
          x5 = FindDroppedByElement(x2, Heal, x4)
          If x5 <> 0 Then
            If p(x2).Weapon <> 0 Then DropWeapon x2, "%SN hastily tosses the %T aside."
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
  If Item2Get <> 0 And Rand(1, 100) <= p(x2).Greed Then
    MTD% = pGet
    Tgt% = 0
    Exit Sub
  End If
  If Rand(1, 100) <= p(x2).Arrogance And Rand(1, 5) = 3 And p(x2).HP >= 250 Then
    MTD% = pTaunt
    Tgt% = 0
    Exit Sub
  End If
  If p(x2).Status(sMorph) <> 0 Then
    If XTimer - p(x2).Status(sMorph) >= 120 Then
      Send (p(x2).ScrNam + " reverts back to " + Senshi(p(x2).OldCharID).FullName + ".")
      UnmorphMe (x2)
      GoTo Beginning
    End If
  End If
  If p(x2).Weapon = 0 Then
    For x4 = 1 To MaxDropped
      If Dropped(x4) <> 0 Then
        CPUGetWeapon x2, x4
        WpnTk = 1
        Exit For
      End If
    Next x4
  End If
  x5 = FindMoveByElement(x2, Morph, x2)
  If x5 <> 0 And (Rand(1, 10) = 2 Or (NumMoves(x2) <= 4 And Rand(1, 5) = 2)) Then
    Tgt% = x2
    MTD% = x5
    MT$ = Senshi(Rand(1, NumSenshi)).FullName
    Exit Sub
  End If
  Tgt% = CPUTarget(x2)
  MTD% = StrongestPossibleMove(x2, Tgt%)
  If MTD < 15 And WpnTk = 0 And p(x2).Weapon <> 0 Then
    p(x2).UselessWpn = p(x2).UselessWpn + 1
    If p(x2).UselessWpn >= 3 Then
      DropWeapon x2, "%SN decides that the %T is of no use and drops it."
    End If
  End If
  If HPx(Tgt%) <= (MaxHP / 6) And Senshi(p(x2).CharID).Fatality.Cmdkey <> "" Then MTD% = pFatal
  If (Tgt% <> 0) And (MTD% <> 0) Then Exit Sub
  MTD% = pRest
  Tgt% = x2
End Sub

Sub CPUGetWeapon (ByVal x2%, ByVal x4%)
  p(x2).Weapon = Dropped(x4)
  Dropped(x4) = 0
  InitMoves (x2)
  p(x2).WpnUsesLeft = DropUses(x4)
  DropUses(x4) = 0
  Send (Parse(Weapons(p(x2).Weapon).SelectStr, p(x2).ScrNam, "", "", ""))
  p(x2).UselessWpn = 0
End Sub

Function FindChiCtr (ByVal x2%, ByVal Tg%)
Dim X%, Strn%, M%, Pj%, Pg%
  Strn% = -1
  M% = 0
  For X = 1 To MaxMoves
    Pj% = ProjectedTotalDamage(x2, X, Tg%)
    If p(x2).Moves(X).Cmdkey <> "" And DoesDamage(p(x2).Moves(X).Element) And p(x2).Moves(X).Element <> MPTheft And Pj% > Strn% Then
      If Not (p(x2).Moves(X).Element <> Phys And p(x2).Status(sMute) <> 0) Then
        M% = X
        Strn% = Pj%
      End If
    End If
  Next X
  If Strn% < HPx(Tg) Then
    If p(Tg).Moves(p(Tg).CurMove).Element <> Phys Then Pg% = FindMoveByStatus(x2, Tg, sMute)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sFreeze)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sStun)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sSleep)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sChaos)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sBerserk)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sMushroom)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sMIA)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sStop)
    If Pg% = 0 Then Pg% = FindMoveByStatus(x2, Tg, sSlow)
    If Pg% = 0 Then Pg% = M%
  Else
    Pg% = M%
  End If
  FindChiCtr = Pg%
End Function

Function FindCounter (ByVal x2 As Integer, ByVal Tg%)
Dim X%, Strn%, M%, Pj%
  Strn% = -1
  M% = 0
  For X = 1 To MaxMoves
    Pj% = ProjectedTotalDamage(x2, X, Tg%)
    If p(x2).Moves(X).Cmdkey <> "" And DoesDamage(p(x2).Moves(X).Element) And p(x2).Moves(X).Element <> MPTheft And Pj% > Strn% Then
      If Not (p(x2).Moves(X).Element <> Phys And p(x2).Status(sMute) <> 0) Then
        M% = X
        Strn% = Pj%
      End If
    End If
  Next X
  FindCounter = M%
End Function

Function FindDroppedByElement (ByVal x2 As Integer, ByVal Ele%, ByVal Q%)
Dim X%, Strn%, Sug%, W%
  If Ele% <> Phys And p(x2).Status(sMute) Then
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
    For X% = 1 To 5
      If Weapons(Dropped(W)).Moves(X).Element = Ele% And (Config.Respawn = 0 Or Weapons(Dropped(W)).Moves(X).Target <= 3) And Kor(Q = x2, Weapons(Dropped(W)).Moves(X).Target = OnlySelf) Then
        If Weapons(Dropped(W)).Moves(X).ElementStr > Strn Then
          Strn = Weapons(Dropped(W)).Moves(X).ElementStr
          Sug = W
        End If
      End If
    Next X%
  Next W
  FindDroppedByElement = Sug
End Function

Function FindDroppedByStatus (ByVal x2%, ByVal Q%, ByVal Stat%)
Dim X%, Pct%, T%, W%
  Pct% = 75
  T% = 0
  For W = 1 To MaxDropped
    For X% = 1 To 5
      If Weapons(Dropped(W)).Moves(X).Status(Stat) > Pct% And (Config.Multi = 0 Or Weapons(Dropped(W)).Moves(X).Target <= 3 Or Weapons(Dropped(W)).Moves(X).Target = OnlySelf) And Kor(Q = x2, Weapons(Dropped(W)).Moves(X).Target = OnlySelf) And (p(x2).Status(sMute) = 0 Or Weapons(Dropped(W)).Moves(X).Element = Phys) Then
        T% = W%
        Pct% = Weapons(Dropped(W)).Moves(X).Status(Stat%)
      End If
    Next X%
  Next W
  FindDroppedByStatus = T%
End Function

Function FindMoveByElement (ByVal x2 As Integer, ByVal Ele%, ByVal Q%)
Dim X%, Strn%, Sug%, x5%
  If Ele% <> Phys And p(x2).Status(sMute) Then
    FindMoveByElement = 0
    Exit Function
  End If
  If Ele = Invin And Config.Flag <> 0 Then
    FindMoveByElement = 0
    Exit Function
  End If
  Strn = -1
  Sug = 0
  For X% = 1 To MaxMoves
    If p(x2).Moves(X).Cmdkey <> "" And p(x2).Moves(X).Element = Ele% And (Config.Respawn = 0 Or p(x2).Moves(X).Target <= 3) And Kor(Q = x2, p(x2).Moves(X).Target = OnlySelf) Then
      If (p(x2).Moves(X).ReqAllUses And p(x2).WpnUsesLeft = Weapons(p(x2).Weapon).NumUses) Or (p(x2).Moves(X).ReqAllUses = 0) Then
        x5 = ProjectedTotalDamage(x2, X, Q)
        If p(x2).Moves(X).Element = Heal Then x5 = p(x2).Moves(X).ElementStr
        If x5 > Strn Then
          If p(x2).Cheese + x5 <= MaxCheeseLimit Then
            Strn = x5
            Sug = X
          End If
        End If
      End If
    End If
  Next X%
  FindMoveByElement = Sug
End Function

Function FindMoveByStatus (ByVal x2%, ByVal Q%, ByVal Stat%)
Dim X%, Pct%, T%
  Pct% = 75
  T% = 0
  For X% = 1 To MaxMoves
    If p(x2).Moves(X).Cmdkey <> "" And p(x2).Moves(X).Status(Stat) > Pct% And (Config.Multi = 0 Or p(x2).Moves(X).Target <= 3 Or p(x2).Moves(X).Target = OnlySelf) And Kor(Q = x2, p(x2).Moves(X).Target = OnlySelf) And (p(x2).Status(sMute) = 0 Or p(x2).Moves(X).Element = Phys) Then
      T% = X%
      Pct% = p(x2).Moves(X).Status(Stat%)
    End If
  Next X%
  FindMoveByStatus = T%
End Function

Sub MoveToDo (ByVal x2 As Integer)
' The Artificial Stupidity Routines.
Dim X, X3, x4, x5, LowHP, LowTarget, DoAMove As Integer
Dim Sx As CharType
Dim SN As String
Dim Team, Msg As String
Dim ErCode, ErLine As Integer
  Sx = Senshi(p(x2).CharID)
  Team = p(x2).TeamID
  SN = p(x2).ScrNam
  DoEvents
  BehNormal x2, Mx, Tx, STX
End Sub

Function NumMoves (ByVal x2%)
Dim X%, Ty%
  Ty = 0
  For X = 1 To MaxMoves
    If p(x2).Moves(X).Cmdkey <> "" Then Ty = Ty + 1
  Next X
  NumMoves = Ty
End Function

Function PosesAThreat (M As MoveType) As Integer
  If M.Status(sStun) + M.Status(sMute) + M.Status(sFreeze) + M.Status(sMIA) + M.Status(sStop) + M.Status(sBerserk) >= 30 Then
    PosesAThreat = True
  Else
    PosesAThreat = False
  End If
End Function

Function ProjectedTotalDamage (ByVal x2%, ByVal mo%, ByVal Tg%) As Integer
Dim NumT%, x1%, TD%
  TD = 0
  x1 = p(x2).Moves(mo).Element
  If x1 = Morph Or x1 = Life Or x1 = NoDmg Then
    ProjectedTotalDamage = 0
    Exit Function
  End If
  Select Case p(x2).Moves(mo).Target
    Case Enemy: TD = PredictDamageCount(x2, Tg%, p(x2).Moves(mo), -1)
    Case AllTeam:
      For x1 = 1 To MaxPlayers
        If p(x1).TeamID = TeamID(Tg%) And p(x1).HP > 0 And p(x1).CharID <> 0 And p(x1).Status(sMIA) = 0 Then TD = TD + PredictDamageCount(x2, x1, p(x2).Moves(mo), -1)
      Next x1
    Case AllFoe:
      For x1 = 1 To MaxPlayers
        If p(x1).TeamID <> TeamID(x2%) And p(x1).HP > 0 And p(x1).CharID <> 0 And p(x1).Status(sMIA) = 0 Then TD = TD + PredictDamageCount(x2, x1, p(x2).Moves(mo), -1)
      Next x1
    Case AllButSelf:
      For x1 = 1 To MaxPlayers
        If p(x1).TeamID <> TeamID(x2%) And p(x1).HP > 0 And p(x1).CharID <> 0 And p(x1).Status(sMIA) = 0 Then TD = TD + PredictDamageCount(x2, x1, p(x2).Moves(mo), -1)
        If p(x1).TeamID = TeamID(x2%) And p(x1).HP > 0 And p(x1).CharID <> 0 And p(x1).Status(sMIA) = 0 Then TD = TD - PredictDamageCount(x2, x1, p(x2).Moves(mo), -1)
      Next x1
    Case Everybody:
      For x1 = 1 To MaxPlayers
        If p(x1).TeamID <> TeamID(x2%) And p(x1).HP > 0 And p(x1).CharID <> 0 And p(x1).Status(sMIA) = 0 Then TD = TD + PredictDamageCount(x2, x1, p(x2).Moves(mo), -1)
        If p(x1).TeamID = TeamID(x2%) And p(x1).HP > 0 And p(x1).CharID <> 0 And p(x1).Status(sMIA) = 0 Then TD = TD - PredictDamageCount(x2, x1, p(x2).Moves(mo), -1)
      Next x1
    Case Else:
      TD = 0
  End Select
  ProjectedTotalDamage = TD
End Function

Function StrongestPossibleMove (ByVal x2 As Integer, ByVal Tg%)
Dim X%, Strn%, M%, Pj%
  Strn% = -1
  M% = 0
  For X = 1 To MaxMoves
    Pj% = ProjectedTotalDamage(x2, X, Tg%)
    If p(x2).Moves(X).Cmdkey <> "" And DoesDamage(p(x2).Moves(X).Element) And p(x2).Moves(X).Element <> MPTheft And Pj% > Strn% And (Config.Respawn = 0 Or p(x2).Moves(X).Target <= 3) Then
      If Not (p(x2).Moves(X).Element <> Phys And p(x2).Status(sMute) <> 0) Then
        If p(x2).Cheese + Pj <= MaxCheeseLimit Then
          M% = X
          Strn% = Pj%
        End If
      End If
    End If
  Next X
  StrongestPossibleMove = M%
End Function

Function StrongestPossibleSuper (ByVal x2 As Integer)
Dim X%, Strn%, M%
  Strn% = 0
  M% = 0
  For X = 1 To MaxMoves
    If p(x2).Moves(X).Cmdkey <> "" And DoesDamage(p(x2).Moves(X).Element) And p(x2).Moves(X).ElementStr > Strn% And p(x2).Moves(X).CanSuper And (Config.Respawn = 0 Or p(x2).Moves(X).Target <= 3) Then
      If (p(x2).Moves(X).Element = Phys) Or (p(x2).Status(sMute) = 0) Then
        M% = X
        Strn% = p(x2).Moves(X).ElementStr
      End If
    End If
  Next X
  StrongestPossibleSuper = M%
End Function

