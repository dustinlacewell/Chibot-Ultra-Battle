Option Explicit

Sub LoadMove (ByVal Fil$, ByVal X1%, ByVal X%)
Dim Rx%
  On Error GoTo WhoopsLM
  Open Fil$ For Input As 36
    Input #36, Senshi(X1).Moves(X).Name
    Input #36, Senshi(X1).Moves(X).Cmdkey
    Input #36, Senshi(X1).Moves(X).CanSuper
    Input #36, Senshi(X1).Moves(X).Begin2Attack
    Input #36, Senshi(X1).Moves(X).Begin2SuperAttack
    Input #36, Senshi(X1).Moves(X).Begin2HealSelf
    Input #36, Senshi(X1).Moves(X).Hit
    Input #36, Senshi(X1).Moves(X).SuperHit
    Input #36, Senshi(X1).Moves(X).SuperMiss
    If Senshi(X1).Moves(X).SuperMiss = "0" Then Senshi(X1).Moves(X).SuperMiss = ""
    Input #36, Senshi(X1).Moves(X).HealSelf
    Input #36, Senshi(X1).Moves(X).CritHit
    Input #36, Senshi(X1).Moves(X).HealMeld
    Input #36, Senshi(X1).Moves(X).Miss
    Input #36, Senshi(X1).Moves(X).Status.Mute
    Input #36, Senshi(X1).Moves(X).Status.chaos
    Input #36, Senshi(X1).Moves(X).Status.freeze
    Input #36, Senshi(X1).Moves(X).Status.sleep
    Input #36, Senshi(X1).Moves(X).Status.Poison
    Input #36, Senshi(X1).Moves(X).Status.blind
    Input #36, Senshi(X1).Moves(X).Status.defup
    Input #36, Senshi(X1).Moves(X).Status.haste
    Input #36, Senshi(X1).Moves(X).Status.attup
    Input #36, Senshi(X1).Moves(X).Status.DefDn
    Input #36, Senshi(X1).Moves(X).Status.AttDn
    Input #36, Senshi(X1).Moves(X).Status.slow
    Input #36, Senshi(X1).Moves(X).Status.stun
    Input #36, Senshi(X1).Moves(X).Status.Life3
    Input #36, Senshi(X1).Moves(X).Status.Regen
    Input #36, Senshi(X1).Moves(X).Status.Stop
    Input #36, Senshi(X1).Moves(X).Status.Mushroom
    Input #36, Senshi(X1).Moves(X).Status.MIA
    Input #36, Senshi(X1).Moves(X).Status.Quick
    Input #36, Senshi(X1).Moves(X).Status.Berserk
    Input #36, Senshi(X1).Moves(X).Status.Barrier
    Input #36, Senshi(X1).Moves(X).Status.MBarrier
    Input #36, Senshi(X1).Moves(X).Status.Curse
    Input #36, Senshi(X1).Moves(X).Status.Bless
    Input #36, Rx
    Input #36, Rx
    Input #36, Rx
    Input #36, Rx
    Input #36, Senshi(X1).Moves(X).Element
    Input #36, Senshi(X1).Moves(X).ElementStr
    Input #36, Senshi(X1).Moves(X).Target
    Senshi(X1).Moves(X).MPReq = CalcMP(X1, X)
    Close 36
  Exit Sub
WhoopsLM:
  MsgBox Error$ + " loading move", 16
  Close 36
  Exit Sub
End Sub

Sub SaveMove (ByVal Fil$, ByVal X1%, ByVal X%)
  On Error GoTo WhoopsSM
  Open Fil$ For Output As 37
    Write #37, Senshi(X1).Moves(X).Name
    Write #37, Senshi(X1).Moves(X).Cmdkey
    Write #37, Senshi(X1).Moves(X).CanSuper
    Write #37, Senshi(X1).Moves(X).Begin2Attack
    Write #37, Senshi(X1).Moves(X).Begin2SuperAttack
    Write #37, Senshi(X1).Moves(X).Begin2HealSelf
    Write #37, Senshi(X1).Moves(X).Hit
    Write #37, Senshi(X1).Moves(X).SuperHit
    Write #37, Senshi(X1).Moves(X).SuperMiss
    If Senshi(X1).Moves(X).SuperMiss = "0" Then Senshi(X1).Moves(X).SuperMiss = ""
    Write #37, Senshi(X1).Moves(X).HealSelf
    Write #37, Senshi(X1).Moves(X).CritHit
    Write #37, Senshi(X1).Moves(X).HealMeld
    Write #37, Senshi(X1).Moves(X).Miss
    Write #37, Senshi(X1).Moves(X).Status.Mute
    Write #37, Senshi(X1).Moves(X).Status.chaos
    Write #37, Senshi(X1).Moves(X).Status.freeze
    Write #37, Senshi(X1).Moves(X).Status.sleep
    Write #37, Senshi(X1).Moves(X).Status.Poison
    Write #37, Senshi(X1).Moves(X).Status.blind
    Write #37, Senshi(X1).Moves(X).Status.defup
    Write #37, Senshi(X1).Moves(X).Status.haste
    Write #37, Senshi(X1).Moves(X).Status.attup
    Write #37, Senshi(X1).Moves(X).Status.DefDn
    Write #37, Senshi(X1).Moves(X).Status.AttDn
    Write #37, Senshi(X1).Moves(X).Status.slow
    Write #37, Senshi(X1).Moves(X).Status.stun
    Write #37, Senshi(X1).Moves(X).Status.Life3
    Write #37, Senshi(X1).Moves(X).Status.Regen
    Write #37, Senshi(X1).Moves(X).Status.Stop
    Write #37, Senshi(X1).Moves(X).Status.Mushroom
    Write #37, Senshi(X1).Moves(X).Status.MIA
    Write #37, Senshi(X1).Moves(X).Status.Quick
    Write #37, Senshi(X1).Moves(X).Status.Berserk
    Write #37, Senshi(X1).Moves(X).Status.Barrier
    Write #37, Senshi(X1).Moves(X).Status.MBarrier
    Write #37, Senshi(X1).Moves(X).Status.Curse
    Write #37, Senshi(X1).Moves(X).Status.Bless
    Write #37, 0
    Write #37, 0
    Write #37, 0
    Write #37, 0
    Write #37, Senshi(X1).Moves(X).Element
    Write #37, Senshi(X1).Moves(X).ElementStr
    Write #37, Senshi(X1).Moves(X).Target
    Senshi(X1).Moves(X).MPReq = CalcMP(X1, X)
    Close 37
  Exit Sub
WhoopsSM:
  MsgBox Error$ + " saving move", 16
  Close 37
  Exit Sub
End Sub

