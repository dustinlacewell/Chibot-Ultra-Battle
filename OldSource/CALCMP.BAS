Option Explicit

Function CalcMP (x1 As Integer, x As Integer) As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim S As MStatusType
  Z1 = 0
  Select Case Senshi(x1).Moves(x).Element
    Case Morph: Z1 = 1
    'Case Cover: Z1 = 1
    'Case StopMove: Z1 = 35
    'Case Critical: Z1 = 0
    'Case Sacrifice: Z1 = 0
    Case HPTheft, MPTheft: Z1 = 1
    Case Life: Z1 = 100
    Case Reveal: Z1 = 15
    Case Demi: Z1 = 70
    Case Invin: Z1 = 30
    Case Else:
      If (Senshi(x1).Moves(x).ElementStr > 20) Then
        Z1 = (Senshi(x1).Moves(x).ElementStr * .8)
      Else
        Z1 = 0
      End If
  End Select
  S = Senshi(x1).Moves(x).Status
  If S.Mute = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Mute * .6)
  If S.chaos = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.chaos * .6)
  If S.freeze = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.freeze * .6)
  If S.sleep = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.sleep * .6)
  If S.Poison = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Poison * .6)
  If S.blind = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.blind * .6)
  'If S.DefUp = -1 Then Z1 = Z1 + 20 Else Z1 = Z1 + (S.DefUp * .2)
  If S.haste = -1 Then Z1 = Z1 + 20 Else Z1 = Z1 + (S.haste * .2)
  'If S.AttUp = -1 Then Z1 = Z1 + 20 Else Z1 = Z1 + (S.AttUp * .2)
  If S.slow = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.slow * .6)
  If S.stun = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.stun * .6)
  'If S.DefDn = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.DefDn * .6)
  'If S.AttDn = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.AttDn * .6)
  If S.Regen = -1 Then Z1 = Z1 + 20 Else Z1 = Z1 + (S.Regen * .7)
  If S.Stop = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Stop * .7)
  If S.Life3 = -1 Then Z1 = Z1 + 50 Else Z1 = Z1 + (S.Life3)
  If S.Mushroom = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Mushroom * .6)
  If S.MIA = -1 Then Z1 = Z1 + 100 Else Z1 = Z1 + (S.MIA)
  If S.Quick = -1 Then Z1 = Z1 + 100 Else Z1 = Z1 + (S.Quick)
  If S.Berserk = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Berserk)
  If S.Barrier = -1 Then Z1 = Z1 + 20 Else Z1 = Z1 + (S.Barrier * .5)
  If S.MBarrier = -1 Then Z1 = Z1 + 20 Else Z1 = Z1 + (S.MBarrier * .5)
  If (Senshi(x1).Moves(x).Element = Life) Then Z1 = 100
  Select Case Senshi(x1).Moves(x).Target
    Case 1, 4: Z1 = Z1 * 2
    Case 5: Z1 = Z1 * 2.5
    Case 6, 7: Z1 = Z1 * 3
  End Select
  Z2 = Int(Z1)
  CalcMP = Z2
End Function

Function CheezeNips (x1 As Integer) As Integer
Dim x2%, x4%, x5%
  CheezeNips = 0
End Function

