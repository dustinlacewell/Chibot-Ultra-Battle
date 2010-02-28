Option Explicit

'Global Const NoDmg = 0              ' No Damage -- Effect Only
'Global Const Phys = 1               ' Physical Hit
'Global Const Heal = 2               ' Healing Power
'Global Const Morph = 3              ' Secret Morph
'Global Const StopMove = 5           ' Stop an enemy from attacking you
'Global Const Critical = 6           ' Critical Hit / Mutual Death
'Global Const Sacrifice = 7          ' Sacrifice Move
'Global Const MindMeld = 9           ' Luna, Artemis, Diana Mind Melds
'Global Const Invin = 10             ' Temporary Invincibility
'Global Const HPTheft = 11           ' HP Theft
'Global Const MPTheft = 12           ' MP Theft
'Global Const Life = 17              ' Restore Life
'Global Const Demi = 19              ' Cuts enemy's HP in half
'Global Const MoonE = 20             ' Moon Power
'Global Const Shadow = 21            ' Shadow
'Global Const Water = 22             ' Water
'Global Const Reveal = 23            ' Scan
'Global Const Fire = 24              ' Fire nullifies Freeze.
'Global Const Lit = 25               ' Lightning
'Global Const Heart = 26             ' Heart
'Global Const Earth = 27             ' Earth
'Global Const Wind = 28             ' Random Elemental
'Global Const Mult = 29              ' Multiple Elementals
'Global Const Lum = 30               ' Luminous Energy

Function CalcMP (x As Integer) As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim S(sMaxStatus)  As Integer
  Z1 = 0
  Select Case moves(x).Element
    Case Morph: Z1 = 1
    Case HPTheft, MPTheft: Z1 = 1
    Case Life: Z1 = 100
    Case Reveal: Z1 = 15
    Case Demi: Z1 = 70
    Case Invin: Z1 = 30
    Case Else:
      If (moves(x).ElementStr > 20) Then
        Z1 = (moves(x).ElementStr * .8)
      Else
        Z1 = 0
      End If
  End Select
  S = moves(x).Status
  If S.Mute = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Mute * .6)
  If S.Chaos = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Chaos * .6)
  If S.Freeze = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Freeze * .6)
  If S.Sleep = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Sleep * .6)
  If S.Poison = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Poison * .6)
  If S.Blind = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Blind * .6)
  If S.Haste = -1 Then Z1 = Z1 + 20 Else Z1 = Z1 + (S.Haste * .2)
  If S.Slow = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Slow * .6)
  If S.Stun = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Stun * .6)
  If S.Regen = -1 Then Z1 = Z1 + 20 Else Z1 = Z1 + (S.Regen * .7)
  If S.Stop = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Stop * .7)
  If S.Life3 = -1 Then Z1 = Z1 + 50 Else Z1 = Z1 + (S.Life3)
  If S.Mushroom = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Mushroom * .6)
  If S.MIA = -1 Then Z1 = Z1 + 100 Else Z1 = Z1 + (S.MIA)
  If S.Quick = -1 Then Z1 = Z1 + 100 Else Z1 = Z1 + (S.Quick)
  If S.Berserk = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S.Berserk)
  If S.Barrier = -1 Then Z1 = Z1 + 20 Else Z1 = Z1 + (S.Barrier * .5)
  If S.MBarrier = -1 Then Z1 = Z1 + 20 Else Z1 = Z1 + (S.MBarrier * .5)
  If (moves(x).Element = Life) Then Z1 = 100
  Select Case moves(x).Target
    Case 1, 4: Z1 = Z1 * 2
    Case 5: Z1 = Z1 * 2.5
    Case 6, 7: Z1 = Z1 * 3
  End Select
  Z2 = Int(Z1)
  CalcMP = Z2
End Function

Function CheezeNips (x1 As Integer) As Integer
Dim x2%, x4%, x5%
  'For x2 = 1 To MaxMoves
  '  If moves(Senshi(x1).moves(x2)).MPReq > 40 Then
  '    x4 = x4 + moves(Senshi(x1).moves(x2)).MPReq
  '    x5 = x5 + 1
  '  End If
  'Next x2
  'If x5 = 0 Then
    CheezeNips = 0
  'Else
  '  CheezeNips = Int(x4 / x5)
  '  'If Int(x4 / x5) > 150 Then Stop
  'End If
End Function

