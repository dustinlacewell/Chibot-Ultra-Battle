Attribute VB_Name = "CalMP"
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

Function CalcMP(x As Integer) As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim LCV%
Dim S(sMaxStatus) As Integer
  Z1 = 0
  Select Case Moves(x).Element
    Case Morph: Z1 = 1
    Case HPTheft, MPTheft: Z1 = 1
    Case Life: Z1 = 100
    Case Reveal: Z1 = 15
    Case Demi: Z1 = 70
    Case Invin: Z1 = 30
    Case Else:
      If (Moves(x).ElementStr > 20) And (Moves(x).Element <> Phys) Then
        Z1 = (Moves(x).ElementStr * 0.6 * Moves(x).Status(HitRate) / 100)
      Else
        Z1 = 0
      End If
  End Select
  For LCV = 1 To sMaxStatus
    S(LCV) = Moves(x).Status(LCV)
  Next LCV
  If S(sMute) = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S(sMute) * 0.5)
  If S(sChaos) = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S(sChaos) * 0.4)
  If S(sFreeze) = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S(sFreeze) * 0.6)
  If S(sSleep) = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S(sSleep) * 0.6)
  If S(sPoison) = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S(sPoison) * 0.4)
  If S(sBlind) = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S(sBlind) * 0.4)
  If S(sHaste) = -1 Then Z1 = Z1 + 20 Else Z1 = Z1 + (S(sHaste) * 0.2)
  If S(sSlow) = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S(sSlow) * 0.4)
  If S(sStun) = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S(sStun) * 0.6)
  If S(sRegen) = -1 Then Z1 = Z1 + 20 Else Z1 = Z1 + (S(sRegen) * 0.5)
  If S(sStop) = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S(sStop) * 0.6)
  If S(sReraise) = -1 Then Z1 = Z1 + 50 Else Z1 = Z1 + (S(sReraise))
  If S(sMushroom) = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S(sMushroom) * 0.6)
  If S(sMIA) = -1 Then Z1 = Z1 + 100 Else Z1 = Z1 + (S(sMIA))
  If S(sQuick) = -1 Then Z1 = Z1 + 100 Else Z1 = Z1 + (S(sQuick))
  If S(sBerserk) = -1 Then Z1 = Z1 + 5 Else Z1 = Z1 + (S(sBerserk) * 0.4)
  If S(sBarrier) = -1 Then Z1 = Z1 + 20 Else Z1 = Z1 + (S(sBarrier) * 0.4)
  If S(sMBarrier) = -1 Then Z1 = Z1 + 20 Else Z1 = Z1 + (S(sMBarrier) * 0.4)
  'If (Moves(x).Element = Life) Then Z1 = 100
  Select Case Moves(x).Target
    Case 1, 4: Z1 = Z1 * 2
    Case 5: Z1 = Z1 * 2.5
    Case 6, 7: Z1 = Z1 * 3
  End Select
  Select Case Moves(x).CanSuper
    Case 1 To 5: Z1 = Z1 * (2 / Moves(x).CanSuper)
    Case 6: Z1 = Z1 / 2
    Case 7: Z1 = Z1 / 4
  End Select
  Z2 = Int(Z1)
  'CalcMP = Z2
  CalcMP = 0
End Function

Function CheezeNips(x1 As Integer) As Integer
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

