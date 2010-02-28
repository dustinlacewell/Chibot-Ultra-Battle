Attribute VB_Name = "MoveDmgModule"
Option Explicit

Sub DamageCount(L%, H%)
Dim Df As Integer
Dim ST As Integer
Dim y2 As Integer, Scro%, c%
Dim y3%, Barrier%, LSuperHits%, HSuperHits%, MaxHP%
  MaxHP = Val(DCalc.tMaxHP.Text)
  ST = Val(DCalc.tStr.Text)
  Df = Val(DCalc.tDef.Text)
  Scro = Val(DCalc.tScroller.Text)
  ST = ST + (30 * Scro)
  Barrier = Val(DCalc.tBarrier.Text)
  On Error GoTo Whoops
  L% = Int((Val(DCalc.tMovStr.Text) * ST / 45) / (0.01 * (Df + 50))) + (-MaxHP * 0.05)
  H% = Int((Val(DCalc.tMovStr.Text) * ST / 45) / (0.01 * (Df + 50))) + (MaxHP * 0.05)
  L% = Int(Val(DCalc.tDMult.Text) / 100 * L%)
  H% = Int(Val(DCalc.tDMult.Text) / 100 * H%)
  Select Case Val(DCalc.tSuper.Text)
    Case 0: LSuperHits = 100: HSuperHits = 100
    Case 1: LSuperHits = 200: HSuperHits = 200
    Case 2: LSuperHits = 200: HSuperHits = 250
    Case 3: LSuperHits = 250: HSuperHits = 300
    Case 4: LSuperHits = 300: HSuperHits = 400
    Case 5: LSuperHits = 300: HSuperHits = 500
    Case 6: LSuperHits = 300: HSuperHits = 600
  End Select
  L% = Int(L% / 100 * LSuperHits)
  H% = Int(H% / 100 * HSuperHits)
  If DCalc.cWonder.Value = 1 Then
    L% = 1
    H% = MaxHP
    'Select Case Rand(1, 4)
    '  Case 1: y2 = Rand((MaxHP / 4), (MaxHP * 3 / 4))
    '  Case 2: y2 = Rand(1, 30)
    '  Case 3: y2 = Rand(Int(MaxHP / 2), MaxHP)
    '  Case 4: y2 = Rand(MaxHP / 6, MaxHP / 2)
    'End Select
  End If
  'If Rand(1, 200) <= Df Then y2 = 0
  L% = ProcessDmg(L%)
  H% = ProcessDmg(H%)
  If Barrier = 1999 Then
    L% = 0
    H% = 0
  ElseIf Barrier > 0 Then
    y3 = Int(L% / 2)
    L% = L% - y3
    y3 = Int(H% / 2)
    H% = H% - y3
  End If
  If DCalc.cBless.Value = 1 Then
    L% = Int(L% * 1.25)
    H% = Int(H% * 1.25)
  End If
  If DCalc.cCurse.Value = 1 Then
    L% = Int(L% / 1.25)
    H% = Int(H% / 1.25)
  End If
  'If y2 > 0 Then
    If DCalc.cWeak.Value = 1 Then
      'y2 = Int(y2 * Rand(150, 300) / 100)
      L% = Int(L% * 1.5)
      H% = Int(H% * 3)
    End If
    If DCalc.cStrong.Value = 1 Then
      'y2 = Int(y2 * Rand(25, 80) / 100)
      L% = Int(L% * 0.25)
      H% = Int(H% * 0.8)
    End If
  'End If
  'If y2 < 0 Then y2 = 0
  If L% < 0 Then L% = 0
  If H% < 0 Then H% = 0
  If DCalc.cFire.Value = 1 Then
    L% = Int(L% * 1.25)
    H% = Int(H% * 1.25)
  End If
  If L% > 999 Then L% = 999
  If H% > 999 Then H% = 999
  'DamageCount = y2
  Exit Sub
Whoops:
  'y2 = 999
  'DamageCount = y2
  L% = -1
  H% = -1
  Exit Sub
End Sub

Sub HealCount(L%, H%)
Dim MaxHP%, ST%, X%, Y%
  MaxHP = Val(DCalc.tMaxHP.Text)
  ST = Val(DCalc.tStr.Text)
  X% = Int((Val(DCalc.tMovStr.Text) + Int(ST / 4)) / 2) - (MaxHP / 30)
  Y% = Int((Val(DCalc.tMovStr.Text) + Int(ST / 4)) / 2) + (MaxHP / 30)
  X% = Int(X% * Val(DCalc.tDMult.Text) / 100)
  Y% = Int(Y% * Val(DCalc.tDMult.Text) / 100)
  L% = X
  H% = Y
End Sub

Function ProcessDmg(ByVal y2!) As Long
Dim x1 As Long
Dim Tix As Long
  x1 = Int(y2 * 2 / 3)
  x1 = Int(x1 * Val(DCalc.tAMult.Text))
  x1 = Int(x1 * Val(DCalc.tEMult.Text))
  If DCalc.cDeath.Value = 1 Then x1 = x1 * 2.5
  ProcessDmg = x1
End Function

Function Rand(ByVal a%, ByVal B%) As Integer
Dim X As Integer
  X = Int(Rnd * (B - a + 1)) + a
  Rand = X
End Function

