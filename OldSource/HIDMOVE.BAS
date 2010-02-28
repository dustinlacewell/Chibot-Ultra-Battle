Option Explicit

Global Const MaxHidMoves = 1
Global hm(MaxHidMoves) As MoveType

Sub InitHid ()
  ' /!HD -- Hyper Dispel
  'hm(1).Name = "Hyper Dispel"
  'hm(1).CmdKey = "!HD"
  'hm(1).Element = 0
  'hm(1).Status(sBless) = -1
  'hm(1).Status(sBarrier) = -1
  'hm(1).Status(sMBarrier) = -1
  'hm(1).Status(sLife3) = -1
  'hm(1).Status(sRegen) = -1
  'hm(1).Status(sQuick) = -1
  'hm(1).Status(sBerserk) = -1
  'hm(1).Begin2Attack = "%SN starts to cast ""Hyper Dispel."""
  'hm(1).Begin2HealSelf = "Everyone loses their goodies!"
  'hm(1).Target = AllFoe
End Sub

