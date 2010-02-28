Option Explicit

Global Const NoDmg = 0              ' No Damage -- Effect Only
Global Const Phys = 1               ' Physical Hit
Global Const Heal = 2               ' Healing Power
Global Const Morph = 3              ' Not-So-Secret Morph
Global Const Poison = 70            ' Poison Elemental
Global Const Grass = 71
Global Const Rock = 72
Global Const Dirt = 73
Global Const Psychic = 74
Global Const Ghost = 75
Global Const Invin = 66             ' Temporary Invincibility
Global Const HPTheft = 11           ' HP Theft
Global Const MPTheft = 12           ' MP Theft
Global Const Life = 17              ' Restore Life
Global Const Demi = 19              ' Cuts enemy's HP in half
Global Const MoonE = 20             ' Moon Power
Global Const Shadow = 21            ' Shadow
Global Const Water = 22             ' Water
Global Const Reveal = 23            ' Scan
Global Const Fire = 24              ' Fire
Global Const Lit = 25               ' Lightning
Global Const Heart = 26             ' Heart
Global Const Earth = 27             ' Earth
Global Const Wind = 28             ' Random Elemental
Global Const Ki = 29                ' Ki Power
Global Const Lum = 30               ' Luminous Energy
Global Const StealMove = 42
Global Const MaxEle = 30            ' The Highest Element

' These are number constants for the add-remove status
' functions. if you add more status effects make sure
' to add number constants
Global Const sMute = 101
Global Const sChaos = 102
Global Const sFreeze = 103
Global Const sPoison = 104
Global Const sBlind = 105
Global Const sDefUp = 106
Global Const sHaste = 107
Global Const sAttUp = 108
Global Const sAttDn = 109
Global Const sSlow = 110
Global Const sStun = 111
Global Const sLife3 = 112
Global Const sRegen = 113
Global Const sStop = 114
Global Const sMushroom = 115
Global Const sMIA = 116
Global Const sQuick = 117
Global Const sBerserk = 118
Global Const sSleep = 119
Global Const sDefDn = 120
Global Const sFly = 121
Global Const sHalfMP = 122
Global Const sBarrier = 123
Global Const sMBarrier = 124
Global Const sBless = 125
Global Const sCurse = 126
Global Const sScrow = 127
Global Const sPMS = 128

Global Const AllFriend = 1   ' Hits all friends + default target = self
Global Const Enemy = 2       ' Default target = GetTarget(TeamID)
Global Const Friend = 3      ' Default target = self
Global Const AllTeam = 4     ' Hits all enemies on a team
Global Const AllFoe = 5      ' Hits all people not on your team
Global Const AllButSelf = 6  ' Hits everyone but self
Global Const Everybody = 7   ' Hits EVERYBODY, no questions asked
Global Const OnlySelf = 8    ' Targets Self only

Type MStatusType
  Mute As Integer       ' Can't do moves other than Phys
  chaos As Integer      ' Confusion
  freeze As Integer     ' Frozen -- Wait for Ice to Thaw or use Fire to melt
  sleep As Integer      ' Fast Asleep. Awake with a Phys Hit
  Poison As Integer     ' Lose HP
  blind As Integer      ' Attack Value drops to Zero
  Bless As Integer      ' Attack goes up
  haste As Integer      ' Increases Speed
  Curse As Integer      ' Attack value drops
  Barrier As Integer    ' Phys. Barrier
  MBarrier As Integer   ' Mag. Barrier
  slow As Integer       ' Lowers speed
  stun As Integer       ' Knocked Out
  Life3 As Integer      ' Return to life once after being killed
  Regen As Integer      ' Get HP
  Stop As Integer       ' Frozen in Time
  Mushroom As Integer   ' Can't Act, Regain HP
  MIA As Integer        ' Missing from Battle -- Can't be Targeted
  Quick As Integer      ' Next move hits instantly
  Berserk As Integer    ' Releases Destructive Spirit
End Type

Type MoveType
  Name As String
  Cmdkey As String                ' Command key
  CanSuper As Integer             ' Can this move be Supered?
  Begin2Attack As String          ' Pre-attack.
  Begin2SuperAttack As String     ' Pre-attack for Super Move
  Begin2HealSelf As String        ' Pre-heal when target is self
  MPReq As Integer                ' MP Required
  Hit As String                   ' Uhh... shown when it misses? <shrug>
  SuperHit As String              ' Super Move Hit
  HealSelf As String              ' Healing self
  CritHit As String               ' Supplement to hit if critical hit. (Leave blank for Super Hits)
  HealMeld As String              ' Mindmeld string shown when healed.
  Miss As String                  ' Blocked
  SuperMiss As String             ' Super attack Blocked
  Status As MStatusType           ' Status done
  Element As Integer
  MoveStr As Integer              ' Default Move Str.
  ElementStr As Integer           ' Avg. Damage (modified by Sx.Mag/PhysStr)
  Target As Integer               ' Who does it hit?
  DestroyWeapon As Integer
  ' 0 = Doesn't destroy weapon
  ' 1 = # of uses decreased
  ' 2 = Weapon is dropped after this move. /weapon to pick it back up.
  ' 3 = Weapon is Instantly Destroyed
  InstantHit As Integer
  ReqAllUses As Integer
End Type

Type WeaponType
  Name As String
  Pickme As String
  SelectStr As String
  Desc(2) As String
  Moves(5) As MoveType
  NumUses As Integer
End Type

Global Wpn() As WeaponType
Global Changed%
Global M%
Global Const Num = 1
Global Const MaxMoves = 5
Global Fil$

Sub ClearWeapon ()
  ReDim Wpn(1)
  Changed = 0
End Sub

Function First8 (ByVal S As String) As String
Dim S2$, X%, S3
  S3 = ""
  For X = 1 To Len(S)
    Select Case Mid$(S, X, 1)
      Case " ", "\", "/", ":", "*", "?", """", "<", ">", "|":
      Case Else: S3 = S3 + Mid$(S, X, 1)
    End Select
  Next X
  If Len(S3) <= 8 Then
    S2 = S3
  Else
    S2 = Left$(S3, 8)
  End If
  First8 = S2
End Function

Sub LoadWeapon (ByVal FileName$, ByVal x1%)
Dim X As Integer
Dim x2 As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim Rx As Integer
  On Error GoTo NotOk41
    Open FileName For Input As #43
      Input #43, Wpn(x1).Name
      Input #43, Wpn(x1).Pickme
      Input #43, Wpn(x1).SelectStr
      Input #43, Wpn(x1).Desc(1)
      Input #43, Wpn(x1).Desc(2)
      Input #43, Wpn(x1).NumUses
      For X = 1 To 5
	Input #43, Wpn(x1).Moves(X).Name
	Input #43, Wpn(x1).Moves(X).Cmdkey
	Input #43, Wpn(x1).Moves(X).CanSuper
	Input #43, Wpn(x1).Moves(X).Begin2Attack
	Input #43, Wpn(x1).Moves(X).Begin2SuperAttack
	Input #43, Wpn(x1).Moves(X).Begin2HealSelf
	Input #43, Wpn(x1).Moves(X).Hit
	Input #43, Wpn(x1).Moves(X).SuperHit
	Input #43, Wpn(x1).Moves(X).DestroyWeapon
	Input #43, Wpn(x1).Moves(X).HealSelf
	Input #43, Wpn(x1).Moves(X).CritHit
	Input #43, Wpn(x1).Moves(X).HealMeld
	Input #43, Wpn(x1).Moves(X).Miss
	Input #43, Wpn(x1).Moves(X).Status.Mute
	Input #43, Wpn(x1).Moves(X).Status.chaos
	Input #43, Wpn(x1).Moves(X).Status.freeze
	Input #43, Wpn(x1).Moves(X).Status.sleep
	Input #43, Wpn(x1).Moves(X).Status.Poison
	Input #43, Wpn(x1).Moves(X).Status.blind
	Input #43, Wpn(x1).Moves(X).InstantHit
	Input #43, Wpn(x1).Moves(X).Status.haste
	Input #43, Wpn(x1).Moves(X).ReqAllUses
	Input #43, x2
	Input #43, Wpn(x1).Moves(X).SuperMiss
	If Wpn(x1).Moves(X).SuperMiss = "0" Then Wpn(x1).Moves(X).SuperMiss = ""
	Input #43, Wpn(x1).Moves(X).Status.slow
	Input #43, Wpn(x1).Moves(X).Status.stun
	Input #43, Wpn(x1).Moves(X).Status.Life3
	Input #43, Wpn(x1).Moves(X).Status.Regen
	Input #43, Wpn(x1).Moves(X).Status.Stop
	Input #43, Wpn(x1).Moves(X).Status.Mushroom
	Input #43, Wpn(x1).Moves(X).Status.MIA
	Input #43, Wpn(x1).Moves(X).Status.Quick
	Input #43, Wpn(x1).Moves(X).Status.Berserk
	Input #43, Wpn(x1).Moves(X).Status.Barrier
	Input #43, Wpn(x1).Moves(X).Status.MBarrier
	Input #43, Wpn(x1).Moves(X).Status.Curse
	Input #43, Wpn(x1).Moves(X).Status.Bless
	Input #43, Rx
	Input #43, Rx
	Input #43, Rx
	Input #43, Rx
	Input #43, Wpn(x1).Moves(X).Element
	Input #43, Wpn(x1).Moves(X).ElementStr
	Input #43, Wpn(x1).Moves(X).Target
	Wpn(x1).Moves(X).MPReq = CalcMP(x1, X)
      Next X
    Close #43
    Exit Sub
  On Error GoTo 0
NotOk41:
  MsgBox Error$(Err) + " in weapon " + FileName, 16, "ChUB 2000 Weapon Error"
  Close #43
  Stop
End Sub

Sub SaveWeapon (ByVal FileName$, ByVal x1%)
Dim X As Integer
Dim x2 As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim Rx As Integer
  On Error GoTo NotOk51
    Open FileName For Output As #93
      Write #93, Wpn(x1).Name
      Write #93, Wpn(x1).Pickme
      Write #93, Wpn(x1).SelectStr
      Write #93, Wpn(x1).Desc(1)
      Write #93, Wpn(x1).Desc(2)
      Write #93, Wpn(x1).NumUses
      For X = 1 To 5
	Write #93, Wpn(x1).Moves(X).Name
	Write #93, Wpn(x1).Moves(X).Cmdkey
	Write #93, Wpn(x1).Moves(X).CanSuper
	Write #93, Wpn(x1).Moves(X).Begin2Attack
	Write #93, Wpn(x1).Moves(X).Begin2SuperAttack
	Write #93, Wpn(x1).Moves(X).Begin2HealSelf
	Write #93, Wpn(x1).Moves(X).Hit
	Write #93, Wpn(x1).Moves(X).SuperHit
	Write #93, Wpn(x1).Moves(X).DestroyWeapon
	Write #93, Wpn(x1).Moves(X).HealSelf
	Write #93, Wpn(x1).Moves(X).CritHit
	Write #93, Wpn(x1).Moves(X).HealMeld
	Write #93, Wpn(x1).Moves(X).Miss
	Write #93, Wpn(x1).Moves(X).Status.Mute
	Write #93, Wpn(x1).Moves(X).Status.chaos
	Write #93, Wpn(x1).Moves(X).Status.freeze
	Write #93, Wpn(x1).Moves(X).Status.sleep
	Write #93, Wpn(x1).Moves(X).Status.Poison
	Write #93, Wpn(x1).Moves(X).Status.blind
	Write #93, Wpn(x1).Moves(X).InstantHit
	Write #93, Wpn(x1).Moves(X).Status.haste
	Write #93, Wpn(x1).Moves(X).ReqAllUses
	Write #93, 0
	Write #93, Wpn(x1).Moves(X).SuperMiss
	Write #93, Wpn(x1).Moves(X).Status.slow
	Write #93, Wpn(x1).Moves(X).Status.stun
	Write #93, Wpn(x1).Moves(X).Status.Life3
	Write #93, Wpn(x1).Moves(X).Status.Regen
	Write #93, Wpn(x1).Moves(X).Status.Stop
	Write #93, Wpn(x1).Moves(X).Status.Mushroom
	Write #93, Wpn(x1).Moves(X).Status.MIA
	Write #93, Wpn(x1).Moves(X).Status.Quick
	Write #93, Wpn(x1).Moves(X).Status.Berserk
	Write #93, Wpn(x1).Moves(X).Status.Barrier
	Write #93, Wpn(x1).Moves(X).Status.MBarrier
	Write #93, Wpn(x1).Moves(X).Status.Curse
	Write #93, Wpn(x1).Moves(X).Status.Bless
	Write #93, 0
	Write #93, 0
	Write #93, 0
	Write #93, 0
	Write #93, Wpn(x1).Moves(X).Element
	Write #93, Wpn(x1).Moves(X).ElementStr
	Write #93, Wpn(x1).Moves(X).Target
	Wpn(x1).Moves(X).MPReq = CalcMP(x1, X)
      Next X
    Close #93
    Exit Sub
  On Error GoTo 0
NotOk51:
  MsgBox Error$(Err) + " in weapon " + FileName, 16, "KUB FiNAL Character Error"
  Close #93
  Stop
End Sub

Function trimstr (ByVal S As Variant)
  trimstr = Trim(Str$(S))
End Function

