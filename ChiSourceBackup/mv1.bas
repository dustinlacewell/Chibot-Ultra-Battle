Option Compare Text
Option Explicit

Global Const MaxMoves = 12

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

Global Const Allfriend = 1   ' Hits all friends + default target = self
Global Const Enemy = 2       ' Default target = GetTarget(TeamID)
Global Const Friend = 3      ' Default target = self
Global Const AllTeam = 4     ' Hits all enemies on a team
Global Const AllFoe = 5      ' Hits all people not on your team
Global Const AllButSelf = 6  ' Hits everyone but self
Global Const Everybody = 7   ' Hits EVERYBODY, no questions asked

Type MStatusType
  Mute As Integer       ' Can't do moves other than Phys
  chaos As Integer      ' Confusion
  freeze As Integer     ' Frozen -- Wait for Ice to Thaw or use Fire to melt
  sleep As Integer      ' Fast Asleep. Awake with a Phys Hit
  Poison As Integer     ' Lose HP
  blind As Integer      ' Attack Value drops to Zero
  defup As Integer      ' Increases Defense
  haste As Integer      ' Increases Speed
  attup As Integer      ' Increases Attack
  DefDn As Integer      ' Defense Down
  AttDn As Integer      ' Attack Down
  slow As Integer       ' Lowers speed
  stun As Integer       ' Knocked Out
  Life3 As Integer      ' Return to life once after being killed
  Regen As Integer      ' Get HP
  Stop As Integer       ' Frozen in Time
  Mushroom As Integer   ' Can't Act, Regain HP
  MIA As Integer        ' Missing from Battle -- Can't be Targeted
  Quick As Integer      ' Next move hits instantly
  Berserk As Integer    ' Releases Destructive Spirit
  Barrier As Integer
  MBarrier As Integer
  Curse As Integer
  Bless As Integer
  Scarecrow As Integer
  Charm As Integer
  R1 As Integer
  R2 As Integer
End Type

Type MoveType
' The Senshi's Move Type
  Name As String
  CmdKey As String                ' Command key
  CanSuper As Integer             ' Can this move be Supered?
  Begin2Attack As String          ' Pre-attack.
  Begin2SuperAttack As String     ' Pre-attack for Super Move
  Begin2HealSelf As String        ' Pre-heal when target is self
  MPReq As Integer                ' MP Required
  Hit As String                   ' Uhh... shown when it misses? <shrug>
  SuperHit As String              ' Super Move Hit
  MaxSuperHits As Integer         ' Maximum number of hits allowed for Super Combo
  HealSelf As String              ' Healing self
  CritHit As String               ' Supplement to hit if critical hit. (Leave blank for Super Hits)
  HealMeld As String              ' Mindmeld string shown when healed.
  Miss As String                  ' Blocked
  SuperMiss As String             ' Super attack Blocked
  Status As MStatusType           ' Status done
  Element As Integer              ' Elemental
  ElementStr As Integer           ' Avg. Damage (modified by Sx.Mag/PhysStr)
  Target As Integer               ' Who does it hit?
  ' Combo moves removed in FiNAL
End Type
' The best way to learn how these work is to go look at the characters themselves...

Type FatalType
  CmdKey As String           ' Command Key
  PreFatal As String         ' Before Fatality
  FatalMove As String        ' Fatality
End Type

Type SenshiType
' An important data type... these are what the characters are built on.
  FullName As String         ' Senshi's Full Name ('Sailor Moon')
  SenshiID As String         ' Targeting identifier
  Species As String          ' Human, droid, whatever.
  PickMe As String           ' Command to get (ex: 'moon' for /moon)
  SelectStr As String        ' Shown at selection
  SelectJoin As String       ' Shown when joining during battle
  ' All players go at same speed now
  PhysStr As Integer         ' PhysStr
  PhysDef As Integer         ' PhysDef
  MagStr As Integer          ' MagStr
  MagDef As Integer          ' MagDef
  WeakTo As Integer          ' Weak to this Element
  Rest As String             ' String shown when /rest
  Block As String            ' Shown when /block
  BlockFail As String        ' When /block fails
  BlockYes As String
  Taunt(5) As String         ' Taunt Strings
  Fatality As FatalType      ' Fatality
  Moves(MaxMoves) As MoveType
  DeathStr(5) As String
  KillStr(5)  As String
  Desc(4) As String
End Type

Global Fil As String
Global Changed As Integer

Global Senshi(1) As SenshiType


Global Const Num = 1
Global Const M = 1

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

Function GetStr (ByVal FileNum%) As String
Dim X%, C$, Ln%, O$
  O$ = ""
  C$ = ""
  Get #FileNum, , Ln%
  O$ = String$(Ln%, 0)
  'For x = 1 To Ln%
  '  Get #FileNum, , C$
  '  O$ = O$ + C$
  'Next x
  Get #FileNum, , O$
  GetStr = O$
End Function

Function KInStr (ByVal S1$, ByVal S2$) As Integer
' Is S1 a subset of S2?
Dim x1, X2 As Integer
  KInStr = False
  X2 = KLen(S1)
  If (X2 < 5) Then
    Exit Function
  End If
  For x1 = 1 To KLen(S2) - X2 + 1
    If LCase(Mid$(S2, x1, X2)) = LCase(S1) Then
      KInStr = True
    End If
  Next x1
End Function

Function KLen (ByVal S As String) As Integer
Dim X, x1 As Integer
  x1 = 0
  For X = 1 To Len(S)
    If (Asc(Mid$(S, X, 1)) <> 0) Then
      x1 = x1 + 1
    Else
      Exit For
    End If
  Next X
  KLen = x1
End Function

Sub PutStr (ByVal FileNum%, ByVal O$)
Dim X%, Ln%, C As String * 1
  C$ = ""
  Ln = Len(O$)
  Put #FileNum, , Ln%
  'For x = 1 To Ln%
  '  C$ = Mid$(O$, x, 1)
  '  Put #FileNum, , C$
  'Next x
  Put #FileNum, , O$
End Sub

Function Rand (ByVal A%, ByVal b%) As Integer
' Returns a Random Number in the range (A..B) inclusive
Dim X As Integer
  X = Int(Rnd * (b - A + 1)) + A
  Rand = X
End Function

Function trimstr (ByVal S As Variant) As String
' A fast way of doing Trim(Str$("Whatever"))
  trimstr = Trim(Str$(S))
End Function

