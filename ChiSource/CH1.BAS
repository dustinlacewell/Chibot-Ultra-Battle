Attribute VB_Name = "CH1"
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

Global Const AllAlly = 1   ' Hits all Allys + default target = self
Global Const Enemy = 2       ' Default target = GetTarget(TeamID)
Global Const Ally = 3      ' Default target = self
Global Const AllTeam = 4     ' Hits all enemies on a team
Global Const AllFoe = 5      ' Hits all people not on your team
Global Const AllButSelf = 6  ' Hits everyone but self
Global Const Everybody = 7   ' Hits EVERYBODY, no questions asked
Global Const OnlySelf = 8

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
  Resist As Integer
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

Global M As Integer
Global Fil As String
Global Changed As Integer

Global Senshi() As SenshiType


Global Const Num = 1

Sub ClearChar(ByVal x1 As Integer)
Dim X As Integer
  ReDim Senshi(1)
End Sub

' / \ : * ? " < > |
Function First8(ByVal S As String) As String
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

Function GetStr(ByVal FileNum%) As String
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

Function KInStr(ByVal S1$, ByVal S2$) As Integer
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

Function KLen(ByVal S As String) As Integer
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

Sub LoadChar(ByVal FileName As String, ByVal x1 As Integer)
Dim X As Integer
Dim X2 As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim Rx%
  Senshi(x1).Desc(1) = ""
  Senshi(x1).Desc(2) = ""
  Senshi(x1).Desc(3) = ""
  Senshi(x1).Desc(4) = ""
  If Len(FileName) > 4 Then
    If UCase(Right$(FileName, 3)) <> "CHR" Then
      LoadNewChar FileName, x1
      Exit Sub
    End If
  End If
  On Error GoTo NotOK1
    Open FileName For Input As #9
      Input #9, Senshi(x1).FullName
      Input #9, Senshi(x1).SenshiID
      Input #9, Senshi(x1).Species
      Input #9, Senshi(x1).PickMe
      Input #9, Senshi(x1).SelectStr
      Input #9, Senshi(x1).SelectJoin
      Input #9, Senshi(x1).PhysStr
      Input #9, Senshi(x1).PhysDef
      Input #9, Senshi(x1).MagStr
      Input #9, Senshi(x1).MagDef
      Input #9, Senshi(x1).WeakTo
      Input #9, Senshi(x1).Rest
      Input #9, Senshi(x1).Block
      Input #9, Senshi(x1).BlockFail
        For X = 1 To 5
          Input #9, Senshi(x1).Taunt(X)
        Next X
        Input #9, Senshi(x1).Fatality.CmdKey
        Input #9, Senshi(x1).Fatality.PreFatal
        Input #9, Senshi(x1).Fatality.FatalMove
        'On Error GoTo 0
        For X = 1 To MaxMoves
          Input #9, Senshi(x1).Moves(X).Name
          Input #9, Senshi(x1).Moves(X).CmdKey
          Input #9, Senshi(x1).Moves(X).CanSuper
          Input #9, Senshi(x1).Moves(X).Begin2Attack
          Input #9, Senshi(x1).Moves(X).Begin2SuperAttack
          Input #9, Senshi(x1).Moves(X).Begin2HealSelf
          Input #9, Senshi(x1).Moves(X).Hit
          Input #9, Senshi(x1).Moves(X).SuperHit
          Input #9, Senshi(x1).Moves(X).MaxSuperHits
          Input #9, Senshi(x1).Moves(X).HealSelf
          Input #9, Senshi(x1).Moves(X).CritHit
          Input #9, Senshi(x1).Moves(X).HealMeld
          Input #9, Senshi(x1).Moves(X).Miss
          Input #9, Senshi(x1).Moves(X).Status.Mute
          Input #9, Senshi(x1).Moves(X).Status.chaos
          Input #9, Senshi(x1).Moves(X).Status.freeze
          Input #9, Senshi(x1).Moves(X).Status.sleep
          Input #9, Senshi(x1).Moves(X).Status.Poison
          Input #9, Senshi(x1).Moves(X).Status.blind
          Input #9, Senshi(x1).Moves(X).Status.defup
          Input #9, Senshi(x1).Moves(X).Status.haste
          Input #9, Senshi(x1).Moves(X).Status.attup
          Input #9, Senshi(x1).Moves(X).Status.DefDn
          Input #9, Senshi(x1).Moves(X).Status.slow
          Input #9, Senshi(x1).Moves(X).Status.stun
          Input #9, Senshi(x1).Moves(X).Element
          Input #9, Senshi(x1).Moves(X).ElementStr
          Input #9, Senshi(x1).Moves(X).Target
        Next X
        On Error Resume Next
        For X = 1 To 5
          Input #9, Senshi(x1).DeathStr(X)
        Next X
        For X = 1 To 5
          Input #9, Senshi(x1).KillStr(X)
        Next X
    Close #9
    Exit Sub
  On Error GoTo 0
NotOK1:
    MsgBox Error$(Err) + " in character " + FileName, 16, "ChUB 2000 Character Error"
    Close #9
    Exit Sub
End Sub

Sub LoadEncryptedChar(ByVal FileName$, ByVal x1%)
Dim X As Integer
Dim X2 As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim Rx%, C$
  Open FileName$ For Binary Access Read Lock Read Write As #84
    C$ = String$(24, 0)
    'For x = 1 To Len("Encrypted ChUB Character")
      Get #84, , C$
    'Next x
    Get #84, , Senshi(x1).PhysStr
    Get #84, , Senshi(x1).PhysDef
    Get #84, , Senshi(x1).MagStr
    Get #84, , Senshi(x1).MagDef
    Get #84, , Senshi(x1).WeakTo
    Get #84, , Senshi(x1).Resist
    Senshi(x1).FullName = GetStr(84)
    Senshi(x1).SenshiID = GetStr(84)
    Senshi(x1).PickMe = GetStr(84)
    Senshi(x1).SelectStr = GetStr(84)
    Senshi(x1).SelectJoin = GetStr(84)
    Senshi(x1).BlockYes = GetStr(84)
    Senshi(x1).Rest = GetStr(84)
    Senshi(x1).Block = GetStr(84)
    Senshi(x1).BlockFail = GetStr(84)
    Senshi(x1).Fatality.CmdKey = GetStr(84)
    Senshi(x1).Fatality.PreFatal = GetStr(84)
    Senshi(x1).Fatality.FatalMove = GetStr(84)
    For X = 1 To 5
      Senshi(x1).Taunt(X) = GetStr(84)
    Next X
    For X = 1 To 5
      Senshi(x1).DeathStr(X) = GetStr(84)
    Next X
    For X = 1 To 5
      Senshi(x1).KillStr(X) = GetStr(84)
    Next X
    For X = 1 To 4
      Senshi(x1).Desc(X) = GetStr(84)
    Next X
    For X = 1 To MaxMoves
      Get #84, , Senshi(x1).Moves(X).CanSuper
      Get #84, , Senshi(x1).Moves(X).Element
      Get #84, , Senshi(x1).Moves(X).ElementStr
      Get #84, , Senshi(x1).Moves(X).Target
      Get #84, , Senshi(x1).Moves(X).Status.Mute
      Get #84, , Senshi(x1).Moves(X).Status.Mute
      Get #84, , Senshi(x1).Moves(X).Status.chaos
      Get #84, , Senshi(x1).Moves(X).Status.freeze
      Get #84, , Senshi(x1).Moves(X).Status.sleep
      Get #84, , Senshi(x1).Moves(X).Status.Poison
      Get #84, , Senshi(x1).Moves(X).Status.blind
      Get #84, , Senshi(x1).Moves(X).Status.haste
      Get #84, , Senshi(x1).Moves(X).Status.slow
      Get #84, , Senshi(x1).Moves(X).Status.stun
      Get #84, , Senshi(x1).Moves(X).Status.Life3
      Get #84, , Senshi(x1).Moves(X).Status.Regen
      Get #84, , Senshi(x1).Moves(X).Status.Stop
      Get #84, , Senshi(x1).Moves(X).Status.Mushroom
      Get #84, , Senshi(x1).Moves(X).Status.MIA
      Get #84, , Senshi(x1).Moves(X).Status.Quick
      Get #84, , Senshi(x1).Moves(X).Status.Berserk
      Get #84, , Senshi(x1).Moves(X).Status.Barrier
      Get #84, , Senshi(x1).Moves(X).Status.MBarrier
      Get #84, , Senshi(x1).Moves(X).Status.Curse
      Get #84, , Senshi(x1).Moves(X).Status.Bless
      Senshi(x1).Moves(X).Name = GetStr(84)
      If Senshi(x1).Moves(X).Name = "morestat" Then
        Get #84, , Senshi(x1).Moves(X).Status.Scarecrow
        Get #84, , Senshi(x1).Moves(X).Status.Charm
        Get #84, , Senshi(x1).Moves(X).Status.R1
        Get #84, , Senshi(x1).Moves(X).Status.R2
        Senshi(x1).Moves(X).Name = GetStr(84)
      End If
      Senshi(x1).Moves(X).CmdKey = GetStr(84)
      Senshi(x1).Moves(X).Begin2Attack = GetStr(84)
      Senshi(x1).Moves(X).Begin2SuperAttack = GetStr(84)
      Senshi(x1).Moves(X).Begin2HealSelf = GetStr(84)
      Senshi(x1).Moves(X).Hit = GetStr(84)
      Senshi(x1).Moves(X).SuperHit = GetStr(84)
      Senshi(x1).Moves(X).HealSelf = GetStr(84)
      Senshi(x1).Moves(X).CritHit = GetStr(84)
      Senshi(x1).Moves(X).HealMeld = GetStr(84)
      Senshi(x1).Moves(X).Miss = GetStr(84)
      Senshi(x1).Moves(X).SuperMiss = GetStr(84)
    Next X
  Close #84
End Sub

Sub LoadNewChar(ByVal FileName$, ByVal x1%)
Dim X As Integer
Dim X2 As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim Rx As Integer, Sx$
Dim C$
  On Error GoTo Notok11
    Open FileName For Input As #3
      Input #3, Senshi(x1).FullName
      C$ = "Encrypted ChUB Character"
      If Left$(Senshi(x1).FullName, Len(C$)) = "Encrypted ChUB Character" Then
        'MsgBox "You cannot edit an encrypted character.", 16, "ChUB CharEdit"
        'ClearChar (x1)
        Close #3
        LoadEncryptedChar FileName$, x1%
        Exit Sub
      End If
      Input #3, Senshi(x1).SenshiID
      On Error GoTo Whoops91
      Input #3, Rx
      Senshi(x1).WeakTo = Rx Mod 100
      Senshi(x1).Resist = (Rx - Senshi(x1).WeakTo) / 100
      GoTo Whoops92
Whoops91:
      Senshi(x1).WeakTo = Shadow
      Resume Whoops92
Whoops92:
      On Error GoTo Notok11
      Input #3, Senshi(x1).PickMe
      Input #3, Senshi(x1).SelectStr
      Input #3, Senshi(x1).SelectJoin
      Input #3, Senshi(x1).PhysStr
      Input #3, Senshi(x1).PhysDef
      Input #3, Senshi(x1).MagStr
      Input #3, Senshi(x1).MagDef
      On Error GoTo Whoops93
      Input #3, Senshi(x1).BlockYes
      GoTo Whoops94
Whoops93:
      Senshi(x1).BlockYes = ""
      Resume Whoops94
Whoops94:
      If Senshi(x1).BlockYes = "0" Then Senshi(x1).BlockYes = ""
      On Error GoTo Notok11
      Input #3, Senshi(x1).Rest
      Input #3, Senshi(x1).Block
      Input #3, Senshi(x1).BlockFail
        For X = 1 To 5
          Input #3, Senshi(x1).Taunt(X)
        Next X
        Input #3, Senshi(x1).Fatality.CmdKey
        Input #3, Senshi(x1).Fatality.PreFatal
        Input #3, Senshi(x1).Fatality.FatalMove
        For X = 1 To MaxMoves
          Input #3, Senshi(x1).Moves(X).Name
           Input #3, Senshi(x1).Moves(X).CmdKey
           Input #3, Senshi(x1).Moves(X).CanSuper
           Input #3, Senshi(x1).Moves(X).Begin2Attack
           Input #3, Senshi(x1).Moves(X).Begin2SuperAttack
           Input #3, Senshi(x1).Moves(X).Begin2HealSelf
           Input #3, Senshi(x1).Moves(X).Hit
           Input #3, Senshi(x1).Moves(X).SuperHit
           Input #3, Sx$
           Input #3, Senshi(x1).Moves(X).HealSelf
           Input #3, Senshi(x1).Moves(X).CritHit
           Input #3, Senshi(x1).Moves(X).HealMeld
           Input #3, Senshi(x1).Moves(X).Miss
           Input #3, Senshi(x1).Moves(X).Status.Mute
           Input #3, Senshi(x1).Moves(X).Status.chaos
           Input #3, Senshi(x1).Moves(X).Status.freeze
           Input #3, Senshi(x1).Moves(X).Status.sleep
           Input #3, Senshi(x1).Moves(X).Status.Poison
           Input #3, Senshi(x1).Moves(X).Status.blind
           Input #3, Rx
           Input #3, Senshi(x1).Moves(X).Status.haste
           Input #3, Rx
           Input #3, Rx
           Input #3, Senshi(x1).Moves(X).SuperMiss
           If Senshi(x1).Moves(X).SuperMiss = "0" Then Senshi(x1).Moves(X).SuperMiss = ""
           Input #3, Senshi(x1).Moves(X).Status.slow
           Input #3, Senshi(x1).Moves(X).Status.stun
           Input #3, Senshi(x1).Moves(X).Status.Life3
           Input #3, Senshi(x1).Moves(X).Status.Regen
           Input #3, Senshi(x1).Moves(X).Status.Stop
           Input #3, Senshi(x1).Moves(X).Status.Mushroom
           Input #3, Senshi(x1).Moves(X).Status.MIA
           Input #3, Senshi(x1).Moves(X).Status.Quick
           Input #3, Senshi(x1).Moves(X).Status.Berserk
           Input #3, Senshi(x1).Moves(X).Status.Barrier
           Input #3, Senshi(x1).Moves(X).Status.MBarrier
           Input #3, Senshi(x1).Moves(X).Status.Curse
           Input #3, Senshi(x1).Moves(X).Status.Bless
           Input #3, Rx
           Input #3, Rx
           Input #3, Rx
           Input #3, Rx
           Input #3, Senshi(x1).Moves(X).Element
           Input #3, Senshi(x1).Moves(X).ElementStr
           Input #3, Senshi(x1).Moves(X).Target
           Senshi(x1).Moves(X).MPReq = CALCMP(x1, X)
        Next X
        On Error Resume Next
        For X = 1 To 5
          Input #3, Senshi(x1).DeathStr(X)
        Next X
        For X = 1 To 5
          Input #3, Senshi(x1).KillStr(X)
        Next X
        Input #3, Senshi(x1).Desc(1)
        Input #3, Senshi(x1).Desc(2)
        Input #3, Senshi(x1).Desc(3)
        Input #3, Senshi(x1).Desc(4)
    Close #3
    Exit Sub
  On Error GoTo 0
Notok11:
  MsgBox Error$(Err) + " in character " + FileName, 16, "ChUB 2000 Character Error"
  Close #3
  Stop
End Sub

Sub PutStr(ByVal FileNum%, ByVal O$)
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

Function Rand(ByVal a%, ByVal b%) As Integer
' Returns a Random Number in the range (A..B) inclusive
Dim X As Integer
  X = Int(Rnd * (b - a + 1)) + a
  Rand = X
End Function

Sub SaveChar(ByVal FileName As String, ByVal x1 As Integer)
Dim X As Integer
    Open FileName For Output As #1
        Print #1, Chr$(34) + Senshi(x1).FullName + Chr$(34)
        Print #1, Chr$(34) + Senshi(x1).SenshiID + Chr$(34)
        Print #1, Chr$(34) + Senshi(x1).Species + Chr$(34)
        Print #1, Chr$(34) + Senshi(x1).PickMe + Chr$(34)
        Print #1, Chr$(34) + Senshi(x1).SelectStr + Chr$(34)
        Print #1, Chr$(34) + Senshi(x1).SelectJoin + Chr$(34)
        Print #1, Senshi(x1).PhysStr
        Print #1, Senshi(x1).PhysDef
        Print #1, Senshi(x1).MagStr
        Print #1, Senshi(x1).MagDef
        Print #1, Senshi(x1).WeakTo
        Print #1, Chr$(34) + Senshi(x1).Rest + Chr$(34)
        Print #1, Chr$(34) + Senshi(x1).Block + Chr$(34)
        Print #1, Chr$(34) + Senshi(x1).BlockFail + Chr$(34)
        For X = 1 To 5
          Print #1, Chr$(34) + Senshi(x1).Taunt(X) + Chr$(34)
        Next X
        Print #1, Chr$(34) + Senshi(x1).Fatality.CmdKey + Chr$(34)
        Print #1, Chr$(34) + Senshi(x1).Fatality.PreFatal + Chr$(34)
        Print #1, Chr$(34) + Senshi(x1).Fatality.FatalMove + Chr$(34)
        For X = 1 To MaxMoves
          Print #1, Chr$(34) + Senshi(x1).Moves(X).Name + Chr$(34)
          Print #1, Chr$(34) + Senshi(x1).Moves(X).CmdKey + Chr$(34)
          Print #1, Senshi(x1).Moves(X).CanSuper
          Print #1, Chr$(34) + Senshi(x1).Moves(X).Begin2Attack + Chr$(34)
          Print #1, Chr$(34) + Senshi(x1).Moves(X).Begin2SuperAttack + Chr$(34)
          Print #1, Chr$(34) + Senshi(x1).Moves(X).Begin2HealSelf + Chr$(34)
          Print #1, Chr$(34) + Senshi(x1).Moves(X).Hit + Chr$(34)
          Print #1, Chr$(34) + Senshi(x1).Moves(X).SuperHit + Chr$(34)
          Print #1, Senshi(x1).Moves(X).MaxSuperHits
          Print #1, Chr$(34) + Senshi(x1).Moves(X).HealSelf + Chr$(34)
          Print #1, Chr$(34) + Senshi(x1).Moves(X).CritHit + Chr$(34)
          Print #1, Chr$(34) + Senshi(x1).Moves(X).HealMeld + Chr$(34)
          Print #1, Chr$(34) + Senshi(x1).Moves(X).Miss + Chr$(34)
          Print #1, Senshi(x1).Moves(X).Status.Mute
          Print #1, Senshi(x1).Moves(X).Status.chaos
          Print #1, Senshi(x1).Moves(X).Status.freeze
          Print #1, Senshi(x1).Moves(X).Status.sleep
          Print #1, Senshi(x1).Moves(X).Status.Poison
          Print #1, Senshi(x1).Moves(X).Status.blind
          Print #1, Senshi(x1).Moves(X).Status.defup
          Print #1, Senshi(x1).Moves(X).Status.haste
          Print #1, Senshi(x1).Moves(X).Status.attup
          Print #1, Senshi(x1).Moves(X).Status.DefDn
          Print #1, Senshi(x1).Moves(X).Status.slow
          Print #1, Senshi(x1).Moves(X).Status.stun
          Print #1, Senshi(x1).Moves(X).Element
          Print #1, Senshi(x1).Moves(X).ElementStr
          Print #1, Senshi(x1).Moves(X).Target
        Next X
        For X = 1 To 5
          Write #1, Senshi(x1).DeathStr(X)
        Next X
        For X = 1 To 5
          Write #1, Senshi(x1).KillStr(X)
        Next X
    Close #1
    Exit Sub
End Sub

Sub SaveEncryptedChar(ByVal FileName$, ByVal x1%)
Dim X As Integer
Dim X2 As Integer
Dim Z1 As Single
Dim Z2 As Integer
Dim Rx%, C$, D$
  Open FileName$ For Binary Access Write Lock Read Write As #85
    C$ = "Encrypted ChUB Character"
    'For x = 1 To Len(C$)
    '  D$ = Mid$(C$, x, 1)
      Put #85, , C$
    'Next x
    Put #85, , Senshi(x1).PhysStr
    Put #85, , Senshi(x1).PhysDef
    Put #85, , Senshi(x1).MagStr
    Put #85, , Senshi(x1).MagDef
    Put #85, , Senshi(x1).WeakTo
    Put #85, , Senshi(x1).Resist
    PutStr 85, Senshi(x1).FullName
    PutStr 85, Senshi(x1).SenshiID
    PutStr 85, Senshi(x1).PickMe
    PutStr 85, Senshi(x1).SelectStr
    PutStr 85, Senshi(x1).SelectJoin
    PutStr 85, Senshi(x1).BlockYes
    PutStr 85, Senshi(x1).Rest
    PutStr 85, Senshi(x1).Block
    PutStr 85, Senshi(x1).BlockFail
    PutStr 85, Senshi(x1).Fatality.CmdKey
    PutStr 85, Senshi(x1).Fatality.PreFatal
    PutStr 85, Senshi(x1).Fatality.FatalMove
    For X = 1 To 5
      PutStr 85, Senshi(x1).Taunt(X)
    Next X
    For X = 1 To 5
      PutStr 85, Senshi(x1).DeathStr(X)
    Next X
    For X = 1 To 5
      PutStr 85, Senshi(x1).KillStr(X)
    Next X
    For X = 1 To 4
      PutStr 85, Senshi(x1).Desc(X)
    Next X
    For X = 1 To MaxMoves
      Put #85, , Senshi(x1).Moves(X).CanSuper
      Put #85, , Senshi(x1).Moves(X).Element
      Put #85, , Senshi(x1).Moves(X).ElementStr
      Put #85, , Senshi(x1).Moves(X).Target
      Put #85, , Senshi(x1).Moves(X).Status.Mute
      Put #85, , Senshi(x1).Moves(X).Status.Mute
      Put #85, , Senshi(x1).Moves(X).Status.chaos
      Put #85, , Senshi(x1).Moves(X).Status.freeze
      Put #85, , Senshi(x1).Moves(X).Status.sleep
      Put #85, , Senshi(x1).Moves(X).Status.Poison
      Put #85, , Senshi(x1).Moves(X).Status.blind
      Put #85, , Senshi(x1).Moves(X).Status.haste
      Put #85, , Senshi(x1).Moves(X).Status.slow
      Put #85, , Senshi(x1).Moves(X).Status.stun
      Put #85, , Senshi(x1).Moves(X).Status.Life3
      Put #85, , Senshi(x1).Moves(X).Status.Regen
      Put #85, , Senshi(x1).Moves(X).Status.Stop
      Put #85, , Senshi(x1).Moves(X).Status.Mushroom
      Put #85, , Senshi(x1).Moves(X).Status.MIA
      Put #85, , Senshi(x1).Moves(X).Status.Quick
      Put #85, , Senshi(x1).Moves(X).Status.Berserk
      Put #85, , Senshi(x1).Moves(X).Status.Barrier
      Put #85, , Senshi(x1).Moves(X).Status.MBarrier
      Put #85, , Senshi(x1).Moves(X).Status.Curse
      Put #85, , Senshi(x1).Moves(X).Status.Bless
      PutStr 85, "morestat"
      Put #85, , Senshi(x1).Moves(X).Status.Scarecrow
      Put #85, , Senshi(x1).Moves(X).Status.Charm
      Put #85, , Senshi(x1).Moves(X).Status.R1
      Put #85, , Senshi(x1).Moves(X).Status.R2
      PutStr 85, Senshi(x1).Moves(X).Name
      PutStr 85, Senshi(x1).Moves(X).CmdKey
      PutStr 85, Senshi(x1).Moves(X).Begin2Attack
      PutStr 85, Senshi(x1).Moves(X).Begin2SuperAttack
      PutStr 85, Senshi(x1).Moves(X).Begin2HealSelf
      PutStr 85, Senshi(x1).Moves(X).Hit
      PutStr 85, Senshi(x1).Moves(X).SuperHit
      PutStr 85, Senshi(x1).Moves(X).HealSelf
      PutStr 85, Senshi(x1).Moves(X).CritHit
      PutStr 85, Senshi(x1).Moves(X).HealMeld
      PutStr 85, Senshi(x1).Moves(X).Miss
      PutStr 85, Senshi(x1).Moves(X).SuperMiss
    Next X
  Close #85
End Sub

Sub SaveNewChar(ByVal FileName$, ByVal x1%)
Dim X As Integer, Rx%
    Open FileName For Output As #1
        Write #1, Senshi(x1).FullName
        Write #1, Senshi(x1).SenshiID
        Rx = Senshi(x1).Resist * 100 + Senshi(x1).WeakTo
        Write #1, Rx
        Write #1, Senshi(x1).PickMe
        Write #1, Senshi(x1).SelectStr
        Write #1, Senshi(x1).SelectJoin
        Write #1, Senshi(x1).PhysStr
        Write #1, Senshi(x1).PhysDef
        Write #1, Senshi(x1).MagStr
        Write #1, Senshi(x1).MagDef
        Write #1, Senshi(x1).BlockYes
        Write #1, Senshi(x1).Rest
        Write #1, Senshi(x1).Block
        Write #1, Senshi(x1).BlockFail
        For X = 1 To 5
          Write #1, Senshi(x1).Taunt(X)
        Next X
        Write #1, Senshi(x1).Fatality.CmdKey
        Write #1, Senshi(x1).Fatality.PreFatal
        Write #1, Senshi(x1).Fatality.FatalMove
        For X = 1 To MaxMoves
          Write #1, Senshi(x1).Moves(X).Name
          Write #1, Senshi(x1).Moves(X).CmdKey
          Write #1, Senshi(x1).Moves(X).CanSuper
          Write #1, Senshi(x1).Moves(X).Begin2Attack
          Write #1, Senshi(x1).Moves(X).Begin2SuperAttack
          Write #1, Senshi(x1).Moves(X).Begin2HealSelf
          Write #1, Senshi(x1).Moves(X).Hit
          Write #1, Senshi(x1).Moves(X).SuperHit
          Write #1, 0
          Write #1, Senshi(x1).Moves(X).HealSelf
          Write #1, Senshi(x1).Moves(X).CritHit
          Write #1, Senshi(x1).Moves(X).HealMeld
          Write #1, Senshi(x1).Moves(X).Miss
          Write #1, Senshi(x1).Moves(X).Status.Mute
          Write #1, Senshi(x1).Moves(X).Status.chaos
          Write #1, Senshi(x1).Moves(X).Status.freeze
          Write #1, Senshi(x1).Moves(X).Status.sleep
          Write #1, Senshi(x1).Moves(X).Status.Poison
          Write #1, Senshi(x1).Moves(X).Status.blind
          Write #1, 0
          Write #1, Senshi(x1).Moves(X).Status.haste
          Write #1, 0
          Write #1, 0
          Write #1, Senshi(x1).Moves(X).SuperMiss
          Write #1, Senshi(x1).Moves(X).Status.slow
          Write #1, Senshi(x1).Moves(X).Status.stun
          Write #1, Senshi(x1).Moves(X).Status.Life3
          Write #1, Senshi(x1).Moves(X).Status.Regen
          Write #1, Senshi(x1).Moves(X).Status.Stop
          Write #1, Senshi(x1).Moves(X).Status.Mushroom
          Write #1, Senshi(x1).Moves(X).Status.MIA
          Write #1, Senshi(x1).Moves(X).Status.Quick
          Write #1, Senshi(x1).Moves(X).Status.Berserk
          Write #1, Senshi(x1).Moves(X).Status.Barrier
          Write #1, Senshi(x1).Moves(X).Status.MBarrier
          Write #1, Senshi(x1).Moves(X).Status.Curse
          Write #1, Senshi(x1).Moves(X).Status.Bless
          Write #1, Senshi(x1).Moves(X).Status.Scarecrow
          Write #1, Senshi(x1).Moves(X).Status.Charm
          Write #1, Senshi(x1).Moves(X).Status.R1
          Write #1, Senshi(x1).Moves(X).Status.R2
          Write #1, Senshi(x1).Moves(X).Element
          Write #1, Senshi(x1).Moves(X).ElementStr
          Write #1, Senshi(x1).Moves(X).Target
        Next X
        For X = 1 To 5
          Write #1, Senshi(x1).DeathStr(X)
        Next X
        For X = 1 To 5
          Write #1, Senshi(x1).KillStr(X)
        Next X
        For X = 1 To 4
          Write #1, Senshi(x1).Desc(X)
        Next X
    Close #1
    Exit Sub
End Sub

Function TrimStr(ByVal S As Variant) As String
' A fast way of doing Trim(Str$("Whatever"))
  TrimStr = Trim(Str$(S))
End Function

