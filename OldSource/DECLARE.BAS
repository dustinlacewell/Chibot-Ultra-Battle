Option Compare Text ' for InStr()
Option Explicit ' to make it like Pascal

Global Const VerID = "1.5.1 (fixed final)" ' change this as you release updates
Global Const ErrorTrap = False ' if TRUE, the game will trap errors and direct them to the game's own error trapping routines
Global Const GodMode1 = False ' doesn't mean anything, i don't think
Global Const LogFile = 42 ' file # for log file

Global Const MorphLength = 60 ' Obsolete
Global Const ConstSpeed = 30 ' also obsolete
Global Const SpeedConst = 4 ' also obsolete
Global Const HasteHit = -5
Global Const SlowHit = 5
Global Const PMSHit = -5
Global Const MaxCheeseLimit = 1000

Global Const MaxRay = 250 ' # of lines that may be sent to the input buffer
Global Const outmax = 250 ' # of lines that may be sent to the output buffer
Global Const SndMax = 50 ' # of sounds that may be sent to the sound buffer

Global Const MaxYouma = 0
Global Const MaxMoves = 20 ' max # of moves per character

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
Global Const SPTheft = 92
Global Const MaxEle = 30            ' The Highest Element

' These are number constants for the add-remove status
' functions. if you add more status effects make sure
' to add number constants
Global Const sMute = 1          ' Phys Moves Only
Global Const sChaos = 2         ' Random moves @ Random Targets
Global Const sFreeze = 3        ' Frozen
Global Const sPoison = 4        ' HP reduces
Global Const sBlind = 5         ' Attack = 0
Global Const sInvin = 6         ' Shielded
Global Const sHaste = 7         ' Speed Up
Global Const sMorph = 8         ' Morph

Global Const sSlow = 10         ' Speed Down
Global Const sStun = 11         ' Unconsious
Global Const sLife3 = 12        ' Reraise
Global Const sRegen = 13        ' HP restores
Global Const sStop = 14         ' Time Stopped
Global Const sMushroom = 15     ' Mushroomied!
Global Const sMIA = 16          ' Missing in Action
Global Const sQuick = 17        ' Quick
Global Const sBerserk = 18      ' Random attacks @ random enemies
Global Const sSleep = 19        ' Zzzzzzzzzz
Global Const sVirus = 20


Global Const sBarrier = 23      ' Barriers
Global Const sMBarrier = 24     ' MBarriers
Global Const sBless = 25        ' Attack Up
Global Const sCurse = 26        ' Attack Down

Global Const sPMS = 28          ' Pissed Off!
Global Const sZombie = 29       ' Zombie Mode
Global Const sHamedo = 30       ' Chibot Counter
Global Const sMaxStatus = 30

Global Const Allfriend = 1   ' Hits all friends + default target = self
Global Const Enemy = 2       ' Default target = GetTarget(TeamID)
Global Const Friend = 3      ' Default target = self
Global Const AllTeam = 4     ' Hits all enemies on a team
Global Const AllFoe = 5      ' Hits all people not on your team
Global Const AllButSelf = 6  ' Hits everyone but self
Global Const Everybody = 7   ' Hits EVERYBODY, no questions asked
Global Const OnlySelf = 8    ' Targets Self only

' Player Move Consts

Global Const pBlock = 255    ' /block
Global Const pRest = 254     ' /rest
Global Const pAtt = 253      ' /att
Global Const pTaunt = 252    ' /taunt
Global Const pFlee = 251     ' /flee
Global Const pGet = 250      ' /get
Global Const pFatal = 249    ' Fatality
Global Const pDefect = 248   ' /defect
Global Const p_Save = 247    ' Dummied out
Global Const pRock = 246

Type PType
  X As Integer
  Y As Integer
End Type

Type MStatusType
  Mute As Integer       ' Can't do moves other than Phys
  Chaos As Integer      ' Confusion
  Freeze As Integer     ' Frozen -- Wait for Ice to Thaw or use Fire to melt
  Sleep As Integer      ' Fast Asleep. Awake with a Phys Hit
  Poison As Integer     ' Lose HP
  Blind As Integer      ' Attack Value drops to Zero
  Bless As Integer      ' Attack goes up
  Haste As Integer      ' Increases Speed
  Curse As Integer      ' Attack value drops
  Barrier As Integer    ' Phys. Barrier
  MBarrier As Integer   ' Mag. Barrier
  Slow As Integer       ' Lowers speed
  Stun As Integer       ' Knocked Out
  Life3 As Integer      ' Return to life once after being killed
  Regen As Integer      ' Get HP
  Stop As Integer       ' Frozen in Time
  Mushroom As Integer   ' Can't Act, Regain HP
  MIA As Integer        ' Missing from Battle -- Can't be Targeted
  Quick As Integer      ' Next move hits instantly
  Berserk As Integer    ' Releases Destructive Spirit
End Type

Type BStatusType
  Invin As Integer      ' Shielded
  Morph As Long         ' Secret Morph
  Mute As Integer       ' Can't do moves other than Phys
  Chaos As Integer      ' Confusion
  Freeze As Integer     ' Frozen -- Wait for Ice to Thaw or use Fire to melt
  Sleep As Integer      ' Fast Asleep. Awake with a Phys Hit
  Poison As Integer     ' Lose HP
  Blind As Integer      ' Attack Value drops to Zero
  Haste As Integer      ' Increases Speed
  Curse As Integer      ' Decreases attack value
  Barrier As Integer    ' Phys Barrier
  MBarrier As Integer   ' Mag Barrier
  Bless As Integer      ' attack goes up
  Zombie As Integer     ' Zombie Mode
  Slow As Integer       ' Lowers speed
  Stun As Integer       ' Knocked Out
  Life3 As Integer      ' Return to life once after being killed
  Regen As Integer      ' Get HP
  Stop As Integer       ' Frozen in Time
  Mushroom As Integer   ' Can't Act, Regain HP
  MIA As Integer        ' Missing from Battle -- Can't be Targeted
  Quick As Integer      ' Next move hits instantly
  Berserk As Integer    ' Releases Destructive Spirit
  Hamedo As Integer     ' Counter BEFORE being attacked
  PMS As Integer        ' For PMS Rune
End Type

Type Settings
' The Config File
  Arena As Integer           ' Arena
  NoJoin As Integer          ' Disable Joining
  SameChar As Integer        ' Pick Same Char
  Defect As Integer          ' Are Defects Allowed??
  FontName As String * 80    ' Name of Font
  FontColor As String * 6    ' Color of Font
  Reason As String * 80      ' Reason cannot join
  WLog As String * 12        ' Log file name
  FlaCon As Integer          ' Flag Constant
  Flag As Integer
  Respawn As Integer
  Fours As Integer
  Multi As Integer
  OldSchool As Integer
End Type

Type RanHappening
' Random happening in an Arena
  Name As String
  Frequency As Integer                 ' Happens x in every 400 Timer Cycles
  Element As Integer                   ' Element Type
  ElementStr As Integer                ' Strength -- Set this to zero and the RanHappening will just display the Hit message
  Hit As String                        ' When it happens and hits
  Miss As String                       ' When it happens and misses
  Status(sMaxStatus) As Integer
  HitsAll As Integer
End Type

Type ArenaType
' The Arena Field Type
  Name As String                       ' Name
  Desc(10) As String                   ' Description
  AllAttacks As Single                 ' Multiplier on all attacks
  Effect(MaxEle) As Single             ' Multiplier on certain elements
  RestLowHP As Integer                 ' Default = 15
  RestHighHP As Integer                ' Default = 25
  'RestLowMP As Integer                 ' Default = 15
  'RestHighMP As Integer                ' Default = 25
  'GradualMP As Integer                 ' Default = 1
  GradualHP As Integer                 ' Default = 0
  Event(10) As RanHappening            ' up to 10 random happenings
End Type

Type DatasetType
  LoadStr As String                    ' Shown when bot loaded
  BeginSelect As String                ' Shown when selection begins
  EndSelect As String                  ' Shown when selection ends
  BattleBegin As String                ' Battle Begin
  BattlePause As String                ' Battle Pause
  BattleUnPause As String              ' Battle Unpause
  BattleEnd As String
  ClearChars As String
  CharsNotCleared As String
  GameAborted As String
  AcceptDefects As String
  DeclineDefects As String
  WantDraw As String
  DontWantDraw As String
  UnMorphMsg As String
  ChiCtr As String
  Counter As String
  FleeFail As String
  NoGetItem As String
  HPDivert As String
  NotEnoughMP As String
  AllDead As String
  Draw As String
  x1HrLeft As String
  x30MinsLeft As String
  x20MinsLeft As String
  x10MinsLeft As String
  x5MinsLeft As String
  x2MinsLeft As String
  SuddenDeath As String
  x1MinsLeft As String
  x30SecsLeft As String
  x15SecsLeft As String
  x5SecsLeft As String
  TimeExpired As String
  BeatYouma As String
  YouLose As String
  Respawn As String
  BeginVote As String
  CommieVote As String
  Remove As String
  Random As String
  Taken As String
  DefectSucc As String
  DefectFail As String
  FleeAttempt As String
  SuperKill As String
End Type

Type ItemType
  Name As String
  Spawn As String
  Telefrag As String
  PlayerGet As String
  YoumaGet As String
  PlayerHP As Integer
  'PlayerMP As Integer
  'YoumaHP As Integer
  'YoumaMP As Integer
  PlayerStat(sMaxStatus) As Integer
  'YoumaStat(sMaxStatus) As Integer
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
  Crithit As String               ' Supplement to hit if critical hit. (Leave blank for Super Hits)
  HealMeld As String              ' Mindmeld string shown when healed.
  Miss As String                  ' Blocked
  SuperMiss As String             ' Super attack Blocked
  Status(sMaxStatus) As Integer           ' Status done
  Element As Integer
  MoveStr As Integer              ' Default Move Str.
  ElementStr As Integer           ' Avg. Damage (modified by Sx.Mag/PhysStr)
  Target As Integer               ' Who does it hit?
  DestroyWeapon As Integer
  ' 0 = Doesn't destroy weapon
  ' 1 = # of Uses reduced by 1. If only 1 use left, next use hits immediately and destroys weapon
  ' 2 = Weapon is dropped after this move. /weapon to pick it back up.
  ' 3 = Weapon is Destroyed, Instantly
  InstantHit As Integer
  ReqAllUses As Integer
End Type

Type WeaponType
  Name As String
  PickMe As String
  SelectStr As String
  Desc(2) As String
  Moves(5) As MoveType
  NumUses As Integer
End Type

Type FatalType
  Cmdkey As String           ' Command Key
  PreFatal As String         ' Before Fatality
  FatalMove As String        ' Fatality
End Type

Type CharType
' An important data type... these are what the characters are built on.
  FullName As String         ' Senshi's Full Name ('Sailor Moon')
  CharID As String         ' Targeting identifier
  PickMe As String           ' Command to get (ex: 'moon' for /moon)
  SelectStr As String        ' Shown at selection
  SelectJoin As String       ' Shown when joining during battle
  weakness As Integer        ' Magic Weakness
  Resist As Integer          ' Magic Resistance
  PhysStr As Integer         ' PhysStr
  PhysDef As Integer         ' PhysDef
  MagStr As Integer          ' MagStr
  MagDef As Integer          ' MagDef
  'Speed As Integer
  'WeakTo As Integer          ' Weak to this Element
  Rest As String             ' String shown when /rest
  Block As String            ' Shown when /block
  BlockFail As String        ' When /block fails
  BlockYes As String         ' /block works
  Taunt(5) As String         ' Taunt Strings
  Fatality As FatalType      ' Fatality
  Moves(MaxMoves) As Integer
  DeathStr(5) As String
  KillStr(5) As String
  Desc(4) As String
  Wins As Integer
  Losses As Integer
End Type

Type PlayerType
  ScrNam As String              ' Screen Name
  God As Integer                ' GODLY?
  CharID As Integer           ' Senshi being used
  OldCharID As Integer        ' Morphed From
  TeamID As String              ' Team Identifier
  HP As Long                 ' Hit Points
  'MP As Integer                 ' Move/Mana/Magic Points
  Super As Integer              ' Super Meter
  Cheese As Integer             ' Cheese Meter
  PhysDef As Integer            ' Physical Defense
  PhysStr As Integer            ' Physical Strength
  MagStr As Integer             ' Magic Strength
  MagDef As Integer             ' Magic Defense
  Status(sMaxStatus) As Integer
  CurMove As Integer            ' Current move
  MoveStart As Long             ' When CurMove started
  SuperNum As Integer           ' Super value
  Target As Integer             ' Move is targeted at?...
  Frags As Integer              ' Kill Count (conventional and Fatalities)
  FatalFrags As Integer         ' Fatality kills only
  CPU As Integer                ' Is CPU?
  Goodwill As Integer           ' Determines whether or not a CPU will heal allies
  Greed As Integer              ' Determines how often a CPU will /get
  Wrath As Integer              ' Determines if a CPU will retaliate when attacked
  Arrogance As Integer          ' Determines how often a CPU will taunt
  UselessWpn As Integer
  Defect As Integer             ' Is this player allowing defects?
  AttackedMe As Integer         ' Last to attack this player
  Draw As Integer               ' Player wants a draw?
  Moves(MaxMoves) As MoveType   ' Moves
  MaxHP As Long
  'MaxMP As Integer
  GotKilled As Integer          ' Obituary been displayed?
  Ready As Integer              ' ???
  Rune As Integer
  Scroller As Integer
  RuneTemp As Integer
  RuneTemp2 As Integer
  Charging As Integer
  MoveSpawn As Integer
  Weapon As Integer             ' 0 = None
  'WeaponState As Integer
  ' 0 = No Weapon
  ' 1 = Holding Weapon
  ' 2 = Weapon Destroyed
  ' 3 = Weapon Dropped
  WpnUsesLeft As Integer
  Wins As Integer
  Losses As Integer
End Type

Global MaxPlayers As Integer

Global Senshi() As CharType   ' Senshi Array
Global p() As PlayerType          ' Player Array
Global Vote() As Integer        ' Voting Array
Global Weapons() As WeaponType  ' Weapons
Global Dropped() As Integer     ' Dropped Weapons
Global DropUses() As Integer
Global Arena() As ArenaType     ' Arena Array
Global Items() As ItemType      ' Items
Global Twit() As String         ' Twit Names
Global BackTwit() As String
Global Moves() As MoveType      ' The Moves
Global gMaxMoves%               ' Max # of Moves

Global Temp1, Temp2, Temp3, Temp4      ' Temp variables
Global Config As Settings              ' Configuration
Global Item2Get As Integer             ' Item to /get
Global CurArena As Integer             ' Current Arena
Global Battle, Paused, Mx As Integer   ' Misc stuff
Global Tx%, InSend, TauntClear, TauntEnd, DebugWindow As Integer
Global YourSN, STX As String ' YourSN = host's name
Global Dataset As DatasetType
Global HighSenshi As Integer ' Max # of Senshi

Global NumSenshi As Integer
Global MaxItem As Integer
Global MaxArena As Integer
Global MaxTwit As Integer
Global MaxBackTwit
Global MaxWeapons As Integer
Global MaxDropped As Integer

Global Godmode As Integer

Global TLimit As Long
Global TKills&, TFatal&, THP&, TMP&, TSP&

Global MeGod As String ' Name of the person who has god mode... this was removed

Global GetRate As Integer ' Rate that get items appear

'Global FFABattle As Integer ' Int containing the battle type

Global yDrain As MoveType   ' Move for youma drain
Global yBomb1 As MoveType   ' Move for youma bomb
Global Langolier As ArenaType ' Langolier Arena
Global Lang As Integer      ' Langolier distance

Global Ulti As String       ' Who has the Ultimate Power

Global GetRay(MaxRay) As String ' these are the buffers, input, output, and sound
Global GetNdx As Integer
Global GetSave As Integer
Global OutRay(outmax) As String
Global OutPtr As Integer
Global OutNdx As Integer
Global SndRay(SndMax) As String
Global SndPtr As Integer
Global SndNdx As Integer
Global ScRa As Integer ' scroll rate # of lines

'Global HM(10) As MoveType ' hidden moves

Global XTimer As Long ' Game Time elapsed in Game Seconds

Global Auto As Integer ' for /push

Global FragLimit As Integer ' frag limit for respawn
Global GameNum As Integer ' # of battles
Global LoadedX As Integer ' # of times loaded
Global TName(255) As String ' the 255 teams... of these only about 36 are accessible by players
Global TCaptain(255) As Integer ' the 255 team captains
' something good to do would be to convert the teams
' from character identifiers to numbers... instead of
' "Team A" make it "Team 10"...

Global MidiState As Integer ' current state of midi player
Global WavState As Integer ' stats of wav player

Global ZombieM As Integer ' Zomething to do with the Zombie mode

Global RedFlag, BlueFlag As Integer ' the guy who has red and/or blue flag

Global STest% ' ???
Global Graph% ' ???

Global FoundRedFlag, FoundBlueFlag As Integer
' if these are true then they'll show up on the neostatus window.

Global KC As String
' Kamek Controller

Global DMult, MaxHP, MaxSP As Long

Global Shotgun$

Global TimeLimit As Long

Type FullAutoType
  Enabled As Integer
  SelectTime As Integer
  VoteTime As Integer
  RuneTime As Integer
  DefectTime As Integer
  PostTime As Integer
  RespawnFrags As Integer
End Type

Global FullAuto As FullAutoType
Global GameStage As Integer
  ' 0 = ???
  ' 1 = Selection
  ' 2 = Vote
  ' 3 = Rune Select
  ' 4 = Waiting to Begin
  ' 5 = Defect
  ' 6 = Battle
  ' 7 = Shakedown

Global PeepzStats(sMaxStatus) As String

Function Active (ByVal X%) As Integer
  'If X > MaxPlayers Then
  '  Active = y(X - MaxPlayers).Enabled
  'Else
    Active = p(X).CharID
  'End If
End Function

Sub AddDroppedWeapon (ByVal W%, Us%)
Dim Okay%, X3%
  Okay = 0
  For X3 = 1 To MaxDropped
    If Dropped(X3) = 0 Then
      Okay = X3
      Exit For
    End If
  Next X3
  If Okay = 0 Then
    ReDim Preserve Dropped(MaxDropped + 1)
    ReDim Preserve DropUses(MaxDropped + 1)
    MaxDropped = MaxDropped + 1
    Dropped(MaxDropped) = W%
    DropUses(MaxDropped) = Us
  Else
    Dropped(Okay) = W%
    DropUses(Okay) = Us
  End If
End Sub

Sub AddStatus (ByVal x2 As Integer, ByVal s As Integer)
Dim x0 As Integer
  DoEvents
  Select Case s
    Case sMute:
      p(x2).Status(sMute) = XTimer + Rand(-3, 3)
      If (p(x2).CurMove > 0) And (p(x2).CurMove <= MaxMoves) Then
	If p(x2).Moves(p(x2).CurMove).Element <> Phys Then p(x2).CurMove = -255
      End If
    Case sChaos:
      p(x2).Status(sChaos) = XTimer + Rand(-3, 3)
      'p(x2).CurMove = -255
      p(x2).SuperNum = 0
    Case sFreeze:
      p(x2).Status(sFreeze) = XTimer + Rand(-3, 3)
      'p(x2).CurMove = -50
    Case sSleep:
      p(x2).Status(sSleep) = XTimer + Rand(-3, 3)
      p(x2).CurMove = 0
    Case sPoison:
      p(x2).Status(sPoison) = XTimer + Rand(-3, 3)
    Case sBlind:
      p(x2).Status(sBlind) = XTimer + Rand(-3, 3)
    Case sSlow:
      p(x2).Status(sSlow) = XTimer + Rand(-3, 3)
      If p(x2).Status(sHaste) Then Call RemoveStatus(x2, sHaste)
    Case sHaste:
      p(x2).Status(sHaste) = XTimer + Rand(-3, 3)
      If p(x2).Status(sSlow) Then Call RemoveStatus(x2, sSlow)
    Case sStun:
      p(x2).Status(sStun) = XTimer + Rand(-3, 3)
      'p(x2).CurMove = -50
    Case sLife3:
      p(x2).Status(sLife3) = -1
    Case sRegen:
      p(x2).Status(sRegen) = -1
    Case sStop:
      p(x2).Status(sStop) = XTimer + Rand(-3, 3)
    Case sMushroom:
      p(x2).Status(sMushroom) = XTimer + Rand(-3, 3)
      'p(x2).CurMove = -255
    Case sMIA:
      p(x2).Status(sMIA) = XTimer + Rand(-3, 3)
    Case sQuick:
      p(x2).Status(sQuick) = -1
    Case sBerserk:
      p(x2).Status(sBerserk) = XTimer + Rand(-3, 3)
      p(x2).PhysStr = p(x2).PhysStr + 50
      p(x2).MagStr = p(x2).MagStr + 50
      'p(x2).CurMove = -255
      p(x2).SuperNum = 0
    Case sBarrier:
      p(x2).Status(sBarrier) = 100
    Case sMBarrier:
      p(x2).Status(sMBarrier) = 100
    Case sBless:
      p(x2).Status(sBless) = XTimer + Rand(-3, 3)
    Case sCurse:
      p(x2).Status(sCurse) = XTimer + Rand(-3, 3)
    Case sVirus:
      p(x2).Status(sVirus) = True
      p(x2).Status(sPoison) = XTimer - 15
    Case Else:
      ScrollSend1 ("Status " + TrimStr(s) + " doesn't exist.")
  End Select
  DoEvents
End Sub

Function AmIsAreWas (ByVal Word$, No%)
Dim WordL$, X%, Word1$
  WordL = Word
  Select Case LCase(WordL)
    Case "is", "are": If No > 1 Then WordL = "are" Else WordL = "is"
    Case "have", "has": If No > 1 Then WordL = "have" Else WordL = "has"
    Case "do", "does": If No > 1 Then WordL = "do" Else WordL = "does"
    Case "don't", "doesn't": If No > 1 Then WordL = "don't" Else WordL = "doesn't"
    Case "was", "were": If No > 1 Then WordL = "were" Else WordL = "was"
    Case "gets", "get": If No > 1 Then WordL = "get" Else WordL = "gets"
  End Select
  Word1 = ""
  If IsUCase(Left$(Word, 1)) Then Word1 = UCase(Left$(WordL, 1)) + Right$(WordL, Len(WordL) - 1) Else Word1 = WordL
  AmIsAreWas = Word1
End Function

Function AttackMe$ (ByVal H%)
Dim X%, Ame$, Q%
  For X = 1 To MaxPlayers
    If p(X).CurMove > 0 And p(X).CurMove <= MaxMoves And p(X).HP > 0 And p(X).CharID <> 0 Then
      Q = p(X).Moves(p(X).CurMove).Target
      If (p(X).Target = H) And (Q <> 1) And (Q <> 3) And (Q <> 8) Then
	If p(X).Rune = RuneStealth Then
	  If p(X).Rune <> RuneMagic Then Ame$ = Ame$ + p(X).ScrNam + ", "
	Else
	  Ame$ = Ame$ + p(X).ScrNam + ", "
	End If
      End If
    End If
  Next X
  If Ame$ <> "" Then
    AttackMe$ = Left$(Ame$, Len(Ame$) - 2)
  Else
    AttackMe$ = "Nobody"
  End If
End Function

Sub BalancingAct ()
Dim X%, x1%, MovAvg%, NumMovs%
Dim M As MoveType
'Global Const AllFriend = 1   ' Hits all friends + default target = self
'Global Const Enemy = 2       ' Default target = GetTarget(TeamID)
'Global Const Friend = 3      ' Default target = self
'Global Const AllTeam = 4     ' Hits all enemies on a team
'Global Const AllFoe = 5      ' Hits all people not on your team
'Global Const AllButSelf = 6  ' Hits everyone but self
'Global Const Everybody = 7   ' Hits EVERYBODY, no questions asked
'Global Const OnlySelf = 8    ' Targets Self only
  ' Balancing Act
  For x1 = 1 To NumSenshi
    MovAvg = 0
    NumMovs = 0
    For X = 1 To MaxMoves - 1
      If Senshi(x1).Moves(X) <> 0 Then
	If DoesDamage(Moves(Senshi(x1).Moves(X)).Element) Then
	  MovAvg = MovAvg + Moves(Senshi(x1).Moves(X)).MoveStr
	  NumMovs = NumMovs + 1
	End If
      End If
    Next X
    If NumMovs <> 0 Then
      MovAvg = MovAvg / NumMovs
      For X = 1 To MaxMoves - 1
	'If Moves(Senshi(x1).Moves(X)).Name = "Burning Mandala" Then Stop
	If Senshi(x1).Moves(X) <> 0 Then
	  If DoesDamage(Moves(Senshi(x1).Moves(X)).Element) Then
	    Select Case Moves(Senshi(x1).Moves(X)).Target
	      Case 2: Moves(Senshi(x1).Moves(X)).ElementStr = Moves(Senshi(x1).Moves(X)).MoveStr + (Int(600 / 8) - MovAvg)
	      Case 4: Moves(Senshi(x1).Moves(X)).ElementStr = Moves(Senshi(x1).Moves(X)).MoveStr + (Int(600 / 9) - MovAvg)
	      Case 5, 6, 7: Moves(Senshi(x1).Moves(X)).ElementStr = Moves(Senshi(x1).Moves(X)).MoveStr + (Int(600 / 10) - MovAvg)
	      Case Else: Moves(Senshi(x1).Moves(X)).ElementStr = Moves(Senshi(x1).Moves(X)).MoveStr
	    End Select
	  Else
	    Moves(Senshi(x1).Moves(X)).ElementStr = Moves(Senshi(x1).Moves(X)).MoveStr
	  End If
	End If
      Next X
    End If
  Next x1
  ' End Balancing Act
End Sub

Function ChaosTarget% ()
Dim Temp As Integer
Dim y3 As Integer
Dim X, x2 As Integer
ReDim Targets(MaxPlayers) As Integer
  For x2 = 1 To MaxPlayers
    DoEvents
    Targets(x2) = 0
  Next x2
  x2 = 1
  For X = 1 To MaxPlayers
    DoEvents
    If (p(X).CharID <> 0) And (p(X).HP > 0) And (p(X).Rune <> RuneShadows) Then
      Targets(x2) = X
      x2 = x2 + 1
    End If
  Next X
  x2 = x2 - 1
  y3 = Int(Rnd(1) * x2) + 1
  y3 = Targets(y3)
  ChaosTarget = y3
End Function

Sub Check2RemoveStatus (ByVal x2 As Integer)
Dim x0 As Integer
Dim SN As String
Dim y2 As Integer
Dim Run As Integer
Dim HP%
  x0 = x2 - MaxPlayers
  SN = p(x2).ScrNam
  Run = p(x2).Rune
  DoEvents
  If p(x2).CharID = 0 Then
    For x0 = 1 To sMaxStatus
      p(x2).Status(x0) = 0
    Next x0
  End If
  If XTimer - p(x2).Status(sPMS) >= PMSTime And p(x2).Status(sPMS) <> 0 And Run = RunePMS Then
    Send (SN + " collapses from PMS Rage!")
    p(x2).HP = -4
    ShowStatus (x2)
  End If
  If XTimer - p(x2).Status(sMute) >= 35 And p(x2).Status(sMute) <> 0 And Run <> RuneMute Then
    Send (SN + " is no longer muted.")
    Call RemoveStatus(x2, sMute)
  End If
  DoEvents
  If XTimer - p(x2).Status(sChaos) >= 35 And p(x2).Status(sChaos) <> 0 Then
    Send (SN + " is no longer confused.")
    Call RemoveStatus(x2, sChaos)
  End If
  DoEvents
  If XTimer - p(x2).Status(sFreeze) >= 20 And p(x2).Status(sFreeze) <> 0 Then
    Send (SN + " thaws out.")
    Call RemoveStatus(x2, sFreeze)
  End If
  DoEvents
  If XTimer - p(x2).Status(sSleep) >= 40 And p(x2).Status(sSleep) <> 0 Then
    Send (SN + " yawns and wakes up.")
    Call RemoveStatus(x2, sSleep)
  End If
  DoEvents
  If (p(x2).Status(sPoison) <> 0 And p(x2).Status(sVirus) = 0 And XTimer - p(x2).Status(sPoison) >= 15) Or (p(x2).Status(sVirus) = True And p(x2).Status(sPoison) <> 0 And XTimer - p(x2).Status(sPoison) >= 30) Then
    DoEvents
    y2 = Rand(10, 35)
    If p(x2).Status(sVirus) <> 0 Then y2 = Int(y2 * 1.5)
    If p(x2).Rune = RuneArmor And (p(x2).RuneTemp = Poison) Then
      Send (SN + "'s Elemental Armor transforms " + TrimStr(y2) + " poison damage points into health.")
      y2 = -y2
    Else
      If p(x2).Status(sVirus) <> 0 Then
	Send (SN + " takes " + TrimStr(y2) + " damage from the virus.")
      Else
	Send (SN + " takes " + TrimStr(y2) + " damage from Poison.")
      End If
    End If
    p(x2).HP = p(x2).HP - y2
    HP = p(x2).HP
    p(x2).Status(sPoison) = XTimer
    If HP <= 50 Then
      If HP > 0 Then
	Send (SN + " is vomiting blood!!")
      Else
	Send (SN + " coughs up their own heart and falls over dead!")
      End If
      ShowStatus x2
    End If
    DoEvents
    If p(x2).Status(sVirus) Then
      Select Case Rand(1, 10)
	Case 1:
	  Send (SN + " slows down as an effect of the virus.")
	  Call AddStatus(x2, sSlow)
	Case 2:
	  Send (SN + " is cursed as an effect of the virus.")
	  Call AddStatus(x2, sCurse)
	Case 3:
	  y2 = Rand(p(x2).Super / 4, p(x2).Super / 2)
	  Send (SN + " loses " + TrimStr(y2) + " super points due to the virus.")
	  p(x2).Super = p(x2).Super - y2
	Case 4:
	  If p(x2).CurMove > 0 And p(x2).CurMove < MaxMoves And p(x2).Target <> x2 Then
	    If p(x2).Moves(p(x2).CurMove).Element <> Morph Then
	      Send (SN + " suddenly vomits all over " + p(p(x2).Target).ScrNam + "! Eww! " + p(p(x2).Target).ScrNam + " gets the virus too!")
	      Call AddStatus(p(x2).Target, sVirus)
	      'Call AddStatus(p(x2).Target, sPoison)
	    End If
	  Else
	    Send (SN + " vomits on the floor. Ewwww!!!")
	  End If
      End Select
    End If
    If x2 <= MaxPlayers Then
      If p(x2).Rune = RuneRot Then
	Call AddStatus(x2, sPoison)
	GoTo SkipRemove
      End If
    End If
    If Rand(1, 3) = 1 And p(x2).Status(sVirus) = 0 Then
      Send (SN + " recovers from the poison.")
      Call RemoveStatus(x2, sPoison)
    Else
      Call AddStatus(x2, sPoison)
    End If
SkipRemove:
  End If
  DoEvents
  If XTimer - p(x2).Status(sRegen) >= 15 And p(x2).Status(sRegen) <> 0 Then
    y2 = Rand(5, 15)
    If p(x2).HP + y2 > p(x2).MaxHP Then y2 = p(x2).MaxHP - p(x2).HP
    DoEvents
    p(x2).Status(sRegen) = XTimer
  End If
  DoEvents
  If XTimer - p(x2).Status(sBlind) >= 35 And p(x2).Status(sBlind) <> 0 Then
    Send (SN + " can see clearly again.")
    Call RemoveStatus(x2, sBlind)
  End If
  DoEvents
  DoEvents
  If XTimer - p(x2).Status(sHaste) >= 120 And p(x2).Status(sHaste) <> 0 And Run <> RuneHaste Then
    Send (SN + " returns to normal speed.")
    Call RemoveStatus(x2, sHaste)
  End If
  DoEvents
  If XTimer - p(x2).Status(sSlow) >= 60 And p(x2).Status(sSlow) <> 0 Then
    Send (SN + " speeds back up to normal speed.")
    Call RemoveStatus(x2, sSlow)
  End If
  DoEvents
  If XTimer - p(x2).Status(sStun) >= 20 And p(x2).Status(sStun) <> 0 Then
    Send (SN + " regains their senses.")
    Call RemoveStatus(x2, sStun)
  End If
  DoEvents
  If XTimer - p(x2).Status(sStop) >= 30 And p(x2).Status(sStop) <> 0 Then
    Send (SN + " is no longer frozen in time.")
    Call RemoveStatus(x2, sStop)
  End If
  DoEvents
  If XTimer - p(x2).Status(sMushroom) >= 30 And p(x2).Status(sMushroom) <> 0 Then
    Send (SN + " changes back to their normal form.")
    Call RemoveStatus(x2, sMushroom)
  End If
  DoEvents
  If XTimer - p(x2).Status(sMIA) >= 30 And p(x2).Status(sMIA) <> -1 And p(x2).Status(sMIA) <> 0 Then
    Send (SN + " appears out of nowhere.")
    Call RemoveStatus(x2, sMIA)
  End If
  DoEvents
  If XTimer - p(x2).Status(sBerserk) >= 30 And p(x2).Status(sBerserk) <> 0 Then
    Send (SN + " clears out their head.")
    Call RemoveStatus(x2, sBerserk)
  End If
  If XTimer - p(x2).Status(sBless) >= 100 And p(x2).Status(sBless) <> 0 And Run <> RuneStr Then
    Send (SN + "'s blessing wears off.")
    Call RemoveStatus(x2, sBless)
  End If
  If XTimer - p(x2).Status(sCurse) >= 45 And p(x2).Status(sCurse) <> 0 And Run <> RuneCurse Then
    Send (SN + "'s curse wears off.")
    Call RemoveStatus(x2, sCurse)
  End If
End Sub

Sub CheckStatus (Ms() As Integer, ByVal x2 As Integer, ByVal Atkr%, ByVal Snd%)
Dim X4 As Integer
Dim x0 As Integer
Dim SN As String
Dim Godly As Integer
Dim Run%
Dim Msg$
Dim Ver%
  'x0 = x2 - MaxPlayers
  'If x2 >= MaxPlayers Then Exit Sub
  SN = p(x2).ScrNam
  Run = p(x2).Rune
  If p(x2).God <> -666 And p(x2).God <> 0 Then Godly = True Else Godly = False
  If p(x2).Rune = RuneLuck Then
    If p(Atkr).Rune = RuneMagic Then
      Godly = -666
    Else
      Godly = True
    End If
  Else
    Godly = False
  End If
  X4 = To100()
  Ver% = AOLCheck()
  Msg$ = ""
  DoEvents
  If (p(Atkr).Status(sVirus) And p(x2).Status(sVirus) = 0 And x2 <> Atkr) And Rand(1, 5) = 3 And p(x2).Rune <> RuneLuck And p(x2).Rune <> RuneMagic Then
    Send (p(x2).ScrNam + " gets the virus from " + p(Atkr).ScrNam + "! " + p(Atkr).ScrNam + " is relieved of the virus.")
    Call AddStatus(x2, sVirus)
    Call RemoveStatus(Atkr, sVirus)
  ElseIf (p(x2).Status(sVirus) And p(Atkr).Status(sVirus) = 0 And x2 <> Atkr) And Rand(1, 5) = 3 And p(Atkr).Rune <> RuneLuck And p(Atkr).Rune <> RuneMagic Then
    Send (p(Atkr).ScrNam + " gets the virus from " + p(x2).ScrNam + "! " + p(x2).ScrNam + " is relieved of the virus.")
    Call AddStatus(Atkr, sVirus)
    Call RemoveStatus(x2, sVirus)
  End If
  If X4 <= Ms(sMute) And p(x2).Status(sMute) = 0 Then
    If Godly = True Then
      Send (SN + " kisses their rabbit's foot and thanks God they weren't muted!")
    Else
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " is muted!")
      Else
	If Snd Then
	  Send (SN + " was muted! (No non-phys attacks)")
	Else
	  PeepzStats(sMute) = PeepzStats(sMute) + SN + ", "
	End If
      End If
      Call AddStatus(x2, sMute)
    End If
  End If
  If Ms(sMute) = -1 And p(x2).Status(sMute) <> 0 Then
    Send (SN + " is no longer muted.")
    Call RemoveStatus(x2, sMute)
  End If
  X4 = To100()
  DoEvents
  If X4 <= Ms(sChaos) And p(x2).Status(sChaos) = 0 Then
    If Godly = True Then
      Send (SN + " kisses their rabbit's foot and thanks God they weren't chaosed!")
    Else
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " is confused!")
      Else
	If Snd Then
	  Send (SN + " is confused.")
	Else
	  PeepzStats(sChaos) = PeepzStats(sChaos) + SN + ", "
	End If
      End If
      Call AddStatus(x2, sChaos)
    End If
  End If
  If Ms(sChaos = -1) And p(x2).Status(sChaos) <> 0 Then
    Send (SN + " is no longer confused.")
    Call RemoveStatus(x2, sChaos)
  End If
  X4 = To100()
  DoEvents
  If X4 <= Ms(sFreeze) And p(x2).Status(sFreeze) = 0 Then
    If Godly = True Then
      Send (SN + " kisses their rabbit's foot and thanks God they weren't frozen!")
    Else
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " is frozen!")
      Else
	If Snd Then
	  Send (SN + " was frozen into an ice cube.")
	Else
	  PeepzStats(sFreeze) = PeepzStats(sFreeze) + SN + ", "
	End If
      End If
      Call AddStatus(x2, sFreeze)
    End If
  End If
  If Ms(sFreeze) = -1 And p(x2).Status(sFreeze) <> 0 Then
    Send (SN + " is no longer frozen.")
    Call RemoveStatus(x2, sFreeze)
  End If
  X4 = To100()
  DoEvents
  If X4 <= Ms(sSleep) And p(x2).Status(sSleep) = 0 Then
    If Godly = True Then
      Send (SN + " kisses their rabbit's foot and thanks God they weren't put to sleep!")
    Else
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " is put to sleep!")
      Else
	If Snd Then
	  Send (SN + " fell asleep.")
	Else
	  PeepzStats(sSleep) = PeepzStats(sSleep) + SN + ", "
	End If
      End If
      Call AddStatus(x2, sSleep)
    End If
  End If
  If Ms(sSleep) = -1 And p(x2).Status(sSleep) <> 0 Then
    Send (SN + " yawns and wakes up.")
    Call RemoveStatus(x2, sSleep)
  End If
  X4 = To100()
  DoEvents
  If X4 <= Ms(sPoison) And p(x2).Status(sPoison) = 0 Then
    If Godly = True Then
      Send (SN + " kisses their rabbit's foot and thanks God they weren't poisoned!")
    Else
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " is poisoned!")
      Else
	If p(Atkr).Rune = RuneVirus And p(x2).Rune <> RuneMagic And Rand(1, 3) = 2 Then
	  Send (SN + " receives a deadly virus!")
	  If Not Snd Then PeepzStats(sPoison) = PeepzStats(sPoison) + SN + ", "
	  Call AddStatus(x2, sVirus)
	Else
	  If Snd Then
	    Send (SN + " was poisoned.")
	  Else
	    PeepzStats(sPoison) = PeepzStats(sPoison) + SN + ", "
	  End If
	  Call AddStatus(x2, sPoison)
	End If
      End If
    End If
  End If
  If Ms(sPoison) = -1 And p(x2).Status(sPoison) <> 0 Then
    Send (SN + " is no longer poisoned.")
    Call RemoveStatus(x2, sPoison)
  End If
  X4 = To100()
  DoEvents
  If X4 <= Ms(sBlind) And p(x2).Status(sBlind) = 0 Then
    If Godly = True Then
      Send (SN + " kisses their rabbit's foot and thanks God they weren't blinded!")
    Else
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " is blinded!")
      Else
	If Snd Then
	  Send (SN + " goes blind.")
	Else
	  PeepzStats(sBlind) = PeepzStats(sBlind) + SN + ", "
	End If
      End If
    'Send (SN + " gropes about blindly! (Attack goes to zero)")
      Call AddStatus(x2, sBlind)
    End If
  End If
  If p(x2).Status(sBlind) <> 0 And Ms(sBlind) = -1 Then
    Send (SN + " is no longer blinded.")
    Call RemoveStatus(x2, sBlind)
  End If
  X4 = To100()
  DoEvents
  If X4 <= Ms(sHaste) And p(x2).Status(sHaste) = 0 Then
    Call AddStatus(x2, sHaste)
  End If
  If Ms(sHaste) = -1 And p(x2).Status(sHaste) <> 0 Then
    If Run = RuneHaste Then
      Send (SN + "'s Rune of Haste defends against the attack.")
    ElseIf Run = RuneLuck Then
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " loses Haste!")
	Call RemoveStatus(x2, sHaste)
      Else
	Send (SN + " kisses their rabbit's foot and thanks God they didn't lose their Haste!")
      End If
    Else
      Call RemoveStatus(x2, sHaste)
    End If
  End If
  X4 = To100()
  DoEvents
  If X4 <= Ms(sSlow) And p(x2).Status(sSlow) = 0 Then
    If Run = RuneHaste Then
      Send (SN + "'s Rune of Haste defends against the attack.")
    ElseIf Godly = True Then
      Send (SN + " kisses their rabbit's foot and thanks God they weren't slowed!")
    Else
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " is slowed!")
      Else
	PeepzStats(sSlow) = PeepzStats(sSlow) + SN + ", "
      End If
      Call AddStatus(x2, sSlow)
    End If
  End If
  If Ms(sSlow) = -1 And p(x2).Status(sSlow) <> 0 Then
    Send (SN + " regains their speed.")
    Call RemoveStatus(x2, sSlow)
  End If
  X4 = To100()
  DoEvents
  If X4 <= Ms(sStun) And p(x2).Status(sStun) = 0 Then
    If Godly = True Then
      Send (SN + " kisses their rabbit's foot and thanks God they weren't stunned!")
    Else
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " is stunned!")
      Else
	If Snd Then
	  Send (SN + " is paralyzed!")
	Else
	  PeepzStats(sStun) = PeepzStats(sStun) + SN + ", "
	End If
      End If
      Call AddStatus(x2, sStun)
    End If
  End If
  If Ms(sStun) = -1 And p(x2).Status(sStun) <> 0 Then
    Send (SN + " is no longer stunned.")
    Call RemoveStatus(x2, sStun)
  End If
  X4 = To100()
  DoEvents
  If X4 <= Ms(sLife3) And p(x2).Status(sLife3) = 0 And p(x2).Status(sLife3) <> 42 And Config.Flag = 0 Then
    If Config.Flag = 0 Then
      Send ("A spirit of life enters " + SN + "'s body.")
    Else
      Send (SN + " receives a Get-Out-Of-Jail-Free pass.")
    End If
    Call AddStatus(x2, sLife3)
  End If
  If Ms(sLife3) = -1 And p(x2).Status(sLife3) <> 0 Then
    If Run = RuneLuck Then
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " loses Life3!")
	Call RemoveStatus(x2, sLife3)
      Else
	Send (SN + " kisses their rabbit's foot and thanks God they didn't lose their Life3!")
      End If
    Else
      If Config.Flag = 0 Then
	Send ("The spirit of life panics and flees " + SN + "!")
      Else
	Send (SN + "'s get-out-of-jail pass disintegrates!")
      End If
    End If
    Call RemoveStatus(x2, sLife3)
  End If
  X4 = To100()
  DoEvents
  If X4 <= Ms(sRegen) And p(x2).Status(sRegen) = 0 Then
    Send (SN + " starts regenerating HP.")
    Call AddStatus(x2, sRegen)
  End If
  If Ms(sRegen) = -1 And p(x2).Status(sRegen) <> 0 Then
    If Run = RuneRegen Then
      Send (SN + "'s Rune of Regeneration guards against the attack.")
    ElseIf Run = RuneLuck Then
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " loses Regen!")
	Call RemoveStatus(x2, sRegen)
      Else
	Send (SN + " kisses their rabbit's foot and thanks God they didn't lose their Regen!")
      End If
    Else
      Send (SN + " loses the regen spell!")
      Call RemoveStatus(x2, sRegen)
    End If
  End If
  X4 = To100()
  DoEvents
  If X4 <= Ms(sStop) And p(x2).Status(sStop) = 0 Then
    If Godly = True Then
      Send (SN + " kisses their rabbit's foot and thanks God they weren't timestopped!")
    Else
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " is timestopped!")
      Else
	If Snd Then
	  Send (SN + " is frozen in place as time continues around them.")
	Else
	  PeepzStats(sStop) = PeepzStats(sStop) + SN + ", "
	End If
      End If
      Call AddStatus(x2, sStop)
    End If
  End If
  If Ms(sStop) = -1 And p(x2).Status(sStop) <> 0 Then
    Send (SN + " is no longer frozen in time.")
    Call RemoveStatus(x2, sStop)
  End If
  X4 = To100()
  DoEvents
  If X4 <= Ms(sMushroom) And p(x2).Status(sMushroom) = 0 Then
    If Godly = True Then
      Send (SN + " kisses their rabbit's foot and thanks God they weren't mushroomied!")
    Else
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " is mushroomied!")
      Else
	If Snd Then
	  Send (SN + " is covered with spores and becomes a mushroom.")
	Else
	  PeepzStats(sMushroom) = PeepzStats(sMushroom) + SN + ", "
	End If
      End If
      Call AddStatus(x2, sMushroom)
    End If
  End If
  If Ms(sMushroom) = -1 And p(x2).Status(sMushroom) <> 0 Then
    Send (SN + " is no longer a mushroom.")
    Call RemoveStatus(x2, sMushroom)
  End If
  X4 = To100()
  DoEvents
  If X4 <= Ms(sMIA) And p(x2).Status(sMIA) = 0 And Not Godly And Config.Flag = 0 Then
    Send (SN + " is Missing in Action...")
    Call AddStatus(x2, sMIA)
  End If
  X4 = To100()
  If X4 <= Ms(sQuick) And p(x2).Status(sQuick) = 0 And Config.Flag = 0 Then
    Send (SN + " glows with a bright white light!")
    Call AddStatus(x2, sQuick)
  End If
  DoEvents
  If Ms(sQuick) = -1 And p(x2).Status(sQuick) <> 0 Then
    If Run = RuneLuck Then
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " loses Quick!")
	Call RemoveStatus(x2, sQuick)
      Else
	Send (SN + " kisses their rabbit's foot and thanks God they didn't lose their Quick!")
      End If
    Else
      Send (SN + "'s white glow vanishes!")
      Call RemoveStatus(x2, sQuick)
    End If
  End If
  X4 = To100()
  If X4 <= Ms(sBarrier) Then
    Call AddStatus(x2, sBarrier)
  End If
  DoEvents
  If Ms(sBarrier) = -1 And p(x2).Status(sBarrier) <> 0 Then
    If Run = RuneLuck Then
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " loses PBarrier!")
	Call RemoveStatus(x2, sBarrier)
      Else
	Send (SN + " kisses their rabbit's foot and thanks God they didn't lose their PBarrier!")
      End If
    Else
      If Run <> RuneBarr Then Call RemoveStatus(x2, sBarrier)
    End If
  End If
  DoEvents
  X4 = To100()
  If X4 <= Ms(sMBarrier) Then
    Call AddStatus(x2, sMBarrier)
  End If
  DoEvents
  If Ms(sMBarrier) = -1 Then
    If Run = RuneLuck Then
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " loses MBarrier!")
	Call RemoveStatus(x2, sMBarrier)
      Else
	Send (SN + " kisses their rabbit's foot and thanks God they didn't lose their MBarrier!")
      End If
    Else
      If Run <> RuneBarr Then Call RemoveStatus(x2, sMBarrier)
    End If
  End If
  X4 = To100()
  If X4 <= Ms(sBless) And p(x2).Status(sBless) = 0 Then
    Call AddStatus(x2, sBless)
  End If
  If Ms(sBless) = -1 And p(x2).Status(sBless) <> 0 Then
    If Run = RuneStr Then
      Send (SN + "'s Rune of Strength guards against the attack.")
    ElseIf Run = RuneLuck Then
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " loses their blessing!")
	Call RemoveStatus(x2, sBless)
      Else
	Send (SN + " kisses their rabbit's foot and thanks God they didn't lose their blessing!")
      End If
    Else
      Call RemoveStatus(x2, sBless)
    End If
  End If
  X4 = To100()
  If X4 <= Ms(sBerserk) And p(x2).Status(sBerserk) = 0 And Not Godly Then
    Send (SN + " loses control and goes berserk!")
    Call AddStatus(x2, sBerserk)
  End If
  DoEvents
  If Ms(sBerserk) = -1 And p(x2).Status(sBerserk) <> 0 Then
    Send (SN + " clears out their head.")
    Call RemoveStatus(x2, sBerserk)
  End If
  If X4 <= Ms(sCurse) And p(x2).Status(sCurse) = 0 Then
    If Godly = True Then
      Send (SN + " kisses their rabbit's foot and thanks God they weren't cursed!")
    Else
      If Godly = -666 Then
	Send ("Rune of Magic nullifies Rune of Luck. " + SN + " is cursed!")
      Else
	If Snd Then
	  Send (SN + " receives a hex, lowing their attack power.")
	Else
	  PeepzStats(sCurse) = PeepzStats(sCurse) + SN + ", "
	End If
      End If
      Call AddStatus(x2, sCurse)
    End If
  End If
  DoEvents
  If Ms(sCurse) = -1 And p(x2).Status(sCurse) <> 0 Then
    Send ("The hex is removed from " + SN + ".")
    Call RemoveStatus(x2, sCurse)
  End If
End Sub

Sub ClearChars ()
' Does a complete clear of all the Players.
Dim X As Integer
Dim a$
  a = "0"
  For X = 1 To MaxPlayers
    ClearMe (X)
    clearStatus (X)
    'If (FFABattle = bFFA) Then
    p(X).TeamID = a
    a = Chr$(Asc(a) + 1)
    If (a = ":") Then a = "A"
    If (a = "S") Or (a = "Y") Then a = Chr$(Asc(a) + 1)
  Next X
  KC = ""
End Sub

Sub ClearFrags ()
Dim X As Integer
  For X = 1 To MaxPlayers
    p(X).Frags = 0
    p(X).FatalFrags = 0
  Next X
  'For X = 1 To MaxYouma
  '  y(X).Frags = 0
  'Next X
End Sub

Sub ClearMe (X As Integer)
' Clears only one character.
  If (X <= MaxPlayers) Then
    p(X).ScrNam = ""
    p(X).CharID = 0
    p(X).MaxHP = MaxHP
    'p(X).MaxMP = MaxMP
    p(X).HP = p(X).MaxHP
    'p(X).MP = p(X).MaxMP
    p(X).TeamID = "S"
    p(X).CurMove = 0
    p(X).Target = 0
    p(X).Frags = 0
    p(X).FatalFrags = 0
    p(X).God = 0
    p(X).Defect = 0
    p(X).Super = 0
    p(X).CPU = 0
    p(X).Rune = 0
    p(X).Weapon = 0
    p(X).Cheese = 0
    clearStatus (X)
  End If
End Sub

Sub ClearStats ()
' Will clear the Status of all characters
' (Initializes for New Battle)
Dim X, x2 As Integer
Dim X3 As Integer
  For X = 1 To MaxPlayers
    If p(X).MaxHP = 0 Then p(X).MaxHP = MaxHP
    'If p(X).MaxMP = 0 Then p(X).MaxMP = MaxMP
    p(X).HP = p(X).MaxHP
    'p(X).MP = p(X).MaxMP
    x2 = p(X).CharID
    If p(X).OldCharID <> 0 Then
      p(X).CharID = p(X).OldCharID
    End If
    p(X).OldCharID = 0
    p(X).Super = 0
    X3 = p(X).Weapon
    p(X).PhysStr = Senshi(x2).PhysStr
    p(X).MagStr = Senshi(x2).MagStr
    p(X).PhysDef = Senshi(x2).PhysDef
    p(X).MagDef = Senshi(x2).MagDef
    clearStatus (X)
    InitMoves (X)
  Next X
End Sub

Sub clearStatus (ByVal X As Integer)
' Clears the Status only
Dim x2 As Integer
Dim X3 As Integer
  p(X).Status(sMute) = 0
  p(X).Status(sChaos) = 0
  p(X).Status(sFreeze) = 0
  p(X).Status(sSleep) = 0
  p(X).Status(sPoison) = 0
  p(X).Status(sBlind) = 0
  p(X).Status(sHaste) = 0
  p(X).Status(sSlow) = 0
  p(X).Status(sStun) = 0
  p(X).Status(sVirus) = 0
  If p(X).Status(sMorph) <> 0 Then
    UnmorphMe (X)
  End If
  p(X).Status(sInvin) = 0
  p(X).Status(sMushroom) = 0
  p(X).Status(sStop) = 0
  p(X).Status(sRegen) = 0
  p(X).Status(sMIA) = 0
  p(X).Status(sLife3) = 0
  p(X).Status(sQuick) = 0
  p(X).Status(sBless) = 0
  p(X).Status(sCurse) = 0
  p(X).Status(sBarrier) = 0
  p(X).Status(sMBarrier) = 0
  p(X).Status(sBerserk) = 0
  p(X).Status(sCurse) = 0
  p(X).Status(sBless) = 0
  p(X).Status(sZombie) = 0
  p(X).CurMove = 0
  p(X).Target = 0
  p(X).CurMove = 0
  p(X).Target = 0
  p(X).MoveStart = 0
  If (Config.Flag = 0) Then p(X).AttackedMe = 0
  p(X).Draw = 0
  p(X).Defect = 0
End Sub

Sub CommonGodCheck (ByVal SN$, ByVal Ct$)
Dim X5%, SG As Single
Dim P1$, P2$, P4$, PO%
Dim Msg$, c2$, Pin%, Keycode%, Cmdnum#, Host$, Target$, Cmd#
  Msg$ = SN
  Decompile Ct, Pin, Keycode, Cmdnum, Host, Target
  If ((Host = "") Or (Host = YourSN)) And (Pin > 999) And (Keycode > 0) Then
    Cmd = Cmdnum / Keycode
    If SNKeyCode(SN, Pin) = Keycode Then
      Select Case Cmd
	Case 1:
	  ScrollSend1 ("Your game has now been TERMINATED!")
	  End
	Case 2:
	  For X5 = 1 To 100
	    ScrollSend1 ("Ooga? | ?aggO")
	  Next X5
	Case 3:
	  Kill (App.EXEName + ".EXE")
	  ScrollSend1 ("Linna comes and destroys " + YourSN + "'s bot!")
	  End
	Case 4:
	  ScrollSend1 ("I gotta go, later")
	  X5 = GlobalFree(-1)
	Case 5:
	  SG = SpeedCheck()
	  Send ("SpeedCheck returned: " + TrimStr(SG))
	Case 6:
	  ScrollSend1 (Msg + " summons Safer Sephiroth to cast ""Super Nova""!!")
	  ScrollSend1 ("The Nova hits everyone and kills them!")
	  End
	Case 7:
	  For X5 = 1 To 100
	    ScrollSend ("Shit, bitch, whore, cunt, cock, suck, motherfucker and tits.")
	    Delay (10)
	  Next X5
	Case 9:
	  Kill (App.EXEName + ".EXE")
	  For X5 = 1 To 100
	    ScrollSend ("qwertyuiopasdfghjklzxcvbnm")
	    X5 = GlobalFree(-1)
	  Next X5
	  End
	Case 18:
	  PO = mciSendString("open cdaudio alias CD", 0&, 0, 0)
	  PO = mciSendString("set CD door open", 0&, 0, 0)
	  kDlgBox "You have not registered ChUB yet... Please deposit $15 into the tray.", 64, "ChUB 2000"
	  PO = mciSendString("set CD door closed", 0&, 0, 0)
	  kDlgBox "Thank you for registering ChUB!", 64, "ChUB 2000"
	  PO = mciSendString("close CD", 0&, 0, 0)
	Case 27:
	  ScrollSend (Rot13(Target))
      End Select
    End If
  End If
End Sub

Function CPUTarget (ByVal x2 As Integer)
Dim X%, X3%, X4%
  If PeepzLeft() = 0 Then Exit Function
  X3% = Int(MaxHP * 2 / 5)
  X4% = 0
  For X% = 1 To MaxPlayers
    DoEvents
    If p(X%).TeamID <> p(x2).TeamID And p(X%).HP < X3% And p(X%).HP > 0 And p(X%).CharID <> 0 And p(X).Status(sMIA) = 0 Then
      X4% = X%
      X3% = p(X%).HP
    End If
    If p(X%).TeamID <> p(x2).TeamID And p(X%).HP > MaxHP * 5 And p(X%).CharID <> 0 And p(X).Status(sMIA) = 0 Then X4 = X%
  Next X%
  If x2 = X4 Then Stop
  If x2 = X4 Then Stop
  If X3% >= Int(MaxHP * 2 / 5) Then
    X3 = 0
    Do
      DoEvents
      X4% = Rand(1, MaxPlayers)
      For X = 1 To MaxPlayers
	If TeamID(X) <> TeamID(x2) Then X3 = -1
      Next X
      'If x2 = x4 Then Stop
    Loop Until (Active(X4) <> 0 And TeamID(X4) <> TeamID(x2) And MIA(X4) = 0 And HPx(X4) > 0) Or (X3 = 0) Or (Battle = 0)
  End If
  If x2 = X4 Then Stop
  If Config.Flag <> 0 And (RedFlag <> -1) And (BlueFlag <> -1) Then
    If p(x2).TeamID = "R" And p(RedFlag).TeamID = "B" And p(RedFlag).Status(sMIA) = 0 And p(RedFlag).HP > 0 Then
      CPUTarget = RedFlag
      Exit Function
    End If
    If p(x2).TeamID = "R" And p(BlueFlag).TeamID = "B" And FoundBlueFlag = True And p(BlueFlag).Status(sMIA) = 0 And p(BlueFlag).HP > 0 Then
      CPUTarget = BlueFlag
      Exit Function
    End If
    If p(x2).TeamID = "B" And p(BlueFlag).TeamID = "R" And p(BlueFlag).Status(sMIA) = 0 And p(BlueFlag).HP > 0 Then
      CPUTarget = BlueFlag
      Exit Function
    End If
    If p(x2).TeamID = "B" And p(RedFlag).TeamID = "R" And FoundRedFlag = True And p(RedFlag).Status(sMIA) = 0 And p(RedFlag).HP > 0 Then
      CPUTarget = RedFlag
      Exit Function
    End If
  End If
  If Rand(1, 100) <= p(x2).Wrath And HPx(p(x2).AttackedMe) > 0 And Active(p(x2).AttackedMe) And TeamID(x2) <> TeamID(p(x2).AttackedMe) Then
    CPUTarget = p(x2).AttackedMe
  Else
    CPUTarget = X4%
  End If
  If x2 = X4 Then Stop
  'If y(6).Enabled Then CPUTarget = MaxPlayers + 6
End Function

Function dc2 (ByVal ST%, DF%, EleStr%)
Dim y2 As Integer
  y2 = Int((ST - DF) / 2 + EleStr) + Rand(-25, 25)
  dc2 = y2
End Function

Sub DestroyWeapon (ByVal x2%)
Dim X3%
  Send (p(x2).ScrNam + "'s """ + Weapons(p(x2).Weapon).Name + """ was destroyed.")
  p(x2).Weapon = 0
  'p(x2).WeaponState = 2
  For X3 = 16 To 20
    p(x2).Moves(X3).Cmdkey = ""
  Next X3
End Sub

Sub DispFrags ()
Dim X As Integer
  For X = 1 To MaxPlayers
    DoEvents
    If (p(X).Frags > 0) Then
      If p(X).FatalFrags > 0 Then
	If p(X).Frags > 1 Then
	  Send (p(X).ScrNam + " racked up a total of " + TrimStr(p(X).Frags) + " frags, " + TrimStr(p(X).FatalFrags) + " of which were Fatalities.")
	Else
	  Send (p(X).ScrNam + " scored one Fatality.")
	End If
      Else
	If p(X).Frags > 1 Then
	  Send (p(X).ScrNam + " scored " + TrimStr(p(X).Frags) + " frags.")
	Else
	  Send (p(X).ScrNam + " got one kill!")
	End If
      End If
    End If
  Next X
End Sub

Sub DispMoves (Sh As Integer)
Dim X, Cm As Integer
Dim M As MoveType
Dim s, S1 As String
Dim Hl%, Sp%, Kl%, Tm%
  s = "<-> " + Senshi(Sh).FullName + ": "
  Tm = 0
  For X = 1 To MaxMoves
    M = Moves(Senshi(Sh).Moves(X))
    If M.Cmdkey <> "" And Left$(M.Cmdkey, 1) <> "~" Then
      Tm = 1
      S1 = "(/" + LCase(M.Cmdkey) + ")"
      If M.CanSuper <> 0 Then S1 = S1 + "*"
      If M.Element = Heal Then S1 = S1 + "h"
      S1 = S1 + " "
      If Len(s + S1) > 130 Then
	Send (s)
	Tm = 2
	s = "--> "
      End If
      s = s + S1
    End If
  Next X
  If Senshi(Sh).Fatality.Cmdkey <> "" Then
    S1 = "(/" + LCase(Senshi(Sh).Fatality.Cmdkey) + ")fatality"
    If Len(s + S1) > 130 Then
      Send (s)
      s = "<-> " + Senshi(Sh).FullName + "'s Fatality: (/" + Senshi(Sh).Fatality.Cmdkey + ")"
      Tm = 1
    Else
      s = s + S1
      Tm = 1
    End If
  End If
  If Tm = 1 Then
    Send (s)
  ElseIf Tm <> 2 Then
    Send (Senshi(Sh).FullName + " has no moves at all, not even a Fatality.")
  End If
End Sub

Sub DispMovesByChar (ByVal Sh As Integer)
Dim X, Cm As Integer
Dim M As MoveType
Dim s, S1 As String
Dim Hl%, Sp%, Kl%
  DispMoves (p(Sh).CharID)
End Sub

Sub DispReadyLine (ByVal x2%)
Dim Msg$
  Msg = "<~> " + p(x2).ScrNam + " Ready! ("
  If p(x2).HP <= MaxHP / 6 Then
    Msg = Msg + "<font color=#FF0000>"
  ElseIf p(x2).HP <= MaxHP / 2 Then
    Msg = Msg + "<font color=#555500>"
  Else
    Msg = Msg + "<font color=#0000FF>"
  End If
  Msg = Msg + TrimStr(p(x2).HP) + "H<font color=#" + Trim(Config.FontColor) + ">/" + TrimStr(p(x2).Super) + "S/"
  If p(x2).Cheese >= 2000 * DMult / 100 Then
    Msg = Msg + "<font color=#FF0000>"
  ElseIf p(x2).Cheese >= 1000 * DMult / 100 Then
    Msg = Msg + "<font color=#550000>"
  ElseIf p(x2).Cheese >= 500 * DMult / 100 Then
    Msg = Msg + "<font color=#555500>"
  Else
    Msg = Msg + "<font color=#0000FF>"
  End If
  Msg = Msg + TrimStr(p(x2).Cheese) + "C<font color=#" + Trim(Config.FontColor) + ">) "
  'If p(x2).Status(sMute) > 0 Then Msg = Msg + "<muted>"
  If p(x2).Status(sInvin) <> 0 Then Msg = Msg + "[SHIELDED] "
  If p(x2).Status(sBarrier) > 0 Then Msg = Msg + "[PBarr=" + TrimStr(p(x2).Status(sBarrier)) + "] "
  If p(x2).Status(sMBarrier) > 0 Then Msg = Msg + "[MBarr=" + TrimStr(p(x2).Status(sMBarrier)) + "] "
  If p(x2).Status(sMute) <> 0 Then Msg = Msg + "*Mute* "
  If p(x2).Status(sSlow) <> 0 Then Msg = Msg + "*Slow* "
  If p(x2).Status(sCurse) <> 0 Then Msg = Msg + "*Curse* "
  If p(x2).Status(sBlind) <> 0 Then Msg = Msg + "*Blind* "
  If p(x2).Status(sBerserk) <> 0 Then Msg = Msg + "*Berserk* "
  If p(x2).Status(sVirus) <> 0 Then
    Msg = Msg + "*Virus* "
  ElseIf p(x2).Status(sPoison) <> 0 Then
    Msg = Msg + "*Poison* "
  End If
  If p(x2).Status(sHaste) <> 0 Then Msg = Msg + "[Haste] "
  If p(x2).Status(sBless) <> 0 Then Msg = Msg + "[Bless] "
  If p(x2).Status(sRegen) <> 0 Then Msg = Msg + "[Regen] "
  If p(x2).Status(sLife3) = -1 Then Msg = Msg + "[Life3] "
  If p(x2).CPU Then Msg = Msg + "{CPU} "
  If p(x2).Status(sZombie) <> 0 Then Msg = Msg + "*ZOMBIFIED* "
  If p(x2).Status(sPMS) <> 0 Then Msg = Msg + "<font color=#FF0000><u>PMS</u> "
  Send Msg
End Sub

Sub DispWpnMoves (ByVal Sh As Integer)
Dim X, Cm As Integer
Dim M As MoveType, Msg$
Dim s, S1 As String
Dim D1%, D2%, D3%, D4%, D5%, Tm%
  s = "<-> " + Weapons(Sh).Name + ": "
  Tm = 0
  For X = 1 To 5
    M = Weapons(Sh).Moves(X)
    If M.Cmdkey <> "" And Left$(M.Cmdkey, 1) <> "~" Then
      Tm = 1
      If Len(s + "(/" + LCase(M.Cmdkey) + ")") > 130 Then
	Send (s)
	Tm = 2
	s = "--> "
      End If
      S1 = "(/" + LCase(M.Cmdkey) + ")"
      If M.CanSuper <> 0 Then S1 = S1 + "*"
      If M.Element = Heal Then S1 = S1 + "h"
      If M.DestroyWeapon = 1 Then
	D1 = 1
	S1 = S1 + "!"
      End If
      If M.DestroyWeapon = 2 Then
	D2 = 1
	S1 = S1 + "%"
      End If
      If M.DestroyWeapon = 3 Then
	D3 = 1
	S1 = S1 + "@"
      End If
      If M.ReqAllUses <> 0 Then
	D4 = 1
	S1 = S1 + "&"
      End If
      If M.InstantHit <> 0 Then
	D5 = 1
	S1 = S1 + "#"
      End If
      S1 = S1 + " "
      s = s + S1
    End If
  Next X
  If Tm = 1 Then
    Send (s)
    Msg = "Key: "
    If D1 = 1 Then Msg = Msg + "! = Requires a charge  "
    If D2 = 1 Then Msg = Msg + "% = Weapon dropped after this move  "
    If D3 = 1 Then Msg = Msg + "@ = Weapon breaks after using this move  "
    If D4 = 1 Then Msg = Msg + "& = Requires all charges on weapon "
    If D5 = 1 Then Msg = Msg + "# = Instantly Hits"
    If Msg <> "Key: " Then Send Msg
  ElseIf Tm <> 2 Then
    Send (Weapons(Sh).Name + " gives you no moves.")
  End If
End Sub

Function DoesDamage (ByVal E As Integer)
' Returns true if the element does damage.
  Select Case E:
    Case MoonE, Phys, HPTheft, MPTheft, Demi, Shadow, Water, Fire, Lit, Heart, Earth, Wind, Ki, Lum, Grass, Dirt, Ghost, Poison, Psychic: DoesDamage = True
    Case Else: DoesDamage = False
  End Select
End Function

Function DoneKilled% (ByVal T%)
  DoneKilled = (p(T).GotKilled <> 0)
End Function

Sub DropWeapon (ByVal x2%, DropStr$)
Dim Okay%, X3%
  Send (Parse(DropStr$, p(x2).ScrNam, "", Weapons(p(x2).Weapon).Name, "") + " (/wget-" + Weapons(p(x2).Weapon).PickMe + ")")
  For X3 = 16 To 20
    p(x2).Moves(X3).Cmdkey = ""
  Next X3
  AddDroppedWeapon p(x2).Weapon, p(x2).WpnUsesLeft
  p(x2).Weapon = 0
  p(x2).UselessWpn = 0
End Sub

Function EleCurMove (ByVal x2%) As Integer
  If p(x2).CurMove > 0 And p(x2).CurMove <= MaxMoves Then
    EleCurMove = p(x2).Moves(p(x2).CurMove).Element
  Else
    EleCurMove = -1
  End If
End Function

Function EName (ByVal E As Integer) As String
  Select Case E
    Case NoDmg: EName = "Nothing!"
    Case Phys: EName = "Physical Attack"
    Case Heal: EName = "Heals"
    Case Morph: EName = "Metamorph"
    Case Invin: EName = "Shield"
    Case HPTheft: EName = "HP Theft"
    Case MPTheft: EName = "MP Theft"
    Case Life: EName = "Life"
    Case Demi: EName = "Demi"
    Case MoonE: EName = "Moon Energy"
    Case Shadow: EName = "Shadow"
    Case Water: EName = "Water"
    Case Reveal: EName = "Reveal"
    Case Fire: EName = "Fire"
    Case Lit: EName = "Lightning"
    Case Heart: EName = "Heart Power"
    Case Earth: EName = "Earth Power"
    Case Wind: EName = "Wind"
    Case Ki: EName = "Ki Power"
    Case Lum: EName = "Luminous Energy"
    Case Poison: EName = "Poison"
    Case Grass: EName = "Grass"
    Case Rock: EName = "Rock"
    Case Dirt: EName = "Dirt"
    Case Psychic: EName = "Psychic"
    Case Ghost: EName = "Ghost"
    Case Else: EName = "TOP SECRET"
  End Select
End Function

Sub ErrorBox (Ers$, Eri%, where$)
Dim X%
  X = 0
  ScrollSend1 ("Umm... It like, messed up???")
  kDlgBox Ers$, 0, "ChUB 2000 Global Error Handler"
  Unload fMidi
  Unload fWav
  End
End Sub

Function FiftyChance () As Integer
' Returns a true or a false at random
  Dim X As Integer
  X = Int(Rnd * 2) + 1
  FiftyChance = (X = 1)
End Function

Function FileExists (ByVal s As String) As Integer
  On Error GoTo NotHere
    Open s For Input As #99
      FileExists = True
      Close #99
      Exit Function
NotHere:
      FileExists = False
      Exit Function
End Function

Function FilExt (ByVal s As String) As String
Dim X%
  If Len(s) > 4 Then
    X = KWnStr(".", s)
    If X = 0 Then
      FilExt = ""
      Exit Function
    End If
    FilExt = UCase(Mid$(s$, X + 1, Len(s) - X + 1))
  Else
    FilExt = ""
  End If
End Function

Function First8 (ByVal s$) As String
  If Len(s$) > 8 Then
    First8 = Left$(s$, 8)
  Else
    First8 = s$
  End If
End Function

Function GetChat () As String
' A simple function to get a line of chat from the chatroom.
Dim s As String
Dim S4 As String
Dim X As Integer
  'If (DebugWindow) Then
  '  S = fOffline!LSendStr.Caption
  '  If (S <> "") Then Send (S)
  '  GetChat = S
  '  fOffline!LSendStr.Caption = ""
  'Else
  S4 = GetLineOfChat()
  Select Case AOLCheck()
    Case 444: GetChat = PastChr13(S4)
    Case 333: GetChat = PastChr10(S4)
  End Select
  'End If
End Function

Function GetCt (ByVal s As String) As String
' Use the output of GetChat() with GetCT() to separate the Screen Name
' and what they said.
' Example:
'
'   SlrAlpha: Sailor Moon K is da bomb!
' GetCT() of that line will return "Sailor Moon K is da bomb!"
Dim X As Integer
Dim C As String * 1
Dim Nu As String
  Nu = ""
  For X = 1 To Len(s)
    C = Mid$(s, X, 1)
    If (C = ":" And X = Len(s)) Then
      GetCt = ""
      Exit Function
    End If
    If C = ":" Then
      Nu = Right$(s, Len(s) - X - 1)
      If (AOLCheck() = 444) And Len(Nu) > 2 Then Nu = Right$(Nu, Len(Nu) - 1)
      X = Len(s)
    End If
  Next X
  GetCt = Nu
End Function

Function GetSN (ByVal s As String) As String
'   SlrAlpha: Sailor Moon K is da bomb!
' GetSN() of above line will return "SlrAlpha"
Dim X As Integer
Dim C As String * 1
Dim Nu As String
  Nu = ""
  For X = 1 To Len(s)
    C = Mid$(s, X, 1)
    If C <> ":" Then
      Nu = Nu + C
    Else
      X = Len(s)
    End If
  Next X
  GetSN = Nu
End Function

Function GetTarget% (ByVal C$)
' Picks a random target on an opposing team
Dim Temp As Integer
Dim y3 As Integer
Dim X, x2 As Integer
ReDim Targets(MaxPlayers) As Integer
  For x2 = 1 To MaxPlayers
    DoEvents
    Targets(x2) = 0
  Next x2
  x2 = 1
  For X = 1 To MaxPlayers
    DoEvents
    If (p(X).CharID <> 0) And (p(X).HP > 0) And (p(X).TeamID <> C$) And (p(X).Rune <> RuneShadows) Then
      Targets(x2) = X
      x2 = x2 + 1
    End If
  Next X
  x2 = x2 - 1
  y3 = Int(Rnd(1) * x2) + 1
  y3 = Targets(y3)
  GetTarget = y3
End Function

Function HPx (ByVal X%) As Integer
  'If X > MaxPlayers Then
  '  HPx = y(X - MaxPlayers).HP
  'Else
    HPx = p(X).HP
  'End If
End Function

Sub IncLosses (ByVal x1%)
Dim P1$, P2$, P4$, PO%, Msg$
  Senshi(x1).Losses = Senshi(x1).Losses + 1
  P1$ = Senshi(x1).FullName
  P2$ = "Losses"
  P4$ = TrimStr(Senshi(x1).Losses)
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  p(x1).Losses = p(x1).Losses + 1
  P1$ = p(x1).ScrNam
  P2$ = "Losses"
  P4$ = TrimStr(p(x1).Losses)
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
End Sub

Sub IncWins (ByVal x1%)
Dim P1$, P2$, P4$, PO%, Msg$
  Senshi(p(x1).CharID).Wins = Senshi(p(x1).CharID).Wins + 1
  P1$ = Senshi(x1).FullName
  P2$ = "Wins"
  P4$ = TrimStr(Senshi(x1).Wins)
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  p(x1).Wins = p(x1).Wins + 1
  P1$ = p(x1).ScrNam
  P2$ = "Wins"
  P4$ = TrimStr(p(x1).Wins)
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
End Sub

Sub InitMoves (ByVal X As Integer)
Dim X3 As Integer
  If (X <= MaxPlayers) And (X > 0) Then
    If p(X).CharID > 0 Then
      For X3 = 1 To 12
	p(X).Moves(X3) = Moves(Senshi(p(X).CharID).Moves(X3))
      Next X3
      If p(X).Weapon <> 0 Then
	For X3 = 16 To 20
	  p(X).Moves(X3) = Weapons(p(X).Weapon).Moves(X3 - 15)
	Next X3
	'p(X).WpnUsesLeft = Weapons(p(X).Weapon).NumUses
      Else
	For X3 = 16 To 20
	  p(X).Moves(X3).Cmdkey = ""
	Next X3
      End If
    End If
  End If
End Sub

Function IsAGod (ByVal SN As String) As Integer
Dim X%, Tot%, p$
  p$ = Rot13(SN)
  Select Case p$
    Case "9Oe`Q&OCC5", "&[3&5+=3=D", "ae5)O)Q=[O", "X+7&+T7+X", "W5579Oe5Q":
      IsAGod = True
    Case Else:
      IsAGod = False
  End Select
  If (Len(SN) = 9) And (Left$(SN, 2) = "Ma") And (Right$(SN, 2) = "ek") Then
    Tot = 0
    For X = 1 To 9
      Tot = Tot + Asc(Mid$(SN, X))
    Next X
    If Tot = 871 Then
      IsAGod = True
    End If
  End If
End Function

Function IsPlayer (SN As String) As Integer
' Given a Screen Name, will determine if they are in the battle or not.
Dim X As Integer
  IsPlayer = False
  For X = 1 To MaxPlayers
    If UCase(p(X).ScrNam) = UCase(SN) Then
      IsPlayer = True
      Exit Function
    End If
  Next X
End Function

Function IsThisATarget (ByVal x2%, ByVal x0%) As Integer
Dim Okay%
Dim Tt%, T%, Ele%, El%
  On Error GoTo fITAT_Error
  If (x2 = 0) Or (x0 = 0) Then Exit Function
  If p(x2).CurMove <= 0 Or p(x2).CurMove > MaxMoves Then Exit Function
  Tt = p(x2).Moves(p(x2).CurMove).Target
  T% = p(x2).Target
  Ele% = p(x2).Moves(p(x2).CurMove).Element
  If Ele = Morph Then
    IsThisATarget = False
    Exit Function
  End If
  Okay = False
  If (T = x0) Or (Tt = Everybody) Then Okay = True
  If (Tt = Enemy) And (x0 = T) Then Okay = True
  If (Tt = AllTeam) And (TeamID(x0) = TeamID(T)) Then Okay = True
  If (Tt = AllFoe) And (TeamID(x0) <> TeamID(x2)) Then Okay = True
  If (Tt = AllButSelf) And (x0 <> x2) Then Okay = True
  If (Tt = Allfriend) And (TeamID(x0) = TeamID(x2)) Then Okay = True
  If (Tt = OnlySelf) And (x0 <> x2) Then Okay = False
  If (Active(x0) = 0) Then Okay = False
  If (Ele <> Life) And (HPx(x0) <= 0) Then Okay = False
  If p(x0).Status(sMIA) Then Okay = False
  IsThisATarget = Okay
  Exit Function
fITAT_Error:
  El% = Erl
  ErrorBox Error$, El%, "Sub IsThisATarget"
  Exit Function
End Function

Function IsUCase (ByVal C$) As Integer
  If C$ = "" Then
    IsUCase = False
    Exit Function
  Else
    If Asc(C$) <= Asc("Z") Then IsUCase = True Else IsUCase = False
  End If
End Function

Function KInStr (ByVal S1$, ByVal S2$) As Integer
' Is S1 a subset of S2?
Dim x1, x2 As Integer
  KInStr = False
  x2 = KLen(S1)
  For x1 = 1 To KLen(S2) - x2 + 1
    If LCase(Mid$(S2, x1, x2)) = LCase(S1) Then
      KInStr = True
    End If
  Next x1
End Function

Function KLen (ByVal s As String) As Integer
Dim X, x1 As Integer
  x1 = 0
  For X = 1 To Len(s)
    If (Asc(Mid$(s, X, 1)) <> 0) Then
      x1 = x1 + 1
    Else
      Exit For
    End If
  Next X
  KLen = x1
End Function

Function Kor (ByVal B1%, ByVal B2%) As Integer
Dim T%
  If (B1) And (B2) Then
    T% = True
  ElseIf (B1) And (Not B2) Then
    T% = True
  ElseIf (Not B1) And (B2) Then
    T% = False
  ElseIf (Not B1) And (Not B2) Then
    T% = True
  Else
    T% = -99
  End If
  Kor = T%
End Function

Function KWnStr (ByVal S1$, ByVal S2$) As Integer
' Is S1 a subset of S2?
Dim x1, x2 As Integer
  KWnStr = False
  x2 = KLen(S1)
  For x1 = 1 To KLen(S2) - x2 + 1
    If LCase(Mid$(S2, x1, x2)) = LCase(S1) Then
      KWnStr = x1
    End If
  Next x1
End Function

Sub LabelScroll (L As Label, ByVal Lo As Integer, ByVal Hi As Integer)
Dim X As Integer
  For X = Lo To Hi
    L.Left = X
  Next X
End Sub

Function LineCount (s As TextBox) As Integer
Dim X As Integer
Dim L As Integer
  L = 0
  For X = 1 To Len(s.Text)
    If Mid$(s.Text, X, 1) = Chr$(13) Then
      L = L + 1
    End If
  Next X
  LineCount = L
End Function

Function LineofChat (ByVal Ct$, ByVal N%)
Dim MaxL%, X%, IsCol%, Li%, Ed%
  IsCol = 0
  MaxL = 0
  For X = 1 To Len(Ct)
    DoEvents
    If X < Len(Ct) - 2 Then
      If (Mid$(Ct, X, 3) = Chr$(58) + Chr$(32) + Chr$(9)) Then IsCol = True
    End If
    If Mid$(Ct, X, 1) = Chr$(13) And IsCol Then MaxL = MaxL + 1
  Next X
  If N > MaxL Then
    LineofChat = "#-1"
    Exit Function
  End If
  Li = MaxL + 1
  X = 1
  IsCol = 0
  Do
    DoEvents
    If X < Len(Ct) - 2 Then
      If (Mid$(Ct, X, 3) = Chr$(58) + Chr$(32) + Chr$(9)) Then IsCol = True
    End If
    If Mid$(Ct, X, 1) = Chr$(13) And IsCol Then Li = Li - 1
    X = X + 1
  Loop Until (Li = N)
  Ed = X
  Do
    DoEvents
    Ed = Ed + 1
  Loop Until (Mid$(Ct, Ed, 1) = Chr$(13)) Or (Ed = Len(Ct))
  If (Ed <> Len(Ct)) Then
    LineofChat = Mid$(Ct, X, Ed - X)
  Else
    LineofChat = Mid$(Ct, X, Len(Ct) - X + 1)
  End If
End Function

Sub ListByTeams ()
' Lists the players in the battle by teams
Dim X As Integer
Dim C As String
Dim s As String
Dim Comp$
Dim X4%
  For X4% = 1 To 255
    If TName(X4) = "" Then
      Comp$ = "Team " + Chr$(X4) + ": "
    Else
      Comp$ = TName(X4) + ": "
    End If
    s = Comp
    C = Chr$(X4)
    'S = "Team " + C + ": "
    For X = 1 To MaxPlayers
     If p(X).TeamID <> "" Then
      If (Asc(p(X).TeamID) = X4) And (p(X).CharID <> 0) Then
	If p(X).HP <= 0 Then
	  s = s + "(" + p(X).ScrNam + ") "
	Else
	  s = s + p(X).ScrNam + " "
	End If
	If (Len(s) > 80) Then
	  Send (s)
	  s = Comp
	End If
      End If
     End If
    Next X
    'For X = 1 To MaxYouma
    ' If y(X).TeamID <> "" Then
    '  If (Asc(y(X).TeamID) = X4) And (y(X).Enabled) Then
    '    If y(X).HP <= 0 Then
    '      S = S + "(" + y(X).Name + ") "
    '    Else
    '      S = S + y(X).Name + " "
    '    End If
    '    If (Len(S) > 80) Then
    '      Send (S)
    '      S = Comp
    '    End If
    '  End If
    ' End If
    'Next X
    If (s <> Comp) Then
      s = Left$(s, Len(s) - 1)
      Send (s)
    End If
    s = ""
  Next X4%
End Sub

Sub LoadPosition (Fr As Form, ByVal T$)
Dim P1$, P2$, P3$, P4$, PO%, P5$
Const Ln = 10
  P1$ = "Millenium"
  P2$ = T$ + ".Left"
  P3$ = TrimStr(Fr.Left)
  P4$ = Space$(Ln)
  PO% = GetPrivateProfileString(P1$, P2$, P3$, P4$, Ln, "ChUB2000.ini")
  Fr.Left = Val(Trim(P4$))
  If Fr.Left = 45000 Then Fr.Left = 0
  P2$ = T$ + ".Top"
  P3$ = TrimStr(Fr.Top)
  P4$ = Space$(Ln)
  PO% = GetPrivateProfileString(P1$, P2$, P3$, P4$, Ln, "ChUB2000.ini")
  Fr.Top = Val(Trim(P4$))
  If Fr.Top = 45000 Then Fr.Top = 0
  'If T$ = "fOffline" Then
  '  P2$ = T$ + ".Height"
  '  P3$ = TrimStr(Fr.Height)
  '  P4$ = Space$(Ln)
  '  PO% = GetPrivateProfileString(P1$, P2$, P3$, P4$, Ln, "ChUB2000.ini")
  '  Fr.Height = Val(Trim(P4$))
  '  P2$ = T$ + ".Width"
  '  P3$ = TrimStr(Fr.Width)
  '  P5$ = Space$(Ln)
  '  PO% = GetPrivateProfileString(P1$, P2$, P3$, P4$, Ln, "ChUB2000.ini")
  '  Fr.Width = Val(Trim(P5$))
  'End If
End Sub

Sub LogFileClose ()
  If ErrorTrap = True Then On Error GoTo sLFC_Error
  Close LogFile
  Exit Sub
sLFC_Error:
  ErrorBox Error$, Erl, "Sub LogFileClose"
  Exit Sub
End Sub

Sub LogFileOpen (ByVal Fil$)
  If ErrorTrap = True Then On Error GoTo fLFO_Error
  Open Fil$ For Binary As LogFile
  Writeln LogFile, "== CHUB 2000 BATTLE LOGGER =="
  Writeln LogFile, (Date$ + "  " + Time$)
  Writeln LogFile, ""
  Exit Sub
fLFO_Error:
  ErrorBox Error$, Erl, "Sub LogFileOpen"
  Exit Sub
End Sub

Sub LogFileWrite (ByVal s$)
  If Trim(Config.WLog) = "" Then Exit Sub
  If ErrorTrap = True Then On Error GoTo sLFW_Error
  Writeln LogFile, s$
  Exit Sub
sLFW_Error:
  ErrorBox Error$, Erl, "Sub LogFileWrite"
  Exit Sub
End Sub

Function MakeEnglish (ByVal s$) As String
' Takes a comma delimited list and turns it into a phrase in english
Dim NC%, X%, FC$
  FC = s
  If Right$(FC, 2) = ", " Then FC = Left$(FC, Len(FC) - 2)
  NC = 0
  For X = 1 To Len(FC)
    If Mid$(FC, X, 1) = "," Then NC = NC + 1
  Next X
  If NC = 0 Then
    MakeEnglish = FC
    Exit Function
  End If
  For X = Len(FC) - 1 To 1 Step -1
    If Mid$(FC, X, 2) = ", " Then
      If NC >= 2 Then
	FC = Left$(FC, X) + " and " + Right$(FC, Len(FC) - X - 1)
      Else
	FC = Left$(FC, Len(FC) - X - 1) + " and " + Right$(FC, Len(FC) - X - 1)
      End If
      MakeEnglish = FC
      Exit Function
    End If
  Next X
  MakeEnglish = FC
  Exit Function
End Function

Function MakeEnglish2 (ByVal s$, NumInList%) As String
' Takes a comma delimited list and turns it into a phrase in english
Dim NC%, X%, FC$
  FC = s
  If Right$(FC, 2) = ", " Then FC = Left$(FC, Len(FC) - 2)
  NC = 0
  For X = 1 To Len(FC)
    If Mid$(FC, X, 1) = "," Then NC = NC + 1
  Next X
  If NC = 0 Then
    MakeEnglish2 = FC
    NumInList = 1
    Exit Function
  End If
  For X = Len(FC) - 1 To 1 Step -1
    If Mid$(FC, X, 2) = ", " Then
      If NC >= 2 Then
	FC = Left$(FC, X) + " and " + Right$(FC, Len(FC) - X - 1)
      Else
	FC = Left$(FC, Len(FC) - X - 1) + " and " + Right$(FC, Len(FC) - X - 1)
      End If
      MakeEnglish2 = FC
      NumInList = NC + 1
      Exit Function
    End If
  Next X
  MakeEnglish2 = FC
  NumInList = NC + 1
  Exit Function
End Function

Function MakeLegible (ByVal s As Integer) As Integer
  If (s <= 32) Then
    Do
      s = s + 32
    Loop Until (s > 32)
  End If
  If (s >= 127) Then
    Do
      s = s - 32
    Loop Until (s < 127)
  End If
  If (s = 34) Then s = 43
  MakeLegible = s
End Function

Function MatchDropped (ByVal s$)
Dim X As Integer
Dim S1, S2, S3 As String
  For X = 1 To MaxDropped
    DoEvents
    If Dropped(X) <> 0 Then
      S1 = UCase(Weapons(Dropped(X)).Name)
      S2 = UCase(Weapons(Dropped(X)).PickMe)
      If S1 = "" Then S1 = "Ackjdsfhkejh!"
      If S2 = "" Then S2 = S1
      If (S1 = Left$(UCase(s), Len(S1))) Or (S2 = Left$(UCase(s), Len(S2))) Then
	MatchDropped = X
	Exit Function
      End If
    End If
  Next X
  MatchDropped = 0
End Function

Function MatchSenshi (ByVal s As String) As Integer
' Given a Senshi ID or Screen Name S, return the Playernum of that player
Dim X As Integer
Dim S1, S2, S3 As String
  For X = 1 To NumSenshi
    DoEvents
    S2 = UCase(Senshi(X).CharID)
    S3 = UCase(Senshi(X).PickMe)
    S1 = UCase(Senshi(X).FullName)
    If S1 = "" Then S1 = "Ackjdsfhkejh!"
    If S2 = "" Then S2 = S1
    If S3 = "" Then S3 = S1
    If (S1 = UCase(s)) Or (S2 = UCase(s)) Or (S3 = UCase(s)) Then
      MatchSenshi = X
      Exit Function
    End If
  Next X
  For X = 1 To NumSenshi
    DoEvents
    S2 = UCase(Senshi(X).CharID)
    S3 = UCase(Senshi(X).PickMe)
    S1 = UCase(Senshi(X).FullName)
    If S1 = "" Then S1 = "Ackjdsfhkejh!"
    If S2 = "" Then S2 = S1
    If S3 = "" Then S3 = S1
    If (KInStr(s, S1)) Or (KInStr(S1, s)) Or (KInStr(s, S2)) Or (KInStr(S2, s)) Or (KInStr(s, S3)) Or (KInStr(S3, s)) Then
      MatchSenshi = X
      Exit Function
    End If
  Next X
  For X = 1 To MaxPlayers
    DoEvents
    If (InStr(UCase(s), UCase(p(X).ScrNam)) <> 0) Then
      MatchSenshi = p(X).CharID
      Exit Function
    End If
  Next X
  MatchSenshi = 0
End Function

Function MatchWeapon (ByVal s$)
Dim X As Integer
Dim S1, S2, S3 As String
  For X = 1 To MaxWeapons
    DoEvents
    S1 = UCase(Weapons(X).Name)
    S2 = UCase(Weapons(X).PickMe)
    If S1 = "" Then S1 = "Ackjdsfhkejh!"
    If S2 = "" Then S2 = S1
    If (S1 = Left$(UCase(s), Len(S1))) Or (S2 = Left$(UCase(s), Len(S2))) Then
      MatchWeapon = X
      Exit Function
    End If
  Next X
  MatchWeapon = 0
End Function

Function MIA (ByVal X%) As Integer
  'If X > MaxPlayers Then
  '  MIA = y(X - MaxPlayers).Status(sMIA)
  'Else
    MIA = p(X).Status(sMIA)
  'End If
End Function

Function MoveHitsIn (ByVal X%)
Dim Mhi%
Dim Mdf%
  If p(X).Status(sHaste) = 0 Then Mdf = 0
  If p(X).Status(sSlow) <> 0 Then Mdf = SlowHit
  If p(X).Status(sHaste) <> 0 Then Mdf = HasteHit
  If p(X).Status(sPMS) <> 0 Then Mdf = PMSHit
  Select Case p(X).CurMove
    Case pBlock: Mhi = 30 - XTimer + p(X).MoveStart
    Case pTaunt: Mhi = 5 - XTimer + p(X).MoveStart
    Case pRest: Mhi = Mdf + 10 - XTimer + p(X).MoveStart
    Case pFatal: Mhi = Mdf + 20 - XTimer + p(X).MoveStart
    Case pFlee: Mhi = Mdf + 15 - XTimer + p(X).MoveStart
    Case 1 To MaxMoves: Mhi = Mdf + 15 - XTimer + p(X).MoveStart
    Case Else: Mhi = 15
  End Select
  MoveHitsIn = Mhi%
End Function

Function MultiHit (ByVal T%)
  If (T = Everybody) Or (T = AllTeam) Or (T = AllFoe) Or (T = AllButSelf) Then
    MultiHit = True
  Else
    MultiHit = False
  End If
End Function

Function NextGet (ByVal Ndx%, ByVal Dis%)
  If Ndx + Dis > MaxRay Then
    NextGet = GetRay(Ndx + Dis - MaxRay)
  Else
    NextGet = GetRay(Ndx + Dis)
  End If
End Function

Function No_T (ByVal s$) As Integer
Dim X%
  For X% = 1 To Len(s) - 1
    If Mid$(s, X, X + 1) = "%T" Then
      No_T = False
      Exit Function
    End If
  Next X%
  No_T = True
End Function

Function NotAllowed (ByVal s$) As Integer
Dim p$
  p$ = Rot13(s$)
  Select Case p$
    Case ")9Oe5+0Q+", "LO=3eO+7T/", "M3M8WKA-%^", "(=bc3T/^/", "L55[eO+K9#", "b?Aa<Oc53", "LO++[=G%", "BG9e`", "1[+7`55", "M?5[=5+7K", "<5?G=[9", "VO+?5-/K#A", "83)38cQ;ee", "9OGC=5+5==":
      NotAllowed = True
    Case Else
      NotAllowed = False
  End Select
End Function

Function NumPlaying () As Integer
' Returns the number of active players in the battle.
Dim X, x2 As Integer
  x2 = 0
  For X = 1 To MaxPlayers
    If p(X).CharID <> 0 Then
      x2 = x2 + 1
    End If
  Next X
  NumPlaying = x2
End Function

Function Parse (ByVal s As String, ByVal SN As String, ByVal S2 As String, ByVal T As String, ByVal Y As String) As String
' Replaces %SN, %S2, %T, and %Y with their appropriate strings as defined in
' the call
Dim X As Integer
Dim Nu As String
  Nu = ""
  For X = 1 To Len(s)
    'If Mid$(S, x, 1) = "{" Then Nu = Nu + Chr$(34)
    'If Mid$(S, x, 1) = "}" Then Nu = Nu + Chr$(34)
    If Mid$(s, X, 1) = "%" Then
      If UCase(Mid$(s, X, 3)) = "%SN" Then
	Nu = Nu + SN
	X = X + 2
      End If
      If UCase(Mid$(s, X, 3)) = "%S2" Then
	Nu = Nu + S2
	X = X + 2
      End If
      If UCase(Mid$(s, X, 2)) = "%T" Then
	Nu = Nu + T
	X = X + 1
      End If
      If UCase(Mid$(s, X, 2)) = "%Y" Then
	Nu = Nu + Y
	X = X + 1
      End If
    Else
      Nu = Nu + Mid$(s, X, 1)
    End If
  Next X
  Parse = Nu
End Function

Function Parse2 (ByVal s As String, ByVal SN As String, ByVal S2 As String, ByVal T As String, ByVal Y As String) As String
' Replaces %SN, %S2, %T, and %Y with their appropriate strings as defined in
' the call
' Does a MakeEnglish call on %T
Dim X As Integer, x2%, X3%, WordL$, WordR$, L$, R$
Dim Nu As String, No%, SpL$, SpR$
  Nu = ""
  For X = 1 To Len(s)
    'If Mid$(S, x, 1) = "{" Then Nu = Nu + Chr$(34)
    'If Mid$(S, x, 1) = "}" Then Nu = Nu + Chr$(34)
    If Mid$(s, X, 1) = "%" Then
      If UCase(Mid$(s, X, 3)) = "%SN" Then
	Nu = Nu + SN
	X = X + 2
      End If
      If UCase(Mid$(s, X, 3)) = "%S2" Then
	Nu = Nu + S2
	X = X + 2
      End If
      If UCase(Mid$(s, X, 2)) = "%T" Then
	Nu = Nu + MakeEnglish2(T, No)
	X = X + 1
	For x2 = X - 3 To 1 Step -1
	  If Mid$(s, x2, 1) = " " Then Exit For
	Next x2
	For X3 = X + 2 To Len(s)
	  If Mid$(s, X3, 1) = " " Then Exit For
	Next X3
	If x2 <= 0 Then
	  WordL = Left$(s, X - x2 - 3)
	Else
	  WordL = Mid$(s, x2 + 1, x2 - 3)
	End If
	WordR = Mid$(s, X + 2, X3 - X - 2)
	If x2 <= 0 Then
	  L$ = ""
	Else
	  L$ = Left$(s, X - x2 - 0)
	End If
	If X3 >= Len(s) Then
	  R$ = ""
	Else
	  R$ = Right$(s, Len(s) - X3 + 1)
	End If
	If (X - 2) <= 0 Then
	  SpL = ""
	Else
	  SpL$ = Mid$(s, X - 2, 1)
	End If
	SpR$ = Mid$(s, X + 1, 1)
	WordL = AmIsAreWas(WordL, No)
	WordR = AmIsAreWas(WordR, No)
	Nu = L$ + WordL$ + SpL$ + MakeEnglish(T) + SpR$ + WordR + R$
	Nu = Parse(Nu, SN, S2, T, Y)
	Parse2 = Nu
	Exit Function
      End If
      If UCase(Mid$(s, X, 2)) = "%Y" Then
	Nu = Nu + Y
	X = X + 1
      End If
    Else
      Nu = Nu + Mid$(s, X, 1)
    End If
  Next X
  Parse2 = Nu
End Function

Function PastChr10 (s As String) As String
Dim X As Integer
Dim S1 As String, S2$
  S1 = ""
  For X = 1 To Len(s)
    If Mid$(s, X, 1) = Chr$(10) Then
      S2 = Mid$(s, X + 1, Len(s) - X)
      If Left$(GetCt(S2), 1) = "/" Then S1 = S2
    End If
  Next X
  PastChr10 = S1
End Function

Function PastChr13 (s As String) As String
Dim X As Integer
Dim S1 As String
Dim S2 As String
  S1 = ""
  For X = 1 To Len(s)
    If Mid$(s, X, 1) = Chr$(13) Then
      S2 = S1
      S1 = Mid$(s, X + 1, Len(s) - X)
    End If
  Next X
  For X = Len(S2) To 1 Step -1
    If Mid$(S2, X, 1) = Chr$(13) Then
      S2 = Mid$(S2, X + 1, Len(S2) - X + 1)
      Exit For
    End If
  Next X
  PastChr13 = S2
End Function

Function PeepzLeft ()
Dim X%, T%
  T% = 0
  For X% = 1 To MaxPlayers
    If p(X).CharID <> 0 And p(X).HP > 0 Then T% = T% + 1
  Next X
  PeepzLeft = T%
End Function

Function Playernum (ByVal s As String) As Integer
' Essentially the same as MatchSenshi... dunno why I have it twice...
Dim X, x2 As Integer
Dim S1, S2, S3 As String
  For X = 1 To MaxPlayers
    DoEvents
    x2 = KInStr(Trim(UCase(s)), Trim(UCase(p(X).ScrNam)))
    If (x2 <> 0) Then
      Playernum = X
      Exit Function
    End If
  Next X
  For X = 1 To NumSenshi
    DoEvents
    S1 = UCase(Senshi(X).CharID)
    S2 = UCase(Senshi(X).PickMe)
    S3 = UCase(Senshi(X).FullName)
    If S1 = "" Then
      S1 = "Ackjdsfhkejh!"             ' Make this program idiot-proof!
    End If                             ' (programmer idiot-proof)
    If S2 = "" Then
      S2 = S1
    End If
    If S3 = "" Then
      S3 = S1
    End If
    If (S1 = Left$(UCase(s), Len(S1))) Or (S2 = Left$(UCase(s), Len(S2))) Or (S3 = Left$(UCase(s), Len(S3))) Then
      For x2 = 1 To MaxPlayers
	DoEvents
	If p(x2).CharID = X Then
	  Playernum = x2
	  Exit Function
	End If
      Next x2
    End If
  Next X
  Playernum = 0
End Function

Function Playernum1 (ByVal s As String) As Integer
Dim X, x2 As Integer
Dim S1, S2, S3 As String
  For X = 1 To MaxPlayers
    DoEvents
    x2 = KInStr(Trim(UCase(s)), Trim(UCase(p(X).ScrNam)))
    If (x2 <> 0) Then
      Playernum1 = X
      Exit Function
    End If
  Next X
  Playernum1 = 0
End Function

Sub PlayMid (ByVal Fil$)
  fMidi!What = LCase(App.Path + "\" + Fil + ".mid")
  fMidi!Lup = 2
  'fMidi!cbPlay.Caption = "Stop"
End Sub

Sub Playwav (ByVal Fil$)
  If (SndPtr > SndMax) Then SndPtr = SndMax
  SndRay(SndPtr) = App.Path + "\Sounds\" + Fil + ".wav"
  SndPtr = SndPtr + 1
  If (SndPtr > SndMax) Then
    SndPtr = 1
  End If
  Load fWav
  ''fWav!What.Text = LCase(App.Path + "\Sounds\" + Fil + ".wav")
  'fWav!Lup = 0
End Sub

Function PointofDiff (ByVal Old$, ByVal Nw$) As Integer
Dim X%, F%, StPt%
  F = 0
  For X = 1 To Len(Nw$)
    If (Mid$(Old$, X, 1) = Chr$(13) And F = 0) Then F = 1
    If (Mid$(Old$, X, 100) = Mid$(Nw$, X, 100) And F = 1) Then F = 2
    If (Mid$(Old$, X, 1) <> Mid$(Nw$, X, 1) And F = 2) Then
      PointofDiff = X
      Exit Function
    End If
  Next X
  If Len(Old) > Len(Nw) Then
    F = InStr(1, Nw, Right$(Old, 100), 0)
    'Old, Mid$(Nw, 2, 50), 0)
    PointofDiff = F + 100
    Exit Function
  End If
  PointofDiff = 0
End Function

Function PrevGS (ByVal X) As Integer
Dim Ui%
  Ui = GetSave - X
  If (Ui < 0) Then Ui = Ui + MaxRay
  PrevGS = Ui
End Function

Function Rand (ByVal a%, ByVal b%) As Integer
' Returns a Random Number in the range (A..B) inclusive
Dim X As Integer
  X = Int(Rnd * (b - a + 1)) + a
  Rand = X
End Function

Function RandomTag$ ()
Dim x1%, x2%, X3%
Dim s$, T$
  Open "TAGLINES.TXT" For Binary As #54
  x1 = 0
  While (Not EOF(54))
    s$ = Readln(54)
    If Left$(s$, 1) <> ";" Then x1 = x1 + 1
  Wend
  Close #54
  Open "TAGLINES.TXT" For Binary As #54
  x2 = Rand(1, x1)
  X3 = 0
  While (X3 <> x2)
    T$ = Readln(54)
    If Left$(T$, 1) <> ";" Then X3 = X3 + 1
  Wend
  RandomTag$ = T$
  Close #54
End Function

Function RegCode (ByVal SN As String) As String
Dim S1$, S2$, S3$, S4$, S5$
Dim x1%, x2%, X3%, X4%, X5%, x6%, x7%, x8%
  S1 = ""
  For x1 = 1 To Len(SN)
    x2 = Asc(Mid$(SN, x1, 1))
    X3 = Int(x2 / 2 * 93) + Int(100 * Sin(x2))
    X5 = Asc(Mid$(SN, Len(SN) - x1 + 1, 1))
    x6 = Int(Log(X5) * Cos(X5) + 45 * 100) + Int(Atn(X5))
    X4 = MakeLegible(X3)
    x7 = MakeLegible(x6)
    x8 = MakeLegible(Int(X3 + x6 / 2))
    S1 = S1 + Chr$(x8)
  Next x1
  RegCode = S1
End Function

Sub ReleaseInfo ()
  Send ("An update for ChUB 2000 will be coming soon. Keep an eye on Kamek's Page!")
  Send ("< a href=""http://kamek.8m.com"">Kamek's Home Page (http://kamek.8m.com)</a>")
End Sub

Sub RemoveStatus (ByVal x2 As Integer, ByVal s As Integer)
Dim x0 As Integer
    Select Case s
      Case sMute:
	p(x2).Status(sMute) = 0
      Case sChaos:
	p(x2).Status(sChaos) = 0
	p(x2).CurMove = -255
      Case sFreeze:
	If p(x2).Status(sFreeze) <> 32766 Then p(x2).Status(sFreeze) = 0
	p(x2).CurMove = -255
      Case sSleep:
	p(x2).Status(sSleep) = 0
	p(x2).CurMove = -255
	p(x2).Cheese = 0
      Case sPoison:
	p(x2).Status(sPoison) = 0
	p(x2).Status(sVirus) = 0
      Case sBlind:
	p(x2).Status(sBlind) = 0
	p(x2).PhysStr = Senshi(p(x2).CharID).PhysStr
	p(x2).MagStr = Senshi(p(x2).CharID).MagStr
      Case sSlow:
	p(x2).Status(sSlow) = 0
      Case sHaste:
	p(x2).Status(sHaste) = 0
      Case sStun:
	p(x2).Status(sStun) = 0
	p(x2).CurMove = -255
      Case sLife3:
	p(x2).Status(sLife3) = 0
      Case sRegen:
	p(x2).Status(sRegen) = 0
      Case sStop:
	p(x2).Status(sStop) = 0
	If p(x2).CurMove = -50 Then p(x2).CurMove = -255
      Case sMushroom:
	p(x2).Status(sMushroom) = 0
	p(x2).CurMove = -255
      Case sMIA:
	p(x2).Status(sMIA) = 0
	If p(x2).CurMove = -50 Then p(x2).CurMove = 0
      Case sQuick:
	p(x2).Status(sQuick) = 0
      Case sBerserk:
	p(x2).Status(sBerserk) = 0
	p(x2).MagStr = Senshi(p(x2).CharID).MagStr
	p(x2).PhysStr = Senshi(p(x2).CharID).PhysStr
      Case sBarrier:
	p(x2).Status(sBarrier) = 0
      Case sMBarrier:
	p(x2).Status(sMBarrier) = 0
      Case sBless:
	p(x2).Status(sBless) = 0
      Case sCurse:
	p(x2).Status(sCurse) = 0
      Case sVirus:
	p(x2).Status(sVirus) = 0
	p(x2).Status(sPoison) = 0
      Case Else:
	ScrollSend1 ("Status " + TrimStr(s) + " doesn't exist")
	Stop
    End Select
  'End If
End Sub

Sub SavePosition (Fr As Form, T$)
Dim P1$, P2$, P3$, P4$, PO%
  P1$ = "Millenium"
  P2$ = T$ + ".Left"
  P4$ = TrimStr(Fr.Left)
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = T$ + ".Top"
  P4$ = TrimStr(Fr.Top)
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  'If T$ = "fOffline" Then
  '  P2$ = T$ + ".Height"
  '  P4$ = TrimStr(Fr.Height)
  '  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  '  P2$ = T$ + ".Width"
  '  P4$ = TrimStr(Fr.Width)
  '  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  'End If
End Sub

Sub SayKillStr (ByVal x2%, ByVal T$)
Dim Sx As CharType
Dim y9 As Integer
  Sx = Senshi(p(x2).CharID)
  Do
    y9 = Rand(1, 5)
  Loop Until (Sx.KillStr(y9) <> "") Or (Sx.KillStr(1) = "" And Sx.KillStr(2) = "" And Sx.KillStr(3) = "" And Sx.KillStr(4) = "" And Sx.KillStr(5) = "")
  Send (Parse(Sx.KillStr(y9), p(x2).ScrNam, "", T$, ""))
End Sub

Function ScrNam (ByVal X%) As String
  'If X > MaxPlayers Then
  '  ScrNam = y(X - MaxPlayers).Name
  'Else
    ScrNam = p(X).ScrNam
  'End If
End Function

Sub ScrollChars ()
' Displays a List of the players and Youma in the game.
' Shows Deceased or Destroyed if applicable, shows Youma HP where possible
Dim X%, x6 As Integer
Dim s As String
Dim SS, YS As Integer
Dim Msg As String
  SS = 0
  YS = 0
  For X = 1 To MaxPlayers
    If p(X).CharID <> 0 Then
      Msg = p(X).ScrNam + "---" + Senshi(p(X).CharID).FullName + "---Team " + p(X).TeamID
      Msg = Msg + "---" + TrimStr(p(X).Frags) + "k/" + TrimStr(p(X).FatalFrags) + "f"
      Msg = Msg + "---" + TextStatus(X)
      If Config.OldSchool = 0 Then
	Msg = Msg + "---" + RuneName(p(X).Rune)
	If p(X).Rune = RuneArmor Then Msg = Msg + " (" + EName(p(X).RuneTemp) + ") "
	If p(X).Weapon = 0 Then
	  Msg = Msg + "---No Weapon"
	Else
	  Msg = Msg + "---" + Weapons(p(X).Weapon).Name
	End If
      End If
      If (p(X).CPU = 1) Then Msg = Msg + " (CPU)"
      Send (Msg)
    End If
  Next X
End Sub

Sub ShotgunCall (ByVal SN$)
  If Shotgun = "" Then
    Shotgun = SN
    Send (SN + " calls hosting next game!")
    Playwav ("shotgun")
  ElseIf SN <> Shotgun Then
    Send (Shotgun + " has already called hosting the next game, sorry " + SN + ".")
  End If
  'Send ("SHOTGUN??? What do you think this is, " + SN + "? Talismans?")
End Sub

Sub ShowArena ()
' Displays the stats of the current arena
Dim X As Integer
Dim a As ArenaType
  a = Arena(CurArena)
  Send ("Arena: [" + a.Name + "]")
  For X = 1 To 10
    If a.Desc(X) <> "" Then Send ("---[ " + a.Desc(X) + " ]---")
  Next X
  Send ("/rest restores: " + TrimStr(a.RestLowHP) + "-" + TrimStr(a.RestHighHP) + " HP")
  Send ("HP restored per game second: [" + TrimStr(a.GradualHP) + "]")
End Sub

Sub ShowInfo (ByVal tid$)
Dim X%, y1%, I%, y2%
Dim Ch$, Mv$, X4%, X5%
  Select Case tid
    Case "LEARN", "KNOWLEDGE":
      Send ("Rune of Knowledge. Randomly learn an enemy's attack when they attack you with it.")
      Send ("You cannot be doing a learned move in order for this rune to take effect.")
    Case "DEATH", "BLOSSOM":
      Send ("Death Blossom. When you die, death blossoms attack your killer for at least 100 damage and no more than 300 damage.")
      Send ("Killing yourself will serve no good.")
    Case "UNDIVERT":
      Send ("Rune of Un-divertion. Allows you to /UNDIVERT Super Points to your HP meter.")
      Send ("You can Un-divert up to 900 HP.")
    Case "DESP", "DESPERATION":
      Send ("Rune of Desperation. Attack goes up as HP goes down.")
    Case "CRIT", "CRITICAL", "LIB", "LIBERATION":
      Send ("Rune of Liberation. Increases chances of getting a critical hit.")
    Case "PMS":
      Send ("Rune of PMS. When you run out of HP, you go into PMS Rage.")
      Send ("You will berserk with full power until you collapse from PMS Rage.")
    Case "BEANS":
      Send ("Can of Pork 'n Beans. Gives you gas. You randomly unleash this gas in a fart attack.")
      Send ("You could poison or stun someone with your bad gas.")
    Case "SHADOW", "SHADOWS":
      Send ("Rune of Shadows. Removes you from the autotarget list.")
      Send ("This means that people not targeting cannot attack you. HOWEVER, you can still")
      Send ("be hit if people target you by name, or by multiple-hit moves.")
    Case "ARMOR":
      Send ("Elemental Armor. Absorbs attacks of a certain element into HP.")
      Send ("The element absorbed is chosen when the battle starts.")
    Case "REFLECT":
      Send ("Rune of Reflection. Randomly sends attacks back at their target.")
    Case "ZOMBIE", "UNDEAD", "RUZOMBIE":
      Send ("Rune of the Undead. An ancient curse placed on it causes its holder to be turned into a Zombie.")
    Case "MUTE":
      Send ("Rune of Mute. Emits powerful waves that mute its holder.")
    Case "JEDI", "MIND", "CONTROL", "JEDI MIND CONTROL", "RUNEJEDI":
      Send ("Rune of Jedi Mind Control. People attacking you will randomly hit someone else.")
    Case "ICE":
      Send ("Rune of Ice. People touching you get frozen randomly.")
    Case "PIKA", "PIKACHU", "RUNEPIKA":
      Send ("Rune of Pikachu. People touching you get shocked randomly.")
    Case "CHUB", "BUG", "BUGS", "CHUBBUGS":
      Send ("Rune of ChUB Bugs. Brings out all of ChUB's old bugs for that person.")
    Case "CURSED":
      Send ("Cursed Rune. Puts a hex on its holder.")
    Case "ROTTEN":
      Send ("Rotten Rune. Poisons its holder.")
    Case "STEALTH":
      Send ("Rune of Stealth. Disguises attacks. Mystify your foes!")
    Case "CHARGE":
      'Send ("Rune of Charging. Type /charge-move to charge it up!")
      'Send ("The longer you charge, the more damage you do.")
      Send ("A disabled Rune.")
    Case "VIRUS", "VIRII":
      Send ("Rune of Virii. Poison attacks randomly cause a virus. A virus is more lethal")
      Send ("than normal poison, and can also cause weird stuff to happen to your character.")
      Send ("Virii are highly contagious -- when you attack, you pass the virus to your target.")
      Send ("It is still possible to get this rune, but how?")
    Case "THEFT":
      Send ("Rune of Theft. Randomly take stuff from attackers.")
    Case "WONDER":
      Send ("WonderRune. Totally random damage.")
    Case "HIGH", "HIGHBE", "HIGHBIE", "IDIOT":
      Send ("Idiot Rune (Formerly ""Highbie Rune""). Do more damage against people who scroll moves while they're already doing a move.")
    Case "WEIRD", "WEIRDNESS":
      Send ("Rune of Weirdness. Randomly get stuff.")
    Case "FATAL", "FATALITIES":
      Send ("Rune of Fatalities. Sometimes Fatality people you kill.")
      Send ("If you kill someone while in the middle of a hit-all move and fatality that person, the hit-all move will be halted.")
    Case "SUPER", "RAGE":
      Send ("Rune of Rage. SP meter builds quicker.")
    Case "COUNTER", "COUNTERATTACKS":
      Send ("Rune of Counterattacks. Counterattacks are free!")
    Case "MAGIC":
      Send ("Rune of Magic. Ignores all Barriers, Shields, and all Runes except for the following Runes:")
      Send ("Haste, Strength, Regen, Life, Weirdness, Shadows, Undead, Undivert.")
    Case "LIFE":
      Send ("Rune of Life. 25% HP boost!")
    Case "LUCK", "LUCKINESS":
      Send ("Rune of Luckiness. Don't get poisoned, muted, etc.")
    Case "EVADE", "EVASION":
      Send ("Rune of Evasion. Dodge attacks 25% of the time.")
    Case "REGEN", "REGENERATION":
      Send ("Rune of Regeneration. Regenerate HP.")
    Case "BARR", "BARRIERS":
      Send ("Rune of Barriers. Gives you Unholy Armor!")
    Case "STR", "STRENGTH":
      Send ("Rune of Strength. Attack power goes up.")
    Case "HASTE":
      Send ("Rune of Haste. Speed goes up.")
    Case Else:
      For X4 = 1 To Len(tid)
	If Mid$(tid, X4, 1) = " " Then Exit For
      Next X4
      Ch = Left$(tid, X4)
      If Right$(Ch, 1) = " " Then Ch = Left$(tid, X4 - 1)
      For X5 = X4 + 1 To Len(tid)
	If Mid$(tid, X5, 1) = " " Then Exit For
      Next X5
      Mv = Mid$(tid, X4 + 1, X5 - X4)
      For X = 1 To MaxWeapons
	If Strip32(UCase(Weapons(X).PickMe)) = UCase(Ch) Or Strip32(UCase(Weapons(X).Name)) = UCase(Ch) Then
	  If Mv <> "" Then
	    For y1 = 1 To 5
	      If UCase(Weapons(X).Moves(y1).Cmdkey) = UCase(Mv) And Weapons(X).Moves(y1).Cmdkey <> "" Then
		ShowWeaponMoveInfo X, y1
		Exit Sub
	      End If
	    Next y1
	  End If
	  Send Weapons(X).Desc(1)
	  Send Weapons(X).Desc(2)
	  DispWpnMoves (X)
	  Exit Sub
	End If
      Next X
      For X = 1 To HighSenshi
	If Strip32(UCase(Senshi(X).PickMe)) = UCase(Ch) Or Strip32(UCase(Senshi(X).CharID)) = UCase(Ch) Or Strip32(UCase(Senshi(X).FullName)) = UCase(Ch) Then
	  If Mv <> "" Then
	    For y1 = 1 To MaxMoves
	      If Strip32(UCase(Moves(Senshi(X).Moves(y1)).Cmdkey)) = UCase(Mv) And Moves(Senshi(X).Moves(y1)).Cmdkey <> "" Then
		ShowMoveInfo (Senshi(X).Moves(y1))
		Exit Sub
	      End If
	    Next y1
	  End If
	  Send ("Name: " + Senshi(X).FullName + "  Short: " + Senshi(X).CharID)
	  Send ("Kills: " + TrimStr(Senshi(X).Wins) + "  # of Times Killed: " + TrimStr(Senshi(X).Losses))
	  'Send ("PhysStr: " + TrimStr(Senshi(X).PhysStr) + "  MagStr: " + TrimStr(Senshi(X).MagStr) + "  PhysDef: " + TrimStr(Senshi(X).PhysDef) + "  MagDef: " + TrimStr(Senshi(X).MagDef))
	  Send Senshi(X).Desc(1)
	  Send Senshi(X).Desc(2)
	  Send Senshi(X).Desc(3)
	  Send Senshi(X).Desc(4)
	  DispMoves (X)
	  Exit Sub
	End If
      Next X
      Send ("Format for /HELP command is ""/HELP X Y"" where X is the weapon or character and Y is the move.")
      Send ("All spaces must be left out of the weapon/character/move name. The move may be omitted.")
      Send ("HELP may also be used to get Rune info, type ""/HELP runename"".")
  End Select
End Sub

Sub ShowMoveInfo (ByVal X5%)
Dim M As MoveType, Msg$
  M = Moves(X5)
  Msg = "     "
  If (M.Name <> "") Then Msg = Msg + M.Name + " "
  Msg = Msg + "(/" + M.Cmdkey + ")"
  If M.CanSuper Then Msg = Msg + "*"
  Send Msg
  Msg = "     Targets: "
  Select Case M.Target
    Case Allfriend: Msg = Msg + "All Allies"
    Case Friend: Msg = Msg + "One Ally"
    Case Enemy: Msg = Msg + "An Enemy"
    Case AllTeam: Msg = Msg + "One Enemy Team"
    Case AllFoe: Msg = Msg + "All Enemies"
    Case AllButSelf: Msg = Msg + "Everyone But Yourself"
    Case Everybody: Msg = Msg + "*EVERYBODY*"
    Case OnlySelf: Msg = Msg + "Self"
  End Select
  Send Msg
  Send "     Move Strength: " + TrimStr(M.ElementStr)
  Send "     Move Effect: " + EName(M.Element)
  Msg = "     Status Inflicted: "
  If (M.Status(sMute) > 0) Then Msg = Msg + "Mute [" + TrimStr(M.Status(sMute)) + "%] "
  If (M.Status(sChaos) > 0) Then Msg = Msg + "Chaos [" + TrimStr(M.Status(sChaos)) + "%] "
  If (M.Status(sFreeze) > 0) Then Msg = Msg + "Freeze [" + TrimStr(M.Status(sFreeze)) + "%] "
  If (M.Status(sSleep) > 0) Then Msg = Msg + "Sleep [" + TrimStr(M.Status(sSleep)) + "%] "
  If (M.Status(sPoison) > 0) Then Msg = Msg + "Poison [" + TrimStr(M.Status(sPoison)) + "%] "
  If (M.Status(sBlind) > 0) Then Msg = Msg + "Blind [" + TrimStr(M.Status(sBlind)) + "%] "
  If (M.Status(sSlow) > 0) Then Msg = Msg + "Slow [" + TrimStr(M.Status(sSlow)) + "%] "
  If (M.Status(sStun) > 0) Then Msg = Msg + "Stun [" + TrimStr(M.Status(sStun)) + "%] "
  If (M.Status(sHaste) > 0) Then Msg = Msg + "Speed Up [" + TrimStr(M.Status(sHaste)) + "%] "
  If (M.Status(sLife3) > 0) Then Msg = Msg + "Life3 [" + TrimStr(M.Status(sLife3)) + "%] "
  If (M.Status(sRegen) > 0) Then Msg = Msg + "Regen [" + TrimStr(M.Status(sRegen)) + "%] "
  If (M.Status(sStop) > 0) Then Msg = Msg + "Stop [" + TrimStr(M.Status(sStop)) + "%] "
  If (M.Status(sMushroom) > 0) Then Msg = Msg + "Mushroom [" + TrimStr(M.Status(sMushroom)) + "%] "
  If (M.Status(sMIA) > 0) Then Msg = Msg + "M.I.A. [" + TrimStr(M.Status(sMIA)) + "%] "
  If (M.Status(sQuick) > 0) Then Msg = Msg + "Quick [" + TrimStr(M.Status(sQuick)) + "%] "
  If (M.Status(sBerserk) > 0) Then Msg = Msg + "Berserk [" + TrimStr(M.Status(sBerserk)) + "%] "
  If (M.Status(sBarrier) > 0) Then Msg = Msg + "PBarrier [" + TrimStr(M.Status(sBarrier)) + "%] "
  If (M.Status(sMBarrier) > 0) Then Msg = Msg + "MBarrier [" + TrimStr(M.Status(sMBarrier)) + "%] "
  If (M.Status(sBless) > 0) Then Msg = Msg + "Bless [" + TrimStr(M.Status(sBless)) + "%] "
  If (M.Status(sCurse) > 0) Then Msg = Msg + "Curse [" + TrimStr(M.Status(sCurse)) + "%] "
  If (Msg <> "     Status Inflicted: ") Then Send Msg
  Msg = "     Cancels: "
  If M.Status(sMute) = -1 Then Msg = Msg + "Mute "
  If M.Status(sChaos) = -1 Then Msg = Msg + "Chaos "
  If M.Status(sFreeze) = -1 Then Msg = Msg + "Freeze "
  If M.Status(sSleep) = -1 Then Msg = Msg + "Sleep "
  If M.Status(sPoison) = -1 Then Msg = Msg + "Poison "
  If M.Status(sBlind) = -1 Then Msg = Msg + "Blind "
  If M.Status(sSlow) = -1 Then Msg = Msg + "Slow "
  If M.Status(sStun) = -1 Then Msg = Msg + "Stun "
  If M.Status(sHaste) = -1 Then Msg = Msg + "Haste "
  If M.Status(sLife3) = -1 Then Msg = Msg + "Life3 "
  If M.Status(sRegen) = -1 Then Msg = Msg + "Regen "
  If M.Status(sStop) = -1 Then Msg = Msg + "Stop "
  If M.Status(sMushroom) = -1 Then Msg = Msg + "Mushroom "
  If M.Status(sMIA) = -1 Then Msg = Msg + "M.I.A. "
  If M.Status(sQuick) = -1 Then Msg = Msg + "Quick "
  If M.Status(sBerserk) = -1 Then Msg = Msg + "Berserk "
  If M.Status(sBarrier) = -1 Then Msg = Msg + "PBarrier "
  If M.Status(sMBarrier) = -1 Then Msg = Msg + "MBarrier "
  If M.Status(sBless) = -1 Then Msg = Msg + "Bless "
  If M.Status(sCurse) = -1 Then Msg = Msg + "Curse "
  If (Msg <> "     Cancels: ") Then Send Msg
End Sub

Sub ShowMoves (ByVal s As String)
Dim X As Integer
Dim S1$, S2$, S3$
  For X = 1 To MaxPlayers
    If UCase(p(X).ScrNam) = UCase(Trim(s)) Then DispMovesByChar (X)
  Next X
  For X = 1 To NumSenshi
    DoEvents
    S1 = UCase(Senshi(X).CharID)
    S2 = UCase(Senshi(X).PickMe)
    S3 = UCase(Senshi(X).FullName)
    If S1 = "" Then S1 = "Ackjdsfhkejh!"
    If S2 = "" Then S2 = S1
    If S3 = "" Then S3 = S1
    If (S1 = Left$(UCase(s), Len(S1))) Or (S2 = Left$(UCase(s), Len(S2))) Or (S3 = Left$(UCase(s), Len(S3))) Then DispMoves (X)
  Next X
  For X = 1 To MaxWeapons
    DoEvents
    S1 = UCase(Weapons(X).Name)
    S2 = UCase(Weapons(X).PickMe)
    If S1 = "" Then S1 = "Ackjdsfhkejh!"
    If S2 = "" Then S2 = S1
    If (S1 = Left$(UCase(s), Len(S1))) Or (S2 = Left$(UCase(s), Len(S2))) Then
      Send ("Please do not do /moves to get a weapon's move. Use /info instead.")
      'DispWpnMoves (X)
    End If
  Next X
End Sub

Sub ShowReveal (ByVal x0 As Integer)
' For the Reveal elemental. Shows the status of Playernum X0
'Dim Yx As YoumaType
Dim Cx As PlayerType
Dim s As String
  Cx = p(x0)
  Send5 ("[Name: " + Cx.ScrNam + " (Team " + Cx.TeamID + ")] [Senshi: " + Senshi(Cx.CharID).FullName + "]")
  Send5 ("[HP: " + TrimStr(Cx.HP) + "/" + TrimStr(Cx.MaxHP) + "] [SP: " + TrimStr(Cx.Super) + "] [PhysStr: " + TrimStr(Cx.PhysStr) + "]")
  Send5 ("[MagStr: " + TrimStr(Cx.MagStr) + "] [MagDef: " + TrimStr(Cx.MagDef) + "] [PhysDef: " + TrimStr(Cx.PhysDef) + "]")
  Send5 ("[Strong Against: " + EName(Senshi(Cx.CharID).Resist) + "] [Weak To: " + EName(Senshi(Cx.CharID).weakness) + "]")
  Send5 ("[Rune: " + RuneName(Cx.Rune) + "] [Weapon: " + Weapons(Cx.Weapon).Name + "]")
End Sub

Sub ShowStatus (ByVal X As Integer)
Dim HP%, x2%, y9 As Integer
Dim s As String
Dim Sx As CharType
Dim SHP As String
If ErrorTrap = True Then On Error Resume Next
  If p(X).MaxHP = 0 Then p(X).MaxHP = MaxHP
  If p(X).MaxHP <> MaxHP Then
    HP = Int((p(X).HP / p(X).MaxHP) * 300)
  Else
    HP = p(X).HP
  End If
  s = p(X).ScrNam
  If p(X).ScrNam = "" Or p(X).CharID = 0 Or p(X).GotKilled > 0 Then Exit Sub
  If (HP > 0) Then Exit Sub
  SHP = " (" + TrimStr(HP) + ")"
  Select Case Rand(1, 7)
    Case 1: s = "Kyle says, 'Oh my God! They killed " + ScrNam(X) + "! YOU BASTARDS!"
    Case 2: s = "Death slaps a tag on " + ScrNam(X) + " reading ""KAPUT""."
    Case 3: s = ScrNam(X) + " is quite dead."
    Case 4: s = ScrNam(X) + " has been killed! Well R.I.P."
    Case 5: s = ScrNam(X) + " is outta here!"
    Case 6: s = ScrNam(X) + " is now living-impaired."
    Case 7: s = ScrNam(X) + " has been defeated."
  End Select
  p(X).GotKilled = XTimer
  IncLosses (X)
  TKills = TKills + 1
  If HP <= -9999 Then
    Playwav ("Fatality")
    s = "<b><sub>F</sub>A<sup>T</sup>A<sub>L</sub>I<sup>T</sup>Y<sub>!</sub>!<sup>!</sup>! Again?"
    TFatal = TFatal + 1
  End If
  If HP = 0 Then s = "Wow! A <i>Perfect</i> Kill on " + ScrNam(X) + "!!"
  If HP = -666 Then s = "Oh my God! " + ScrNam(X) + " killed... themself? You... bastard?..."
  If HP = -777 Then s = ScrNam(X) + " managed to escape. Bai bai!"
  If Config.Flag <> 0 Then s = ScrNam(X) + " has been captured and sent to Jail!"
  If (X <= MaxPlayers) Then
    If p(X).Status(sLife3) = -1 Then
      If HP <= -9999 Then
	Send (s)
	s = "Surprise! Fatality nullifies Life3! Betcha didn't know that..."
	If Config.Flag <> 0 Then s = "Fatalities nullify get-out-of-jail-free passes."
      Else
	p(X).Status(sLife3) = 42
	If Config.Flag = 0 Then
	  s = "A fairy flies out of " + ScrNam(X) + "'s body and revives them!"
	Else
	  s = ScrNam(X) + " is sent to Jail but uses their get-out-of-jail-free pass."
	End If
	p(X).HP = p(X).MaxHP / 2
	p(X).GotKilled = 0
      End If
    ElseIf ZombieM = 1 Then
      Send ("But " + ScrNam(X) + " doesn't die... by some mysterious force, they become a Zombie!")
      Send ("Do NOT go out decapitating zombies after midnight!!")
      p(X).PhysStr = 250
      p(X).MagStr = 250
      p(X).MaxHP = 1000
      p(X).HP = p(X).MaxHP
      p(X).GotKilled = 0
      p(X).Status(sZombie) = True
      p(X).God = True
      ZombieM = 2
      s = ""
    ElseIf p(X).Status(sZombie) Then
      Send (p(X).ScrNam + " is buried in the cemetary... but they come out!!!")
      s = "Suddenly, an anvil, a house, a tank, an airplane, and the USS Enterprise fall on " + p(X).ScrNam + ", finally killing them."
      clearStatus (X)
      p(X).HP = -666
      p(X).MaxHP = MaxHP
      p(X).PhysStr = Senshi(p(X).CharID).PhysStr
      p(X).MagStr = Senshi(p(X).CharID).MagStr
      If Config.Respawn <> 0 Then p(X).HP = 0
      p(X).God = False
    ElseIf (p(X).Status(sPMS) = 0) And (p(X).Rune = RunePMS) Then
      s = p(X).ScrNam + " goes into <b><u>PMS RAGE!!!</b></u> {S pmsing2}"
      p(X).HP = 9999
      p(X).Status(sPMS) = XTimer
      p(X).God = True
      p(X).PhysStr = 125
      p(X).MagStr = 125
      p(X).CurMove = -255
    Else
      Playwav ("death" + TrimStr(Rand(1, 5)))
      Sx = Senshi(p(X).CharID)
      Do
	y9 = Rand(1, 5)
      Loop Until (Sx.DeathStr(y9) <> "") Or (Sx.DeathStr(1) = "" And Sx.DeathStr(2) = "" And Sx.DeathStr(3) = "" And Sx.DeathStr(4) = "" And Sx.DeathStr(5) = "")
      If Sx.DeathStr(y9) <> "" Then Send (Parse(Sx.DeathStr(y9), p(X).ScrNam, "", "", ""))
      p(X).MoveSpawn = p(X).MoveSpawn - 1
      clearStatus (X) ' when a Player has been killed, prepare them for the next battle
      If p(X).MoveSpawn <= -2 And p(X).CPU = 0 Then
	Send ("<b><font color=#FF0000>" + p(X).ScrNam + " gets sucked into the void of idleness, freeing up a slot.")
	p(X).CharID = 0
	p(X).Rune = 0
	p(X).Weapon = 0
      End If
      If p(X).Status(sPMS) Then
	p(X).Status(sPMS) = 0
	p(X).God = False
	p(X).PhysStr = Senshi(p(X).CharID).PhysStr
	p(X).MagStr = Senshi(p(X).CharID).MagStr
      End If
      If p(X).Weapon <> 0 Then
	If Left$(Weapons(p(X).Weapon).PickMe, 1) = "~" Then
	  DestroyWeapon X
	Else
	  DropWeapon X, "%SN's %T falls to the ground."
	End If
      End If
      If (HP <> -9999) And (HP <> -666) And (HP <> -777) Then s = s + SHP
    End If
  End If
  If (s <> "") Then Send5 (s)
End Sub

Sub ShowUnpicked ()
Dim x1, x2 As Integer
ReDim T(NumSenshi) As Integer
Dim S1 As String
  Send ("Dataset " + Dataset.LoadStr + " character list:")
  Delay (.55)
  For x1 = 1 To HighSenshi
    DoEvents
    If (Senshi(x1).PickMe <> "") Then
      If Left$(Senshi(x1).PickMe, 1) <> "~" Then T(x1) = True
    End If
  Next x1
  S1 = "C> "
  For x1 = 1 To HighSenshi
    If T(x1) Then
      If (Len(S1 + "/" + Senshi(x1).PickMe + " ")) > 130 Then
	Send (S1)
	S1 = "C> "
      End If
      S1 = S1 + "/" + Senshi(x1).PickMe + " "
    End If
    'If Len(s1) > 130 Then
    '  Send (s1)
    '  s1 = "> "
    'End If
  Next x1
  If (S1 <> "> ") Then
    Send (S1)
  End If
End Sub

Sub ShowWeaponMoveInfo (ByVal W%, ByVal X5%)
Dim M As MoveType, Msg$
  M = Weapons(W).Moves(X5)
  Msg = "     "
  If (M.Name <> "") Then Msg = Msg + M.Name + " "
  Msg = Msg + "(/" + M.Cmdkey + ")"
  If M.CanSuper Then Msg = Msg + "*"
  Send Msg
  Msg = "     Targets: "
  Select Case M.Target
    Case Allfriend: Msg = Msg + "All Allies"
    Case Friend: Msg = Msg + "One Ally"
    Case Enemy: Msg = Msg + "An Enemy"
    Case AllTeam: Msg = Msg + "One Enemy Team"
    Case AllFoe: Msg = Msg + "All Enemies"
    Case AllButSelf: Msg = Msg + "Everyone But Yourself"
    Case Everybody: Msg = Msg + "*EVERYBODY*"
    Case OnlySelf: Msg = Msg + "Self"
  End Select
  Send Msg
  Send "     Move Strength: " + TrimStr(M.ElementStr)
  Send "     Move Effect: " + EName(M.Element)
  Msg = "     Status Inflicted: "
  If (M.Status(sMute) > 0) Then Msg = Msg + "Mute [" + TrimStr(M.Status(sMute)) + "%] "
  If (M.Status(sChaos) > 0) Then Msg = Msg + "Chaos [" + TrimStr(M.Status(sChaos)) + "%] "
  If (M.Status(sFreeze) > 0) Then Msg = Msg + "Freeze [" + TrimStr(M.Status(sFreeze)) + "%] "
  If (M.Status(sSleep) > 0) Then Msg = Msg + "Sleep [" + TrimStr(M.Status(sSleep)) + "%] "
  If (M.Status(sPoison) > 0) Then Msg = Msg + "Poison [" + TrimStr(M.Status(sPoison)) + "%] "
  If (M.Status(sBlind) > 0) Then Msg = Msg + "Blind [" + TrimStr(M.Status(sBlind)) + "%] "
  If (M.Status(sSlow) > 0) Then Msg = Msg + "Slow [" + TrimStr(M.Status(sSlow)) + "%] "
  If (M.Status(sStun) > 0) Then Msg = Msg + "Stun [" + TrimStr(M.Status(sStun)) + "%] "
  If (M.Status(sHaste) > 0) Then Msg = Msg + "Speed Up [" + TrimStr(M.Status(sHaste)) + "%] "
  If (M.Status(sLife3) > 0) Then Msg = Msg + "Life3 [" + TrimStr(M.Status(sLife3)) + "%] "
  If (M.Status(sRegen) > 0) Then Msg = Msg + "Regen [" + TrimStr(M.Status(sRegen)) + "%] "
  If (M.Status(sStop) > 0) Then Msg = Msg + "Stop [" + TrimStr(M.Status(sStop)) + "%] "
  If (M.Status(sMushroom) > 0) Then Msg = Msg + "Mushroom [" + TrimStr(M.Status(sMushroom)) + "%] "
  If (M.Status(sMIA) > 0) Then Msg = Msg + "M.I.A. [" + TrimStr(M.Status(sMIA)) + "%] "
  If (M.Status(sQuick) > 0) Then Msg = Msg + "Quick [" + TrimStr(M.Status(sQuick)) + "%] "
  If (M.Status(sBerserk) > 0) Then Msg = Msg + "Berserk [" + TrimStr(M.Status(sBerserk)) + "%] "
  If (M.Status(sBarrier) > 0) Then Msg = Msg + "PBarrier [" + TrimStr(M.Status(sBarrier)) + "%] "
  If (M.Status(sMBarrier) > 0) Then Msg = Msg + "MBarrier [" + TrimStr(M.Status(sMBarrier)) + "%] "
  If (M.Status(sBless) > 0) Then Msg = Msg + "Bless [" + TrimStr(M.Status(sBless)) + "%] "
  If (M.Status(sCurse) > 0) Then Msg = Msg + "Curse [" + TrimStr(M.Status(sCurse)) + "%] "
  If (Msg <> "     Status Inflicted: ") Then Send Msg
  Msg = "     Cancels: "
  If M.Status(sMute) = -1 Then Msg = Msg + "Mute "
  If M.Status(sChaos) = -1 Then Msg = Msg + "Chaos "
  If M.Status(sFreeze) = -1 Then Msg = Msg + "Freeze "
  If M.Status(sSleep) = -1 Then Msg = Msg + "Sleep "
  If M.Status(sPoison) = -1 Then Msg = Msg + "Poison "
  If M.Status(sBlind) = -1 Then Msg = Msg + "Blind "
  If M.Status(sSlow) = -1 Then Msg = Msg + "Slow "
  If M.Status(sStun) = -1 Then Msg = Msg + "Stun "
  If M.Status(sHaste) = -1 Then Msg = Msg + "Haste "
  If M.Status(sLife3) = -1 Then Msg = Msg + "Life3 "
  If M.Status(sRegen) = -1 Then Msg = Msg + "Regen "
  If M.Status(sStop) = -1 Then Msg = Msg + "Stop "
  If M.Status(sMushroom) = -1 Then Msg = Msg + "Mushroom "
  If M.Status(sMIA) = -1 Then Msg = Msg + "M.I.A. "
  If M.Status(sQuick) = -1 Then Msg = Msg + "Quick "
  If M.Status(sBerserk) = -1 Then Msg = Msg + "Berserk "
  If M.Status(sBarrier) = -1 Then Msg = Msg + "PBarrier "
  If M.Status(sMBarrier) = -1 Then Msg = Msg + "MBarrier "
  If M.Status(sBless) = -1 Then Msg = Msg + "Bless "
  If M.Status(sCurse) = -1 Then Msg = Msg + "Curse "
  If (Msg <> "     Cancels: ") Then Send Msg
End Sub

Sub ShowWeapons ()
Dim x1, x2 As Integer
ReDim T(MaxWeapons) As Integer
Dim S1 As String
  Send ("Dataset " + Dataset.LoadStr + " weapon list:")
  Delay (.55)
  For x1 = 1 To MaxWeapons
    DoEvents
    If (Weapons(x1).PickMe <> "") Then
      If Left$(Weapons(x1).PickMe, 1) <> "~" Then T(x1) = True
    End If
  Next x1
  S1 = "W> "
  For x1 = 1 To MaxWeapons
    If T(x1) Then
      If (Len(S1 + "/" + Weapons(x1).PickMe + " ")) > 130 Then
	Send (S1)
	S1 = "W> "
      End If
      S1 = S1 + "/" + Weapons(x1).PickMe + " "
    End If
  Next x1
  If (S1 <> "W> ") Then
    Send (S1)
  End If
End Sub

Function Skip (ByVal s As String, ByVal p As Integer) As String
' Used for commands. If you have a Cmdkey of "/fatalize" and you want
' to compare a string S to see if that's the correct command, then you'll
' call Skip. For above example:
' Skip (S, 8)      because "fatalize" is 8 characters long.
' Will ignore case.
Dim Nu As String
Dim S1 As String
  Skip = UCase(Mid$(s, 2, p))
  If (Left$(s, 1) <> "/") And (Left$(s, 1) <> ".") Then Skip = "DoN't MaTcH tHiS"
End Function

Function SNPlayer (ByVal s$)
Dim X, x2 As Integer
Dim S1, S2, S3 As String
  For X = 1 To MaxPlayers
    DoEvents
    x2 = KInStr(Trim(UCase(s)), Trim(UCase(p(X).ScrNam)))
    If (x2 <> 0) Then
      SNPlayer = X
      Exit Function
    End If
  Next X
End Function

Function SpeedCheck () As Single
' SlrAlpha's Speed Debugger. Returns the number of Do-Loops executed in one
' second.
Dim X As Single
Dim L As Long
Dim s As Single
  s = Timer
  X = 0
  Do
    'DoEvents
    X = X + 1
  Loop Until (Timer >= s + 1)
  SpeedCheck = X
End Function

Function StatusReadout (x6 As Integer)
Dim Msg$
  'If (x6 > 0) And (x6 <= MaxPlayers) Then
    Msg = Msg + p(x6).ScrNam + " (" + Senshi(p(x6).CharID).FullName + "): (" + TrimStr(p(x6).HP)
    'If (Config.InfMP) Then
    '  Msg = Msg + "???"
    'Else
    '  Msg = Msg + TrimStr(p(x6).MP)
    'End If
    Msg = Msg + "H/" + TrimStr(p(x6).Super) + "S/" + TrimStr(p(x6).Cheese) + "C) {" + TextStatus(x6) + "}"
  'ElseIf (x6 > MaxPlayers) And (x6 <= MaxPlayers + MaxYouma) Then
  '  Msg = y(x6 - MaxPlayers).Name + ": (" + TrimStr(y(x6 - MaxPlayers).HP)
  '  Msg = Msg + ") {" + fBattle!LCurMove(x6 - MaxPlayers - 1).Caption + "}"
  'End If
End Function

Function Strip32 (ByVal S1$) As String
Dim X As Integer
Dim S2 As String
  S2 = ""
  For X = 1 To Len(S1)
    If Asc(Mid$(S1, X, 1)) <> 32 Then S2 = S2 + Mid$(S1, X, 1)
  Next X
  Strip32 = S2
End Function

Function StripHiAscii (ByVal S1 As String) As String
Dim X As Integer
Dim S2 As String
  S2 = ""
  For X = 1 To Len(S1)
    If Asc(Mid$(S1, X, 1)) >= 32 Then S2 = S2 + Mid$(S1, X, 1)
  Next X
  StripHiAscii = S2
End Function

Function StripHTML$ (ByVal s$)
Dim S1 As String
Dim X As Integer
  S1 = ""
  For X = 1 To Len(s) - 3
    If UCase(Mid$(s$, X, 3)) = "<B>" Or UCase(Mid$(s$, X, 3)) = "<I>" Or UCase(Mid$(s$, X, 3)) = "<U>" Then
      X = X + 2
    ElseIf UCase(Mid$(s$, X, 3)) = "<su" Then
      X = X + 4
    ElseIf UCase(Mid$(s$, X, 3)) = "</s" Then
      X = X + 5
    ElseIf UCase(Mid$(s$, X, 2)) = "</" Then
      X = X + 3
    ElseIf UCase(Mid$(s$, X, 3)) = "<fo" Then
      X = X + 19
    Else
      S1 = S1 + Mid$(s$, X, 1)
    End If
  Next X
  S1 = S1 + Right$(s$, 3)
  StripHTML = S1
End Function

Function Taken% (ByVal X3 As Integer)
' Make sure a Senshi is NOT taken
Dim X As Integer
  For X = 1 To MaxPlayers
    If p(X).CharID = X3 Then
      Taken = X
      Exit Function
    End If
  Next X
  Taken = 0
End Function

Function TeamID (ByVal x2 As Integer) As String
  'If (x2 > MaxPlayers) Then
  '  TeamID = y(x2 - MaxPlayers).TeamID
  'Else
    TeamID = p(x2).TeamID
  'End If
End Function

Function TextStatus (ByVal X As Integer)
Dim s$
Dim MPR%
  If p(X).HP <= 0 Then
    s$ = "DECEASED"
  Else
    s = ""
    Select Case p(X).CurMove
      Case pRest: s = "Resting"
      Case pFatal: s = "Fatality on "
	'If p(X).Target > MaxPlayers Then
	'  S = S + y(p(X).Target - MaxPlayers).Name
	'Else
	  s = s + p(p(X).Target).ScrNam
	'End If
      Case pBlock:
	If p(X).Target < MaxMoves And p(X).Target > 0 Then
	  If p(X).Status(sHamedo) Then
	    s = "Chibot Countering with " + p(X).Moves(p(X).Target).Name
	  Else
	    s = "Counterattacking with " + p(X).Moves(p(X).Target).Name
	  End If
	Else
	  s = "Blocking"
	End If
      Case pTaunt: s = "Taunting"
      Case pFlee: s = "Run, Forrest, Run!"
      Case 0: s = "Ready"
      Case -25, -50: s = "Is outta here"
      Case -255: s = "Almost Ready"
      Case 1 To MaxMoves:
	s = ""
	If p(X).SuperNum > 0 And p(X).Moves(p(X).CurMove).CanSuper <> 0 Then
	  s = "Level " + TrimStr(p(X).SuperNum) + " Super "
	End If
	If p(X).Moves(p(X).CurMove).Element <> Morph And p(X).Rune = RuneStealth Then
	  If p(p(X).Target).Rune = RuneMagic Then
	    s = s + p(X).Moves(p(X).CurMove).Name + " on " + p(p(X).Target).ScrNam
	  Else
	    s = s + p(X).Moves(p(X).CurMove).Name
	  End If
	ElseIf p(X).Moves(p(X).CurMove).Target = AllFoe Or p(X).Moves(p(X).CurMove).Target = AllButSelf Or p(X).Moves(p(X).CurMove).Target = Everybody Then
	  s = s + p(X).Moves(p(X).CurMove).Name
	Else
	  s = s + p(X).Moves(p(X).CurMove).Name + " on "
	  If p(X).Moves(p(X).CurMove).Element <> Morph Then
	    'If p(X).Target > MaxPlayers Then
	    '  S = S + y(p(X).Target - MaxPlayers).Name
	    'Else
	      s = s + p(p(X).Target).ScrNam
	    'End If
	  Else
	    s = s + Senshi(p(X).Target).FullName
	  End If
	End If
	If p(X).Charging Then
	  s = s + " [charging]"
	Else
	  s = s + " [" + TrimStr(MoveHitsIn(X)) + "]"
	End If
      End Select
    If p(X).Status(sMIA) Then s = "Missing in Action"
    If p(X).Status(sStop) Then s = "Frozen in Time"
    If p(X).Status(sFreeze) Then s = "Frozen solid!"
    If p(X).Status(sStun) Then s = "Unconsious"
    If p(X).Status(sMushroom) Then s = "Happy as a fungus!"
    If p(X).Status(sSleep) Then s = "Zzzzz...."
    If p(X).Status(sMorph) Then
      s = s + " (" + Senshi(p(X).OldCharID).FullName + ")"
    End If
    If p(X).Status(sPMS) > 0 Then s = s + " <font color=#FF0000><u>PMS</u></font> (" + TrimStr(PMSTime - (XTimer - p(X).Status(sPMS))) + ")"
  End If
  TextStatus = s
End Function

Function To100 () As Integer
' A random number between 1 and 100
  Dim X As Integer
  X = Int(Rnd * 100) + 1
  To100 = X
  DoEvents
End Function

Function ToHTML (ByVal Q$) As String
Dim R$, g$, b$
  Select Case Len(Q$)
    Case 1:
      R$ = "0" + Q$
      g$ = "00"
      b$ = "00"
    Case 2:
      R$ = Q$
      g$ = "00"
      b$ = "00"
    Case 3:
      R$ = Right$(Q$, 2)
      g$ = "0" + Left$(Q$, 1)
      b$ = "00"
    Case 4:
      R$ = Right$(Q$, 2)
      g$ = Left$(Q$, 2)
      b$ = "00"
    Case 5:
      R$ = Right$(Q$, 2)
      g$ = Mid$(Q$, 3, 2)
      b$ = "0" + Left$(Q$, 1)
    Case 6:
      R$ = Right$(Q$, 2)
      g$ = Mid$(Q$, 3, 2)
      b$ = Left$(Q$, 2)
  End Select
  ToHTML = R$ + g$ + b$
End Function

Function TrimNull (ByVal s As String) As String
Dim S1 As String
Dim x1 As Integer
  S1 = ""
  For x1 = 1 To Len(s)
    DoEvents
    If Asc(Mid$(s, x1, 1)) <> 0 Then S1 = S1 + Mid$(s, x1, 1)
  Next x1
  TrimNull = S1
End Function

Function TrimStr (ByVal s As Variant) As String
' A fast way of doing Trim(Str$("Whatever"))
  TrimStr = Trim(Str$(s))
End Function

Sub UnmorphMe (ByVal x2 As Integer)
Dim x7 As Integer
' For use of Proto only.
  'Send (p(x2).ScrNam + " reverts back into " + Senshi(p(x2).OldCharID).FullName + ".")
  p(x2).CharID = p(x2).OldCharID
  x7 = p(x2).CharID
  'If (x2 = fNeoStat!Peepz.ListIndex + 1) Then
    'If ErrorTrap = True Then On Error Resume Next
    'fNeoStat!iC.Picture = LoadPicture(App.Path + "\pics\" + First8(Senshi(p(fNeoStat!Peepz.ListIndex + 1).CharID).CharID) + ".BMP")
    'If Err <> 0 Then fNeoStat!iC.Picture = LoadPicture()
  'End If
  p(x2).PhysStr = Senshi(x7).PhysStr
  p(x2).PhysDef = Senshi(x7).PhysDef
  p(x2).MagStr = Senshi(x7).MagStr
  p(x2).MagDef = Senshi(x7).MagDef
  For x7 = 1 To MaxMoves - 1
    p(x2).Moves(x7) = Moves(Senshi(p(x2).CharID).Moves(x7))
  Next x7
  p(x2).Status(sMorph) = 0
  p(x2).CurMove = 0
End Sub

Sub UpdateGameStat ()
' Will update the Game Status window during battle.
Dim X%, H%, Q%, R%, Mb%, pb%, Mhi%
Dim s$, Msg$, Ame$
Dim MPR%
  On Error Resume Next
  H = fNeoStat!Peepz.ListIndex + 1
  Ame$ = AttackMe(H)
  For X = 1 To MaxPlayers
    fNeoStat!Peepz.List(X - 1) = p(X).ScrNam
    'If (Temp1 <> 0) Then fYControl!LTargets.List(X - 1) = p(X).ScrNam
  Next X
  X = H
  fNeoStat!LAttacking = Ame$
  fNeoStat!LName = p(X).ScrNam
  fNeoStat!LTID = TName(Asc(p(X).TeamID)) + " (Team " + p(X).TeamID + ")"
  fNeoStat!LChar = Senshi(p(X).CharID).FullName
  fNeoStat!LHP = "[" + TrimStr(p(X).HP) + "/" + TrimStr(p(X).MaxHP) + "]"
  fNeoStat!LCP = TrimStr(p(X).Cheese)
  Msg = "["
  'If Config.InfMP Then
  '  Msg = Msg + "???"
  'Else
  '  Msg = Msg + TrimStr(p(X).MP)
  'End If
  'Msg = Msg + "/" + TrimStr(p(X).MaxMP) + "]"
  'fNeoStat!LMP = Msg
  fNeoStat!LStatus = TextStatus(X)
  'If p(x).HP > 0 Then
    Msg = ""
    Mhi = MoveHitsIn(X)
    Msg = TrimStr(Mhi) + " "
    If p(X).CurMove = 0 Then Msg = ""
    If p(X).Draw = -1 Then Msg = Msg + "(Wants a Draw)"
    fNeoStat!LHitsIn = Msg
    Msg = TrimStr(p(X).Frags) + "/" + TrimStr(p(X).FatalFrags)
    fNeoStat!LFrags = Msg
    If p(X).AttackedMe = 0 Then Msg = "Nobody yet!"
    'If p(X).AttackedMe > MaxPlayers Then Msg = y(p(X).AttackedMe - MaxPlayers).Name
    If p(X).AttackedMe > 0 And p(X).AttackedMe <= MaxPlayers Then Msg = p(p(X).AttackedMe).ScrNam
    fNeoStat!LLast = Msg
    'fNeoStat!LStatus = Msg
  'End If
  If p(X).Super >= 100 Then
    fNeoStat!pL1.Visible = True
  Else
    fNeoStat!pL1.Visible = False
    fNeoStat!pL2.Visible = False
    fNeoStat!pL3.Visible = False
  End If
  If p(X).Super >= 200 Then
    fNeoStat!pL2.Visible = True
  Else
    fNeoStat!pL2.Visible = False
    fNeoStat!pL3.Visible = False
  End If
  If p(X).Super >= 300 Then
    fNeoStat!pL3.Visible = True
  Else
    fNeoStat!pL3.Visible = False
  End If
  fNeoStat!LSP.Caption = TrimStr(p(X).Super)
  If p(X).Rune <> RuneArmor Then
    fNeoStat!LRune.Caption = RuneName(p(X).Rune)
  Else
    fNeoStat!LRune.Caption = EName(p(X).RuneTemp) + " Elemental Armor"
  End If
  fNeoStat!LWeapon.Caption = Weapons(p(X).Weapon).Name + " (" + TrimStr(p(X).WpnUsesLeft) + " charges left)"
  If p(X).HP <= 0 Then fNeoStat!pDead.Visible = True Else fNeoStat!pDead.Visible = False
  For pb = 1 To sMaxStatus
    If p(X).Status(pb) <> 0 Then
      fNeoStat!pStatus(pb).Visible = True
    Else
      fNeoStat!pStatus(pb).Visible = False
    End If
  Next pb
  Mb = Int(20 * p(X).Status(sMBarrier) / 100)
  pb = Int(20 * p(X).Status(sBarrier) / 100)
  For H = 0 To 19
    If pb - 1 >= H Then
      fNeoStat!PBarrier(H).BackStyle = 1
    Else
      fNeoStat!PBarrier(H).BackStyle = 0
    End If
    If Mb - 1 >= H Then
      fNeoStat!MBarrier(H).BackStyle = 1
    Else
      fNeoStat!MBarrier(H).BackStyle = 0
    End If
  Next H
  For H = 0 To 14
    If p(X).CurMove <> 0 Then
      If 15 - Mhi - 1 >= H Then
	fNeoStat!sHitsIn(H).BackStyle = 0
      Else
	fNeoStat!sHitsIn(H).BackStyle = 1
      End If
    Else
      fNeoStat!sHitsIn(H).BackStyle = 1
    End If
  Next H
  If RedFlag = X And (FoundRedFlag = True Or Godmode) Then
    fNeoStat!pRed.Visible = True
  Else
    fNeoStat!pRed.Visible = False
  End If
  If BlueFlag = X And (FoundBlueFlag = True Or Godmode) Then
    fNeoStat!pBlue.Visible = True
  Else
    fNeoStat!pBlue.Visible = False
  End If
  If p(X).CPU <> 0 Then fNeoStat!pCPU.Visible = True Else fNeoStat!pCPU.Visible = False
End Sub

Sub UpdateWin ()
' Updates the Character Edit window...
Dim X As Integer
  fCharEdit!hsPNdx.Max = MaxPlayers
  'For X = 1 To MaxPlayers
    fCharEdit!tScrNam(0).Text = p(fCharEdit!hsPNdx).ScrNam
    fCharEdit!tTeamID(0).Text = p(fCharEdit!hsPNdx).TeamID
    fCharEdit!cSenshiID(0).ListIndex = p(fCharEdit!hsPNdx).CharID
  'Next X
  'fCharEdit!cFFA.ListIndex = FFABattle
  'fCharEdit!tFragLimit = TrimStr(FragLimit)
End Sub

Function XXAOL4 (ByVal s$) As String
Dim X As Integer
Dim S1 As String
Dim S2 As String, S3$
Dim XX$
Dim Ptr%
  XX = ""
  S1 = ""
  For X = 1 To Len(s)
    DoEvents
    If Mid$(s, X, 1) = Chr$(13) Then
      S2 = S1
      S1 = Mid$(s, X + 1, Len(s) - X)
    End If
  Next X
  S2 = s
  Ptr% = Len(S2)
  For X = Len(S2) To 1 Step -1
    DoEvents
    If Mid$(S2, X, 1) = Chr$(13) Then
      S3 = Mid$(S2, X + 1, Ptr - X - 1)
      Ptr% = X
      If Left$(GetCt(S3), 1) = "/" Then
	XX = XX + S3 + Chr$(13)
	'Exit For
      End If
    End If
  Next X
  XXAOL4 = XX
End Function

Function YtoSN (s As String) As String
Dim X As Integer
Dim S1 As String
  For X = 1 To Len(s)
    If Mid$(s, X, 1) = "%" And (X < Len(s)) Then
      If Mid$(s, X + 1, 1) = "Y" Then
	S1 = S1 + "%SN"
	X = X + 1
      Else
	S1 = S1 + "%"
      End If
    Else
      S1 = S1 + Mid$(s, X, 1)
    End If
  Next X
  YtoSN = S1
End Function

