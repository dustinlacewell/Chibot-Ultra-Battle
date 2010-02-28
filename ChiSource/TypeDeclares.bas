Attribute VB_Name = "TypeDeclares"
Option Explicit

Type PType
  x As Integer
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
  Reraise As Integer      ' Return to life once after being killed
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
  Reraise As Integer      ' Return to life once after being killed
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
  RuneEnable As Integer
  WeaponEnable As Integer
  MDIOnTop As Integer
  Status As Integer
  Moves As Integer
  CPU As Integer
  Attacking As Integer
  Version As Integer
  Help As Integer
  TypeCommand As Integer
  FragCount As Integer
  RuneCmd As Integer
  GetRune As Integer
  Weapon As Integer
  WpnList As Integer
  Lag As Integer
  LearnedMove As Integer
  NewUser As Integer
  Tips As Integer
  StatusE As Integer
End Type

Type RanHappening
' Random happening in an Arena
  name As String
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
  name As String                       ' Name
  Desc(10) As String                   ' Description
  AllAttacks As Single                 ' Multiplier on all attacks
  Effect(MaxEle) As Single             ' Multiplier on certain elements
  RestLowHP As Integer                 ' Default = 15
  RestHighHP As Integer                ' Default = 25
  RestLowMP As Integer                 ' Default = 15
  RestHighMP As Integer                ' Default = 25
  GradualMP As Integer                 ' Default = 1
  GradualHP As Integer                 ' Default = 0
  Happening(10) As RanHappening            ' up to 10 random happenings
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
  GoCharge As String
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
  name As String
  Spawn As String
  Telefrag As String
  PlayerGet As String
  YoumaGet As String
  PlayerHP As Integer
  PlayerStat(sMaxStatus) As Integer
End Type

Type MoveType
  name As String
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
  'ElementStr As Integer              ' Default Move Str.
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
  name As String
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
  MP As Integer                 ' Move/Mana/Magic Points
  MaxMP As Integer
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
  ChargeLevel As Integer
  GotKilled As Integer          ' Obituary been displayed?
  Ready As Integer              ' ???
  Rune As Integer
  Scroller As Integer
  RuneTemp As Integer
  RuneTemp2 As Integer
  Charging As Integer
  MoveSpawn As Integer
  Weapon As Integer             ' 0 = None
  OwnedBy As Integer
  'WeaponState As Integer
  ' 0 = No Weapon
  ' 1 = Holding Weapon
  ' 2 = Weapon Destroyed
  ' 3 = Weapon Dropped
  WpnUsesLeft As Integer
  Wins As Integer
  Losses As Integer
End Type
