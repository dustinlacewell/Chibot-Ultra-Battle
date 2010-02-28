Option Explicit

Global Const MaxMoves = 15

Type MStatusType
    ' Bad Status
  Mute As Integer            ' Can't do non-phys moves
  Scrow As Integer           ' Can't do phys. moves
  Freeze As Integer          ' Block of Ice
  Chaos As Integer           ' Uncontrollable Attack/Target
  Sleep As Integer           ' Asleep
  Poison As Integer          ' Loses -1 HP/s
  Blind As Integer           ' Attacks usually miss
  Charm As Integer           ' Attacks allies, cures enemies
  Curse As Integer           ' Takes 1.5x damage
  Slow As Integer            ' Speed Down
  Stop As Integer            ' Time Stop

    ' Good Status
  ReRaise As Integer         ' Return to Life after Hit
  Shell As Integer           ' Magic Damage halved
  Safe As Integer            ' Phys Damage halved
  Haste As Integer           ' Speed Up
  Regen As Integer           ' Gains +1 HP/s
  Quick As Integer           ' Next non-morph non-life move hits Instantly
  Bless As Integer           ' Causes 1.5x damage
  Fly As Integer             ' Half of attacks hit
  HalfMP As Integer          ' MP cost cut in two

    ' Varies good/bad
  Mushroom As Integer        ' Can't act, regain HP at each turn
  Berserk As Integer         ' Randomly attacks enemies, Attack Up
  MIA As Integer             ' Missing in Action
End Type ' MStatusType

' Status Constants
Global Const sMute = 101
Global Const sScrow = 102
Global Const sFreeze = 103
Global Const sChaos = 104
Global Const sSleep = 105
Global Const sPoison = 106
Global Const sBlind = 107
Global Const sCharm = 108
Global Const sCurse = 109
Global Const sSlow = 110
Global Const sStop = 111
Global Const sReRaise = 112
Global Const sShell = 113
Global Const sSafe = 114
Global Const sHaste = 115
Global Const sRegen = 116
Global Const sQuick = 117
Global Const sBless = 118
Global Const sFly = 119
Global Const sHalfMP = 120
Global Const sMushroom = 121
Global Const sBerserk = 122
Global Const sMIA = 123

Type BStatusType
    ' Bad Status
  Mute As Integer            ' Can't do non-phys moves
  Scrow As Integer           ' Can't do phys. moves
  Freeze As Integer          ' Block of Ice
  Chaos As Integer           ' Uncontrollable Attack/Target
  Sleep As Integer           ' Asleep
  Poison As Integer          ' Loses -1 HP/s
  Blind As Integer           ' Attacks usually miss
  Charm As Integer           ' Attacks allies, cures enemies
  Curse As Integer           ' Takes 1.5x damage
  Slow As Integer            ' Speed Down
  Stop As Integer            ' Time Stop

    ' Good Status
  ReRaise As Integer         ' Return to Life after Hit
  Shell As Integer           ' Magic Damage halved
  Safe As Integer            ' Phys Damage halved
  Haste As Integer           ' Speed Up
  Regen As Integer           ' Gains +1 HP/s
  Quick As Integer           ' Next non-morph non-life move hits Instantly
  Bless As Integer           ' Causes 1.5x damage
  Fly As Integer             ' Half of attacks hit
  HalfMP As Integer          ' MP cost cut in two
  Invin As Integer           ' Shielded
  Morph As Integer           ' Morphed

    ' Varies good/bad
  Mushroom As Integer        ' Can't act, regain HP at each turn
  Berserk As Integer         ' Randomly attacks enemies, Attack Up
  MIA As Integer             ' Missing in Action
End Type ' BStatusType

' Element Types
Global Const elZero = 0      ' No Element (Cure, Demi, etc.)
Global Const elPhys = 1      ' Physical Element
Global Const elNone = 2      ' Non-elemental Magic Damage
Global Const elPearl = 3     ' Pearl / Moon Energy
Global Const elDark = 4      ' Darkness / Shadow
Global Const elWater = 5     ' Water / Ice
Global Const elFire = 6      ' Fire
Global Const elLit = 7       ' Lightning
Global Const elHeart = 8     ' Heart
Global Const elEarth = 9     ' Earth
Global Const elWind = 10     ' Wind
Global Const elLight = 11    ' Light
Global Const elGrav = 12     ' Gravity
Global Const elWhat = 13     ' Mystery Element

' Effect Types
Global Const efZero = 0      ' No Effect
Global Const efStatus = 1    ' Status Change
Global Const efHPDmg = 2     ' HP Damage
Global Const efMPDmg = 3     ' MP Damage
Global Const efSpDmg = 4     ' SP Damage
Global Const efHPDrain = 5   ' HP Drain
Global Const efMPDrain = 6   ' MP Drain
Global Const efSpDrain = 7   ' SP Drain
Global Const efLife = 8      ' Restores Life
Global Const efDemi = 9      ' Halves HP
Global Const efQuartr = 10   ' Quarters HP
Global Const efSac = 11      ' Sacrifice Move (Maxes HP, MP, SP)
Global Const efHPBack = 12   ' Restores HP
Global Const efMPBack = 13   ' Restores MP
Global Const efHPTrans = 14  ' Transfers HP
Global Const efMPTrans = 15  ' Transfers MP
Global Const efSpTrans = 16  ' Transfers SP
Global Const efShield = 17   ' Shields
Global Const efMorph = 18    ' Shape Shift
Global Const efPeek = 19     ' ShowReveal
Global Const efSteal = 20    ' Move Theft
Global Const efFatal = 21    ' Fatality
Global Const efAllDrain = 22 ' Drains HP, MP, SP
Global Const efAllBack = 23  ' Restores HP, MP (Chakra)
Global Const efAllTrans = 24 ' Transfers HP, MP, SP

' Super Secret Speshul Stuff
Global Const ssNone = 0      ' No Secrets
Global Const ssHamedo = 1    ' Like Hamedo in FFT.
			     ' If person is attacked, move hits before the
			     ' enemy's attack. If not attacked, move hits at
			     ' the normal time.
Global Const ssEternal = 2   ' Move never hits. Use with Shields, or with
			     ' Hamedo to wait for an attack. Use /stop to
			     ' halt move.
Global Const ssWhat = 3      ' ????

Type MoveType
  Name As String * 50        ' Move Name
  CmdKey As String * 20      ' Command Key

  MPReq As Integer           ' MP Required
  SuperReq As Integer        ' Super Meter Points Required
  CanSuper As Integer        ' 0 = No Super. 1 = Can Super
  Evade As Integer           ' 100% = Hits Always, 0% = Hits Never
  Mult As Integer            ' 0 = Hits Target Only, 1 = Hits Team, 2 = All
			     ' Enemies, 3 = Everyone in Battle
  Default As Integer         ' 0 = Enemy, 1 = Friend, 2 = Self Only
  ForceMult As Integer       ' Force attack to hit as Mult specifies
  ForceDefault As Integer    ' Force attack to hit as Default specifies
  Status As MStatusType      ' Status Caused
  Element As Integer         ' Element Number
  Effect As Integer          ' Effect Number
  MoveStr As Integer         ' Move Strength
  Speshul As Integer      ' Super Secret Speshul Stuff

    ' Combo Crap
  Combo As Integer           ' 0 = No Combo. 1 = Optionally Combos
			     ' 2 = Requires 2 People. 3 = Requires 3 People
			     ' 4 = 4 People, etc.
  ComboID As String * 50     ' Combo Identification String

    ' Attack Strings
  SBegin As String * 100     ' Begin to Attack Self
  SHit As String * 100       ' Hits Self
  SMiss As String * 100      ' Misses Self
  'SBlock As String * 100     ' Will never be displayed.

  FBegin As String * 100     ' Begin to Attack Foe/Friend
  FHit As String * 100       ' Hits Foe/Friend
  FMiss As String * 100      ' Misses Foe/Friend
  FBlock As String * 100     ' Foe/Friend Blocks

  MHit As String * 100       ' Multi-target attack finishes execution
			     ' Individual FHit/SHit strings WILL NOT be shown
			     ' Miss/Block strings WILL be shown.

  CHit As String * 100       ' Combo Hits Foe
  CMiss As String * 100      ' Combo Misses
  CBlock As String * 100     ' Combo was Blocked

  SABegin As String * 100    ' Begin to Super Attack
  SAHit As String * 100      ' Super Attack Hits
  SAMiss As String * 100     ' Super Attack Misses
  SABlock As String * 100    ' Super Attack is Blocked.

  CritHit As String * 100    ' Shown when a move hits for double damage (Critical hit)
End Type ' MoveType

Type SenshiType
  FullName As String * 50    ' Full Name
  TargetID As String * 10    ' Targeting ID
  CmdKey As String * 20      ' Command Key
  Creator As String * 20     ' Creator
  Locked As Integer          ' If Locked, CharEdit will not open.

  Speed As Integer           ' Speed
  PhysStr As Integer         ' Phys Str
  PhysDef As Integer         ' Phys Def
  MagStr As Integer          ' Mag Str
  MagDef As Integer          ' Mag Def
  MaxHP As Integer           ' Maximum Hit Points
  MaxMP As Integer           ' Max Move Points
  HP As Integer              ' Starting HP
  MP As Integer              ' Starting MP

  SelectStr As String * 100
  SelectJoin As String * 100
  Rest As String * 100
  Block As String * 100
  BlockFail As String * 100
  BlockYes As String * 100

  Taunt(5) As String * 100
  DeathStr(5) As String * 100
  KillStr(5) As String * 100

  Female As Integer          ' -1 = Female (She)
  Male As Integer            ' -1 = Male (He)
  ' If (Female=0) And (Male=0) then character is Asexual (It).
  ' If (Female=-1) And (Male=-1) then character is Hermaphrodite (S/he).

  Moves(MaxMoves) As MoveType
  Resist(elWhat) As Integer  ' Resistance to Elements
  ' Resist(x) = 100: Takes regular damage.
  ' Resist(x) = 200: Takes double damage.
  ' Resist(x) = 50: Takes half damage.
  ' Resist(x) = 0: Takes no damage.
  ' Resist(x) = -50: Absorbs half damage.
  ' Resist(x) = -100: Absorbs entire attack.
End Type ' SenshiType

Type CharType
  ScrNam As String * 10      ' Screen Name (max 10 chars)
  TeamID As String * 1

  Female As Integer          ' -1 = Female (She)
  Male As Integer            ' -1 = Male (He)
  ' If (Female=0) And (Male=0) then character is Asexual (It).
  ' If (Female=-1) And (Male=-1) then character is Hermaphrodite (S/he).

  Ribbon As Integer          ' 0 = No Ribbon, 1 = Has Ribbon
  CharID As Integer          ' Current Character
  MCharID As Integer         ' Character Morphed From
  MaxHP As Integer           ' Max HP
  HP As Integer              ' Hit Points
  MaxMP As Integer           ' Max MP
  MP As Integer              ' Move Points
  Super As Integer           ' Super Points

  Frags As Integer           ' # of Kills
  FatalFrags As Integer      ' # of Fatalities

  Defect As Integer          ' 0 = Defects Allowed, 1 = Don't Defect to Me
  Draw As Integer            ' 0 = No Draw, -1 = Wants a Draw
  GotKilled As Integer       ' Time at which character was registered.
			     ' as killed. 0 = Not Killed
  AttackedMe As Integer

  Speed As Integer           ' Speed
  PhysStr As Integer         ' Physical Attack
  PhysDef As Integer         ' Physical Defense
  MagStr As Integer          ' Magic Attack
  MagDef As Integer          ' Magic Defense

  Status As BStatusType
  Moves(MaxMoves) As MoveType

  CurMove As Integer
  MoveStart As Long
  SuperNum As Integer
  Target As Integer          ' Numerical Target of Person
  TeamTarg As Integer        ' Attacking this Team #
  TargID As Integer          ' 0 = Target%, 1 = TeamTarg% (Entire Team),
			     ' 2 = AllFoe, 3 = Everybody, -1 = Self

  CPU As Integer             ' Is CPU?
  Goodwill As Integer        ' Determines whether or not a CPU will heal allies
  Greed As Integer           ' Determines how often a CPU will /get
  Wrath As Integer           ' Determines if a CPU will retaliate when attacked
  Arrogance As Integer       ' Determines how often a CPU will taunt
  Behavior As Integer        ' Behavior # of CPU
End Type ' CharType

Type ItemType
  Name As String * 30        ' Item Name
  Spawn As String * 100      ' Spawns
  Telefrag As String * 100   ' Telefragged
  PlayerGet As String * 100  ' Player Grabs It.
  YoumaGet As String * 100   ' Youma Grabs It.
  PlayerHP As Integer
  PlayerMP As Integer
  PlayerStat As MStatusType
  YoumaHP As Integer
End Type ' ItemType

