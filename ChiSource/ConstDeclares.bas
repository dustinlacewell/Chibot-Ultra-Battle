Attribute VB_Name = "ConstDeclares"
Option Explicit

'Global Const mIRCChannel = "#ChUB"
Global Const VerID = "Arcade"
Global Const ErrorTrap = False
Global Const GodMode1 = False
Global Const LogFile = 42

Global Const GodKey = 843

Global Const ccColor = ""
Global Const ccBold = ""
Global Const ccUnderline = ""
Global Const ccReverse = ""

Global Const HasteHit = -5
Global Const SlowHit = 5
Global Const PMSHit = -10
Global Const BlockHit = 30
Global Const TauntHit = 5
Global Const RestHit = 10
Global Const SlotHit = 15
Global Const FatalHit = 16
Global Const FleeHit = 15
Global Const ChargeHit = 15
Global Const NMoveHit = 15
Global Const OtherHit = 15
Global Const SummonCharge = 20
Global Const ArmoryWait = 20
Global Const MimicCharge = 20
Global Const MaxCheeseLimit = 1100

Global Const MaxRay = 250 ' # of lines that may be sent to the input buffer
Global Const outmax = 250 ' # of lines that may be sent to the output buffer
Global Const SndMax = 50 ' # of sounds that may be sent to the sound buffer

Global Const MaxMoves = 30 ' max # of moves per character

' YoshiHelp Constants

Global Const yhDone = -1
Global Const yh1stTime = 0
Global Const yhMOTD = 1
Global Const yhNoMoreYoshi = 2
Global Const yhWelcomeBack = 3
Global Const yhNoTips = 4
Global Const yhTipsBack = 5
Global Const yhAFKBot = 100
Global Const yhAFK_Reason = 101
Global Const yhAFK_Freq = 102
Global Const yhAFK_Msg = 103
Global Const yhAFK_Start = 104
Global Const yhAFK_Msgs = 105
Global Const yhAFK_Clear = 106
Global Const yhCharEdit = 200
Global Const yhChar_ScrNam = 201
Global Const yhChar_CPU = 202
Global Const yhChar_AllCPUs = 203
Global Const yhChar_TeamID = 204
Global Const yhChar_EZTeams = 205
Global Const yhChar_QwikCPU = 206
Global Const yhChar_RandChar = 207
Global Const yhChar_RandCPU = 208
Global Const yhChar_tN = 209
Global Const yhChar_TeamName = 210
Global Const yhCPUEdit = 300
Global Const yhCPU_IsCPU = 301
Global Const yhCPU_Goodwill = 302
Global Const yhCPU_Greed = 303
Global Const yhCPU_Wrath = 304
Global Const yhCPU_Arrogance = 305
Global Const yhMidi = 400
Global Const yhMidi_DirBox = 401
Global Const yhMidi_File = 402
Global Const yhMidi_Lup = 403
Global Const yhMidi_Play = 404
Global Const yhMidi_Stop = 405
Global Const yhMidi_Pause = 406
Global Const yhMidi_Hide = 407
Global Const yhOptions = 500
Global Const yhOpt_SameChar = 501
Global Const yhOpt_Weapons = 502
Global Const yhOpt_Runes = 503
Global Const yhOpt_Respawn = 504
Global Const yhOpt_CTF = 505
Global Const yhOpt_MultiTarget = 506
Global Const yhOpt_MDITop = 507
Global Const yhOpt_ExtendedSupers = 508
Global Const yhOpt_FragLimit = 509
Global Const yhOpt_GetRate = 510
Global Const yhOpt_FlagLimit = 511
Global Const yhOpt_TLimit = 512
Global Const yhOpt_DmgMult = 513
Global Const yhOpt_MaxHP = 514
Global Const yhOpt_MaxSP = 515
Global Const yhOpt_NoJoin = 516
Global Const yhOpt_Status = 517
Global Const yhOpt_Moves = 518
Global Const yhOpt_CPU = 519
Global Const yhOpt_Attacking = 520
Global Const yhOpt_Version = 521
Global Const yhOpt_Help = 522
Global Const yhOpt_Defect = 523
Global Const yhOpt_Type = 524
Global Const yhOpt_FragCount = 525
Global Const yhOpt_LearnedMove = 526
Global Const yhOpt_RuneCmd = 527
Global Const yhOpt_GetRune = 528
Global Const yhOpt_Weapon = 529
Global Const yhOpt_WpnList = 530
Global Const yhOpt_OutScroll = 531
Global Const yhOpt_ScRa = 532
Global Const yhOpt_MaxPlayers = 533
Global Const yhOpt_Lag = 534
Global Const yhOpt_StatusX = 535
Global Const yhChat_Chat = 600
Global Const yhRant = 700
Global Const yhTwit = 800
Global Const yhWhat = 900
Global Const yhGraph = 1000
Global Const yhIniLoad = 1100
Global Const yhPotty = 1200
Global Const yhGiveUp = 1300

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
Global Const MaxEle = 75            ' The Highest Element
Global Const eSlot = 100
Global Const HPBased = 101
Global Const StealStat = 255
Global Const BarrierLore = 254
Global Const Clone = 253
Global Const WeaponBreak = 252
Global Const SuperPBarrier = 251
Global Const Disabler = 250

' Secret Elements....
' 255 - Locke - Take good status away from enemy - Always hits one enemy only
' 254 - Barrier??? - Targeted people get a bad status effect if they have PBarr or MBarr
' 253 - Mewtwo - Clones enemy...
' 252 - Destroy's opponent's weapon
' 251 - Protects self from physical attacks (Kamek's Super PBarrier)
' 250 - Remove effects of 251 and fully restores all allies (Yoshi's Disabler)


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
Global Const sScarecrow = 9     ' Scarecrow
Global Const sSlow = 10         ' Speed Down
Global Const sStun = 11         ' Unconsious
Global Const sReraise = 12        ' Reraise
Global Const sRegen = 13        ' HP restores
Global Const sStop = 14         ' Time Stopped
Global Const sMushroom = 15     ' Mushroomied!
Global Const sMIA = 16          ' Missing in Action
Global Const sQuick = 17        ' Quick
Global Const sBerserk = 18      ' Random attacks @ random enemies
Global Const sSleep = 19        ' Zzzzzzzzzz
Global Const sVirus = 20
Global Const sCPUWait = 21           ' R1
Global Const HitRate = 22           ' R2
Global Const sBarrier = 23      ' Barriers
Global Const sMBarrier = 24     ' MBarriers
Global Const sBless = 25        ' Attack Up
Global Const sCurse = 26        ' Attack Down
Global Const sCharm = 27        ' Charm
Global Const sPMS = 28          ' Pissed Off!
Global Const sZombie = 29       ' Zombie Mode
Global Const sHamedo = 30       ' Chibot Counter
Global Const sR3 = 31
Global Const sR4 = 32
Global Const sMaxStatus = 32

Global Const Allfriend = 1   ' Hits all friends + default target = self
Global Const Enemy = 2       ' Default target = GetTarget(TeamID)
Global Const Ally = 3      ' Default target = self
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
Global Const pSlot = 245
Global Const pCharge = 244
