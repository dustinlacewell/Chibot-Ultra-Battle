Attribute VB_Name = "ChibotInitialize"
' CHARS.BAS
' Defines Arenas, Group Attacks, and some of the Characters.

Option Explicit

Sub InitVars()
' Initializes variables, loads the Youma and Config files, and calls the
' subs to init the Characters.
Dim x, x2 As Integer
Dim Tw%, S$
Dim P1$, P2$, P4$, PO%, Msg$
  Config.Arena = 1
  CurArena = 1
  Config.NoJoin = Val(GetSetting("ChUB Resurrection", "Settings", "Join", "1"))
  Config.SameChar = Val(GetSetting("ChUB Resurrection", "Settings", "SameChar", "1"))
  Config.Flag = Val(GetSetting("ChUB Resurrection", "Settings", "Flag", "0"))
  Config.Fours = Val(GetSetting("ChUB Resurrection", "Settings", "Fours", "1"))
  Config.Multi = Val(GetSetting("ChUB Resurrection", "Settings", "Multitarget", "1"))
  Config.Respawn = Val(GetSetting("ChUB Resurrection", "Settings", "Respawn", "0"))
  Config.Defect = Val(GetSetting("ChUB Resurrection", "Settings", "Defect", "0"))
  Config.RuneEnable = Val(GetSetting("ChUB Resurrection", "Settings", "RuneEnable", "1"))
  Config.WeaponEnable = Val(GetSetting("ChUB Resurrection", "Settings", "WeaponEnable", "1"))
  'Config.FontName = GetSetting("ChUB Resurrection", "Settings", "FontName", "Arial")
  'Config.FontColor = GetSetting("ChUB Resurrection", "Settings", "FontColor", "000000")
  Config.FlaCon = Val(GetSetting("ChUB Resurrection", "Settings", "FlaCon", "160"))
  Config.Attacking = Val(GetSetting("ChUB Resurrection", "Settings", "Attacking", "1"))
  Config.LearnedMove = Val(GetSetting("ChUB Resurrection", "Settings", "LearnedMove", "1"))
  ScRa = Val(GetSetting("ChUB Resurrection", "Settings", "ScRa", "1"))
  mIRCChannel = GetSetting("ChUB Resurrection", "Settings", "Channel", "#ChUB")
  fChUBMain.tiOutput.interval = Val(GetSetting("ChUB Resurrection", "Settings", "OutScroll", "200"))
  DMult = Val(GetSetting("ChUB Resurrection", "Settings", "DMult", "100"))
  MaxHP = Val(GetSetting("ChUB Resurrection", "Settings", "MaxHP", "600"))
  MaxMP = MaxHP
  MaxSP = Val(GetSetting("ChUB Resurrection", "Settings", "MaxSP", "500"))
  GetRate = Val(GetSetting("ChUB Resurrection", "Settings", "GetRate", "30"))
  FragLimit = Val(GetSetting("ChUB Resurrection", "Settings", "FragLimit", "0"))
  Config.MDIOnTop = Val(GetSetting("ChUB Resurrection", "Settings", "MDIOnTop", "1"))
  Config.Status = Val(GetSetting("ChUB Resurrection", "Settings", "Status", "1"))
  Config.Moves = Val(GetSetting("ChUB Resurrection", "Settings", "Moves", "1"))
  Config.CPU = Val(GetSetting("ChUB Resurrection", "Settings", "CPU", "0"))
  Config.Version = Val(GetSetting("ChUB Resurrection", "Settings", "Version", "1"))
  Config.Help = Val(GetSetting("ChUB Resurrection", "Settings", "Help", "1"))
  Config.TypeCommand = Val(GetSetting("ChUB Resurrection", "Settings", "TypeCommand", "1"))
  Config.FragCount = Val(GetSetting("ChUB Resurrection", "Settings", "FragCount", "1"))
  Config.RuneCmd = Val(GetSetting("ChUB Resurrection", "Settings", "RuneCmd", "1"))
  Config.GetRune = Val(GetSetting("ChUB Resurrection", "Settings", "GetRune", "1"))
  Config.Weapon = Val(GetSetting("ChUB Resurrection", "Settings", "Weapon", "1"))
  Config.WpnList = Val(GetSetting("ChUB Resurrection", "Settings", "WpnList", "1"))
  Config.Lag = Val(GetSetting("ChUB Resurrection", "Settings", "Lag-O-Meter", "2500"))
  Config.NewUser = Val(GetSetting("ChUB Resurrection", "Yoshi!", "New User", "0"))
  Config.Tips = Val(GetSetting("ChUB Resurrection", "Yoshi!", "Tips", "0"))
  Config.StatusE = Val(GetSetting("ChUB Resurrection", "Settings", "StatusE", "1"))
  MaxPlayers = Val(GetSetting("ChUB Resurrection", "Settings", "MaxPlayers", "10"))
  On Error GoTo eTwit
  MaxTwit = 0
  MaxBackTwit = 0
  Open "TWIT.TXT" For Input As #1
  Tw% = 1
  Do
    Do
      Input #1, S$
    Loop Until Mid$(S$, 1, 1) <> ";" Or EOF(1)
    If Left$(S$, 1) <> ";" Then
      ReDim Preserve Twit(Tw%)
      Twit(Tw%) = S$
      Tw% = Tw% + 1
    End If
  Loop Until EOF(1)
  MaxTwit = Tw% - 1
  Close #1
  GoTo ContinueOn
eTwit:
  Close #1
  Resume ContinueOn
ContinueOn:
  On Error GoTo NoBackTwit
  Open "BACKTWIT.TXT" For Input As #33
  Tw% = 1
  Msg = ""
  Do
    Do
      Input #33, S$
    Loop Until Left$(S$, 1) <> ";" Or EOF(33)
    If Left$(S$, 1) <> ";" Then
      ReDim Preserve BackTwit(Tw%)
      BackTwit(Tw%) = S$
      If Len(Msg + S$ + " ") > 130 Then
        Send Msg
        Msg = ""
      End If
      Msg = Msg + S$ + " "
      Tw = Tw + 1
    End If
  Loop Until EOF(33)
  MaxBackTwit = Tw - 1
  If MaxBackTwit > 0 Then
    Send ("BackTwit.TXT found! Only these people will be allowed to play:")
    Send Msg
  End If
  GoTo NextCont
NoBackTwit:
  Resume NextCont
NextCont:
  Close #33
  Lang = -1
  x = Len(Config)
  On Error GoTo 0
  CurArena = 1
  GetRate = 100
  On Error GoTo 0 ' We don't need error-trapping anymore!
  ReDim Preserve Senshi(0)
  Senshi(0).FullName = "(Empty Slot)"
  ReDim P(MaxPlayers)
  ReDim Vote(MaxPlayers)
  ToChUBBot ("CHUBMAXP " + TrimStr(MaxPlayers))
  For x = 1 To MaxPlayers
    P(x).ScrNam = ""
    P(x).TeamID = "S"
    P(x).Goodwill = 50
    P(x).Greed = 50
    P(x).Wrath = 50
    P(x).Arrogance = 50
    'p(x).Darkness = 50
    'p(X).Smart = 1
  Next x
  InitXMoves
End Sub

Sub InitXMoves()
Dim x%
  Langolier.name = "The Langoliers."
  Langolier.Desc(1) = "= Langoliers Arena based on Steven King novella ="
  Langolier.Desc(2) = "You are one of few survivors of a terrible"
  Langolier.Desc(3) = "mishap. Your plane has crash-landed in the past."
  Langolier.Desc(4) = "This world is dying, and the keepers of time,"
  Langolier.Desc(5) = "the langoliers, will soon come to devour the past,"
  Langolier.Desc(6) = "and anything in it... including you."
  Langolier.Desc(7) = "You have 10 game minutes... As time passes your attacks"
  Langolier.Desc(8) = "will gradually lose power, and resting will prove"
  Langolier.Desc(9) = "less refreshing. When time is up, the langoliers will"
  Langolier.Desc(10) = "have their main course... YOU!!"
  Langolier.RestLowHP = 8
  Langolier.RestHighHP = 16
  'Langolier.RestLowMP = 35
  'Langolier.RestHighMP = 50
  'Langolier.GradualMP = 1
  Langolier.AllAttacks = 1
  For x = 1 To MaxEle
    Langolier.Effect(x) = 1
  Next x
  InitHid
  SM(0).Cmdkey = "summon-demidemon"
  SM(0).name = "Demi"
  SM(0).Begin2Attack = "%SN uses the power of the summon rune to summon the evil DemiDemon on %T!"
  SM(0).Hit = "DemiDemon casts Demi on %T."
  SM(0).Miss = "DemiDemon casts Demi on %T but it missed."
  SM(0).Element = Demi
  SM(0).Target = Enemy
  
  SM(1).Cmdkey = "summon-bowser"
  SM(1).name = "Bowser Summon"
  SM(1).Begin2Attack = "%SN steps on the Bowser Square. Bowser appears and asks %SN what they want."
  SM(1).Hit = "%SN tells Bowser to steal %T's Super Points. Bowser is only too happy to steal stuff!"
  SM(1).Miss = "%SN tells Bowser to steal %T's Super Points. But Bowser doesn't like being ordered around..."
  SM(1).Element = SPTheft
  SM(1).Target = Enemy
  
  SM(2).Cmdkey = "summon-kappa"
  SM(2).name = "Imp Song"
  SM(2).Begin2Attack = "%SN casts Imp Song and Kappa appears."
  SM(2).Hit = "Kappa uses magic to turn %T into a mushroom!"
  SM(2).Miss = "Kappa casts a spell on %T, but falls flat on his face."
  SM(2).Element = Shadow
  SM(2).ElementStr = 80
  SM(2).Status(sMushroom) = 100
  SM(2).Target = Enemy
  
  SM(3).Cmdkey = "summon-locke"
  SM(3).name = "Locke Summon"
  SM(3).Begin2Attack = "%SN summons Locke to steal %T's good status."
  SM(3).Hit = "Locke steals %T's good status and gives it to %SN."
  SM(3).Miss = "Locke was unable to steal %T's status..."
  SM(3).Element = 255
  
  SM(4).Cmdkey = "summon-strago"
  SM(4).name = "Barrier???"
  SM(4).Begin2Attack = "Strago appears before %SN and casts the ""Barrier???"" Lore spell."
  SM(4).Begin2HealSelf = "Strago finishes the spell."
  SM(4).Element = 254
  SM(4).Target = AllFoe
  
  SM(5).Cmdkey = "summon-mewtwo"
  'SM(5).Begin2Attack = "Sorry, the Mewtwo summon was removed... ..."
  SM(5).name = "Mewtwo Summon"
  SM(5).Begin2Attack = "%SN throws out a Pokeball containing Mewtwo. %SN tells Mewtwo, ""Clone %T!"""
  SM(5).Hit = "Mewtwo makes a duplicate copy of %T and sends it out into battle to fight on %SN's side."
  SM(5).Miss = "Mewtwo's copy machine breaks down."
  SM(5).Element = 253
  SM(5).Target = Enemy
  
  SM(6).Cmdkey = "summon-kamek"
  SM(6).name = "Kamek Summon"
  SM(6).Begin2Attack = "%SN summons Kamek to cast the ""Sword Rain"" spell."
  SM(6).Begin2HealSelf = "Kamek casts ""Sword Rain"". Magical swords rain down on %SN's enemies!"
  SM(6).Miss = "%T dodges the sword rain."
  SM(6).Element = Shadow
  SM(6).ElementStr = 120
  SM(6).HealSelf = "The sword rain ends and Kamek vanishes in a puff of smoke."
  SM(6).Target = AllFoe
    
  SM(7).Cmdkey = "summon-peach"
  SM(7).name = "Super Group Hug"
  SM(7).Begin2HealSelf = "%SN summons Princess Peach, and she appears in a ray of light."
  SM(7).Begin2Attack = "%SN summons Princess Peach, and she appears in a ray of light."
  SM(7).Hit = "Princess Peach blows a kiss to %T's team."
  SM(7).HealSelf = "Pricess Peach blows a kiss to %SN's team."
  SM(7).Status(sQuick) = 100
  SM(7).Status(sRegen) = 100
  SM(7).Status(sBarrier) = 100
  SM(7).Status(sMBarrier) = 100
  SM(7).Status(sHaste) = 100
  SM(7).Element = Heal
  SM(7).ElementStr = 600
  SM(7).Status(sBless) = 100
  SM(7).Status(sReraise) = 100
  SM(7).Target = Allfriend
  
  SM(8).Cmdkey = "summon-null"
  SM(8).name = "Null Summon"
  SM(8).Begin2Attack = "%SN summons the most powerful monster of all, Null!"
  SM(8).Hit = "Oops?.... Null didn't do anything...."
  SM(8).Miss = "Oops?.... Null didn't do anything...."
  
  SM(9).Cmdkey = ""
  SM(10).Cmdkey = ""
  
  Dim Alt255 As String * 1
  Alt255 = Chr$(255)
  
  KM(1).name = "Super PBarrier"
  KM(1).Cmdkey = "spbarrier"
  KM(1).Begin2Attack = "%SN casts ""Super PBarrier""."
  KM(1).Hit = "A wall of protective shielding surrounds %SN. Let's see you hit him with a Physical Attack now."
  KM(1).Element = 251
  KM(1).Target = OnlySelf
  
  KM(2).name = "Hyper Shield"
  KM(2).Cmdkey = "hs"
  KM(2).Begin2Attack = "%SN raises his magic wand and a red shield appears around himself."
  KM(2).Hit = "The red shield around %SN vanishes, leaving him with all sorts of goodies!"
  KM(2).Element = Invin
  KM(2).Target = OnlySelf
  KM(2).Status(sHaste) = 100
  KM(2).Status(sMBarrier) = 100
  KM(2).Status(sBless) = 100
  KM(2).Status(sRegen) = 100
  
  KM(3).name = "Lightning Bolt"
  KM(3).Cmdkey = "bolt"
  KM(3).Begin2Attack = "%SN raises his wand and an electrical storm moves in."
  KM(3).Hit = "A lightning bolt from the heavens strikes %T!"
  KM(3).Crithit = "%T is electrocuted by a lightning bolt from the heavens!"
  KM(3).Miss = "The electrical storm moves out as %SN shouts obscenities."
  KM(3).Begin2SuperAttack = "%SN draws power from a 10,000-volt generator!"
  KM(3).SuperHit = "%SN channels the lightning through his wand. The thunder arcs through the air, zapping %T into fine specks of dust!"
  KM(3).SuperMiss = "%T shuts down %SN's generator. ""CURSE YOU %T!!!"" %SN shouts."
  KM(3).Element = Lit
  KM(3).ElementStr = 200
  KM(3).Target = Enemy
  KM(3).CanSuper = True
  
  KM(4).name = "Bio"
  KM(4).Cmdkey = "bio"
  KM(4).Begin2Attack = "%SN points and a poison cloud engulfs %T!"
  KM(4).Begin2SuperAttack = "%SN spikes the punch bowl!"
  KM(4).Hit = "%T is choked by the cloud of poison."
  KM(4).Crithit = "%T is choked to death by the poisonous cloud!"
  KM(4).SuperHit = "%T drinks out of the punch bowl. %T doesn't feel good..."
  KM(4).SuperMiss = "%SN forgot he spiked the punch and takes a drink! %SN spews halfway across the room onto Yoshi!"
  KM(4).Miss = "The cloud of poison around %T dissipates."
  KM(4).Element = Poison
  KM(4).ElementStr = 200
  KM(4).Target = Enemy
  KM(4).CanSuper = True
  
  KM(5).name = "HP Buster"
  KM(5).Cmdkey = "hpd"
  KM(5).Begin2Attack = "%SN raises his magic wand as a cloud of fog surrounds %T."
  KM(5).Hit = "The fog sucks up %T's hit points. %SN calls the cloud back and takes the HP!"
  KM(5).Miss = "%T knocks %SN down and disrupts the spell."
  KM(5).Element = HPTheft
  KM(5).ElementStr = 300
  KM(5).Target = Enemy
  
  KM(6).name = "R. Raven"
  KM(6).Cmdkey = "raven"
  KM(6).Begin2Attack = "%SN gestures with his magic wand and %T is teleported to the moon."
  KM(6).Hit = "Raphael the Raven squashes %T!"
  KM(6).Miss = "%SN forgets the spell and %T returns to earth."
  KM(6).Begin2SuperAttack = "%SN raises his magic wand and the moon begins to glow."
  KM(6).SuperHit = "The moon emits a powerful ray of moonlight at %T, disintegrating them!"
  KM(6).Miss = "Raphael the Raven stomps on %SN!"
  KM(6).Element = MoonE
  KM(6).ElementStr = 200
  KM(6).Target = Enemy
  KM(6).CanSuper = True
  
  KM(7).name = "Dismemberment"
  KM(7).Cmdkey = "dis"
  KM(7).Begin2Attack = "%SN raises his magic wand and a black globe appears in the sky."
  KM(7).Hit = "%T looks up as the black globe crashes down, exploding all over %T!"
  KM(7).Miss = "%T looks up as the black globe explodes in midair!"
  KM(7).Begin2SuperAttack = "%SN points at %T and a black dome surrounds them."
  KM(7).SuperHit = "The black globe around %T explodes into a SUPER NOVA!!"
  KM(7).SuperMiss = "The black globe around %T shatters. %SN curses out loud!"
  KM(7).Element = Shadow
  KM(7).ElementStr = 200
  KM(7).Target = Enemy
  KM(7).CanSuper = True
  
  KM(8).name = "Niagra Falls Teleport"
  KM(8).Cmdkey = "niagra"
  KM(8).Begin2Attack = "%SN waves his magic wand and teleports %T to Niagra Falls."
  KM(8).Hit = "%T floats in the middle of Niagra Falls and gets drenched!"
  KM(8).Miss = "%SN has a memory lapse and %T reappears."
  KM(8).Begin2SuperAttack = "%SN waves his magic wand and teleports %T to Niagra Falls -- in a barrel!!"
  KM(8).SuperHit = "%T goes over Niagra Falls in a barrel!!"
  KM(8).SuperMiss = "%T manages to escape from the barrel somehow."
  KM(8).Element = Water
  KM(8).ElementStr = 200
  KM(8).Target = Enemy
  KM(8).CanSuper = True
  
  KM(9).name = "Gargantua Blaarg"
  KM(9).Cmdkey = "blaarg"
  KM(9).Begin2Attack = "%SN waves his magic wand and Gargantua Blaarg appears."
  KM(9).Hit = "Gargantua Blaarg blasts %T with fire!"
  KM(9).Miss = "Gargantua Blaarg gets mad and burns %SN!"
  KM(9).Begin2SuperAttack = "%SN waves his magic wand and summons an army of Gargantua Blaargs!"
  KM(9).SuperHit = "The Blaargs all burn %T with fire!"
  KM(9).SuperMiss = "A mouse appears, and all the Blaargs attack the mouse instead of %T!"
  KM(9).Element = Fire
  KM(9).ElementStr = 200
  KM(9).Target = Enemy
  KM(9).CanSuper = True
  
  KM(10).name = "Flying Wiggler"
  KM(10).Cmdkey = "wiggler"
  KM(10).Begin2Attack = "%SN summons a Flying Wiggler and makes it mad."
  KM(10).Hit = "Flying Wiggler gets all PMSy and beats the crap outta %T!"
  KM(10).Miss = "Flying Wiggler gets all PMSy and beats the crap outta %SN!"
  KM(10).Begin2SuperAttack = "%SN summons a swarm of Flying Wigglers."
  KM(10).SuperHit = "The Flying Wigglers unleash their energy in a massive PMS attack on %T!"
  KM(10).SuperMiss = "The Flying Wigglers get all PMSy and attack each other!"
  KM(10).Element = Heart
  KM(10).ElementStr = 200
  KM(10).Target = Enemy
  KM(10).CanSuper = True
  
  KM(11).name = "Boulder"
  KM(11).Cmdkey = "boulder"
  KM(11).Begin2Attack = "%SN casts ""Boulder"" and a rock appears over %T."
  KM(11).Hit = "A large rock lands on %T."
  KM(11).Miss = "%T steps aside as the rock falls to the ground."
  KM(11).Begin2SuperAttack = "%SN casts ""Boulder"" and a landslide approaches!!"
  KM(11).SuperHit = "%T is mowed down by the landslide!"
  KM(11).SuperMiss = "%SN is caught in the landslide by mistake!"
  KM(11).Element = Earth
  KM(11).ElementStr = 200
  KM(11).Target = Enemy
  KM(11).CanSuper = True
  
  KM(12).name = "Whirlwind"
  KM(12).Cmdkey = "whirlwind"
  KM(12).Begin2Attack = "%SN waves his magic wand as a tornado approaches."
  KM(12).Hit = "%T is caught up in the tornado, flung around and smashed into the ground!"
  KM(12).Miss = "The tornado dissipates. %SN screams obsenities."
  KM(12).Begin2SuperAttack = "%SN causes a hurricane!"
  KM(12).SuperHit = "%T is assaulted by 150-MPH winds!"
  KM(12).SuperMiss = "The hurricane attacks %SN instead of %T."
  KM(12).Element = Wind
  KM(12).ElementStr = 200
  KM(12).Target = Enemy
  KM(12).CanSuper = True
  
  KM(13).name = "Super Kamehameha"
  KM(13).Cmdkey = "kamehameha"
  KM(13).Begin2Attack = "%SN throws a DBZ videocassette at %T."
  KM(13).Hit = "The DBZ tapes land on the ground in front of %T. %SN casts a spell causing Goku to come out and Kamehameha %T!!"
  KM(13).Begin2SuperAttack = "%SN throws several DBZ videocassettes at %T."
  KM(13).SuperHit = "The DBZ tapes land around %T. Gokus come out of every tape and use Kamehameha on %T!!"
  KM(13).Miss = "%T quickly grabs and flings the DBZ tape away. It knocks Yoshi in the head."
  KM(13).SuperMiss = "Yoshi gobbles up the DBZ tapes with his tongue."
  KM(13).Target = Enemy
  KM(13).Element = Ki
  KM(13).ElementStr = 200
  KM(13).CanSuper = True
  
  KM(14).name = "Naval Piranha Summon"
  KM(14).Cmdkey = "naval"
  KM(14).Begin2Attack = "%SN summons Naval Piranha Plant."
  KM(14).Hit = "Naval Piranha casts ""Solar Beam"" on %T!"
  KM(14).Miss = "%T grabs a pair of hedge clippers and chops away at Naval Piranha."
  KM(14).Begin2SuperAttack = "%SN summons Naval Piranha Plant."
  KM(14).SuperHit = "Naval Piranha releases sun spores, which cover and burn %T!"
  KM(14).SuperMiss = "%T grabs a pair of hedge clippers and chops away at Naval Piranha."
  KM(14).Element = Lum
  KM(14).ElementStr = 200
  KM(14).Target = Enemy
  KM(14).CanSuper = True
  
  KM(15).name = "Polygon Attack"
  KM(15).Cmdkey = "polygon"
  KM(15).Begin2Attack = "%SN raises his wand and causes a mass of colored triangles, squares, and circles to appear."
  KM(15).Begin2HealSelf = "Colored shapes assault everyone!"
  KM(15).Crithit = "%T are choked by sky-blue circles!"
  KM(15).Hit = "%T are hit by colored shapes."
  KM(15).HealSelf = "The colored polygons hit a stone wall and vanish."
  KM(15).Element = Psychic
  KM(15).ElementStr = 100
  KM(15).Target = AllFoe
  
  KM(16).name = "???"
  KM(16).Cmdkey = "shatter"
  KM(16).Begin2Attack = "%SN points his magic wand at %T."
  KM(16).Hit = "%SN emits a beam at %T, causing their %S2 to shatter!"
  KM(16).Miss = "%SN emits a beam at %T. Nothing happens."
  KM(16).Element = 252
  KM(16).Target = Enemy
  
  KM(17).name = "Shyster"
  KM(17).Cmdkey = "shyster"
  KM(17).Begin2Attack = "%SN summons a Shyster."
  KM(17).Hit = "A Shyster bounds on %T's head."
  KM(17).Miss = "A Shyster bounds around the room, then disappears."
  KM(17).Element = NoDmg
  KM(17).Target = Enemy
  KM(17).Status(sStun) = 100
  KM(17).Status(sHaste) = -1
  KM(17).Status(sBless) = -1
  KM(17).Status(sMBarrier) = -1
  KM(17).Status(sRegen) = -1
  
  KM(18).name = "Toadies"
  KM(18).Cmdkey = "toadies"
  KM(18).Begin2Attack = "%SN summons his Toadies."
  KM(18).Hit = "%SN's Toadies whisk %T away!"
  KM(18).Miss = "%T shoots %SN's Toadies down with a water gun."
  KM(18).Element = Psychic
  KM(18).ElementStr = 140
  KM(18).Target = Enemy
  KM(18).Status(sMIA) = 100
  
  KM(19).name = "Cure 4"
  KM(19).Cmdkey = "cure4"
  KM(19).Begin2Attack = "%SN laughs. ""Ha! You think you have me beat?"""
  KM(19).Hit = "%SN casts ""Cure 4"" on himself. ""I leared a cure spell JUST for this match!"""
  KM(19).Element = Heal
  KM(19).ElementStr = 1500
  KM(19).Target = OnlySelf
  KM(19).Status(sRegen) = 100
  
  YM(1).name = "Disabler"
  YM(1).Cmdkey = "disabler"
  YM(1).Begin2Attack = "%SN calls upon the power of all the Yoshies...."
  YM(1).Hit = "%SN uses the power of the Yoshies to break %T's shield and fully restore everybody!"
  YM(1).Element = 250
  YM(1).ElementStr = 0
  YM(1).Target = Enemy
  
  YM(2).name = "Heal Beam"
  YM(2).Cmdkey = "heal"
  YM(2).Begin2HealSelf = "%SN gobbles up a Yoshi Cookie."
  YM(2).Hit = "%SN releases a wave of energy, curing everyone!"
  YM(2).Element = Heal
  YM(2).ElementStr = 1000
  YM(2).ElementStr = 1000
  YM(2).Target = Allfriend
  
  YM(3).name = "Yoshi Stomp"
  YM(3).Cmdkey = "stomp"
  YM(3).Begin2Attack = "%SN jumps into the air."
  YM(3).Hit = "%SN jumps on %T!"
  YM(3).Element = Phys
  YM(3).ElementStr = 150
  YM(3).ElementStr = 150
  YM(3).Target = Enemy
  YM(4).CanSuper = 0
  
  YM(4).name = "Pound the Ground"
  YM(4).Cmdkey = "pound"
  YM(4).Begin2Attack = "%SN jumps into the air over %T."
  YM(4).Hit = "%SN pounds %T into the ground!"
  YM(4).Begin2SuperAttack = "%SN leaps into the air over %T..."
  YM(4).SuperHit = "%SN pounds %T into a pancake!"
  YM(4).Element = Phys
  YM(4).ElementStr = 170
  YM(4).ElementStr = 170
  YM(4).Target = Enemy
  YM(4).CanSuper = 1
  
  YM(5).name = "Seed Spit"
  YM(5).Cmdkey = "seed"
  YM(5).Begin2Attack = "%SN puts a watermelon into his mouth."
  YM(5).Begin2SuperAttack = "%SN spits watermelon seeds at everyone!"
  YM(5).HealSelf = "%SN runs out of watermelon seeds."
  YM(5).Element = Phys
  YM(5).ElementStr = 130
  YM(5).ElementStr = 130
  YM(5).Target = Enemy
  
  YM(6).name = "Yoshi Cookie"
  YM(6).Cmdkey = "cookie"
  YM(6).Begin2Attack = "%SN eats a Yoshi Cookie."
  YM(6).Hit = "%SN feels supercharged after eating the cookie!"
  YM(6).Element = NoDmg
  YM(6).Status(sHaste) = 100
  YM(6).Status(sBless) = 100
  YM(6).Status(sMBarrier) = 100
  YM(6).Status(sBarrier) = 100
  YM(6).Status(sRegen) = 100
  YM(6).Target = OnlySelf
End Sub
