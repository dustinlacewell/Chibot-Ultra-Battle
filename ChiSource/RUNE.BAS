Attribute VB_Name = "RuneStuff"
Option Explicit

' Runes

Global Const Rune0 = 0
Global Const RuneHaste = 1  ' Perm Haste /haste
Global Const RuneStr = 2    ' Perm Bless /str
Global Const RuneBarr = 3   ' Perm P+MBarrier /barr
Global Const RuneRegen = 4  ' Perm Regen /regen
Global Const RuneEvade = 5  ' 25% Evade /evade
Global Const RuneLuck = 6   ' Rune of Luckiness /luck
Global Const RuneLife = 7   ' 25% more HP /life
Global Const RuneMagic = 8  ' Ignore Barriers and Shields /magic
Global Const RuneBlock = 9  ' Counters cost nil /counter
Global Const RuneSuper = 10 ' Get 2x SP from attacks
Global Const RuneFatal = 11 ' Rune of Fatalities
Global Const RuneWeird = 12 ' Rune of Weirdness
Global Const RuneHigh = 13  ' Rune of Highbes
Global Const RuneWonder = 14 ' WonderRune
Global Const RuneTheft = 15 ' Rune of Theft
Global Const RuneShadows = 16 ' Rune of Shadows (NYI)
Global Const RunePre = 17 ' Rune of Premonitions
Global Const RuneStealth = 18 ' Rune of Stealth
Global Const RuneRot = 19   ' Rotten Rune
Global Const RuneCurse = 20 ' Cursed Rune
Global Const RuneBug = 21   ' Rune of Computer Bugs
Global Const RunePikachu = 22 ' Rune of Pikachu
Global Const RuneIce = 23   ' Rune of Ice
Global Const RuneJedi = 24  ' Rune of Jedi Mind Control
Global Const RuneMute = 25  ' Rune of Mute
Global Const RuneZombie = 26 ' Rune of the Undead
Global Const RuneReflect = 27 ' Rune of Reflection
Global Const RuneArmor = 28 ' Elemental Armor
Global Const RuneBeans = 29 ' Can of Pork 'n Beans
Global Const RunePMS = 30   ' Rune of PMS
Global Const RuneCrit = 31  ' Rune of Liberation
Global Const RuneHeal = 32  ' Rune of Healing
Global Const RuneUndivert = 33 ' Rune of Undivertion
Global Const RuneDeath = 34 ' Death Blosom
Global Const RuneLearn = 35 ' Rune of Knowledge
Global Const RuneSlot = 36 ' Coin Case
Global Const RuneSummon = 37 ' Rune of Summoning
Global Const RuneDrain = 38 ' Drainer
Global Const RuneMute2 = 39 ' Rune of Muting
Global Const RuneReraise = 40 ' Rune of Reraise
Global Const RuneRespawn = 41 ' Rune of Respawn
Global Const RuneSurvival = 42 ' Rune of Survival
Global Const RuneThorn = 43 ' Rune of Thorns
Global Const RuneCheese = 44 ' Rune of Anti-Cheese
Global Const RuneDesp = 45 ' Desperation Rune
Global Const RuneSPGuard = 46 ' Super Guard Rune
Global Const RuneSPSwitch = 47 ' SP Switch
Global Const RuneRabbit = 48 ' Rabbit's Foot
Global Const RuneShoe = 49 ' Horseshoe
Global Const RuneClover = 50 ' Four-leaf clover
Global Const RuneCtrGuard = 51 ' Counter Guard
Global Const RuneCancel = 52 ' Cancellation Rune
Global Const RuneMimic = 53 ' Mimic Rune
Global Const RuneCannibal = 54 ' Rune of Cannibalism
Global Const RuneChiGuard = 55 ' Chibot Guard
Global Const RuneArmory = 56 ' Key to the Armory
Global Const RuneSwiss = 57 ' Swiss Rune

Global Const RuneVirus = -1

Global Const MaxRune = 57
Global Const PMSTime = 30

Function DesirableCPURune(ByVal Run%)
  Select Case Run
    Case RuneVirus, RuneRot, RuneCurse, RuneBug, RuneMute, RuneZombie, RuneRot, RuneCurse, RuneBug, RuneMute, RunePre, RuneUndivert, RuneSlot, RuneSummon, RuneLuck, RuneMimic, RuneCannibal, RuneArmory:
      DesirableCPURune = False
    Case Else:
      DesirableCPURune = True
  End Select
End Function

Function RuneName(ByVal Run%) As String
Dim S$
  Select Case Run%
    Case Rune0: S = "No Rune"
    Case RuneHaste: S = "Rune of Haste"
    Case RuneStr: S = "Rune of Strength"
    Case RuneBarr: S = "Rune of Barriers"
    Case RuneRegen: S = "Rune of Regeneration"
    Case RuneEvade: S = "Rune of Evasion"
    Case RuneLuck: S = "Rune of Luckiness"
    Case RuneLife: S = "Rune of Life"
    Case RuneMagic: S = "Rune of Magic"
    Case RuneBlock: S = "Rune of Counterattacks"
    Case RuneSuper: S = "Rune of Rage"
    Case RuneFatal: S = "Rune of Fatalities"
    Case RuneWeird: S = "Rune of Weirdness"
    Case RuneHigh: S = "Idiot Killer"
    Case RuneWonder: S = "WonderRune"
    Case RuneTheft: S = "Rune of Theft"
    Case RuneStealth: S = "Rune of Stealth"
    'Case RuneCharge: S = "Rune of Charging"
    'Case RuneDeath: S = "Deathblow Materia"
    'Case RuneVirus: S = "Rune of Virii"
    Case RunePre: S = "Rune of Premonitions"
    Case RuneRot: S = "Rotten Rune"
    Case RuneCurse: S = "Cursed Rune"
    Case RuneBug: S = "Rune of ChUB Bugs"
    Case RunePikachu: S = "Rune of Pikachu"
    Case RuneIce: S = "Rune of Ice"
    Case RuneJedi: S = "Rune of Jedi Mind Control"
    Case RuneMute: S = "Rune of Mute"
    Case RuneZombie: S = "Rune of the Undead"
    Case RuneArmor: S = "Elemental Armor"
    Case RuneReflect: S = "Rune of Reflection"
    Case RuneShadows: S = "Rune of Shadows"
    Case RuneBeans: S = "Can of Pork 'n Beans"
    Case RunePMS: S = "Rune of PMS"
    Case RuneCrit: S = "Rune of Liberation"
    Case RuneHeal: S = "Rune of Healing"
    Case RuneUndivert: S = "Rune of Un-divertion"
    Case RuneDeath: S = "Death Blossom"
    Case RuneLearn: S = "Rune of Knowledge"
    Case RuneSlot: S = "Coin Case"
    Case RuneSummon: S = "Rune of Summoning"
    Case RuneDrain: S = "Drainer"
    Case RuneMute2: S = "Rune of Muting"
    Case RuneReraise: S = "Rune of Reraise"
    Case RuneRespawn: S = "Rune of Respawn"
    Case RuneSurvival: S = "Rune of Survival"
    Case RuneThorn: S = "Rune of Thorns"
    Case RuneCheese: S = "Rune of Anti-Cheese"
    Case RuneSPGuard: S = "Super Guard Rune"
    Case RuneDesp: S = "Rune of Desperation"
    Case RuneSPSwitch: S = "SP Switch"
    Case RuneRabbit: S = "Rabbit's Foot"
    Case RuneShoe: S = "Horseshoe"
    Case RuneClover: S = "Four-leaf Clover"
    Case RuneCtrGuard: S = "Counter Guard"
    Case RuneCancel: S = "Rune of Cancellation"
    Case RuneMimic: S = "Mimic Rune"
    Case RuneCannibal: S = "Rune of Cannibalism"
    Case RuneChiGuard: S = "Chibot Guard"
    Case RuneArmory: S = "Key to the Armory"
    Case RuneSwiss: S = "Swiss Rune"
    Case Else: S = "Unknown Rune"
  End Select
  RuneName = S
End Function

