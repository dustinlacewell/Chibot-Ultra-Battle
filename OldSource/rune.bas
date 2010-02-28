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
Global Const RuneWonder = 14' WonderRune
Global Const RuneTheft = 15 ' Rune of Theft
Global Const RuneShadows = 16' Rune of Shadows (NYI)
Global Const RuneVirus = 17 ' Rune of Viruses
'Global Const RuneCharge = 17' Rune of Charging
Global Const RuneStealth = 18' Rune of Stealth
Global Const RuneRot = 19   ' Rotten Rune
Global Const RuneCurse = 20 ' Cursed Rune
Global Const RuneBug = 21   ' Rune of Computer Bugs
Global Const RunePikachu = 22' Rune of Pikachu
Global Const RuneIce = 23   ' Rune of Ice
Global Const RuneJedi = 24  ' Rune of Jedi Mind Control
Global Const RuneMute = 25  ' Rune of Mute
Global Const RuneZombie = 26' Rune of the Undead
Global Const RuneReflect = 27' Rune of Reflection
Global Const RuneArmor = 28 ' Elemental Armor
Global Const RuneBeans = 29 ' Can of Pork 'n Beans
Global Const RunePMS = 30   ' Rune of PMS
Global Const RuneCrit = 31  ' Rune of Liberation
Global Const RuneDesp = 32  ' Rune of Desperation
Global Const RuneUndivert = 33 ' Rune of Undivertion
Global Const RuneDeath = 34
Global Const RuneLearn = 35

Global Const MaxRune = 35
Global Const PMSTime = 30

Function DesirableCPURune (ByVal Run%)
  Select Case Run
    Case RuneVirus, RuneRot, RuneCurse, RuneBug, RuneMute, RuneZombie, RuneShadows, RuneJedi:
      DesirableCPURune = False
    Case Else:
      DesirableCPURune = True
  End Select
End Function

Function RuneName (ByVal Run%) As String
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
    Case RuneVirus: S = "Rune of Virii"
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
    Case RuneDesp: S = "Rune of Desperation"
    Case RuneUndivert: S = "Rune of Un-diversion"
    Case RuneDeath: S = "Death Blossom"
    Case RuneLearn: S = "Rune of Knowledge"
    Case Else: S = "Unknown Rune"
  End Select
  RuneName = S
End Function

