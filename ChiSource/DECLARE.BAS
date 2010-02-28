Attribute VB_Name = "MainDeclares"
Option Compare Text
Option Explicit

Global Const XRunes = -1

Global mIRCChannel$
Global mIRCWindow$
Global GodType1$
Global GlobalTerm%
Global BootNum%
Global Selection%
Global Mov2Edit%
Global YoshiHelp%
Global KamekID%
Global OnFire%
Global FireKills%
Global PKamek%
Global YoshiID%
Global pYoshi%

Global MaxPlayers As Integer

Global Senshi() As CharType   ' Senshi Array
Global EditChar() As CharType
Global P() As PlayerType          ' Player Array
Global Vote() As Integer        ' Voting Array
Global Weapons() As WeaponType  ' Weapons
Global Dropped() As Integer     ' Dropped Weapons
Global DropUses() As Integer
Global Arena() As ArenaType     ' Arena Array
Global Items() As ItemType      ' Items
Global Twit() As String         ' Twit Names
Global BackTwit() As String
Global Death() As String
Global Fatality() As String
Global Moves() As MoveType      ' The Moves
Global gMaxMoves%               ' Max # of Moves

Global Temp1, Temp2, Temp3, Temp4      ' Temp variables
Global Config As Settings              ' Configuration
Global Item2Get As Integer             ' Item to /get
Global CurArena As Integer             ' Current Arena
Global Battle, Paused, Mx As Integer   ' Misc stuff
Global Tx%, InSend, TauntClear, TauntEnd, DebugWindow As Integer
Global YourSN, STX As String ' YourSN = host's name
Global DATASET As DatasetType
Global HighSenshi As Integer ' Max # of Senshi

Global NumSenshi As Integer
Global MaxItem As Integer
Global MaxArena As Integer
Global MaxTwit As Integer
Global MaxBackTwit
Global MaxWeapons As Integer
Global MaxDropped As Integer
Global MaxMoves_ As Integer
Global MaxDeath As Integer
Global MaxFatality As Integer

Global Godmode As Integer

Global TLimit As Long
Global TKills&, TFatal&, tHP&, tMP&, tSP&

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

Global SM(10) As MoveType       ' Summon Moves
Global KM(MaxMoves) As MoveType ' Kamek's Moves!!!
Global YM(MaxMoves) As MoveType ' Yoshi's Moves

Global XTimer As Long ' Game Time elapsed in Game Seconds

Global Auto As Integer ' for /push

Global FragLimit As Integer ' frag limit for respawn
Global GameNum As Integer ' # of battles
Global LoadedX As Integer ' # of times loaded
Global tName(255) As String ' the 255 teams... of these only about 36 are accessible by players
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

Global DMult, MaxHP, MaxSP, MaxMP As Long

Global Shotgun$

Global TimeLimit As Long

Global PeepzStats(sMaxStatus) As String

Function Active(ByVal x%) As Integer
  'If X > MaxPlayers Then
  '  Active = y(X - MaxPlayers).Enabled
  'Else
    Active = P(x).CharID
  'End If
End Function

Sub AddDroppedWeapon(ByVal W%, Us%)
Attribute AddDroppedWeapon.VB_Description = "Adds a dropped weapon W with Us number of uses."
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
    DropUses(MaxDropped) = Abs(Us)
  Else
    Dropped(Okay) = W%
    DropUses(Okay) = Abs(Us)
  End If
End Sub

Sub AddStatus(ByVal x2 As Integer, ByVal S As Integer)
Attribute AddStatus.VB_Description = "Adds the S status to Player X2."
Dim x0 As Integer
  DoEvents
  Select Case S
    Case sMute:
      P(x2).Status(sMute) = XTimer + Rand(-3, 3)
      If (P(x2).CurMove > 0) And (P(x2).CurMove <= MaxMoves) Then
        If P(x2).Moves(P(x2).CurMove).Element <> Phys Then P(x2).CurMove = -255
      End If
      Playwav "amute"
    Case sChaos:
      P(x2).Status(sChaos) = XTimer + Rand(-3, 3)
      'p(x2).CurMove = -255
      P(x2).SuperNum = 0
      Playwav "achaos"
    Case sFreeze:
      P(x2).Status(sFreeze) = XTimer + Rand(-3, 3)
      'p(x2).CurMove = -50
      Playwav "afreeze"
    Case sSleep:
      P(x2).Status(sSleep) = XTimer + Rand(-3, 3)
      P(x2).CurMove = 0
      Playwav "asleep"
    Case sPoison:
      P(x2).Status(sPoison) = XTimer + Rand(-3, 3)
      Playwav "apoison"
    Case sBlind:
      P(x2).Status(sBlind) = XTimer + Rand(-3, 3)
      Playwav "ablind"
    Case sSlow:
      P(x2).Status(sSlow) = XTimer + Rand(-3, 3)
      If P(x2).Status(sHaste) Then Call RemoveStatus(x2, sHaste)
      Playwav "aslow"
    Case sHaste:
      P(x2).Status(sHaste) = XTimer + Rand(-3, 3)
      If P(x2).Status(sSlow) Then Call RemoveStatus(x2, sSlow)
      Playwav "ahaste"
    Case sStun:
      P(x2).Status(sStun) = XTimer + Rand(-3, 3)
      'p(x2).CurMove = -50
      Playwav "astun"
    Case sReraise:
      P(x2).Status(sReraise) = -1
      Playwav "aReraise"
    Case sRegen:
      P(x2).Status(sRegen) = -1
      Playwav "aregen"
    Case sStop:
      P(x2).Status(sStop) = XTimer + Rand(-3, 3)
      Playwav "astop"
    Case sMushroom:
      P(x2).Status(sMushroom) = XTimer + Rand(-3, 3)
      P(x2).CurMove = -50
      P(x2).MoveStart = XTimer - 999
      Playwav "amushroom"
    Case sMIA:
      P(x2).Status(sMIA) = XTimer + Rand(-3, 3)
      Playwav "amia"
    Case sQuick:
      P(x2).Status(sQuick) = -1
      Playwav "aquick"
    Case sBerserk:
      P(x2).Status(sBerserk) = XTimer + Rand(-3, 3)
      P(x2).PhysStr = P(x2).PhysStr + 50
      P(x2).MagStr = P(x2).MagStr + 50
      'p(x2).CurMove = -255
      P(x2).SuperNum = 0
      Playwav "aberserk"
    Case sBarrier:
      P(x2).Status(sBarrier) = 100
      Playwav "abarrier"
    Case sMBarrier:
      P(x2).Status(sMBarrier) = 100
      Playwav "ambarrier"
    Case sBless:
      P(x2).Status(sBless) = XTimer + Rand(-3, 3)
      Playwav "abless"
    Case sCurse:
      P(x2).Status(sCurse) = XTimer + Rand(-3, 3)
      Playwav "acurse"
    Case sVirus:
      P(x2).Status(sVirus) = True
      P(x2).Status(sPoison) = XTimer - 15
      Playwav "avirus"
    Case sScarecrow:
      P(x2).Status(sScarecrow) = XTimer + Rand(-3, 3)
      Playwav "ascarecrow"
    Case Else:
      ScrollSend1 ("Status " + TrimStr(S) + " doesn't exist.")
  End Select
  DoEvents
End Sub

Function AmIsAreWas(ByVal Word$, No%)
Attribute AmIsAreWas.VB_Description = "Some fooked up procedure."
Dim WordL$, x%, Word1$
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

Function AttackMe$(ByVal H%)
Dim x%, Ame$, Q%, Q2%
  For x = 1 To MaxPlayers
    DoEvents
    If P(x).CurMove > 0 And P(x).HP > 0 And P(x).CharID <> 0 Then
      If P(x).CurMove <= MaxMoves Then
        Q = P(x).Moves(P(x).CurMove).Target
      ElseIf P(x).CurMove = pSlot Then
        Q = Enemy
      Else
        GoTo Skipwho
      End If
      Q2 = 0
      If (Q = 5 Or Q = 6) And P(H).TeamID <> P(x).TeamID Then Q2 = True
      If (Q = 7) Then Q2 = True
      If P(x).CurMove > 0 And P(x).CurMove <= MaxMoves Then
        If P(x).Moves(P(x).CurMove).Element <> Morph And Q = 4 Then
          If P(P(x).Target).TeamID = P(H).TeamID Then Q2 = True
        End If
      End If
      If (P(x).Target = H Or Q2) And (Q <> 1) And (Q <> 3) And (Q <> 8) Then
        If P(x).Rune = RuneStealth Then
          If P(H).Rune = RuneMagic Then Ame$ = Ame$ + P(x).ScrNam + ", "
        Else
          Ame$ = Ame$ + P(x).ScrNam + ", "
        End If
      End If
Skipwho:
    End If
  Next x
  If Ame$ <> "" Then
    AttackMe$ = Left$(Ame$, Len(Ame$) - 2)
  Else
    AttackMe$ = "Nobody"
  End If
End Function

Function ChaosTarget%()
Dim temp As Integer
Dim y3 As Integer
Dim x, x2 As Integer
ReDim Targets(MaxPlayers) As Integer
  For x2 = 1 To MaxPlayers
    DoEvents
    Targets(x2) = 0
  Next x2
  x2 = 1
  For x = 1 To MaxPlayers
    DoEvents
    If (P(x).CharID <> 0) And (P(x).HP > 0) And (P(x).Rune <> RuneShadows) Then
      Targets(x2) = x
      x2 = x2 + 1
    End If
  Next x
  x2 = x2 - 1
  y3 = Int(Rnd(1) * x2) + 1
  y3 = Targets(y3)
  ChaosTarget = y3
End Function

Sub Check2RemoveStatus(ByVal x2 As Integer)
Dim x0 As Integer
Dim SN As String
Dim y2 As Integer
Dim Run As Integer
Dim HP%
  x0 = x2 - MaxPlayers
  SN = P(x2).ScrNam
  Run = P(x2).Rune
  DoEvents
  If P(x2).CharID = 0 Then
    For x0 = 1 To sMaxStatus
      P(x2).Status(x0) = 0
    Next x0
  End If
  If XTimer - P(x2).Status(sPMS) >= PMSTime * Int(Config.Lag / 1000) And P(x2).Status(sPMS) <> 0 And Run = RunePMS Then
    Send (SN + " collapses from PMS Rage!")
    P(x2).HP = -4
    ShowStatus (x2)
  End If
  If XTimer - P(x2).Status(sMute) >= 35 * Int(Config.Lag / 1000) And P(x2).Status(sMute) <> 0 And Run <> RuneMute Then
    Send (SN + " is no longer muted.")
    Call RemoveStatus(x2, sMute)
  End If
  If XTimer - P(x2).Status(sScarecrow) >= 35 * Int(Config.Lag / 1000) And P(x2).Status(sScarecrow) <> 0 Then
    Send (SN + " is no longer a scarecrow.")
    Call RemoveStatus(x2, sScarecrow)
  End If
  DoEvents
  If XTimer - P(x2).Status(sChaos) >= 35 * Int(Config.Lag / 1000) And P(x2).Status(sChaos) <> 0 Then
    Send (SN + " is no longer confused.")
    Call RemoveStatus(x2, sChaos)
  End If
  DoEvents
  If XTimer - P(x2).Status(sFreeze) >= 20 * Int(Config.Lag / 1000) And P(x2).Status(sFreeze) <> 0 Then
    Send (SN + " thaws out.")
    Call RemoveStatus(x2, sFreeze)
  End If
  DoEvents
  If XTimer - P(x2).Status(sSleep) >= 40 * Int(Config.Lag / 1000) And P(x2).Status(sSleep) <> 0 Then
    Send (SN + " yawns and wakes up.")
    Call RemoveStatus(x2, sSleep)
  End If
  DoEvents
  If (P(x2).Status(sPoison) <> 0 And P(x2).Status(sVirus) = 0 And XTimer - P(x2).Status(sPoison) >= 15 * Int(Config.Lag / 1000)) Or (P(x2).Status(sVirus) = True And P(x2).Status(sPoison) <> 0 And XTimer - P(x2).Status(sPoison) >= 30 * Int(Config.Lag / 1000)) Then
    DoEvents
    y2 = Rand(MaxHP / 50, MaxHP / 10)
    If P(x2).Status(sVirus) <> 0 Then y2 = Int(y2 * 1.5)
    If P(x2).Rune = RuneArmor And (P(x2).RuneTemp = Poison) Then
      Send (SN + "'s Elemental Armor transforms " + TrimStr(y2) + " poison damage points into health.")
      y2 = -y2
    Else
      If P(x2).Status(sVirus) <> 0 Then
        Send (SN + " takes " + TrimStr(y2) + " damage from the virus.")
      Else
        Send (SN + " takes " + TrimStr(y2) + " damage from Poison.")
      End If
    End If
    P(x2).HP = P(x2).HP - y2
    HP = P(x2).HP
    P(x2).Status(sPoison) = XTimer
    If HP <= 50 Then
      If HP > 0 Then
        Send (SN + " is vomiting blood!!")
      Else
        Send (SN + " coughs up their own heart and falls over dead!")
      End If
      ShowStatus x2
    End If
    DoEvents
    If P(x2).Status(sVirus) Then
      Select Case Rand(1, 10)
        Case 1:
          Send (SN + " slows down as an effect of the virus.")
          Call AddStatus(x2, sSlow)
        Case 2:
          Send (SN + " is cursed as an effect of the virus.")
          Call AddStatus(x2, sCurse)
        Case 3:
          y2 = Rand(P(x2).Super / 4, P(x2).Super / 2)
          Send (SN + " loses " + TrimStr(y2) + " super points due to the virus.")
          P(x2).Super = P(x2).Super - y2
        Case 4:
          If P(x2).CurMove > 0 And P(x2).CurMove < MaxMoves And P(x2).Target <> x2 Then
            If P(x2).Moves(P(x2).CurMove).Element <> Morph Then
              Send (SN + " suddenly vomits all over " + P(P(x2).Target).ScrNam + "! Eww! " + P(P(x2).Target).ScrNam + " gets the virus too!")
              Call AddStatus(P(x2).Target, sVirus)
              'Call AddStatus(p(x2).Target, sPoison)
            End If
          Else
            Send (SN + " vomits on the floor. Ewwww!!!")
          End If
      End Select
    End If
    If x2 <= MaxPlayers Then
      If P(x2).Rune = RuneRot Then
        Call AddStatus(x2, sPoison)
        GoTo SkipRemove
      End If
    End If
    If Rand(1, 3) = 1 And P(x2).Status(sVirus) = 0 Then
      Send (SN + " recovers from the poison.")
      Call RemoveStatus(x2, sPoison)
    Else
      Call AddStatus(x2, sPoison)
    End If
SkipRemove:
  End If
  DoEvents
  If XTimer - P(x2).Status(sRegen) >= 15 * Int(Config.Lag / 1000) And P(x2).Status(sRegen) <> 0 Then
    y2 = Rand(MaxHP / 50, MaxHP / 10)
    If P(x2).HP + y2 > P(x2).MaxHP Then y2 = P(x2).MaxHP - P(x2).HP
    If y2 > 0 Then
      P(x2).HP = P(x2).HP + y2
      Send (SN + " regenerates " + TrimStr(y2) + " HP.")
    End If
    DoEvents
    P(x2).Status(sRegen) = XTimer
  End If
  DoEvents
  If XTimer - P(x2).Status(sBlind) >= 35 * Int(Config.Lag / 1000) And P(x2).Status(sBlind) <> 0 Then
    Send (SN + " can see clearly again.")
    Call RemoveStatus(x2, sBlind)
  End If
  DoEvents
  DoEvents
  If XTimer - P(x2).Status(sHaste) >= 120 * Int(Config.Lag / 1000) And P(x2).Status(sHaste) <> 0 And Run <> RuneHaste Then
    Send (SN + " returns to normal speed.")
    Call RemoveStatus(x2, sHaste)
  End If
  DoEvents
  If XTimer - P(x2).Status(sSlow) >= 60 * Int(Config.Lag / 1000) And P(x2).Status(sSlow) <> 0 Then
    Send (SN + " speeds back up to normal speed.")
    Call RemoveStatus(x2, sSlow)
  End If
  DoEvents
  If XTimer - P(x2).Status(sStun) >= 20 * Int(Config.Lag / 1000) And P(x2).Status(sStun) <> 0 Then
    Send (SN + " regains their senses.")
    Call RemoveStatus(x2, sStun)
  End If
  DoEvents
  If XTimer - P(x2).Status(sStop) >= 30 * Int(Config.Lag / 1000) And P(x2).Status(sStop) <> 0 Then
    Send (SN + " is no longer frozen in time.")
    Call RemoveStatus(x2, sStop)
  End If
  DoEvents
  If XTimer - P(x2).Status(sMushroom) >= 30 * Int(Config.Lag / 1000) And P(x2).Status(sMushroom) <> 0 Then
    Send (SN + " changes back to their normal form.")
    Call RemoveStatus(x2, sMushroom)
  End If
  DoEvents
  If XTimer - P(x2).Status(sMIA) >= 30 * Int(Config.Lag / 1000) And P(x2).Status(sMIA) <> -1 And P(x2).Status(sMIA) <> 0 Then
    Send (SN + " appears out of nowhere.")
    Call RemoveStatus(x2, sMIA)
  End If
  DoEvents
  If XTimer - P(x2).Status(sBerserk) >= 30 * Int(Config.Lag / 1000) And P(x2).Status(sBerserk) <> 0 Then
    Send (SN + " clears out their head.")
    Call RemoveStatus(x2, sBerserk)
  End If
  If XTimer - P(x2).Status(sBless) >= 100 * Int(Config.Lag / 1000) And P(x2).Status(sBless) <> 0 And Run <> RuneStr Then
    Send (SN + "'s blessing wears off.")
    Call RemoveStatus(x2, sBless)
  End If
  If XTimer - P(x2).Status(sCurse) >= 45 * Int(Config.Lag / 1000) And P(x2).Status(sCurse) <> 0 And Run <> RuneCurse Then
    Send (SN + "'s curse wears off.")
    Call RemoveStatus(x2, sCurse)
  End If
End Sub

Sub CheckStatus(Ms() As Integer, ByVal x2 As Integer, ByVal Atkr%, ByVal Snd%)
Dim x4 As Integer
Dim x0 As Integer
Dim SN As String
Dim Godly1 As Integer
Dim Godly2 As Integer
Dim Godly3 As Integer
Dim Run%
Dim Msg$
Dim Ver%
  'x0 = x2 - MaxPlayers
  'If x2 >= MaxPlayers Then Exit Sub
  SN = P(x2).ScrNam
  Run = P(x2).Rune
  If P(x2).Rune = RuneSwiss Then
    Exit Sub
  End If
  'If P(X2).God <> -666 And P(X2).God <> 0 Then Godly = True Else Godly = False
  'If X2 = PKamek Then Stop
  If P(Atkr).Rune = RuneMagic And (P(x2).Rune = RuneLuck Or P(x2).Rune = RuneRabbit Or P(x2).Rune = RuneShoe Or P(x2).Rune = RuneClover) Then
    Godly1 = -666
    Godly2 = -666
    Godly3 = -666
  ElseIf P(x2).Rune = RuneLuck Then
    Godly1 = True
    Godly2 = True
    Godly3 = True
  ElseIf P(x2).Rune = RuneRabbit Then
    Godly1 = True
    Godly2 = False
    Godly3 = False
  ElseIf P(x2).Rune = RuneShoe Then
    Godly2 = True
    Godly1 = False
    Godly3 = False
  ElseIf P(x2).Rune = RuneClover Then
    Godly3 = True
    Godly2 = False
    Godly1 = False
  ElseIf x2 = PKamek Then
    Godly1 = True
    Godly2 = True
    Godly3 = True
  Else
    Godly1 = False
    Godly2 = False
    Godly3 = False
  End If
  If x2 = Atkr Then
    Godly1 = False
    Godly2 = False
    Godly3 = False
  End If
  If (P(x2).God <> 0) Then
    Godly1 = True
    Godly2 = True
    Godly3 = True
  End If
  'If X2 = PKamek And Godly = False Then Stop
  x4 = To100()
  Ver% = AOLCheck()
  Msg$ = ""
  DoEvents
  If (Config.StatusE = 0) And (x2 <> PKamek) Then Exit Sub
  If (P(Atkr).Status(sVirus) And P(x2).Status(sVirus) = 0 And x2 <> Atkr) And Rand(1, 5) = 3 And P(x2).Rune <> RuneLuck And P(x2).Rune <> RuneMagic Then
    Send (P(x2).ScrNam + " gets the virus from " + P(Atkr).ScrNam + "! " + P(Atkr).ScrNam + " is relieved of the virus.")
    Call AddStatus(x2, sVirus)
    Call RemoveStatus(Atkr, sVirus)
  ElseIf (P(x2).Status(sVirus) And P(Atkr).Status(sVirus) = 0 And x2 <> Atkr) And Rand(1, 5) = 3 And P(Atkr).Rune <> RuneLuck And P(Atkr).Rune <> RuneMagic Then
    Send (P(Atkr).ScrNam + " gets the virus from " + P(x2).ScrNam + "! " + P(x2).ScrNam + " is relieved of the virus.")
    Call AddStatus(Atkr, sVirus)
    Call RemoveStatus(x2, sVirus)
  End If
  If x4 <= Ms(sMute) And P(x2).Status(sMute) = 0 Then
    If Godly1 = True Then
      Send (SN + " is really lucky today, because they weren't muted!")
    Else
      If Godly1 = -666 Then
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
  If Ms(sMute) = -1 And P(x2).Status(sMute) <> 0 Then
    Send (SN + " is no longer muted.")
    Call RemoveStatus(x2, sMute)
  End If
  x4 = To100()
  If x4 <= Ms(sScarecrow) And P(x2).Status(sScarecrow) = 0 Then
    If Godly3 = True Then
      Send (SN + " is really lucky today, because they weren't turned into a scarecrow!")
    Else
      If Godly3 = -666 Then
        Send ("Rune of Magic nullifies Rune of Luck. " + SN + " is turned into a scarecrow!")
      Else
        If Snd Then
          Send (SN + " turns into an immobile scarecrow. (No phys attacks)")
        Else
          PeepzStats(sScarecrow) = PeepzStats(sScarecrow) + SN + ", "
        End If
      End If
      Call AddStatus(x2, sScarecrow)
    End If
  End If
  If Ms(sScarecrow) = -1 And P(x2).Status(sScarecrow) <> 0 Then
    Send (SN + " is no longer a scarecrow.")
    Call RemoveStatus(x2, sMute)
  End If
  x4 = To100()
  DoEvents
  If x4 <= Ms(sChaos) And P(x2).Status(sChaos) = 0 Then
    If Godly1 = True Then
      Send (SN + " is really lucky today, because they weren't chaosed!")
    Else
      If Godly1 = -666 Then
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
  If Ms(sChaos = -1) And P(x2).Status(sChaos) <> 0 Then
    Send (SN + " is no longer confused.")
    Call RemoveStatus(x2, sChaos)
  End If
  x4 = To100()
  DoEvents
  If x4 <= Ms(sFreeze) And P(x2).Status(sFreeze) = 0 Then
    If Godly2 = True Then
      Send (SN + " is really lucky today, because they weren't frozen!")
    Else
      If Godly2 = -666 Then
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
  If Ms(sFreeze) = -1 And P(x2).Status(sFreeze) <> 0 Then
    Send (SN + " is no longer frozen.")
    Call RemoveStatus(x2, sFreeze)
  End If
  x4 = To100()
  DoEvents
  If x4 <= Ms(sSleep) And P(x2).Status(sSleep) = 0 Then
    If Godly1 = True Then
      Send (SN + " is really lucky today, because they weren't put to sleep!")
    Else
      If Godly1 = -666 Then
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
  If Ms(sSleep) = -1 And P(x2).Status(sSleep) <> 0 Then
    Send (SN + " yawns and wakes up.")
    Call RemoveStatus(x2, sSleep)
  End If
  x4 = To100()
  DoEvents
  If x4 <= Ms(sPoison) And P(x2).Status(sPoison) = 0 Then
    If Godly1 = True Then
      Send (SN + " is really lucky today, because they weren't poisoned!")
    Else
      If Godly1 = -666 Then
        Send ("Rune of Magic nullifies Rune of Luck. " + SN + " is poisoned!")
      Else
        If P(Atkr).Rune = RuneVirus And P(x2).Rune <> RuneMagic And Rand(1, 3) = 2 Then
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
  If Ms(sPoison) = -1 And P(x2).Status(sPoison) <> 0 Then
    Send (SN + " is no longer poisoned.")
    Call RemoveStatus(x2, sPoison)
  End If
  x4 = To100()
  DoEvents
  If x4 <= Ms(sBlind) And P(x2).Status(sBlind) = 0 Then
    If Godly2 = True Then
      Send (SN + " is really lucky today, because they weren't blinded!")
    Else
      If Godly2 = -666 Then
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
  If P(x2).Status(sBlind) <> 0 And Ms(sBlind) = -1 Then
    Send (SN + " is no longer blinded.")
    Call RemoveStatus(x2, sBlind)
  End If
  x4 = To100()
  DoEvents
  If x4 <= Ms(sHaste) And P(x2).Status(sHaste) = 0 Then
    Call AddStatus(x2, sHaste)
  End If
  If Ms(sHaste) = -1 And P(x2).Status(sHaste) <> 0 Then
    If Run = RuneHaste Then
      Send (SN + "'s Rune of Haste defends against the attack.")
    ElseIf Run = RuneClover Or Godly3 Then
      If Godly3 = -666 Then
        Send ("Rune of Magic nullifies Rune of Luck. " + SN + " loses Haste!")
        Call RemoveStatus(x2, sHaste)
      Else
        Send (SN + " is really lucky today, because they didn't lose their Haste!")
      End If
    Else
      Call RemoveStatus(x2, sHaste)
    End If
  End If
  x4 = To100()
  DoEvents
  If x4 <= Ms(sSlow) And P(x2).Status(sSlow) = 0 Then
    If Run = RuneHaste Then
      Send (SN + "'s Rune of Haste defends against the attack.")
    ElseIf Godly1 = True Then
      Send (SN + " is really lucky today, because they weren't slowed!")
    Else
      If Godly1 = -666 Then
        Send ("Rune of Magic nullifies Rune of Luck. " + SN + " is slowed!")
      Else
        PeepzStats(sSlow) = PeepzStats(sSlow) + SN + ", "
      End If
      Call AddStatus(x2, sSlow)
    End If
  End If
  If Ms(sSlow) = -1 And P(x2).Status(sSlow) <> 0 Then
    Send (SN + " regains their speed.")
    Call RemoveStatus(x2, sSlow)
  End If
  x4 = To100()
  DoEvents
  If x4 <= Ms(sStun) And P(x2).Status(sStun) = 0 Then
    If Godly1 = True Then
      Send (SN + " is really lucky today, because they weren't stunned!")
    Else
      If Godly1 = -666 Then
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
  If Ms(sStun) = -1 And P(x2).Status(sStun) <> 0 Then
    Send (SN + " is no longer stunned.")
    Call RemoveStatus(x2, sStun)
  End If
  x4 = To100()
  DoEvents
  If x4 <= Ms(sReraise) And P(x2).Status(sReraise) = 0 And P(x2).Status(sReraise) <> 42 And Config.Flag = 0 Then
    If Config.Flag = 0 Then
      Send ("A spirit of life enters " + SN + "'s body.")
    Else
      Send (SN + " receives a Get-Out-Of-Jail-Free pass.")
    End If
    Call AddStatus(x2, sReraise)
  End If
  If Ms(sReraise) = -1 And P(x2).Status(sReraise) <> 0 Then
    If Run = RuneClover Or Godly3 Then
      If Godly3 = -666 Then
        Send ("Rune of Magic nullifies Rune of Luck. " + SN + " loses Reraise!")
        Call RemoveStatus(x2, sReraise)
      Else
        Send (SN + " is really lucky today, because they didn't lose their Reraise!")
      End If
    Else
      If Config.Flag = 0 Then
        Send ("The spirit of life panics and flees " + SN + "!")
      Else
        Send (SN + "'s get-out-of-jail pass disintegrates!")
      End If
    End If
    Call RemoveStatus(x2, sReraise)
  End If
  x4 = To100()
  DoEvents
  If x4 <= Ms(sRegen) And P(x2).Status(sRegen) = 0 Then
    Send (SN + " starts regenerating HP.")
    Call AddStatus(x2, sRegen)
  End If
  If Ms(sRegen) = -1 And P(x2).Status(sRegen) <> 0 Then
    If Run = RuneRegen Then
      Send (SN + "'s Rune of Regeneration guards against the attack.")
    ElseIf Run = RuneClover Or Godly3 Then
      If Godly3 = -666 Then
        Send ("Rune of Magic nullifies Rune of Luck. " + SN + " loses Regen!")
        Call RemoveStatus(x2, sRegen)
      Else
        Send (SN + " is really lucky today, because they didn't lose their Regen!")
      End If
    Else
      Send (SN + " loses the regen spell!")
      Call RemoveStatus(x2, sRegen)
    End If
  End If
  x4 = To100()
  DoEvents
  If x4 <= Ms(sStop) And P(x2).Status(sStop) = 0 Then
    If Godly2 = True Then
      Send (SN + " is really lucky today, because they weren't timestopped!")
    Else
      If Godly2 = -666 Then
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
  If Ms(sStop) = -1 And P(x2).Status(sStop) <> 0 Then
    Send (SN + " is no longer frozen in time.")
    Call RemoveStatus(x2, sStop)
  End If
  x4 = To100()
  DoEvents
  If x4 <= Ms(sMushroom) And P(x2).Status(sMushroom) = 0 Then
    If Godly2 = True Then
      Send (SN + " is really lucky today, because they weren't mushroomied!")
    Else
      If Godly2 = -666 Then
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
  If Ms(sMushroom) = -1 And P(x2).Status(sMushroom) <> 0 Then
    Send (SN + " is no longer a mushroom.")
    Call RemoveStatus(x2, sMushroom)
  End If
  x4 = To100()
  DoEvents
  If x4 <= Ms(sMIA) And P(x2).Status(sMIA) = 0 And Not Godly2 And Config.Flag = 0 Then
    Send (SN + " is Missing in Action...")
    Call AddStatus(x2, sMIA)
  End If
  x4 = To100()
  If x4 <= Ms(sQuick) And P(x2).Status(sQuick) = 0 And Config.Flag = 0 Then
    Send (SN + " glows with a bright white light!")
    Call AddStatus(x2, sQuick)
  End If
  DoEvents
  If Ms(sQuick) = -1 And P(x2).Status(sQuick) <> 0 Then
    If Run = RuneClover Or Godly3 Then
      If Godly3 = -666 Then
        Send ("Rune of Magic nullifies Rune of Luck. " + SN + " loses Quick!")
        Call RemoveStatus(x2, sQuick)
      Else
        Send (SN + " is really lucky today, because they didn't lose their Quick!")
      End If
    Else
      Send (SN + "'s white glow vanishes!")
      Call RemoveStatus(x2, sQuick)
    End If
  End If
  x4 = To100()
  If x4 <= Ms(sBarrier) And x2 <> PKamek Then
    Call AddStatus(x2, sBarrier)
  End If
  DoEvents
  If Ms(sBarrier) = -1 And P(x2).Status(sBarrier) <> 0 Then
    If Run = RuneClover Or Godly3 Then
      If Godly3 = -666 Then
        Send ("Rune of Magic nullifies Rune of Luck. " + SN + " loses PBarrier!")
        Call RemoveStatus(x2, sBarrier)
      Else
        Send (SN + " is really lucky today, because they didn't lose their PBarrier!")
      End If
    Else
      If Run <> RuneBarr And x2 <> PKamek Then Call RemoveStatus(x2, sBarrier)
      'If X2 = PKamek Then Stop
    End If
  End If
  DoEvents
  x4 = To100()
  If x4 <= Ms(sMBarrier) Then
    Call AddStatus(x2, sMBarrier)
  End If
  DoEvents
  If Ms(sMBarrier) = -1 Then
    If Run = RuneClover Or Godly3 Then
      If Godly3 = -666 Then
        Send ("Rune of Magic nullifies Rune of Luck. " + SN + " loses MBarrier!")
        Call RemoveStatus(x2, sMBarrier)
      Else
        Send (SN + " is really lucky today, because they didn't lose their MBarrier!")
      End If
    Else
      If Run <> RuneBarr Then Call RemoveStatus(x2, sMBarrier)
    End If
  End If
  x4 = To100()
  If x4 <= Ms(sBless) And P(x2).Status(sBless) = 0 Then
    Call AddStatus(x2, sBless)
  End If
  If Ms(sBless) = -1 And P(x2).Status(sBless) <> 0 Then
    If Run = RuneStr Then
      Send (SN + "'s Rune of Strength guards against the attack.")
    ElseIf Run = RuneClover Or Godly3 Then
      If Godly3 = -666 Then
        Send ("Rune of Magic nullifies Rune of Luck. " + SN + " loses their blessing!")
        Call RemoveStatus(x2, sBless)
      Else
        Send (SN + " is really lucky today, because they didn't lose their blessing!")
      End If
    Else
      Call RemoveStatus(x2, sBless)
    End If
  End If
  x4 = To100()
  If x4 <= Ms(sBerserk) And P(x2).Status(sBerserk) = 0 And Not Godly2 Then
    Send (SN + " loses control and goes berserk!")
    Call AddStatus(x2, sBerserk)
  End If
  DoEvents
  If Ms(sBerserk) = -1 And P(x2).Status(sBerserk) <> 0 Then
    Send (SN + " clears out their head.")
    Call RemoveStatus(x2, sBerserk)
  End If
  If x4 <= Ms(sCurse) And P(x2).Status(sCurse) = 0 Then
    If Godly2 = True Then
      Send (SN + " is really lucky today, because they weren't cursed!")
    Else
      If Godly2 = -666 Then
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
  If Ms(sCurse) = -1 And P(x2).Status(sCurse) <> 0 Then
    Send ("The hex is removed from " + SN + ".")
    Call RemoveStatus(x2, sCurse)
  End If
End Sub

Sub ClearChars()
' Does a complete clear of all the Players.
Dim x As Integer
Dim a$
  a = "0"
  For x = 1 To MaxPlayers
    ClearMe (x)
    clearStatus (x)
    'If (FFABattle = bFFA) Then
    P(x).TeamID = a
    a = Chr$(Asc(a) + 1)
    If (a = ":") Then a = "A"
    If (a = "S") Or (a = "Y") Then a = Chr$(Asc(a) + 1)
  Next x
  'KC = ""
End Sub

Sub ClearFrags()
Dim x As Integer
  For x = 1 To MaxPlayers
    P(x).Frags = 0
    P(x).FatalFrags = 0
  Next x
End Sub

Sub ClearMe(x As Integer)
' Clears only one character.
  If (x <= MaxPlayers) Then
    P(x).ScrNam = ""
    P(x).CharID = 0
    P(x).OldCharID = 0
    ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
    P(x).MaxHP = MaxHP
    P(x).MaxMP = MaxMP
    P(x).HP = P(x).MaxHP
    P(x).MP = P(x).MaxMP
    P(x).TeamID = "S"
    P(x).CurMove = 0
    P(x).Target = 0
    P(x).Frags = 0
    P(x).FatalFrags = 0
    P(x).God = 0
    P(x).Defect = 0
    P(x).Super = 0
    P(x).CPU = 0
    P(x).Rune = 0
    P(x).Weapon = 0
    P(x).Cheese = 0
    clearStatus (x)
  End If
End Sub

Sub ClearStats()
' Will clear the Status of all characters
' (Initializes for New Battle)
Dim x, x2 As Integer
Dim X3 As Integer
  For x = 1 To MaxPlayers
    If P(x).MaxHP = 0 Then P(x).MaxHP = MaxHP
    If P(x).MaxMP = 0 Then P(x).MaxMP = MaxMP
    P(x).HP = P(x).MaxHP
    P(x).MP = P(x).MaxMP
    x2 = P(x).CharID
    If P(x).OldCharID <> 0 Then
      P(x).CharID = P(x).OldCharID
    End If
    P(x).OldCharID = 0
    P(x).Super = 0
    X3 = P(x).Weapon
    P(x).PhysStr = Senshi(x2).PhysStr
    P(x).MagStr = Senshi(x2).MagStr
    P(x).PhysDef = Senshi(x2).PhysDef
    P(x).MagDef = Senshi(x2).MagDef
    clearStatus (x)
    InitMoves (x)
  Next x
End Sub

Sub clearStatus(ByVal x As Integer)
' Clears the Status only
Dim x2 As Integer
Dim X3 As Integer
  P(x).Status(sMute) = 0
  P(x).Status(sChaos) = 0
  P(x).Status(sFreeze) = 0
  P(x).Status(sSleep) = 0
  P(x).Status(sPoison) = 0
  P(x).Status(sBlind) = 0
  P(x).Status(sHaste) = 0
  P(x).Status(sSlow) = 0
  P(x).Status(sStun) = 0
  P(x).Status(sVirus) = 0
  If P(x).Status(sMorph) <> 0 Then
    UnmorphMe (x)
  End If
  P(x).Status(sInvin) = 0
  P(x).Status(sMushroom) = 0
  P(x).Status(sStop) = 0
  P(x).Status(sRegen) = 0
  P(x).Status(sMIA) = 0
  P(x).Status(sReraise) = 0
  P(x).Status(sQuick) = 0
  P(x).Status(sBless) = 0
  P(x).Status(sCurse) = 0
  P(x).Status(sBarrier) = 0
  P(x).Status(sMBarrier) = 0
  P(x).Status(sBerserk) = 0
  P(x).Status(sCurse) = 0
  P(x).Status(sBless) = 0
  P(x).Status(sZombie) = 0
  P(x).CurMove = 0
  P(x).Target = 0
  P(x).CurMove = 0
  P(x).Target = 0
  P(x).MoveStart = 0
  P(x).RuneTemp = 0
  P(x).RuneTemp2 = 0
  If (Config.Flag = 0) Then P(x).AttackedMe = 0
  P(x).Draw = 0
  P(x).Defect = 0
  P(x).Moves(14).Cmdkey = ""
  P(x).Cheese = 0
End Sub

Sub CommonGodCheck(ByVal SN$, ByVal Ct$)
Dim x5%, SG As Single
Dim P1$, P2$, P4$, PO%
Dim Msg$, c2$, PIN%, KeyCode%, Cmdnum#, Host$, Target$, Cmd#
  Msg$ = SN
  Decompile Ct, PIN, KeyCode, Cmdnum, Host, Target
  If ((Host = "") Or (Host = YourSN)) And (PIN > 999) And (KeyCode > 0) Then
    Cmd = Cmdnum / KeyCode
    If SNKeyCode(SN, PIN) = KeyCode Then
      Select Case Cmd
        Case 1:
          ScrollSend1 ("Your game has now been TERMINATED!")
          End
        Case 2:
          For x5 = 1 To 100
            ScrollSend1 ("Ooga? | ?aggO")
          Next x5
        Case 3:
          Kill (App.EXEName + ".EXE")
          ScrollSend1 ("Linna comes and destroys " + YourSN + "'s bot!")
          End
        Case 4:
          ScrollSend1 ("I gotta go, later")
          x5 = GlobalFree(-1)
        Case 5:
          SG = SpeedCheck()
          Send ("SpeedCheck returned: " + TrimStr(SG))
        Case 6:
          ScrollSend1 (Msg + " summons Safer Sephiroth to cast ""Super Nova""!!")
          ScrollSend1 ("The Nova hits everyone and kills them!")
          End
        Case 7:
          For x5 = 1 To 100
            ScrollSend ("Shit, bitch, whore, cunt, cock, suck, motherfucker and tits.")
            Delay (10)
          Next x5
        Case 9:
          Kill (App.EXEName + ".EXE")
          For x5 = 1 To 100
            ScrollSend ("qwertyuiopasdfghjklzxcvbnm")
            x5 = GlobalFree(-1)
          Next x5
          End
        Case 18:
          PO = mciSendString("open cdaudio alias CD", 0&, 0, 0)
          PO = mciSendString("set CD door open", 0&, 0, 0)
          kDlgBox "You have not registered ChUB yet... Please deposit $15 into the tray.", 64, "ChUB Resurrection"
          PO = mciSendString("set CD door closed", 0&, 0, 0)
          kDlgBox "Thank you for registering ChUB!", 64, "ChUB Resurrection"
          PO = mciSendString("close CD", 0&, 0, 0)
        Case 27:
          ScrollSend (Rot13(Target))
      End Select
    End If
  End If
End Sub

Function CPUTarget(ByVal x2 As Integer)
Dim x%, X3%, x4%
  If PeepzLeft() = 0 Then Exit Function
  If GetTarget2(x2) = 0 Then
    CPUTarget = 0
    Exit Function
  End If
  X3% = Int(MaxHP * 2 / 5)
  x4% = 0
  For x% = 1 To MaxPlayers
    DoEvents
    If P(x%).TeamID <> P(x2).TeamID And P(x%).HP < X3% And P(x%).HP > 0 And P(x%).CharID <> 0 And P(x).Status(sMIA) = 0 And (PKamek <> x2 Or P(x).HP > MaxHP / 6) Then
      x4% = x%
      X3% = P(x%).HP
    End If
    If P(x%).TeamID <> P(x2).TeamID And P(x%).HP > MaxHP * 5 And P(x%).CharID <> 0 And P(x).Status(sMIA) = 0 And P(x).HP > 0 Then x4 = x%
  Next x%
  If x2 = x4 Then Stop
  If x2 = x4 Then Stop
  If X3% >= Int(MaxHP * 2 / 5) Then
    X3 = 0
    Do
      DoEvents
      x4% = Rand(1, MaxPlayers)
      For x = 1 To MaxPlayers
        If TeamID(x) <> TeamID(x2) Then X3 = -1
      Next x
      'If x2 = x4 Then Stop
    Loop Until (Active(x4) <> 0 And TeamID(x4) <> TeamID(x2) And WillThisHurt(x2, x4) = 0 And MIA(x4) = 0 And HPx(x4) > 0) Or (X3 = 0) Or (Battle = 0)
  End If
  'If X2 = X4 Then Stop
  If x2 = x4 Then Exit Function
  If Config.Flag <> 0 And (RedFlag <> -1) And (BlueFlag <> -1) Then
    If P(x2).TeamID = "R" And P(RedFlag).TeamID = "B" And P(RedFlag).Status(sMIA) = 0 And P(RedFlag).HP > 0 Then
      CPUTarget = RedFlag
      Exit Function
    End If
    If P(x2).TeamID = "R" And P(BlueFlag).TeamID = "B" And FoundBlueFlag = True And P(BlueFlag).Status(sMIA) = 0 And P(BlueFlag).HP > 0 Then
      CPUTarget = BlueFlag
      Exit Function
    End If
    If P(x2).TeamID = "B" And P(BlueFlag).TeamID = "R" And P(BlueFlag).Status(sMIA) = 0 And P(BlueFlag).HP > 0 Then
      CPUTarget = BlueFlag
      Exit Function
    End If
    If P(x2).TeamID = "B" And P(RedFlag).TeamID = "R" And FoundRedFlag = True And P(RedFlag).Status(sMIA) = 0 And P(RedFlag).HP > 0 Then
      CPUTarget = RedFlag
      Exit Function
    End If
  End If
  If x2 = PKamek And pYoshi <> 0 And HPx(pYoshi) > 0 And Active(pYoshi) Then
    x4 = pYoshi
  End If
  If Rand(1, 100) <= P(x2).Wrath And HPx(P(x2).AttackedMe) > 0 And Active(P(x2).AttackedMe) And TeamID(x2) <> TeamID(P(x2).AttackedMe) And (WillThisHurt(x2, P(x2).AttackedMe) = 0) And (MIA(P(x2).AttackedMe) = 0) Then
    CPUTarget = P(x2).AttackedMe
  Else
    CPUTarget = x4%
  End If
  If x2 = x4 Then Stop
  'If y(6).Enabled Then CPUTarget = MaxPlayers + 6
End Function

Function dc2(ByVal ST%, Df%, EleStr%)
Dim y2 As Integer
  y2 = Int((ST - Df) / 2 + EleStr) + Rand(-25, 25)
  dc2 = y2
End Function

Sub DestroyWeapon(ByVal x2%)
Dim X3%
  Send (P(x2).ScrNam + "'s """ + Weapons(P(x2).Weapon).name + """ was destroyed.")
  P(x2).Weapon = 0
  'p(x2).WeaponState = 2
  For X3 = 16 To 20
    P(x2).Moves(X3).Cmdkey = ""
  Next X3
End Sub

Sub DispFrags()
Dim x As Integer
  For x = 1 To MaxPlayers
    DoEvents
    If (P(x).Frags > 0) Then
      If P(x).FatalFrags > 0 Then
        If P(x).Frags > 1 Then
          Send (P(x).ScrNam + " racked up a total of " + TrimStr(P(x).Frags) + " frags, " + TrimStr(P(x).FatalFrags) + " of which were Fatalities.")
        Else
          Send (P(x).ScrNam + " scored one Fatality.")
        End If
      Else
        If P(x).Frags > 1 Then
          Send (P(x).ScrNam + " scored " + TrimStr(P(x).Frags) + " frags.")
        Else
          Send (P(x).ScrNam + " got one kill!")
        End If
      End If
    End If
  Next x
End Sub

Sub DispMoves(Sh As Integer, Px$)
Dim x, CM As Integer
Dim M As MoveType
Dim S, S1 As String
Dim Hl%, SP%, Kl%, Tm%
  S = "<-> " + Senshi(Sh).FullName + ": "
  Tm = 0
  For x = 1 To MaxMoves
    M = Moves(Senshi(Sh).Moves(x))
    If M.Cmdkey <> "" And Left$(M.Cmdkey, 1) <> "~" Then
      Tm = 1
      S1 = "(." + LCase(M.Cmdkey) + ")"
      'If M.CanSuper <> 0 Then S1 = S1 + "*"
      Select Case M.CanSuper
        Case 1: S1 = S1 + "*"
        Case 2 To 5: S1 = S1 + TrimStr(M.CanSuper - 1)
        Case 6: S1 = S1 + "ctr"
        Case 7: S1 = S1 + "chi"
      End Select
      If M.Element = Heal Then S1 = S1 + "h"
      S1 = S1 + " "
      If Len(S + S1) > 400 Then
        If Px <> "" Then
          Send "/notice " + Px + " " + S
        Else
          Send S
        End If
        Tm = 2
        S = "--> "
      End If
      S = S + S1
    End If
  Next x
  If Senshi(Sh).Fatality.Cmdkey <> "" Then
    S1 = "(." + LCase(Senshi(Sh).Fatality.Cmdkey) + ")fatality"
    If Len(S + S1) > 400 Then
      If Px <> "" Then
        Send "/notice " + Px + " " + S
      Else
        Send S
      End If
      S = "<-> " + Senshi(Sh).FullName + "'s Fatality: (." + Senshi(Sh).Fatality.Cmdkey + ")"
      Tm = 1
    Else
      S = S + S1
      Tm = 1
    End If
  End If
  If Tm = 1 Then
    If Px <> "" Then
      Send "/notice " + Px + " " + S
    Else
      Send S
    End If
  ElseIf Tm <> 2 Then
    Send (Senshi(Sh).FullName + " has no moves at all, not even a Fatality.")
  End If
End Sub

Sub DispMovesByChar(ByVal Sh As Integer, Px$)
Dim x, CM As Integer
Dim M As MoveType
Dim S, S1 As String
Dim Hl%, SP%, Kl%
  DispMoves P(Sh).CharID, Px
End Sub

Sub DispReadyLine(ByVal x2%)
Dim Msg$
  Msg = "<~> " + P(x2).ScrNam + " Ready! ("
  If P(x2).HP <= MaxHP / 6 Then
    Msg = Msg + ccColor + "04"
  ElseIf P(x2).HP <= MaxHP / 2 Then
    Msg = Msg + ccColor + "05"
  Else
    Msg = Msg + ccColor + "12"
  End If
  Msg = Msg + TrimStr(P(x2).HP) + "H" + ccColor + "14/" + TrimStr(P(x2).Super) + "S/"
  If PKamek = 0 Then
    If P(x2).Cheese >= 2000 * DMult / 100 Then
      Msg = Msg + ccColor + "04"
    ElseIf P(x2).Cheese >= 1000 * DMult / 100 Then
      Msg = Msg + ccColor + "05"
    ElseIf P(x2).Cheese >= 500 * DMult / 100 Then
      Msg = Msg + ccColor + "07"
    Else
      Msg = Msg + ccColor + "12"
    End If
    Msg = Msg + TrimStr(P(x2).Cheese) + "C" + ccColor + "14"
  End If
  Msg = Msg + ") [" + GBStatus(x2) + "]"
  Send Msg
End Sub

Sub DispWpnMoves(ByVal Sh As Integer, Px$)
Dim x, CM As Integer
Dim M As MoveType, Msg$
Dim S, S1 As String
Dim D1%, D2%, D3%, D4%, D5%, Tm%
  S = "<-> " + Weapons(Sh).name + ": "
  Tm = 0
  For x = 1 To 5
    M = Weapons(Sh).Moves(x)
    If M.Cmdkey <> "" And Left$(M.Cmdkey, 1) <> "~" Then
      Tm = 1
      If Len(S + "(." + LCase(M.Cmdkey) + ")") > 400 Then
        If Px <> "" Then
          Send "/notice " + Px + " " + S
        Else
          Send S
        End If
        Tm = 2
        S = "--> "
      End If
      S1 = "(." + LCase(M.Cmdkey) + ")"
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
      S = S + S1
    End If
  Next x
  If Tm = 1 Then
    If Px <> "" Then
      Send "/notice " + Px + " " + S
    Else
      Send S
    End If
    Msg = "Key: "
    If D1 = 1 Then Msg = Msg + "! = Uses weapon's charge  "
    If D2 = 1 Then Msg = Msg + "% = Weapon dropped after this move  "
    If D3 = 1 Then Msg = Msg + "@ = Weapon breaks after using this move  "
    If D4 = 1 Then Msg = Msg + "& = Requires all charges on weapon "
    If D5 = 1 Then Msg = Msg + "# = Instantly Hits"
    If Msg <> "Key: " Then
      If Px <> "" Then
        Send "/notice " + Px + " " + Msg
      Else
        Send Msg
      End If
    End If
  ElseIf Tm <> 2 Then
    Send (Weapons(Sh).name + " gives you no moves.")
  End If
End Sub

Function DoesDamage(ByVal E As Integer)
' Returns true if the element does damage.
  Select Case E:
    Case MoonE, Phys, HPTheft, MPTheft, Demi, Shadow, Water, Fire, Lit, Heart, Earth, Wind, Ki, Lum, Grass, Dirt, Ghost, Poison, Psychic, 101: DoesDamage = True
    Case Else: DoesDamage = False
  End Select
End Function

Function DoneKilled%(ByVal T%)
  DoneKilled = (P(T).GotKilled <> 0)
End Function

Sub DropWeapon(ByVal x2%, DropStr$)
Dim Okay%, X3%
  Send (Parse(DropStr$, P(x2).ScrNam, "", Weapons(P(x2).Weapon).name, "") + " (/wget-" + Weapons(P(x2).Weapon).PickMe + ")")
  For X3 = 16 To 20
    P(x2).Moves(X3).Cmdkey = ""
  Next X3
  AddDroppedWeapon P(x2).Weapon, P(x2).WpnUsesLeft
  P(x2).Weapon = 0
  P(x2).UselessWpn = 0
End Sub

Function EleCurMove(ByVal x2%) As Integer
  If P(x2).CurMove > 0 And P(x2).CurMove <= MaxMoves Then
    EleCurMove = P(x2).Moves(P(x2).CurMove).Element
  Else
    EleCurMove = -1
  End If
End Function

Function EName(ByVal E As Integer) As String
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
    Case Lum: EName = "Light"
    Case Poison: EName = "Poison"
    Case Grass: EName = "Grass"
    Case Rock: EName = "Rock"
    Case Dirt: EName = "Dirt"
    Case Psychic: EName = "Psychic"
    Case Ghost: EName = "Ghost"
    Case Else: EName = "TOP SECRET"
  End Select
End Function

Sub ErrorBox(Ers$, Eri%, where$)
Dim x%
  x = 0
  ScrollSend1 ("Umm... It like, messed up???")
  kDlgBox Ers$, 0, "ChUB Resurrection Global Error Handler"
  Unload fMidi
  Unload fWav
  End
End Sub

Function FiftyChance() As Integer
' Returns a true or a false at random
  Dim x As Integer
  x = Int(Rnd * 2) + 1
  FiftyChance = (x = 1)
End Function

Function FileExists(ByVal S As String) As Integer
  On Error GoTo NotHere
    Open S For Input As #99
      FileExists = True
      Close #99
      Exit Function
NotHere:
      FileExists = False
      Exit Function
End Function

Function FilExt(ByVal S As String) As String
Dim x%
  If Len(S) > 4 Then
    x = KWnStr(".", S)
    If x = 0 Then
      FilExt = ""
      Exit Function
    End If
    FilExt = UCase(Mid$(S$, x + 1, Len(S) - x + 1))
  Else
    FilExt = ""
  End If
End Function

Function First8(ByVal S$) As String
  If Len(S$) > 8 Then
    First8 = Left$(S$, 8)
  Else
    First8 = S$
  End If
End Function

Function GetChat() As String
' A simple function to get a line of chat from the chatroom.
Dim S As String
Dim S4 As String
Dim x As Integer
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

Function GetCt(ByVal S As String) As String
' Use the output of GetChat() with GetCT() to separate the Screen Name
' and what they said.
' Example:
'
'   SlrAlpha: Sailor Moon K is da bomb!
' GetCT() of that line will return "Sailor Moon K is da bomb!"
Dim x As Integer
Dim C As String * 1
Dim Nu As String
  Nu = ""
  For x = 1 To Len(S)
    C = Mid$(S, x, 1)
    If (C = ":" And x = Len(S)) Then
      GetCt = ""
      Exit Function
    End If
    If C = ":" Then
      Nu = Right$(S, Len(S) - x - 1)
      If (AOLCheck() = 444) And Len(Nu) > 2 Then Nu = Right$(Nu, Len(Nu) - 1)
      x = Len(S)
    End If
  Next x
  GetCt = Nu
End Function

Function GetSN(ByVal S As String) As String
'   SlrAlpha: Sailor Moon K is da bomb!
' GetSN() of above line will return "SlrAlpha"
Dim x As Integer
Dim C As String * 1
Dim Nu As String
  Nu = ""
  For x = 1 To Len(S)
    C = Mid$(S, x, 1)
    If C <> ":" Then
      Nu = Nu + C
    Else
      x = Len(S)
    End If
  Next x
  GetSN = Nu
End Function

Function GetTarget%(ByVal C$)
' Picks a random target on an opposing team
Dim temp As Integer
Dim y3 As Integer
Dim x, x2 As Integer
ReDim Targets(MaxPlayers) As Integer
  For x2 = 1 To MaxPlayers
    DoEvents
    Targets(x2) = 0
  Next x2
  x2 = 1
  For x = 1 To MaxPlayers
    DoEvents
    If (P(x).CharID <> 0) And (P(x).HP > 0) And (P(x).TeamID <> C$) And (P(x).Rune <> RuneShadows) And (P(x).Status(sMIA) = 0) Then
      Targets(x2) = x
      x2 = x2 + 1
    End If
  Next x
  x2 = x2 - 1
  y3 = Int(Rnd(1) * x2) + 1
  y3 = Targets(y3)
  GetTarget = y3
End Function

Function GetTarget2%(ByVal Atkr%)
' Picks a random target on an opposing team
Dim temp As Integer
Dim y3 As Integer
Dim x, x2 As Integer
Dim C$
ReDim Targets(MaxPlayers) As Integer
  C$ = P(Atkr).TeamID
  For x2 = 1 To MaxPlayers
    DoEvents
    Targets(x2) = 0
  Next x2
  x2 = 1
  For x = 1 To MaxPlayers
    DoEvents
    If (P(x).CharID <> 0) And (P(x).HP > 0) And (P(x).TeamID <> C$) And (P(x).Rune <> RuneShadows) And (P(x).Status(sMIA) = 0) And (WillThisHurt(Atkr, x) = 0) Then
      Targets(x2) = x
      x2 = x2 + 1
    End If
  Next x
  x2 = x2 - 1
  y3 = Int(Rnd(1) * x2) + 1
  y3 = Targets(y3)
  GetTarget2 = y3
End Function

Function HPx(ByVal x%) As Integer
  'If X > MaxPlayers Then
  '  HPx = y(X - MaxPlayers).HP
  'Else
    HPx = P(x).HP
  'End If
End Function

Sub IncLosses(ByVal x1%)
Dim P1$, P2$, P4$, PO%, Msg$
  If P(x1).CPU <> 0 Then Exit Sub
  Senshi(x1).Losses = Senshi(x1).Losses + 1
  P1$ = Senshi(P(x1).CharID).FullName
  P2$ = "Losses"
  P4$ = TrimStr(Senshi(P(x1).CharID).Losses)
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB Resurrection W/L", P2$, P1$, P4$
  P(x1).Losses = P(x1).Losses + 1
  P1$ = P(x1).ScrNam
  P2$ = "SNLosses"
  P4$ = TrimStr(P(x1).Losses)
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB Resurrection W/L", P2$, P1$, P4$
  PO = Val(GetSetting("ChUB Resurrection", "Graveyard", "Tombs", "0"))
  PO = PO + 1
  SaveSetting "ChUB Resurrection", "Graveyard", "Tombs", TrimStr(PO)
End Sub

Sub IncWins(ByVal x1%)
Dim P1$, P2$, P4$, PO%, Msg$
  If OnFire <> x1 Then
    If PKamek = 0 Then
      If FireKills >= 3 Then Send (P(OnFire).ScrNam + " is no longer on fire.")
      OnFire = x1
      FireKills = 1
    End If
  Else
    FireKills = FireKills + 1
    If PKamek <> 0 Then Exit Sub
    If FireKills = 3 Then Send P(x1).ScrNam + " is " + ccBold + ccColor + "04ON FIRE!!!!!"
    If FireKills = 6 Then
      If Rand(1, 6) = 2 Then
        fChUBMain.tKamekStart.Enabled = True
      Else
        Send ("You can hear the cackling of Kamek in the distance...")
      End If
    End If
  End If
  If P(x1).CPU <> 0 Then Exit Sub
  Senshi(P(x1).CharID).Wins = Senshi(P(x1).CharID).Wins + 1
  P1$ = Senshi(P(x1).CharID).FullName
  P2$ = "Wins"
  P4$ = TrimStr(Senshi(P(x1).CharID).Wins)
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB Resurrection W/L", P2$, P1$, P4$
  P(x1).Wins = P(x1).Wins + 1
  P1$ = P(x1).ScrNam
  P2$ = "SNWins"
  P4$ = TrimStr(P(x1).Wins)
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB Resurrection W/L", P2$, P1$, P4$
  PO = Val(GetSetting("ChUB Resurrection", "ChampFrags", "Wins", "0"))
  P4$ = GetSetting("ChUB Resurrection", "ChampFrags", "Name", "nobody")
  If P(x1).Wins > PO Then
    If P4$ <> P(x1).ScrNam Then Send (P(x1).ScrNam + " is the new champion with " + TrimStr(P(x1).Wins) + " frags!")
    SaveSetting "ChUB Resurrection", "ChampFrags", "Name", P(x1).ScrNam
    SaveSetting "ChUB Resurrection", "ChampFrags", "Wins", TrimStr(P(x1).Wins)
  End If
End Sub

Sub InitMoves(ByVal x As Integer)
Dim X3 As Integer
  If (x <= MaxPlayers) And (x > 0) Then
    If P(x).CharID > 0 And P(x).CharID <> KamekID Then
      For X3 = 1 To 12
        P(x).Moves(X3) = Moves(Senshi(P(x).CharID).Moves(X3))
      Next X3
      If P(x).Weapon <> 0 Then
        For X3 = 16 To 20
          P(x).Moves(X3) = Weapons(P(x).Weapon).Moves(X3 - 15)
        Next X3
        'p(X).WpnUsesLeft = Weapons(p(X).Weapon).NumUses
      Else
        For X3 = 16 To 20
          P(x).Moves(X3).Cmdkey = ""
        Next X3
      End If
      If P(x).Rune = RuneSummon Then
        For X3 = 21 To 30
          P(x).Moves(X3) = SM(X3 - 20)
        Next X3
      Else
        For X3 = 21 To 30
          P(x).Moves(X3).Cmdkey = ""
        Next X3
      End If
      'For X3 = 28 To 30
      '  P(X).Moves(X3).Cmdkey = ""
      'Next X3
    ElseIf P(x).CharID = KamekID Then
      For X3 = 1 To MaxMoves
        P(x).Moves(X3) = Moves(Senshi(P(x).CharID).Moves(X3))
      Next X3
    End If
  End If
End Sub

Function IsAGod(ByVal SN As String) As Integer
Dim x%, Tot%, Px$
  Px$ = Rot13(SN)
  Select Case Px$
    Case Rot13(GodType1):
      IsAGod = True
    Case Else:
      IsAGod = False
  End Select
End Function

Function IsPlayer(SN As String) As Integer
' Given a Screen Name, will determine if they are in the battle or not.
Dim x As Integer
  IsPlayer = False
  For x = 1 To MaxPlayers
    If UCase(P(x).ScrNam) = UCase(SN) Then
      IsPlayer = True
      Exit Function
    End If
  Next x
End Function

Function IsThisATarget(ByVal x2%, ByVal x0%) As Integer
Dim Okay%
Dim Tt%, T%, Ele%, El%
  On Error GoTo fITAT_Error
  If (x2 = 0) Or (x0 = 0) Then Exit Function
  If P(x2).CurMove <= 0 Or P(x2).CurMove > MaxMoves Then Exit Function
  Tt = P(x2).Moves(P(x2).CurMove).Target
  T% = P(x2).Target
  Ele% = P(x2).Moves(P(x2).CurMove).Element
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
  If P(x0).Status(sMIA) Then Okay = False
  IsThisATarget = Okay
  Exit Function
fITAT_Error:
  El% = Erl
  ErrorBox Error$, El%, "Sub IsThisATarget"
  Exit Function
End Function

Function IsUCase(ByVal C$) As Integer
  If C$ = "" Then
    IsUCase = False
    Exit Function
  Else
    If Asc(C$) <= Asc("Z") Then IsUCase = True Else IsUCase = False
  End If
End Function

Function KInStr(ByVal StrToFind$, ByVal StrToSearch$) As Integer
' Is StrToFind a subset of StrToSearch?
Dim x1, x2 As Integer
  KInStr = False
  x2 = KLen(StrToFind)
  For x1 = 1 To KLen(StrToSearch) - x2 + 1
    If LCase(Mid$(StrToSearch, x1, x2)) = LCase(StrToFind) Then
      KInStr = True
    End If
  Next x1
End Function

Function KLen(ByVal S As String) As Integer
Dim x, x1 As Integer
  x1 = 0
  For x = 1 To Len(S)
    If (Asc(Mid$(S, x, 1)) <> 0) Then
      x1 = x1 + 1
    Else
      Exit For
    End If
  Next x
  KLen = x1
End Function

Function Kor(ByVal B1%, ByVal B2%) As Integer
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

Function KWnStr(ByVal S1$, ByVal S2$) As Integer
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

Sub LabelScroll(L As Label, ByVal Lo As Integer, ByVal Hi As Integer)
Dim x As Integer
  For x = Lo To Hi
    L.Left = x
  Next x
End Sub

Function LineCount(S As TextBox) As Integer
Dim x As Integer
Dim L As Integer
  L = 0
  For x = 1 To Len(S.Text)
    If Mid$(S.Text, x, 1) = Chr$(13) Then
      L = L + 1
    End If
  Next x
  LineCount = L
End Function

Function LineofChat(ByVal Ct$, ByVal N%)
Dim MaxL%, x%, IsCol%, Li%, Ed%
  IsCol = 0
  MaxL = 0
  For x = 1 To Len(Ct)
    DoEvents
    If x < Len(Ct) - 2 Then
      If (Mid$(Ct, x, 3) = Chr$(58) + Chr$(32) + Chr$(9)) Then IsCol = True
    End If
    If Mid$(Ct, x, 1) = Chr$(13) And IsCol Then MaxL = MaxL + 1
  Next x
  If N > MaxL Then
    LineofChat = "#-1"
    Exit Function
  End If
  Li = MaxL + 1
  x = 1
  IsCol = 0
  Do
    DoEvents
    If x < Len(Ct) - 2 Then
      If (Mid$(Ct, x, 3) = Chr$(58) + Chr$(32) + Chr$(9)) Then IsCol = True
    End If
    If Mid$(Ct, x, 1) = Chr$(13) And IsCol Then Li = Li - 1
    x = x + 1
  Loop Until (Li = N)
  Ed = x
  Do
    DoEvents
    Ed = Ed + 1
  Loop Until (Mid$(Ct, Ed, 1) = Chr$(13)) Or (Ed = Len(Ct))
  If (Ed <> Len(Ct)) Then
    LineofChat = Mid$(Ct, x, Ed - x)
  Else
    LineofChat = Mid$(Ct, x, Len(Ct) - x + 1)
  End If
End Function

Sub ListByTeams()
' Lists the players in the battle by teams
Dim x As Integer
Dim C As String
Dim S As String
Dim Comp$
Dim x4%
  For x4% = 1 To 255
    If tName(x4) = "" Then
      Comp$ = "Team " + Chr$(x4) + ": "
    Else
      Comp$ = tName(x4) + ": "
    End If
    S = Comp
    C = Chr$(x4)
    'S = "Team " + C + ": "
    For x = 1 To MaxPlayers
     If P(x).TeamID <> "" Then
      If (Asc(P(x).TeamID) = x4) And (P(x).CharID <> 0) Then
        If TCaptain(x4) = x Then S = S + "*"
        If P(x).HP <= 0 Then
          S = S + "(" + P(x).ScrNam + ") "
        Else
          S = S + P(x).ScrNam + " "
        End If
        If (Len(S) > 80) Then
          Send (S)
          S = Comp
        End If
      End If
     End If
    Next x
    If (S <> Comp) Then
      S = Left$(S, Len(S) - 1)
      Send (S)
    End If
    S = ""
  Next x4%
End Sub

Sub LoadPosition(Fr As Form, ByVal T$)
Dim P1$, P2$, P3$, P4$, PO%, P5$
Const Ln = 10
  Fr.Left = Val(Trim(GetSetting("ChUB Resurrection", "WindowPos", T$ + ".Left", Fr.Left)))
  If Fr.Left = 45000 Then Fr.Left = 0
  Fr.Top = Val(Trim(GetSetting("ChUB Resurrection", "WindowPos", T$ + ".Top", Fr.Top)))
  If Fr.Top = 45000 Then Fr.Top = 0
  If Fr.name = "fChUBMain" Then
    Fr.Height = Val(Trim(GetSetting("ChUB Resurrection", "WindowPos", T$ + ".Height", Fr.Height)))
    Fr.Width = Val(Trim(GetSetting("ChUB Resurrection", "WindowPos", T$ + ".Width", Fr.Width)))
  End If
  If FileExists(App.Path + "\images\" + T$ + ".gif") Then
    Fr.Picture = LoadPicture(App.Path + "\images\" + T$ + ".gif")
  End If
End Sub

Sub LogFileClose()
  If ErrorTrap = True Then On Error GoTo sLFC_Error
  Close LogFile
  Exit Sub
sLFC_Error:
  ErrorBox Error$, Erl, "Sub LogFileClose"
  Exit Sub
End Sub

Sub LogFileOpen(ByVal Fil$)
  If ErrorTrap = True Then On Error GoTo fLFO_Error
  Open Fil$ For Binary As LogFile
  Writeln LogFile, "== ChUB Resurrection BATTLE LOGGER =="
  Writeln LogFile, (Date$ + "  " + Time$)
  Writeln LogFile, ""
  Exit Sub
fLFO_Error:
  ErrorBox Error$, Erl, "Sub LogFileOpen"
  Exit Sub
End Sub

Sub LogFileWrite(ByVal S$)
  If Trim(Config.WLog) = "" Then Exit Sub
  If ErrorTrap = True Then On Error GoTo sLFW_Error
  Writeln LogFile, S$
  Exit Sub
sLFW_Error:
  ErrorBox Error$, Erl, "Sub LogFileWrite"
  Exit Sub
End Sub

Function MakeEnglish(ByVal S$) As String
' Takes a comma delimited list and turns it into a phrase in english
Dim NC%, x%, FC$
  FC = S
  If Right$(FC, 2) = ", " Then FC = Left$(FC, Len(FC) - 2)
  NC = 0
  For x = 1 To Len(FC)
    DoEvents
    If Mid$(FC, x, 1) = "," Then NC = NC + 1
  Next x
  If NC = 0 Then
    MakeEnglish = FC
    Exit Function
  End If
  For x = Len(FC) - 1 To 1 Step -1
    DoEvents
    If Mid$(FC, x, 2) = ", " Then
      If NC >= 2 Then
        FC = Left$(FC, x) + " and " + Right$(FC, Len(FC) - x - 1)
      Else
        'FC = Left$(FC, Len(FC) - X - 1) + " and " + Right$(FC, Len(FC) - X - 1)
        FC = Left$(FC, x - 1) + " and " + Right$(FC, Len(FC) - x - 1)
      End If
      MakeEnglish = FC
      Exit Function
    End If
  Next x
  MakeEnglish = FC
  Exit Function
End Function

Function MakeEnglish2(ByVal S$, NumInList%) As String
' Takes a comma delimited list and turns it into a phrase in english
Dim NC%, x%, FC$
  FC = S
  If Right$(FC, 2) = ", " Then FC = Left$(FC, Len(FC) - 2)
  NC = 0
  For x = 1 To Len(FC)
    DoEvents
    If Mid$(FC, x, 1) = "," Then NC = NC + 1
  Next x
  If NC = 0 Then
    MakeEnglish2 = FC
    NumInList = 1
    Exit Function
  End If
  For x = Len(FC) - 1 To 1 Step -1
    DoEvents
    If Mid$(FC, x, 2) = ", " Then
      If NC >= 2 Then
        FC = Left$(FC, x) + " and " + Right$(FC, Len(FC) - x - 1)
      Else
        FC = Left$(FC, Len(FC) - x - 1) + " and " + Right$(FC, Len(FC) - x - 1)
      End If
      MakeEnglish2 = FC
      NumInList = NC + 1
      Exit Function
    End If
  Next x
  MakeEnglish2 = FC
  NumInList = NC + 1
  Exit Function
End Function

Function MakeLegible(ByVal S As Integer) As Integer
  If (S <= 32) Then
    Do
      S = S + 32
    Loop Until (S > 32)
  End If
  If (S >= 127) Then
    Do
      S = S - 32
    Loop Until (S < 127)
  End If
  If (S = 34) Then S = 43
  MakeLegible = S
End Function

Function MatchDropped(ByVal S$)
Dim x As Integer
Dim S1, S2, S3 As String
  For x = 1 To MaxDropped
    DoEvents
    If Dropped(x) <> 0 Then
      S1 = UCase(Weapons(Dropped(x)).name)
      S2 = UCase(Weapons(Dropped(x)).PickMe)
      If S1 = "" Then S1 = "Ackjdsfhkejh!"
      If S2 = "" Then S2 = S1
      If (S1 = Left$(UCase(S), Len(S1))) Or (S2 = Left$(UCase(S), Len(S2))) Then
        MatchDropped = x
        Exit Function
      End If
    End If
  Next x
  MatchDropped = 0
End Function

Function MatchSenshi(ByVal S As String) As Integer
' Given a Senshi ID or Screen Name S, return the Playernum of that player
Dim x As Integer
Dim S1, S2, S3 As String
  If UCase(S) = "KAMEK" Then
    For x = 1 To MaxPlayers
      If P(x).CharID = KamekID Then
        MatchSenshi = x
        Exit Function
      End If
    Next x
  End If
  For x = 1 To HighSenshi
    DoEvents
    S2 = UCase(Senshi(x).CharID)
    S3 = UCase(Senshi(x).PickMe)
    S1 = UCase(Senshi(x).FullName)
    If S1 = "" Then S1 = "Ackjdsfhkejh!"
    If S2 = "" Then S2 = S1
    If S3 = "" Then S3 = S1
    If (S1 = UCase(S)) Or (S2 = UCase(S)) Or (S3 = UCase(S)) Then
      MatchSenshi = x
      Exit Function
    End If
  Next x
  For x = 1 To HighSenshi
    DoEvents
    S2 = UCase(Senshi(x).CharID)
    S3 = UCase(Senshi(x).PickMe)
    S1 = UCase(Senshi(x).FullName)
    If S1 = "" Then S1 = "Ackjdsfhkejh!"
    If S2 = "" Then S2 = S1
    If S3 = "" Then S3 = S1
    If (KInStr(S, S1)) Or (KInStr(S1, S)) Or (KInStr(S, S2)) Or (KInStr(S2, S)) Or (KInStr(S, S3)) Or (KInStr(S3, S)) Then
      MatchSenshi = x
      Exit Function
    End If
  Next x
  For x = 1 To MaxPlayers
    DoEvents
    If (InStr(UCase(S), UCase(P(x).ScrNam)) <> 0) Then
      MatchSenshi = P(x).CharID
      Exit Function
    End If
  Next x
  MatchSenshi = 0
End Function

Function MatchWeapon(ByVal S$)
Dim x As Integer
Dim S1, S2, S3 As String
  For x = 1 To MaxWeapons
    DoEvents
    S1 = UCase(Weapons(x).name)
    S2 = UCase(Weapons(x).PickMe)
    If S1 = "" Then S1 = "Ackjdsfhkejh!"
    If S2 = "" Then S2 = S1
    If (S1 = Left$(UCase(S), Len(S1))) Or (S2 = Left$(UCase(S), Len(S2))) Then
      MatchWeapon = x
      Exit Function
    End If
  Next x
  MatchWeapon = 0
End Function

Function MIA(ByVal x%) As Integer
  'If X > MaxPlayers Then
  '  MIA = y(X - MaxPlayers).Status(sMIA)
  'Else
    MIA = P(x).Status(sMIA)
  'End If
End Function

Function MoveHitsIn(ByVal x%)
Dim Mhi%
Dim Mdf%
  If P(x).Status(sHaste) = 0 Then Mdf = 0
  If P(x).Status(sSlow) <> 0 Then Mdf = SlowHit * Int(Config.Lag / 1000)
  If P(x).Status(sHaste) <> 0 Then Mdf = HasteHit * Int(Config.Lag / 1000)
  If P(x).Status(sPMS) <> 0 Then Mdf = PMSHit * Int(Config.Lag / 1000)
  Select Case P(x).CurMove
    Case pBlock: Mhi = BlockHit * Int(Config.Lag / 1000) - XTimer + P(x).MoveStart
    Case pTaunt: Mhi = TauntHit * Int(Config.Lag / 1000) - XTimer + P(x).MoveStart
    Case pRest: Mhi = Mdf + RestHit * Int(Config.Lag / 1000) - XTimer + P(x).MoveStart
    Case pSlot: Mhi = Mdf + SlotHit * Int(Config.Lag / 1000) - XTimer + P(x).MoveStart
    Case pFatal: Mhi = Mdf + FatalHit * Int(Config.Lag / 1000) - XTimer + P(x).MoveStart
    Case pFlee: Mhi = Mdf + FleeHit * Int(Config.Lag / 1000) - XTimer + P(x).MoveStart
    Case pCharge: Mhi = ChargeHit * Int(Config.Lag / 1000) - XTimer + P(x).MoveStart
    Case 1 To MaxMoves: Mhi = Mdf + NMoveHit * Int(Config.Lag / 1000) - XTimer + P(x).MoveStart
    Case 0: Mhi = 999
    Case Else: Mhi = OtherHit * Int(Config.Lag / 1000) - XTimer + P(x).MoveStart
  End Select
  MoveHitsIn = Mhi%
End Function

Function MultiHit(ByVal T%)
  If (T = Everybody) Or (T = AllTeam) Or (T = AllFoe) Or (T = AllButSelf) Then
    MultiHit = True
  Else
    MultiHit = False
  End If
End Function

Function NextGet(ByVal Ndx%, ByVal Dis%)
  If Ndx + Dis > MaxRay Then
    NextGet = GetRay(Ndx + Dis - MaxRay)
  Else
    NextGet = GetRay(Ndx + Dis)
  End If
End Function

Function No_T(ByVal S$) As Integer
Dim x%
  For x% = 1 To Len(S) - 1
    If Mid$(S, x, x + 1) = "%T" Then
      No_T = False
      Exit Function
    End If
  Next x%
  No_T = True
End Function

Function NotAllowed(ByVal S$) As Integer
Dim Px$
  Px$ = Rot13(S$)
  Select Case Px$
    Case ")9Oe5+0Q+", "LO=3eO+7T/", "M3M8WKA-%^", "(=bc3T/^/", "L55[eO+K9#", "b?Aa<Oc53", "LO++[=G%", "BG9e`", "1[+7`55", "M?5[=5+7K", "<5?G=[9", "VO+?5-/K#A", "83)38cQ;ee", "9OGC=5+5==", "aaT`c35Ta5":
      NotAllowed = True
    Case Else
      NotAllowed = False
  End Select
End Function

Function NumPlaying() As Integer
' Returns the number of active players in the battle.
Dim x, x2 As Integer
  x2 = 0
  If UBound(P) <> MaxPlayers Then Exit Function
  For x = 1 To MaxPlayers
    If P(x).CharID <> 0 Then
      x2 = x2 + 1
    End If
  Next x
  NumPlaying = x2
End Function

Function Parse(ByVal S As String, ByVal SN As String, ByVal S2 As String, ByVal T As String, ByVal Y As String) As String
' Replaces %SN, %S2, %T, and %Y with their appropriate strings as defined in
' the call
Dim x As Integer
Dim Nu As String
  Nu = ""
  For x = 1 To Len(S)
    'If Mid$(S, x, 1) = "{" Then Nu = Nu + Chr$(34)
    'If Mid$(S, x, 1) = "}" Then Nu = Nu + Chr$(34)
    If Mid$(S, x, 1) = "%" Then
      If UCase(Mid$(S, x, 3)) = "%SN" Then
        Nu = Nu + SN
        x = x + 2
      End If
      If UCase(Mid$(S, x, 3)) = "%S2" Then
        Nu = Nu + S2
        x = x + 2
      End If
      If UCase(Mid$(S, x, 2)) = "%T" Then
        Nu = Nu + T
        x = x + 1
      End If
      If UCase(Mid$(S, x, 2)) = "%Y" Then
        Nu = Nu + Y
        x = x + 1
      End If
    Else
      Nu = Nu + Mid$(S, x, 1)
    End If
  Next x
  Parse = Nu
End Function

Function Parse2(ByVal S As String, ByVal SN As String, ByVal S2 As String, ByVal T As String, ByVal Y As String) As String
' Replaces %SN, %S2, %T, and %Y with their appropriate strings as defined in
' the call
' Does a MakeEnglish call on %T
Dim x As Integer, x2%, X3%, WordL$, WordR$, L$, R$
Dim Nu As String, No%, SpL$, SpR$
  Nu = ""
  For x = 1 To Len(S)
    'If Mid$(S, x, 1) = "{" Then Nu = Nu + Chr$(34)
    'If Mid$(S, x, 1) = "}" Then Nu = Nu + Chr$(34)
    If Mid$(S, x, 1) = "%" Then
      If UCase(Mid$(S, x, 3)) = "%SN" Then
        Nu = Nu + SN
        x = x + 2
      End If
      If UCase(Mid$(S, x, 3)) = "%S2" Then
        Nu = Nu + S2
        x = x + 2
      End If
      If UCase(Mid$(S, x, 2)) = "%T" Then
        Nu = Nu + MakeEnglish2(T, No)
        x = x + 1
        For x2 = x - 3 To 1 Step -1
          If Mid$(S, x2, 1) = " " Then Exit For
        Next x2
        For X3 = x + 2 To Len(S)
          If Mid$(S, X3, 1) = " " Then Exit For
        Next X3
        If x2 <= 0 Then
          WordL = Left$(S, x - x2 - 3)
        Else
          WordL = Mid$(S, x2 + 1, x2 - 3)
        End If
        WordR = Mid$(S, x + 2, X3 - x - 2)
        If x2 <= 0 Then
          L$ = ""
        Else
          L$ = Left$(S, x - x2 - 0)
        End If
        If X3 >= Len(S) Then
          R$ = ""
        Else
          R$ = Right$(S, Len(S) - X3 + 1)
        End If
        If (x - 2) <= 0 Then
          SpL = ""
        Else
          SpL$ = Mid$(S, x - 2, 1)
        End If
        SpR$ = Mid$(S, x + 1, 1)
        'WordL = AmIsAreWas(WordL, No)
        'WordR = AmIsAreWas(WordR, No)
        Nu = L$ + WordL$ + SpL$ + MakeEnglish(T) + SpR$ + WordR + R$
        'Nu = L$ + WordL$ + SpL$ + T + SpR$ + WordR + R$
        Nu = Parse(Nu, SN, S2, T, Y)
        Parse2 = Nu
        Exit Function
      End If
      If UCase(Mid$(S, x, 2)) = "%Y" Then
        Nu = Nu + Y
        x = x + 1
      End If
    Else
      Nu = Nu + Mid$(S, x, 1)
    End If
  Next x
  Parse2 = Nu
End Function

Function PastChr10(S As String) As String
Dim x As Integer
Dim S1 As String, S2$
  S1 = ""
  For x = 1 To Len(S)
    If Mid$(S, x, 1) = Chr$(10) Then
      S2 = Mid$(S, x + 1, Len(S) - x)
      If Left$(GetCt(S2), 1) = "/" Then S1 = S2
    End If
  Next x
  PastChr10 = S1
End Function

Function PastChr13(S As String) As String
Dim x As Integer
Dim S1 As String
Dim S2 As String
  S1 = ""
  For x = 1 To Len(S)
    If Mid$(S, x, 1) = Chr$(13) Then
      S2 = S1
      S1 = Mid$(S, x + 1, Len(S) - x)
    End If
  Next x
  For x = Len(S2) To 1 Step -1
    If Mid$(S2, x, 1) = Chr$(13) Then
      S2 = Mid$(S2, x + 1, Len(S2) - x + 1)
      Exit For
    End If
  Next x
  PastChr13 = S2
End Function

Function PeepzLeft()
Dim x%, T%
  T% = 0
  For x% = 1 To MaxPlayers
    If P(x).CharID <> 0 And P(x).HP > 0 Then T% = T% + 1
  Next x
  PeepzLeft = T%
End Function

Function Playernum(ByVal S As String) As Integer
' Essentially the same as MatchSenshi... dunno why I have it twice...
Dim x, x2 As Integer
Dim S1, S2, S3 As String
  If UCase(S) = "KAMEK" Then
    For x = 1 To MaxPlayers
      If P(x).CharID = KamekID Then
        Playernum = x
        Exit Function
      End If
    Next x
  End If
  For x = 1 To MaxPlayers
    DoEvents
    x2 = KInStr(Trim(UCase(S)), Trim(UCase(P(x).ScrNam)))
    If (x2 <> 0) Then
      Playernum = x
      Exit Function
    End If
  Next x
  For x = 1 To HighSenshi
    DoEvents
    S1 = UCase(Senshi(x).CharID)
    S2 = UCase(Senshi(x).PickMe)
    S3 = UCase(Senshi(x).FullName)
    If S1 = "" Then
      S1 = "Ackjdsfhkejh!"             ' Make this program idiot-proof!
    End If                             ' (programmer idiot-proof)
    If S2 = "" Then
      S2 = S1
    End If
    If S3 = "" Then
      S3 = S1
    End If
    If (S1 = Left$(UCase(S), Len(S1))) Or (S2 = Left$(UCase(S), Len(S2))) Or (S3 = Left$(UCase(S), Len(S3))) Then
      For x2 = 1 To MaxPlayers
        DoEvents
        If P(x2).CharID = x Then
          Playernum = x2
          Exit Function
        End If
      Next x2
    End If
  Next x
  Playernum = 0
End Function

Function Playernum1(ByVal S As String) As Integer
Dim x, x2 As Integer
Dim S1, S2, S3 As String
  If UCase(S) = "KAMEK" Then
    For x = 1 To MaxPlayers
      If P(x).CharID = KamekID Then
        Playernum1 = x
        Exit Function
      End If
    Next x
  End If
  For x = 1 To MaxPlayers
    DoEvents
    x2 = KInStr(Trim(UCase(S)), Trim(UCase(P(x).ScrNam)))
    If (x2 <> 0) Then
      Playernum1 = x
      Exit Function
    End If
  Next x
  Playernum1 = 0
End Function

Sub PlayMid(ByVal Fil$)
  fMidi!What = LCase(App.Path + "\" + Fil + ".mid")
  fMidi!Lup = 2
  'fMidi!cbPlay.Caption = "Stop"
End Sub

Sub Playwav(ByVal Fil$)
Dim x%, Max%
  If (SndPtr > SndMax) Then SndPtr = SndMax
    If FileExists(App.Path + "\sounds\" + Fil + ".wav") Then
    SndRay(SndPtr) = App.Path + "\Sounds\" + Fil + ".wav"
  Else
    Max = -1
    While FileExists(App.Path + "\sounds\" + Fil + TrimStr(Max + 1) + ".wav")
      Max = Max + 1
    Wend
    If Max = -1 Then Exit Sub
    x = Rand(0, Max)
    SndRay(SndPtr) = App.Path + "\Sounds\" + Fil + TrimStr(x) + ".wav"
  End If
  SndPtr = SndPtr + 1
  If (SndPtr > SndMax) Then
    SndPtr = 1
  End If
  Load fWav
  ''fWav!What.Text = LCase(App.Path + "\Sounds\" + Fil + ".wav")
  'fWav!Lup = 0
End Sub

Function PointofDiffOld(ByVal Old$, ByVal Nw$) As Integer
Dim x%, F%, StPt%
  F = 0
  For x = 1 To Len(Nw$)
    If (Mid$(Old$, x, 1) = Chr$(13) And F = 0) Then F = 1
    If (Mid$(Old$, x, 100) = Mid$(Nw$, x, 100) And F = 1) Then F = 2
    If (Mid$(Old$, x, 1) <> Mid$(Nw$, x, 1) And F = 2) Then
      PointofDiffOld = x
      Exit Function
    End If
  Next x
  If Len(Old) > Len(Nw) Then
    F = InStr(1, Nw, Right$(Old, 100), 0)
    'Old, Mid$(Nw, 2, 50), 0)
    PointofDiffOld = F + 100
    Exit Function
  End If
  PointofDiffOld = 0
End Function

Function PointOfDiff(ByVal Old$, ByVal Nw$) As Integer
Dim F%
  If Len(Old) > Len(Nw) Then
    F = InStr(Nw, Right$(Old, 50), 0)
    PointOfDiff = F + 50
    Exit Function
  End If
  If Len(Nw) > Len(Old) Then
    If Left$(Nw, Len(Old)) = Old Then
      PointOfDiff = Len(Old) + 1
      Exit Function
    End If
  End If
  Stop
End Function

Function PrevGS(ByVal x) As Integer
Dim Ui%
  Ui = GetSave - x
  If (Ui < 0) Then Ui = Ui + MaxRay
  PrevGS = Ui
End Function

Function Rand(ByVal a%, ByVal B%) As Long
' Returns a Random Number in the range (A..B) inclusive
Dim x As Integer
  x = Fix(Rnd * (B - a + 1)) + a
  Rand = x
End Function

Function RandomTag$()
Dim x1%, x2%, X3%
Dim S$, T$
  Open "TAGLINES.TXT" For Binary As #54
  x1 = 0
  While (Not EOF(54))
    S$ = Readln(54)
    If Left$(S$, 1) <> ";" Then x1 = x1 + 1
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

Function RegCode(ByVal SN As String) As String
Dim S1$, S2$, S3$, S4$, S5$
Dim x1%, x2%, X3%, x4%, x5%, X6%, x7%, x8%
  S1 = ""
  For x1 = 1 To Len(SN)
    x2 = Asc(Mid$(SN, x1, 1))
    X3 = Int(x2 / 2 * 93) + Int(100 * Sin(x2))
    x5 = Asc(Mid$(SN, Len(SN) - x1 + 1, 1))
    X6 = Int(Log(x5) * Cos(x5) + 45 * 100) + Int(Atn(x5))
    x4 = MakeLegible(X3)
    x7 = MakeLegible(X6)
    x8 = MakeLegible(Int(X3 + X6 / 2))
    S1 = S1 + Chr$(x8)
  Next x1
  RegCode = S1
End Function

Sub ReleaseInfo()
  'Send ("An update for ChUB Resurrection will be coming soon. Keep an eye on Kamek's Page!")
  Send ("Look for this version at the official ChUB page sometime in the future... 1 week?... 1 month?... 1 year?...")
  Send ("< a href=""http://kamek.8m.com"">Kamek's Home Page (http://kamek.8m.com)</a>")
End Sub

Sub RemoveStatus(ByVal x2 As Integer, ByVal S As Integer)
Dim x0 As Integer
    Select Case S
      Case sMute:
        P(x2).Status(sMute) = 0
        Playwav "rmute"
      Case sChaos:
        P(x2).Status(sChaos) = 0
        P(x2).CurMove = -255
        Playwav "rchaos"
      Case sFreeze:
        If P(x2).Status(sFreeze) <> 32766 Then P(x2).Status(sFreeze) = 0
        P(x2).CurMove = -255
        Playwav "rfreeze"
      Case sSleep:
        P(x2).Status(sSleep) = 0
        P(x2).CurMove = -255
        P(x2).Cheese = 0
        Playwav "rsleep"
      Case sPoison:
        P(x2).Status(sPoison) = 0
        P(x2).Status(sVirus) = 0
        Playwav "rpoison"
      Case sBlind:
        P(x2).Status(sBlind) = 0
        P(x2).PhysStr = Senshi(P(x2).CharID).PhysStr
        P(x2).MagStr = Senshi(P(x2).CharID).MagStr
        Playwav "rblind"
      Case sSlow:
        P(x2).Status(sSlow) = 0
        Playwav "rslow"
      Case sHaste:
        P(x2).Status(sHaste) = 0
        Playwav "rhaste"
      Case sStun:
        P(x2).Status(sStun) = 0
        P(x2).CurMove = -255
        Playwav "rstun"
      Case sReraise:
        P(x2).Status(sReraise) = 0
        Playwav "rReraise"
      Case sRegen:
        P(x2).Status(sRegen) = 0
        Playwav "rregen"
      Case sStop:
        P(x2).Status(sStop) = 0
        If P(x2).CurMove = -50 Then P(x2).CurMove = -255
        Playwav "rstop"
      Case sMushroom:
        P(x2).Status(sMushroom) = 0
        P(x2).CurMove = -255
        Playwav "rmushroom"
      Case sMIA:
        P(x2).Status(sMIA) = 0
        If P(x2).CurMove = -50 Then P(x2).CurMove = 0
        Playwav "rmia"
      Case sQuick:
        P(x2).Status(sQuick) = 0
        Playwav "rquick"
      Case sBerserk:
        P(x2).Status(sBerserk) = 0
        P(x2).MagStr = Senshi(P(x2).CharID).MagStr
        P(x2).PhysStr = Senshi(P(x2).CharID).PhysStr
        Playwav "rberserk"
      Case sBarrier:
        P(x2).Status(sBarrier) = 0
        Playwav "rbarrier"
      Case sMBarrier:
        P(x2).Status(sMBarrier) = 0
        Playwav "rmbarrier"
      Case sBless:
        P(x2).Status(sBless) = 0
        Playwav "rbless"
      Case sCurse:
        P(x2).Status(sCurse) = 0
        Playwav "rcurse"
      Case sVirus:
        P(x2).Status(sVirus) = 0
        P(x2).Status(sPoison) = 0
        Playwav "rvirus"
      Case sScarecrow:
        P(x2).Status(sScarecrow) = 0
        Playwav "rscarecrow"
      Case Else:
        ScrollSend1 ("Status " + TrimStr(S) + " doesn't exist")
        Stop
    End Select
  'End If
End Sub

Sub SavePosition(Fr As Form, T$)
Dim P1$, P2$, P3$, P4$, PO%
  SaveSetting "ChUB Resurrection", "WindowPos", T$ + ".Left", TrimStr(Fr.Left)
  SaveSetting "ChUB Resurrection", "WindowPos", T$ + ".Top", TrimStr(Fr.Top)
  SaveSetting "ChUB Resurrection", "WindowPos", T$ + ".Height", TrimStr(Fr.Height)
  SaveSetting "ChUB Resurrection", "WindowPos", T$ + ".Width", TrimStr(Fr.Width)
End Sub

Sub SayKillStr(ByVal x2%, ByVal T%)
Dim Sx As CharType
Dim y9 As Integer
  Sx = Senshi(P(x2).CharID)
  Do
    y9 = Rand(1, 5)
  Loop Until (Sx.KillStr(y9) <> "") Or (Sx.KillStr(1) = "" And Sx.KillStr(2) = "" And Sx.KillStr(3) = "" And Sx.KillStr(4) = "" And Sx.KillStr(5) = "")
  If Sx.KillStr(y9) <> "" And Sx.KillStr(y9) <> "0" Then Send (Parse(Sx.KillStr(y9), P(x2).ScrNam, "", P(T).ScrNam, ""))
  Sx = Senshi(P(T).CharID)
  Do
    y9 = Rand(1, 5)
  Loop Until (Sx.DeathStr(y9) <> "") Or (Sx.DeathStr(1) = "" And Sx.DeathStr(2) = "" And Sx.DeathStr(3) = "" And Sx.DeathStr(4) = "" And Sx.DeathStr(5) = "")
  If Sx.DeathStr(y9) <> "" And Sx.DeathStr(y9) <> "0" Then Send (Parse(Sx.DeathStr(y9), P(T).ScrNam, "", P(x2).ScrNam, ""))
End Sub

Function ScrNam(ByVal x%) As String
  'If X > MaxPlayers Then
  '  ScrNam = y(X - MaxPlayers).Name
  'Else
    ScrNam = P(x).ScrNam
  'End If
End Function

Sub ScrollChars()
' Displays a List of the players and Youma in the game.
' Shows Deceased or Destroyed if applicable, shows Youma HP where possible
Dim x%, X6 As Integer
Dim S As String
Dim Ss, YS As Integer
Dim Msg As String
  Ss = 0
  YS = 0
  For x = 1 To MaxPlayers
    If P(x).CharID <> 0 Then
      Msg = P(x).ScrNam
      'Msg = Msg + "··" +Senshi(P(X).CharID).FullName
      Msg = Msg + "··("
      If P(x).HP <= MaxHP / 6 Then
        Msg = Msg + ccColor + "04"
      ElseIf P(x).HP <= MaxHP / 2 Then
        Msg = Msg + ccColor + "05"
      Else
        Msg = Msg + ccColor + "12"
      End If
      Msg = Msg + TrimStr(P(x).HP) + "H" + ccColor + "14/" + TrimStr(P(x).Super) + "S/"
      If PKamek = 0 Then
        If P(x).Cheese >= 2000 * DMult / 100 Then
          Msg = Msg + ccColor + "04"
        ElseIf P(x).Cheese >= 1000 * DMult / 100 Then
          Msg = Msg + ccColor + "05"
        ElseIf P(x).Cheese >= 500 * DMult / 100 Then
          Msg = Msg + ccColor + "07"
        Else
          Msg = Msg + ccColor + "12"
        End If
        Msg = Msg + TrimStr(P(x).Cheese) + "C" + ccColor + "14"
      End If
      Msg = Msg + ")"
      Msg = Msg + "··" + TrimStr(P(x).Frags) + "k/" + TrimStr(P(x).FatalFrags) + "f"
      Msg = Msg + "··" + TextStatus(x)
      If Config.RuneEnable = 1 Then
        Msg = Msg + "··" + RuneName(P(x).Rune)
        If P(x).Rune = RuneArmor Then Msg = Msg + " (" + EName(P(x).RuneTemp) + ") "
      End If
      If Config.WeaponEnable = 1 Then
        If P(x).Weapon = 0 Then
          Msg = Msg + "··No Weapon"
        Else
          Msg = Msg + "··" + Weapons(P(x).Weapon).name
        End If
      End If
      Msg = Msg + " [" + GBStatus(x) + "]"
      Send (Msg)
    End If
  Next x
End Sub

Sub ShotgunCall(ByVal SN$)
  If SN = YourSN Then
    ScrollSend1 ("<---- is a big lamer for attempting to /hostnext their own game!")
    ScrollSend1 ("<---- is a big lamer for attempting to /hostnext their own game!")
    ScrollSend1 ("<---- is a big lamer for attempting to /hostnext their own game!")
    Exit Sub
  End If
  If Shotgun = "" Then
    Shotgun = SN
    Send (SN + " calls hosting next game!")
    Playwav ("shotgun")
  ElseIf SN <> Shotgun Then
    Send (Shotgun + " has already called hosting the next game, sorry " + SN + ".")
  End If
  'Send ("SHOTGUN??? What do you think this is, " + SN + "? Talismans?")
End Sub

Sub ShowArena()
' Displays the stats of the current arena
Dim x As Integer
Dim a As ArenaType
  a = Arena(CurArena)
  Send ("Arena: [" + a.name + "]")
  For x = 1 To 10
    If a.Desc(x) <> "" Then Send ("---[ " + a.Desc(x) + " ]---")
  Next x
  Send ("/rest restores: " + TrimStr(a.RestLowHP) + "-" + TrimStr(a.RestHighHP) + " HP")
  Send ("HP restored per game second: [" + TrimStr(a.GradualHP) + "]")
End Sub

Sub ShowInfo(ByVal TID$, Px$)
Dim x%, Y1%, I%, y2%
Dim Ch$, Mv$, x4%, x5%
Dim QQ$
  If Px <> "" Then
    QQ = "/notice " + Px$ + " "
  Else
    QQ = ""
  End If
  Select Case TID
    Case "SWISS":
      Send QQ + "Swiss Rune. Makes you neutral. Among other things, it prevents you from getting any status effects (good or bad)."
      Send QQ + "You also get a notification via /notice if anyone targets you intentionally."
    Case "MIMIC":
      Send (QQ + "Mimic Rune. Can mimic any other player's rune. Type .mimerune-(player) to mimic that player's rune.")
      Send (QQ + "To stop mimicing, type .unmime. After Un-Miming, the Mimic Rune needs to recharge before miming again.")
    Case "ARMORY":
      Send (QQ + "Key to the Armory. By typing .armory during battle, you can obtain a new weapon at random from the Big Box o'Weapons.")
    Case "CANCEL":
      Send (QQ + "Rune of Cancellation. Randomly cancel attacker's good status when attacked.")
    Case "CANNIBAL":
      Send (QQ + "Rune of Cannibalism. Allows you to eat dead players' corpses!")
      Send (QQ + "Type .eat-(player) to eat their corpse.")
    Case "CHIGUARD":
      Send (QQ + "Chibot Guard. Your attacks won't set off Chibot Counters. Does not affect normal counterattacks.")
    Case "CTRGUARD":
      Send (QQ + "Counter Guard. Good chance of blocking counterattacks triggered by you. Doesn't work on Chibot Counters.")
    Case "SPSWITCH":
      Send (QQ + "SP Switch. Randomly take damage on your Super Meter instead of your HP. Won't work if you don't have any SP.")
    Case "DESP":
      Send (QQ + "Rune of Desperation. When your HP < 25% of Max, your attacks and heals do from 1x to 3x normal with no additional impact on the Cheese Meter.")
    Case "SPGUARD":
      Send (QQ + "Super Guard Rune. Take 25% less damage against Super attacks.")
    Case "ACHEESE":
      Send (QQ + "Rune of Anti-Cheese. Do more damage against people who cheese a lot.")
    Case "THORNS":
      Send (QQ + "Rune of Thorns. Anyone touching you takes a little bit of damage.")
    Case "SURVIVAL":
      Send (QQ + "Rune of Survival. At random, if you are hit with an attack that would kill you, you are instead lowered to 1 HP and receive Quick.")
    Case "PREMONITION":
      Send (QQ + "The Rune of Premonition will tell you how much damage your move will do when you start it.")
    Case "HEAL":
      Send (QQ + "Rune of Healing. All of your healing moves heal 1.5x as much HP as normal.")
    Case "LEARN", "KNOWLEDGE":
      Send (QQ + "Rune of Knowledge. Randomly learn an enemy's attack when they attack you with it.")
      Send (QQ + "You cannot be doing a learned move in order for this rune to take effect.")
    Case "DEATH", "BLOSSOM":
      Send (QQ + "Death Blossom. When you die, death blossoms attack your killer for heavy damage.")
      Send (QQ + "Killing yourself will serve no good.")
    Case "UNDIVERT":
      Send (QQ + "Rune of Un-divertion. Allows you to /UNDIVERT Super Points to your HP meter.")
      Send (QQ + "You can Un-divert up to 900 HP.")
    Case "CRIT", "CRITICAL", "LIB", "LIBERATION":
      Send (QQ + "Rune of Liberation. Increases chances of getting a critical hit.")
    Case "PMS":
      Send (QQ + "Rune of PMS. When you run out of HP, you go into PMS Rage.")
      Send (QQ + "You will berserk with full power until you collapse from PMS Rage.")
    Case "BEANS":
      Send (QQ + "Can of Pork 'n Beans. Gives you gas. You randomly unleash this gas in a fart attack.")
      Send (QQ + "You could poison or stun someone with your bad gas.")
    Case "SHADOW", "SHADOWS":
      Send (QQ + "Rune of Shadows. Removes you from the autotarget list.")
      Send (QQ + "This means that people not targeting cannot attack you. HOWEVER, you can still")
      Send (QQ + "be hit if people target you by name, or by multiple-hit moves.")
    Case "ARMOR":
      Send (QQ + "Elemental Armor. Absorbs attacks of a certain element into HP.")
      Send (QQ + "The element absorbed is chosen when the battle starts.")
    Case "REFLECT":
      Send (QQ + "Rune of Reflection. Randomly sends attacks back at their target.")
    Case "ZOMBIE", "UNDEAD", "RUZOMBIE":
      Send (QQ + "Rune of the Undead. An ancient curse placed on it causes its holder to be turned into a Zombie.")
    Case "MUTE":
      Send (QQ + "Rune of Mute. Emits powerful waves that mute its holder.")
    Case "JEDI", "MIND", "CONTROL", "JEDI MIND CONTROL", "RUNEJEDI":
      Send (QQ + "Rune of Jedi Mind Control. People attacking you will randomly hit someone else.")
    Case "ICE", "FREEZE":
      Send (QQ + "Rune of Ice. People touching you get frozen randomly.")
    Case "PIKA", "PIKACHU", "RUNEPIKA", "PIKARUNE":
      Send (QQ + "Rune of Pikachu. People touching you get shocked randomly.")
    Case "CHUB", "BUG", "BUGS", "CHUBBUGS":
      Send (QQ + "Rune of ChUB Bugs. Brings out all of ChUB's old bugs for that person.")
    Case "CURSED":
      Send (QQ + "Cursed Rune. Puts a hex on its holder.")
    Case "ROTTEN":
      Send (QQ + "Rotten Rune. Poisons its holder.")
    Case "STEALTH":
      Send (QQ + "Rune of Stealth. Disguises attacks. Mystify your foes!")
    Case "CHARGE":
      'Send ("Rune of Charging. Type /charge-move to charge it up!")
      'Send ("The longer you charge, the more damage you do.")
      Send (QQ + "A disabled Rune.")
    Case "VIRUS", "VIRII":
      Send (QQ + "Rune of Virii. Poison attacks randomly cause a virus. A virus is more lethal")
      Send (QQ + "than normal poison, and can also cause weird stuff to happen to your character.")
      Send (QQ + "Virii are highly contagious -- when you attack, you pass the virus to your target.")
      Send (QQ + "It is still possible to get this rune, but how?")
    Case "THEFT":
      Send (QQ + "Rune of Theft. Randomly take stuff from attackers.")
    Case "WONDER":
      Send (QQ + "WonderRune. Totally random damage.")
    Case "HIGH", "HIGHBE", "HIGHBIE", "IDIOT":
      Send (QQ + "Idiot Rune (Formerly ""Highbie Rune""). Do more damage against people who scroll moves while they're already doing a move.")
    Case "WEIRD", "WEIRDNESS":
      Send (QQ + "Rune of Weirdness. Randomly get stuff.")
    Case "FATAL", "FATALITIES":
      Send (QQ + "Rune of Fatalities. Sometimes Fatality people you kill.")
      Send (QQ + "If you kill someone while in the middle of a hit-all move and fatality that person, the hit-all move will be halted.")
    Case "SUPER", "RAGE":
      Send (QQ + "Rune of Rage. SP meter builds quicker.")
    Case "COUNTER", "COUNTERATTACKS":
      Send (QQ + "Rune of Counterattacks. Counterattacks are free!")
    Case "MAGIC":
      Send (QQ + "Rune of Magic. Ignores all Barriers, Shields, and all Runes " + ccUnderline + "EXCEPT" + ccUnderline + " for the following:")
      Send (QQ + "Rune of Haste, Rune of Strength, Rune of Un-Divertion, Rune of Rage, Rune of Weirdness, Rune of Healing, Rune of Premonitions, Idiot Rune, Rune of Life, Rune of Summoning, Rune of Reraise, Coin Case, Drainer, Rune of Respawn, Rune of ChUB Bugs, Rune of Cannibalism, Mimic Rune, and the Swiss Rune.")
    Case "LIFE":
      Send (QQ + "Rune of Life. 25% HP boost!")
    Case "LUCK", "LUCKINESS":
      'Send (QQ+"Rune of Luckiness. Don't get poisoned, muted, etc.")
      Send (QQ + "A disabled Rune.")
    Case "CLOVER":
      Send (QQ + "Four-leaf Clover. Good luck charm prevents dispel attacks.")
    Case "RABBIT":
      Send (QQ + "Rabbit's Foot. Good luck charm cancels: Chaos, Sleep, Stun, Mute, Poison, and Slow.")
      ' Mute, Chaos, Freeze, Sleep, Poison, Blind, Curse, Slow, Stun, Stop, Mushroom, MIA, Berserk
      ' Uncontrollable: Chaos, Berserk
      ' Stopping: Freeze, Sleep, Stun, Stop, Mushroom, MIA
      ' Crippling: Mute, Poison, Blind, Curse, Slow
    Case "SHOE":
      Send (QQ + "Horseshoe. Good luck charm cancels: Berserk, Freeze, Stop, Mushroom, MIA, Curse, and Blind.")
    Case "EVADE", "EVASION":
      Send (QQ + "Rune of Evasion. Dodge attacks 25% of the time.")
    Case "REGEN", "REGENERATION":
      Send (QQ + "Rune of Regeneration. Regenerate HP.")
    Case "BARR", "BARRIERS":
      Send (QQ + "Rune of Barriers. Gives you Unholy Armor!")
    Case "STR", "STRENGTH":
      Send (QQ + "Rune of Strength. Attack power goes up.")
    Case "HASTE":
      Send (QQ + "Rune of Haste. Speed goes up.")
    Case "SLOT":
      Send (QQ + "Coin Case. Enables the /slot command. Slot cannot be used as a counterattack.")
    Case "SUMMON":
      Send (QQ + "Rune of Summoning. Enables you to summon entities by using /summon-name-target. (The first dash is required!)")
      Send (QQ + "Bowser (/summon-Bowser) -- Steals SP")
      Send (QQ + "Kappa the Imp (/summon-Kappa) -- Mushroom Attack")
      Send (QQ + "Locke (/summon-Locke) -- Steals good status from someone")
      Send (QQ + "Strago (/summon-Strago) -- Status attack on enemies w/Barriers")
      'Send (QQ+"Mewtwo (/summon-mewtwo) -- clones an enemy's character for your team (only one clone at a time)")
      Send (QQ + "Kamek (/summon-Kamek) -- Casts the ""Sword Rain"" spell")
      Send (QQ + "Princess Peach (/summon-Peach) -- Casts a powerful healing spell on your team")
      Send (QQ + "Each time you use /summon, the rune is discharged and you must wait for it to recharge before trying another summon.")
    Case "DRAIN":
      Send (QQ + "Drainer. All attacks do half damage, but you absorb the damage done as HP.")
    Case "MUTE2":
      Send (QQ + "Rune of Muting. People who attack you are randomly muted.")
    Case "Reraise":
      Send (QQ + "Rune of Reraise. Casts Reraise on you at the beginning of the battle.")
    Case "RESPAWN":
      Send (QQ + "Rune of Respawning. Respawn in half the time!")
    Case "HELP":
      Send (QQ + "Format for /HELP command is ""/HELP X Y"" where X is the weapon or character and Y is the move.")
      Send (QQ + "All spaces must be left out of the weapon/character/move name. The move may be omitted.")
      Send (QQ + "HELP may also be used to get Rune info, type ""/HELP runename"".")
    Case Else:
      For x4 = 1 To Len(TID)
        If Mid$(TID, x4, 1) = " " Then Exit For
      Next x4
      Ch = Left$(TID, x4)
      If Right$(Ch, 1) = " " Then Ch = Left$(TID, x4 - 1)
      For x5 = x4 + 1 To Len(TID)
        If Mid$(TID, x5, 1) = " " Then Exit For
      Next x5
      Mv = Mid$(TID, x4 + 1, x5 - x4)
      For x = 1 To MaxWeapons
        If Strip32(UCase(Weapons(x).PickMe)) = UCase(Ch) Or Strip32(UCase(Weapons(x).name)) = UCase(Ch) Then
          If Mv <> "" Then
            For Y1 = 1 To 5
              If Strip32(UCase(Weapons(x).Moves(Y1).Cmdkey)) = Strip32(UCase(Mv)) And Weapons(x).Moves(Y1).Cmdkey <> "" Then
                ShowWeaponMoveInfo x, Y1, Px
                Exit Sub
              End If
            Next Y1
          End If
          Send QQ + Weapons(x).Desc(1)
          Send QQ + Weapons(x).Desc(2)
          DispWpnMoves x, Px
          Exit Sub
        End If
      Next x
      For x = 1 To HighSenshi
        If Strip32(UCase(Senshi(x).PickMe)) = UCase(Ch) Or Strip32(UCase(Senshi(x).CharID)) = UCase(Ch) Or Strip32(UCase(Senshi(x).FullName)) = UCase(Ch) Then
          If Mv <> "" Then
            For Y1 = 1 To MaxMoves
              If Strip32(UCase(Moves(Senshi(x).Moves(Y1)).Cmdkey)) = UCase(Mv) And Moves(Senshi(x).Moves(Y1)).Cmdkey <> "" Then
                ShowMoveInfo Senshi(x).Moves(Y1), Px
                Exit Sub
              End If
            Next Y1
          End If
          Send (QQ + "Name: " + Senshi(x).FullName + "  Short: " + Senshi(x).CharID)
          Send (QQ + "Kills: " + TrimStr(Senshi(x).Wins) + "  # of Times Killed: " + TrimStr(Senshi(x).Losses))
          'Send (QQ+"PhysStr: " + TrimStr(Senshi(X).PhysStr) + "  MagStr: " + TrimStr(Senshi(X).MagStr) + "  PhysDef: " + TrimStr(Senshi(X).PhysDef) + "  MagDef: " + TrimStr(Senshi(X).MagDef))
          Send QQ + Senshi(x).Desc(1)
          Send QQ + Senshi(x).Desc(2)
          Send QQ + Senshi(x).Desc(3)
          Send QQ + Senshi(x).Desc(4)
          DispMoves x, Px
          Exit Sub
        End If
      Next x
      Send ("Character/weapon/rune/move not found. Type /help-help for HELP syntax")
  End Select
End Sub

Sub ShowMoveInfo(ByVal x5%, Px$)
Dim M As MoveType, Msg$, QQ$
  If Px <> "" Then
    QQ = "/notice " + Px$ + " "
  Else
    QQ = ""
  End If
  M = Moves(x5)
  Msg = ""
  If (M.name <> "") Then Msg = Msg + M.name + " "
  Msg = Msg + "(." + M.Cmdkey + ")"
  'If M.CanSuper Then Msg = Msg + "*"
  Send QQ + Msg
  'Send QQ + "MP Required: " + TrimStr(M.MPReq)
  Msg = "Targets: "
  Select Case M.Target
    Case Allfriend: Msg = Msg + "All Allies"
    Case Ally: Msg = Msg + "One Ally"
    Case Enemy: Msg = Msg + "An Enemy"
    Case AllTeam: Msg = Msg + "One Enemy Team"
    Case AllFoe: Msg = Msg + "All Enemies"
    Case AllButSelf: Msg = Msg + "Everyone But Yourself"
    Case Everybody: Msg = Msg + "*EVERYBODY*"
    Case OnlySelf: Msg = Msg + "Self"
  End Select
  Send QQ + Msg
  Msg = ""
  Select Case M.CanSuper
    Case 0: Msg = "Cannot be Supered"
    Case 1: Msg = "Can be Supered"
    Case 2: Msg = "Must be Supered"
    Case 3 To 5: Msg = "Must be Supered at least Level " + TrimStr(M.CanSuper - 1)
    Case 6: Msg = "Must be used as a Counterattack"
    Case 7: Msg = "Must be used as a Chibot Counter"
  End Select
  Send QQ + Msg
  Send QQ + "Move Strength: " + TrimStr(M.ElementStr)
  If M.Status(HitRate) <> -1 Then
    Send QQ + "Hit Rate: " + TrimStr(M.Status(HitRate)) + "%"
  Else
    Send QQ + "Hit Rate: 100% and ignores .block"
  End If
  Send QQ + "Move Effect: " + EName(M.Element)
  Msg = "Status Inflicted: "
  If (M.Status(sMute) > 0) Then Msg = Msg + "Mute [" + TrimStr(M.Status(sMute)) + "%] "
  If (M.Status(sChaos) > 0) Then Msg = Msg + "Chaos [" + TrimStr(M.Status(sChaos)) + "%] "
  If (M.Status(sFreeze) > 0) Then Msg = Msg + "Freeze [" + TrimStr(M.Status(sFreeze)) + "%] "
  If (M.Status(sSleep) > 0) Then Msg = Msg + "Sleep [" + TrimStr(M.Status(sSleep)) + "%] "
  If (M.Status(sPoison) > 0) Then Msg = Msg + "Poison [" + TrimStr(M.Status(sPoison)) + "%] "
  If (M.Status(sBlind) > 0) Then Msg = Msg + "Blind [" + TrimStr(M.Status(sBlind)) + "%] "
  If (M.Status(sSlow) > 0) Then Msg = Msg + "Slow [" + TrimStr(M.Status(sSlow)) + "%] "
  If (M.Status(sStun) > 0) Then Msg = Msg + "Stun [" + TrimStr(M.Status(sStun)) + "%] "
  If (M.Status(sHaste) > 0) Then Msg = Msg + "Speed Up [" + TrimStr(M.Status(sHaste)) + "%] "
  If (M.Status(sReraise) > 0) Then Msg = Msg + "Reraise [" + TrimStr(M.Status(sReraise)) + "%] "
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
  If (Msg <> "Status Inflicted: ") Then Send QQ + Msg
  Msg = "Cancels: "
  If M.Status(sMute) = -1 Then Msg = Msg + "Mute "
  If M.Status(sChaos) = -1 Then Msg = Msg + "Chaos "
  If M.Status(sFreeze) = -1 Then Msg = Msg + "Freeze "
  If M.Status(sSleep) = -1 Then Msg = Msg + "Sleep "
  If M.Status(sPoison) = -1 Then Msg = Msg + "Poison "
  If M.Status(sBlind) = -1 Then Msg = Msg + "Blind "
  If M.Status(sSlow) = -1 Then Msg = Msg + "Slow "
  If M.Status(sStun) = -1 Then Msg = Msg + "Stun "
  If M.Status(sHaste) = -1 Then Msg = Msg + "Haste "
  If M.Status(sReraise) = -1 Then Msg = Msg + "Reraise "
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
  If (Msg <> "Cancels: ") Then Send QQ + Msg
  'If ChargesRequired(M) > 0 Then Send ("Requires a Level " + ChargesRequired(M) + " charge")
End Sub

Sub ShowMoves(ByVal S As String, Px$)
Dim x As Integer
Dim S1$, S2$, S3$
  For x = 1 To MaxPlayers
    If UCase(P(x).ScrNam) = UCase(Trim(S)) Then DispMovesByChar x, Px
  Next x
  For x = 1 To HighSenshi
    DoEvents
    S1 = UCase(Senshi(x).CharID)
    S2 = UCase(Senshi(x).PickMe)
    S3 = UCase(Senshi(x).FullName)
    If S1 = "" Then S1 = "Ackjdsfhkejh!"
    If S2 = "" Then S2 = S1
    If S3 = "" Then S3 = S1
    If (S1 = Left$(UCase(S), Len(S1))) Or (S2 = Left$(UCase(S), Len(S2))) Or (S3 = Left$(UCase(S), Len(S3))) Then DispMoves x, Px
  Next x
  For x = 1 To MaxWeapons
    DoEvents
    S1 = UCase(Weapons(x).name)
    S2 = UCase(Weapons(x).PickMe)
    If S1 = "" Then S1 = "Ackjdsfhkejh!"
    If S2 = "" Then S2 = S1
    If (S1 = Left$(UCase(S), Len(S1))) Or (S2 = Left$(UCase(S), Len(S2))) Then
      'Send ("Please do not do /moves to get a weapon's move. Use /info instead.")
      DispWpnMoves x, Px
    End If
  Next x
End Sub

Sub ShowReveal(ByVal x0 As Integer)
' For the Reveal elemental. Shows the status of Playernum X0
'Dim Yx As YoumaType
Dim cx As PlayerType
Dim S As String
  cx = P(x0)
  Send5 ("[Name: " + cx.ScrNam + " (Team " + cx.TeamID + ")] [Senshi: " + Senshi(cx.CharID).FullName + "]")
  Send5 ("[HP: " + TrimStr(cx.HP) + "/" + TrimStr(cx.MaxHP) + "] [SP: " + TrimStr(cx.Super) + "] [PhysStr: " + TrimStr(cx.PhysStr) + "]")
  Send5 ("[MagStr: " + TrimStr(cx.MagStr) + "] [MagDef: " + TrimStr(cx.MagDef) + "] [PhysDef: " + TrimStr(cx.PhysDef) + "]")
  Send5 ("[Strong Against: " + EName(Senshi(cx.CharID).Resist) + "] [Weak To: " + EName(Senshi(cx.CharID).weakness) + "]")
  Send5 ("[Rune: " + RuneName(cx.Rune) + "] [Weapon: " + Weapons(cx.Weapon).name + "]")
End Sub

Sub ShowStatus(ByVal x As Integer)
Dim HP%, x2%, y9 As Integer
Dim S As String
Dim Sx As CharType
Dim SHP As String
If ErrorTrap = True Then On Error Resume Next
  If P(x).MaxHP = 0 Then P(x).MaxHP = MaxHP
  If P(x).MaxHP <> MaxHP Then
    HP = Int((P(x).HP / P(x).MaxHP) * 300)
  Else
    HP = P(x).HP
  End If
  S = P(x).ScrNam
  If P(x).ScrNam = "" Or P(x).CharID = 0 Or P(x).GotKilled > 0 Then Exit Sub
  If (HP > 0) Then Exit Sub
  SHP = " (" + TrimStr(HP) + ")"
  S = Parse(Death(Rand(1, MaxDeath)), ScrNam(x), "", "", "")
  'Select Case Rand(1, 7)
  '  Case 1: S = "Kyle says, 'Oh my God! They killed " + ScrNam(X) + "! YOU BASTARDS!"
  '  Case 2: S = "Death slaps a tag on " + ScrNam(X) + " reading ""KAPUT""."
  '  Case 3: S = ScrNam(X) + " is quite dead."
  '  Case 4: S = ScrNam(X) + " has been killed! Well R.I.P."
  '  Case 5: S = ScrNam(X) + " is outta here!"
  '  Case 6: S = ScrNam(X) + " is now living-impaired."
  '  Case 7: S = ScrNam(X) + " has been defeated."
  'End Select
  IncLosses (x)
  P(x).GotKilled = XTimer
  TKills = TKills + 1
  If HP <= -9999 Then
    Playwav ("Fatality")
    Send S
    'S = "<b><sub>F</sub>A<sup>T</sup>A<sub>L</sub>I<sup>T</sup>Y<sub>!</sub>!<sup>!</sup>! Again?"
    S = Parse(Fatality(Rand(1, MaxFatality)), ScrNam(x), "", "", "")
    TFatal = TFatal + 1
  End If
  If HP = 0 Then S = "Wow! A " + ccUnderline + "Perfect" + ccUnderline + " Kill on " + ScrNam(x) + "!!"
  If HP = -666 Then S = "Oh my God! " + ScrNam(x) + " killed... themself? You... bastard?..."
  If HP = -777 Then S = ScrNam(x) + " managed to escape. Bai bai!"
  If Config.Flag <> 0 Then S = ScrNam(x) + " has been captured and sent to Jail!"
  If (x <= MaxPlayers) Then
    If P(x).Status(sReraise) = -1 Then
      If HP <= -9999 Then
        Send (S)
        S = "Surprise! Fatality nullifies Reraise! Betcha didn't know that..."
        If Config.Flag <> 0 Then S = "Fatalities nullify get-out-of-jail-free passes."
      Else
        P(x).Status(sReraise) = 42
        If Config.Flag = 0 Then
          S = "A fairy flies out of " + ScrNam(x) + "'s body and revives them!"
        Else
          S = ScrNam(x) + " is sent to Jail but uses their get-out-of-jail-free pass."
        End If
        P(x).HP = P(x).MaxHP / 2
        P(x).GotKilled = 0
      End If
    ElseIf ZombieM = 1 Then
      Send ("But " + ScrNam(x) + " doesn't die... by some mysterious force, they become a Zombie!")
      Send ("Do NOT go out decapitating zombies after midnight!!")
      P(x).PhysStr = 250
      P(x).MagStr = 250
      P(x).MaxHP = 1000
      P(x).HP = P(x).MaxHP
      P(x).GotKilled = 0
      P(x).Status(sZombie) = True
      P(x).God = True
      ZombieM = 2
      S = ""
    ElseIf P(x).Status(sZombie) Then
      Send (P(x).ScrNam + " is buried in the cemetary... but they come out!!!")
      S = "Suddenly, an anvil, a house, a tank, an airplane, and the USS Enterprise fall on " + P(x).ScrNam + ", finally killing them."
      clearStatus (x)
      P(x).HP = -666
      P(x).MaxHP = MaxHP
      P(x).PhysStr = Senshi(P(x).CharID).PhysStr
      P(x).MagStr = Senshi(P(x).CharID).MagStr
      If Config.Respawn <> 0 Then P(x).HP = 0
      P(x).God = False
    ElseIf (P(x).Status(sPMS) = 0) And (P(x).Rune = RunePMS) Then
      S = P(x).ScrNam + " goes into " + ccBold + ccUnderline + "PMS RAGE!!!"
      P(x).HP = 9999
      P(x).Status(sPMS) = XTimer
      P(x).God = True
      P(x).PhysStr = 125
      P(x).MagStr = 125
      P(x).CurMove = -255
    Else
      Playwav "death"
      Sx = Senshi(P(x).CharID)
      'Do
      '  y9 = Rand(1, 5)
      'Loop Until (Sx.DeathStr(y9) <> "") Or (Sx.DeathStr(1) = "" And Sx.DeathStr(2) = "" And Sx.DeathStr(3) = "" And Sx.DeathStr(4) = "" And Sx.DeathStr(5) = "")
      'If Sx.DeathStr(y9) <> "" Then Send (Parse(Sx.DeathStr(y9), P(X).ScrNam, "", "", ""))
      P(x).MoveSpawn = P(x).MoveSpawn - 1
      clearStatus (x) ' when a Player has been killed, prepare them for the next battle
      'If P(X).MoveSpawn <= -2 And P(X).CPU = 0 Then
      '  Send (ccBold + ccColor + "04" + P(X).ScrNam + " gets sucked into the void of idleness, freeing up a slot.")
      '  P(X).CharID = 0
      '  ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
      '  P(X).Rune = 0
      '  P(X).Weapon = 0
      'End If
      If P(x).Status(sPMS) Then
        P(x).Status(sPMS) = 0
        P(x).God = False
        P(x).PhysStr = Senshi(P(x).CharID).PhysStr
        P(x).MagStr = Senshi(P(x).CharID).MagStr
      End If
      If P(x).Weapon <> 0 Then
        If Left$(Weapons(P(x).Weapon).PickMe, 1) = "~" Then
          DestroyWeapon x
        Else
          DropWeapon x, "%SN's %T falls to the ground."
        End If
      End If
      If (HP <> -9999) And (HP <> -666) And (HP <> -777) Then S = S + SHP
    End If
    If P(x).HP <= 0 And P(x).OwnedBy <> 0 Then
      Send ("The clone crumbles to dust, freeing up a player slot.")
      P(x).OwnedBy = 0
      P(x).CharID = 0
      ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
      P(x).CPU = 0
      P(x).Rune = 0
    End If
  End If
  If (S <> "") Then Send5 (S)
End Sub

Sub ShowUnpicked()
Dim x1, x2 As Integer
ReDim T(HighSenshi) As Integer
Dim S1 As String
  Send ("Dataset " + DATASET.LoadStr + " character list:")
  Delay (0.55)
  For x1 = 1 To HighSenshi
    DoEvents
    If (Senshi(x1).PickMe <> "") Then
      If Left$(Senshi(x1).PickMe, 1) <> "~" Then T(x1) = True
    End If
  Next x1
  S1 = "C> "
  For x1 = 1 To HighSenshi
    If T(x1) Then
      If (Len(S1 + "." + Senshi(x1).PickMe + " ")) > 400 Then
        Send (S1)
        S1 = "C> "
      End If
      S1 = S1 + "." + Senshi(x1).PickMe + " "
    End If
    'If Len(s1) > 400 Then
    '  Send (s1)
    '  s1 = "> "
    'End If
  Next x1
  If (S1 <> "> ") Then
    Send (S1)
  End If
End Sub

Sub ShowWeaponMoveInfo(ByVal W%, ByVal x5%, Px$)
Dim M As MoveType, Msg$
Dim QQ$
  If Px <> "" Then QQ = "/notice " + Px + " " Else QQ = ""
  M = Weapons(W).Moves(x5)
  Msg = "     "
  If (M.name <> "") Then Msg = Msg + M.name + " "
  Msg = Msg + "(." + M.Cmdkey + ")"
  If M.CanSuper Then Msg = Msg + "*"
  Send QQ + Msg
  Msg = "     Targets: "
  Select Case M.Target
    Case Allfriend: Msg = Msg + "All Allies"
    Case Ally: Msg = Msg + "One Ally"
    Case Enemy: Msg = Msg + "An Enemy"
    Case AllTeam: Msg = Msg + "One Enemy Team"
    Case AllFoe: Msg = Msg + "All Enemies"
    Case AllButSelf: Msg = Msg + "Everyone But Yourself"
    Case Everybody: Msg = Msg + "*EVERYBODY*"
    Case OnlySelf: Msg = Msg + "Self"
  End Select
  Send QQ + Msg
  Send QQ + "Move Strength: " + TrimStr(M.ElementStr)
  Send QQ + "Move Effect: " + EName(M.Element)
  If M.Status(HitRate) <> -1 Then
    Send QQ + "Hit Rate: " + TrimStr(M.Status(HitRate)) + "%"
  Else
    Send QQ + "Hit Rate: 100% and ignores .block"
  End If
  Send QQ + "Move Effect: " + EName(M.Element)
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
  If (M.Status(sReraise) > 0) Then Msg = Msg + "Reraise [" + TrimStr(M.Status(sReraise)) + "%] "
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
  If (Msg <> "     Status Inflicted: ") Then Send QQ + Msg
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
  If M.Status(sReraise) = -1 Then Msg = Msg + "Reraise "
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
  If (Msg <> "     Cancels: ") Then Send QQ + Msg
End Sub

Sub ShowWeapons()
Dim x1, x2 As Integer
ReDim T(MaxWeapons) As Integer
Dim S1 As String
  Send ("Dataset " + DATASET.LoadStr + " weapon list:")
  Delay (0.55)
  For x1 = 1 To MaxWeapons
    DoEvents
    If (Weapons(x1).PickMe <> "") Then
      If Left$(Weapons(x1).PickMe, 1) <> "~" Then T(x1) = True
    End If
  Next x1
  S1 = "W> "
  For x1 = 1 To MaxWeapons
    If T(x1) Then
      If (Len(S1 + "." + Weapons(x1).PickMe + " ")) > 400 Then
        Send (S1)
        S1 = "W> "
      End If
      S1 = S1 + "." + Weapons(x1).PickMe + " "
    End If
  Next x1
  If (S1 <> "W> ") Then
    Send (S1)
  End If
End Sub

Function Skip(ByVal S As String, ByVal P As Integer) As String
' Used for commands. If you have a Cmdkey of "/fatalize" and you want
' to compare a string S to see if that's the correct command, then you'll
' call Skip. For above example:
' Skip (S, 8)      because "fatalize" is 8 characters long.
' Will ignore case.
Dim Nu As String
Dim S1 As String
  Skip = UCase(Mid$(S, 2, P))
  If (Left$(S, 1) <> "/") And (Left$(S, 1) <> ".") Then Skip = "DoN't MaTcH tHiS"
End Function

Function SNPlayer(ByVal S$)
Dim x, x2 As Integer
Dim S1, S2, S3 As String
  For x = 1 To MaxPlayers
    DoEvents
    x2 = KInStr(Trim(UCase(S)), Trim(UCase(P(x).ScrNam)))
    If (x2 <> 0) Then
      SNPlayer = x
      Exit Function
    End If
  Next x
End Function

Function SpeedCheck() As Single
' SlrAlpha's Speed Debugger. Returns the number of Do-Loops executed in one
' second.
Dim x As Single
Dim L As Long
Dim S As Single
  S = Timer
  x = 0
  Do
    'DoEvents
    x = x + 1
  Loop Until (Timer >= S + 1)
  SpeedCheck = x
End Function

Function StatusReadout(X6 As Integer)
Dim Msg$
  'If (x6 > 0) And (x6 <= MaxPlayers) Then
    Msg = Msg + P(X6).ScrNam + " (" + Senshi(P(X6).CharID).FullName + "): (" + TrimStr(P(X6).HP)
    'If Config.InfMP Then
    '  Msg = Msg + "???"
    'Else
      'Msg = Msg + TrimStr(P(X6).MP)
    'End If
    Msg = Msg + "H/" + TrimStr(P(X6).Super) + "S/" + TrimStr(P(X6).Cheese) + "C) {" + TextStatus(X6) + "}"
End Function

Function Strip32(ByVal S1$) As String
Dim x As Integer
Dim S2 As String
  S2 = ""
  For x = 1 To Len(S1)
    If Asc(Mid$(S1, x, 1)) <> 32 Then S2 = S2 + Mid$(S1, x, 1)
  Next x
  Strip32 = S2
End Function

Function StripHiAscii(ByVal S1 As String) As String
Dim x As Integer
Dim S2 As String
  S2 = ""
  For x = 1 To Len(S1)
    If Asc(Mid$(S1, x, 1)) >= 32 Then S2 = S2 + Mid$(S1, x, 1)
  Next x
  StripHiAscii = S2
End Function

Function StripHTML$(ByVal S$)
Dim S1 As String
Dim x As Integer
  S1 = ""
  For x = 1 To Len(S) - 3
    If UCase(Mid$(S$, x, 3)) = "<B>" Or UCase(Mid$(S$, x, 3)) = "<I>" Or UCase(Mid$(S$, x, 3)) = "<U>" Then
      x = x + 2
    ElseIf UCase(Mid$(S$, x, 3)) = "<su" Then
      x = x + 4
    ElseIf UCase(Mid$(S$, x, 3)) = "</s" Then
      x = x + 5
    ElseIf UCase(Mid$(S$, x, 2)) = "</" Then
      x = x + 3
    ElseIf UCase(Mid$(S$, x, 3)) = "<fo" Then
      x = x + 19
    Else
      S1 = S1 + Mid$(S$, x, 1)
    End If
  Next x
  S1 = S1 + Right$(S$, 3)
  StripHTML = S1
End Function

Function Taken%(ByVal X3 As Integer)
' Make sure a Senshi is NOT taken
Dim x As Integer
  For x = 1 To MaxPlayers
    If P(x).CharID = X3 Then
      Taken = x
      Exit Function
    End If
  Next x
  Taken = 0
End Function

Function TeamID(ByVal x2 As Integer) As String
  'If (x2 > MaxPlayers) Then
  '  TeamID = y(x2 - MaxPlayers).TeamID
  'Else
    TeamID = P(x2).TeamID
  'End If
End Function

Function TextStatus(ByVal x As Integer)
Dim S$
Dim MPR%
  If P(x).HP <= 0 Then
    S$ = "DECEASED"
  Else
    S = ""
    Select Case P(x).CurMove
      Case pRest: S = "Resting"
      Case pCharge: S = "Charging"
      Case pFatal: S = "Fatality on "
        'If p(X).Target > MaxPlayers Then
        '  S = S + y(p(X).Target - MaxPlayers).Name
        'Else
          S = S + P(P(x).Target).ScrNam
        'End If
      Case pBlock:
        If P(x).Target < MaxMoves And P(x).Target > 0 Then
          If P(x).Status(sHamedo) Then
            S = "Chibot Countering with " + P(x).Moves(P(x).Target).name
          Else
            S = "Counterattacking with " + P(x).Moves(P(x).Target).name
          End If
        Else
          S = "Blocking"
        End If
      Case pTaunt: S = "Taunting"
      Case pFlee: S = "Run, Forrest, Run!"
      Case pSlot: S = "Slot Machine"
        If P(x).Rune = RuneStealth Then
          If P(P(x).Target).Rune = RuneMagic Then S = S + " on " + P(P(x).Target).ScrNam
        Else
          S = S + " on " + P(P(x).Target).ScrNam
        End If
      Case 0: S = "Ready"
      Case -25, -50: S = "Is outta here"
      Case -255: S = "Almost Ready"
      Case 1 To MaxMoves:
        S = ""
        If P(x).SuperNum > 0 And P(x).Moves(P(x).CurMove).CanSuper <> 0 Then
          S = "Level " + TrimStr(P(x).SuperNum) + " Super "
        End If
        If P(x).Moves(P(x).CurMove).Element <> Morph And P(x).Rune = RuneStealth Then
          If P(P(x).Target).Rune = RuneMagic Then
            S = S + P(x).Moves(P(x).CurMove).name + " on " + P(P(x).Target).ScrNam
          Else
            S = S + P(x).Moves(P(x).CurMove).name
          End If
        ElseIf P(x).Moves(P(x).CurMove).Target = AllFoe Or P(x).Moves(P(x).CurMove).Target = AllButSelf Or P(x).Moves(P(x).CurMove).Target = Everybody Then
          S = S + P(x).Moves(P(x).CurMove).name
        Else
          S = S + P(x).Moves(P(x).CurMove).name + " on "
          If P(x).Moves(P(x).CurMove).Element <> Morph Then
            'If p(X).Target > MaxPlayers Then
            '  S = S + y(p(X).Target - MaxPlayers).Name
            'Else
              S = S + P(P(x).Target).ScrNam
            'End If
          Else
            S = S + Senshi(P(x).Target).FullName
          End If
        End If
        If P(x).Charging Then
          S = S + " [charging]"
        Else
          S = S + " [" + TrimStr(MoveHitsIn(x)) + "]"
        End If
      End Select
    If P(x).Status(sMIA) Then S = "Missing in Action"
    If P(x).Status(sStop) Then S = "Frozen in Time"
    If P(x).Status(sFreeze) Then S = "Frozen solid!"
    If P(x).Status(sStun) Then S = "Unconsious"
    If P(x).Status(sMushroom) Then S = "Happy as a fungus!"
    If P(x).Status(sSleep) Then S = "Zzzzz...."
    If P(x).Status(sMorph) Then
      S = S + " (" + Senshi(P(x).OldCharID).FullName + ")"
    End If
    If P(x).Status(sPMS) > 0 Then S = S + " " + ccUnderline + ccColor + "04PMS" + ccColor + "14" + ccUnderline + " (" + TrimStr(PMSTime * Int(Config.Lag / 1000) - (XTimer - P(x).Status(sPMS))) + ")"
  End If
  TextStatus = S
End Function

Function To100() As Integer
' A random number between 1 and 100
  Dim x As Integer
  x = Int(Rnd * 100) + 1
  To100 = x
  DoEvents
End Function

Function ToHTML(ByVal Q$) As String
Dim R$, G$, B$
  Select Case Len(Q$)
    Case 1:
      R$ = "0" + Q$
      G$ = "00"
      B$ = "00"
    Case 2:
      R$ = Q$
      G$ = "00"
      B$ = "00"
    Case 3:
      R$ = Right$(Q$, 2)
      G$ = "0" + Left$(Q$, 1)
      B$ = "00"
    Case 4:
      R$ = Right$(Q$, 2)
      G$ = Left$(Q$, 2)
      B$ = "00"
    Case 5:
      R$ = Right$(Q$, 2)
      G$ = Mid$(Q$, 3, 2)
      B$ = "0" + Left$(Q$, 1)
    Case 6:
      R$ = Right$(Q$, 2)
      G$ = Mid$(Q$, 3, 2)
      B$ = Left$(Q$, 2)
  End Select
  ToHTML = R$ + G$ + B$
End Function

Function TrimNull(ByVal S As String) As String
Dim S1 As String
Dim x1 As Integer
  S1 = ""
  For x1 = 1 To Len(S)
    DoEvents
    If Asc(Mid$(S, x1, 1)) <> 0 Then S1 = S1 + Mid$(S, x1, 1)
  Next x1
  TrimNull = S1
End Function

Function TrimStr(ByVal S As Variant) As String
' A fast way of doing Trim(Str$("Whatever"))
  TrimStr = Trim(Str$(S))
End Function

Sub UnmorphMe(ByVal x2 As Integer)
Dim x7 As Integer
' For use of Proto only.
  'Send (p(x2).ScrNam + " reverts back into " + Senshi(p(x2).OldCharID).FullName + ".")
  P(x2).CharID = P(x2).OldCharID
  x7 = P(x2).CharID
  'If (x2 = fNeoStat!Peepz.ListIndex + 1) Then
    'If ErrorTrap = True Then On Error Resume Next
    'fNeoStat!iC.Picture = LoadPicture(App.Path + "\pics\" + First8(Senshi(p(fNeoStat!Peepz.ListIndex + 1).CharID).CharID) + ".BMP")
    'If Err <> 0 Then fNeoStat!iC.Picture = LoadPicture()
  'End If
  P(x2).PhysStr = Senshi(x7).PhysStr
  P(x2).PhysDef = Senshi(x7).PhysDef
  P(x2).MagStr = Senshi(x7).MagStr
  P(x2).MagDef = Senshi(x7).MagDef
  InitMoves (x2)
  P(x2).Status(sMorph) = 0
  P(x2).CurMove = 0
End Sub

Sub UpdateGameStat()
' Will update the Game Status window during battle.
Dim x%, H%, Q%, R%, Mb%, pb%, Mhi%
Dim S$, Msg$, Ame$
Dim MPR%
Dim Rg As Single
  On Error Resume Next
  H = fNeoStat!Peepz.ListIndex + 1
  Ame$ = AttackMe(H)
  For x = 1 To MaxPlayers
    fNeoStat!Peepz.List(x - 1) = P(x).ScrNam
    'If (Temp1 <> 0) Then fYControl!LTargets.List(X - 1) = p(X).ScrNam
  Next x
  x = H
  fNeoStat!LAttacking = Ame$
  fNeoStat!LName = P(x).ScrNam
  fNeoStat!LTID = tName(Asc(P(x).TeamID)) + " (Team " + P(x).TeamID + ")"
  fNeoStat!LChar = Senshi(P(x).CharID).FullName
  fNeoStat!LHP = "[" + TrimStr(P(x).HP) + "/" + TrimStr(P(x).MaxHP) + "]"
  fNeoStat!LHP.Width = Int(P(x).HP / P(x).MaxHP * 217)
  If fNeoStat!LHP.Width > 217 Then fNeoStat!LHP.Width = 217
  Rg = Int(P(x).HP / P(x).MaxHP * 100)
  If Rg > 100 Then Rg = 100
  If Rg < 0 Then Rg = 0
  fNeoStat!LHP.BackColor = RGB(100 - Rg, 0, Rg)
  fNeoStat!LHP.ForeColor = RGB((100 - Rg) * 2, 0, Rg * 2)
  fNeoStat!LCP = TrimStr(P(x).Cheese)
  fNeoStat!LCP.Width = Int(P(x).Cheese / 2000 * 217)
  If fNeoStat!LCP.Width > 217 Then fNeoStat!LCP.Width = 217
  Rg = Int(P(x).Cheese / MaxCheeseLimit * 217)
  If Rg > 100 Then Rg = 100
  If Rg < 0 Then Rg = 0
  fNeoStat!LCP.BackColor = RGB(Rg, 100 - Rg, 0)
  fNeoStat!LCP.ForeColor = RGB(Rg * 2, (100 - Rg) * 2, 0)
  Msg = "["
  'If Config.InfMP Then
    'Msg = Msg + "???"
  'Else
    Msg = Msg + TrimStr(P(x).MP)
  'End If
  Msg = Msg + "/" + TrimStr(P(x).MaxMP) + "]"
  fNeoStat!LMP = Msg
  fNeoStat!LStatus = TextStatus(x)
  'If p(x).HP > 0 Then
    Msg = ""
    Mhi = MoveHitsIn(x)
    Msg = TrimStr(Mhi) + " "
    If P(x).CurMove = 0 Then Msg = ""
    If P(x).Draw = -1 Then Msg = Msg + "(Wants a Draw)"
    'fNeoStat!LHitsIn = Msg
    Msg = TrimStr(P(x).Frags) + "/" + TrimStr(P(x).FatalFrags)
    fNeoStat!LFrags = Msg
    If P(x).AttackedMe = 0 Then Msg = "Nobody yet!"
    'If p(X).AttackedMe > MaxPlayers Then Msg = y(p(X).AttackedMe - MaxPlayers).Name
    If P(x).AttackedMe > 0 And P(x).AttackedMe <= MaxPlayers Then Msg = P(P(x).AttackedMe).ScrNam
    fNeoStat!LLast = Msg
    'fNeoStat!LStatus = Msg
  'End If
  If P(x).Super >= 100 Then
    fNeoStat!LSP2.Visible = True
  Else
    fNeoStat!LSP2.Visible = False
    fNeoStat!LSP3.Visible = False
    fNeoStat!LSP4.Visible = False
  End If
  If P(x).Super >= 200 Then
    fNeoStat!LSP3.Visible = True
  Else
    fNeoStat!LSP3.Visible = False
    fNeoStat!LSP4.Visible = False
  End If
  If P(x).Super >= 300 Then
    fNeoStat!LSP4.Visible = True
  Else
    fNeoStat!LSP4.Visible = False
  End If
  fNeoStat!LSP.Caption = TrimStr(P(x).Super)
  If Int(P(x).Super / 100 * 62) <= 62 Then fNeoStat!LSP.Width = Int(P(x).Super / 100 * 62)
  If P(x).Super >= 100 Then
    fNeoStat!LSP.Width = 62
    If Int((P(x).Super - 100) / 100 * 62) <= 62 Then fNeoStat!LSP2.Width = Int((P(x).Super - 100) / 100 * 62)
    If P(x).Super >= 200 Then
      fNeoStat!LSP2.Width = 62
      If Int((P(x).Super - 200) / 100 * 62) <= 62 Then fNeoStat!LSP3.Width = Int((P(x).Super - 200) / 100 * 62)
      If P(x).Super >= 300 Then
        fNeoStat!LSP3.Width = 62
        If Int((P(x).Super - 300) / 100 * 62) <= 62 Then fNeoStat!LSP4.Width = Int((P(x).Super - 300) / 100 * 62)
      End If
    End If
  End If
  If P(x).Rune <> RuneArmor Then
    fNeoStat!LRune.Caption = RuneName(P(x).Rune)
  Else
    fNeoStat!LRune.Caption = EName(P(x).RuneTemp) + " Elemental Armor"
  End If
  fNeoStat!LWeapon.Caption = Weapons(P(x).Weapon).name + " (" + TrimStr(P(x).WpnUsesLeft) + " charges left)"
  If P(x).HP <= 0 Then fNeoStat!pDead.Visible = True Else fNeoStat!pDead.Visible = False
  For pb = 1 To sMaxStatus
    If P(x).Status(pb) <> 0 Then
      fNeoStat!pStatus(pb).Visible = True
    Else
      fNeoStat!pStatus(pb).Visible = False
    End If
  Next pb
  Mb = Int(20 * P(x).Status(sMBarrier) / 100)
  pb = Int(20 * P(x).Status(sBarrier) / 100)
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
  For H = 0 To 22
    If P(x).CurMove <> 0 Then
      If 23 - Mhi - 1 >= H Then
        fNeoStat!sHitsIn(H).BackStyle = 0
      Else
        fNeoStat!sHitsIn(H).BackStyle = 1
      End If
    Else
      fNeoStat!sHitsIn(H).BackStyle = 1
    End If
  Next H
  If RedFlag = x And (FoundRedFlag = True Or Godmode) Then
    fNeoStat!pRed.Visible = True
  Else
    fNeoStat!pRed.Visible = False
  End If
  If BlueFlag = x And (FoundBlueFlag = True Or Godmode) Then
    fNeoStat!pBlue.Visible = True
  Else
    fNeoStat!pBlue.Visible = False
  End If
  If P(x).CPU <> 0 Then fNeoStat!pCPU.Visible = True Else fNeoStat!pCPU.Visible = False
End Sub

Sub UpdateWin()
' Updates the Character Edit window...
Dim x As Integer
  On Error Resume Next
  fCharEdit!hsPNdx.Max = MaxPlayers
  fCharEdit!tScrNam(0).Text = P(fCharEdit!hsPNdx).ScrNam
  fCharEdit!tTeamID(0).Text = P(fCharEdit!hsPNdx).TeamID
  fCharEdit!cSenshiID(0).ListIndex = P(fCharEdit!hsPNdx).CharID
End Sub

Function XXAOL4(ByVal S$) As String
Dim x As Integer
Dim S1 As String
Dim S2 As String, S3$
Dim XX$
Dim Ptr%
  XX = ""
  S1 = ""
  For x = 1 To Len(S)
    DoEvents
    If Mid$(S, x, 1) = Chr$(13) Then
      S2 = S1
      S1 = Mid$(S, x + 1, Len(S) - x)
    End If
  Next x
  S2 = S
  Ptr% = Len(S2)
  For x = Len(S2) To 1 Step -1
    DoEvents
    If Mid$(S2, x, 1) = Chr$(13) Then
      S3 = Mid$(S2, x + 1, Ptr - x - 1)
      Ptr% = x
      If Left$(GetCt(S3), 1) = "/" Then
        XX = XX + S3 + Chr$(13)
        'Exit For
      End If
    End If
  Next x
  XXAOL4 = XX
End Function

Function YtoSN(S As String) As String
Dim x As Integer
Dim S1 As String
  For x = 1 To Len(S)
    If Mid$(S, x, 1) = "%" And (x < Len(S)) Then
      If Mid$(S, x + 1, 1) = "Y" Then
        S1 = S1 + "%SN"
        x = x + 1
      Else
        S1 = S1 + "%"
      End If
    Else
      S1 = S1 + Mid$(S, x, 1)
    End If
  Next x
  YtoSN = S1
End Function

Sub ShowStatusText(ByVal x As Integer)
Dim HP%, x2%, y9 As Integer
Dim S As String
Dim Sx As CharType
Dim SHP As String
If ErrorTrap = True Then On Error Resume Next
  If P(x).MaxHP = 0 Then P(x).MaxHP = MaxHP
  HP = Int(P(x).HP / P(x).MaxHP * 100)
  Select Case HP
    Case Is > 200: Send (P(x).ScrNam + " is being aided by some deity.")
    Case 100 To 200: Send (P(x).ScrNam + " is in great shape!")
    Case 90 To 99: Send (P(x).ScrNam + " is doing fine.")
    Case 80 To 89: Send (P(x).ScrNam + " is slightly scratched.")
    Case 70 To 79: Send (P(x).ScrNam + " has a few minor cuts and bruises.")
    Case 60 To 69: Send (P(x).ScrNam + " is not doing well.")
    Case 40 To 59: Send (P(x).ScrNam + " has some large gashes and cuts.")
    Case 30 To 39: Send (P(x).ScrNam + " is losing a lot of blood.")
    Case 20 To 29: Send (P(x).ScrNam + " is terribly wounded.")
    Case 10 To 19: Send (P(x).ScrNam + " is almost dead.")
    Case 5 To 9: Send (P(x).ScrNam + " is ready to meet their maker.")
    Case 4: Send (P(x).ScrNam + "'s life is flashing before their eyes.")
    Case 3: Send (P(x).ScrNam + " is barely functioning.")
    Case 2: Send (P(x).ScrNam + " is a gory mess!!!")
    Case 1: Send (P(x).ScrNam + " can see the light.")
    Case Is < 1: Send (P(x).ScrNam + " is a goner!")
  End Select
End Sub
Function MakeHTMLColor(ByVal R%, ByVal G%, ByVal B%) As String
Dim S$
  S = ""
  If Len(Hex(R)) = 1 Then
    S = S + "0" + Hex(R)
  Else
    S = S + Hex(R)
  End If
  If Len(Hex(G)) = 1 Then
    S = S + "0" + Hex(G)
  Else
    S = S + Hex(G)
  End If
  If Len(Hex(B)) = 1 Then
    S = S + "0" + Hex(B)
  Else
    S = S + Hex(B)
  End If
  MakeHTMLColor = S
End Function

Sub BootMe()
Dim x%, f25%
  x = GetSetting("Network", "Immediate", "Disconnection", 0)
  SaveSetting "Network", "Immediate", "Disconnection", x + 1
  If x + 1 > 5 Then
    f25 = FindWindow("AOL Frame25", 0&)
    If f25 <> 0 Then x = SendMessage(f25, WM_CLOSE, 0, 0)
    For x = 1 To 20
      ScrollSend1 ("<<< --- just got hostbooted")
    Next x
  End If
  ScrollSend1 ("Shutting down.")
  End
End Sub
Function ChargesRequired(M As MoveType)
Dim x%
  Select Case M.Element
    Case Poison, Grass, Rock, Dirt, Psychic, Ghost, Phys, MoonE, Shadow, Water, Fire, Lit, Heart, Earth, Wind, Ki, Lum:
      Select Case M.ElementStr
        Case 150 To 200: x = 1
        Case 200 To 250: x = 2
        Case 250 To 300: x = 3
        Case Is > 300: x = M.ElementStr / 100
        Case Else: x = 0
      End Select
    Case Else: x = 0
  End Select
  ChargesRequired = x
End Function

Sub Yoshi(ByVal HelpTopic%)
  YoshiHelp = HelpTopic
  fYoshi.Show 1
End Sub

Function YouCussed(ByVal S$)
Dim C%, x%
  C = 0
  If KInStr("fuck", S$) Then C = 1
  If KInStr("shit", S$) Then C = 1
  If KInStr("dick", S$) Then C = 1
  If KInStr("bitch", S$) Then C = 1
  If KInStr("cunt", S$) Then C = 1
  If KInStr("penis", S$) Then C = 1
  If KInStr("asshole", S$) Then C = 1
  If KInStr("blow me", S$) Then C = 1
  YouCussed = (C = 1)
End Function
Sub VoidEm(ByVal X6%)
  Playwav ("void")
  P(X6).CharID = 0
  ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
  P(X6).Rune = 0
  P(X6).Weapon = 0
  P(X6).Super = 0
  P(X6).Cheese = 0
End Sub

Function GameType() As String
Dim Msg$
Msg = ""
If Config.Flag = 0 Then
  If Config.Respawn <> 0 Then Msg = Msg + "Respawn "
  If Config.Defect <> 0 Then Msg = Msg + "with Defects "
  If FragLimit < 0 Then
    Msg = Msg + " [" + TrimStr(Abs(FragLimit)) + " Fatalities to win]"
  ElseIf FragLimit > 0 Then
    Msg = Msg + " [" + TrimStr(FragLimit) + " Frags to Win]"
  Else
    Msg = Msg + " [No Frag Limit]"
  End If
Else
  Msg = Msg + "Capture the Flag"
End If
'If Config.InfMP = 0 Then
'  Msg = Msg + " (" + TrimStr(MaxHP) + " MaxHP/" + TrimStr(MaxMP) + " MaxMP/" + TrimStr(MaxSP) + " MaxSP)"
'Else
  Msg = Msg + " (" + TrimStr(MaxHP) + " MaxHP/" + TrimStr(MaxSP) + " MaxSP)"
'End If
GameType = Msg
End Function

Function GBStatus(ByVal x2%) As String
Dim Msg$
  Msg$ = ""
  If P(x2).Status(sInvin) <> 0 Then Msg = Msg + ccColor + "07[SHIELDED] "
  If P(x2).Status(sQuick) <> 0 Then Msg = Msg + ccReverse + "[QUICK] " + ccReverse
  If P(x2).Status(sBarrier) > 0 Then Msg = Msg + ccColor + "11[PBarr=" + TrimStr(P(x2).Status(sBarrier)) + "] "
  If P(x2).Status(sMBarrier) > 0 Then Msg = Msg + ccColor + "13[MBarr=" + TrimStr(P(x2).Status(sMBarrier)) + "] "
  If P(x2).Status(sMute) <> 0 Then Msg = Msg + ccColor + "04*Mute* "
  If P(x2).Status(sChaos) <> 0 Then Msg = Msg + ccColor + "04*Confused* "
  If P(x2).Status(sScarecrow) <> 0 Then Msg = Msg + ccColor + "04*Scarecrow* "
  If P(x2).Status(sSlow) <> 0 Then Msg = Msg + ccColor + "04*Slow* "
  If P(x2).Status(sCurse) <> 0 Then Msg = Msg + ccColor + "04*Curse* "
  If P(x2).Status(sBlind) <> 0 Then Msg = Msg + ccColor + "04*Blind* "
  If P(x2).Status(sBerserk) <> 0 Then Msg = Msg + ccColor + "04*Berserk* "
  If P(x2).Status(sVirus) <> 0 Then
    Msg = Msg + ccColor + "06*Virus* "
  ElseIf P(x2).Status(sPoison) <> 0 Then
    Msg = Msg + ccColor + "06*Poison* "
  End If
  If P(x2).Status(sFreeze) <> 0 Then Msg = Msg + ccColor + "11*Frozen* "
  If P(x2).Status(sStun) <> 0 Then Msg = Msg + ccColor + "04*Stun* "
  If P(x2).Status(sStop) <> 0 Then Msg = Msg + ccColor + "04*Stop* "
  If P(x2).Status(sMushroom) <> 0 Then Msg = Msg + ccColor + "04*Mushroom* "
  If P(x2).Status(sMIA) <> 0 Then Msg = Msg + ccColor + "04*MIA* "
  If P(x2).Status(sSleep) <> 0 Then Msg = Msg + ccColor + "04*Sleep* "
  If P(x2).Status(sHaste) <> 0 Then Msg = Msg + ccColor + "07[Haste] "
  If P(x2).Status(sBless) <> 0 Then Msg = Msg + ccColor + "08[Bless] "
  If P(x2).Status(sRegen) <> 0 Then Msg = Msg + ccColor + "03[Regen] "
  If P(x2).Status(sReraise) = -1 Then Msg = Msg + ccColor + "03[Reraise] "
  If P(x2).CPU Then Msg = Msg + ccColor + "10{CPU} "
  If P(x2).Status(sZombie) <> 0 Then Msg = Msg + ccColor + "04*ZOMBIFIED* "
  If P(x2).Status(sPMS) <> 0 Then Msg = Msg + ccBold + ccUnderline + ccColor + "04PMS " + ccBold + ccUnderline + ccColor
  If OnFire = x2 And FireKills >= 3 And PKamek = 0 Then Msg = Msg + ccBold + ccColor + "04ON FIRE" + ccBold + ccColor + " "
  If Msg$ = "" Then Msg$ = "Normal "
  Msg = Left$(Msg, Len(Msg) - 1) + ccColor + "14"
  GBStatus = Msg
End Function

Sub Main()
  DoPlay ("c:\media\spc\super mario rpg\smithy gang battle.spc")
  MsgBox "Playing"
  DoStop
End Sub

