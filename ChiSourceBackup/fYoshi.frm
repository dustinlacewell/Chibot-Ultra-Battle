VERSION 5.00
Begin VB.Form fYoshi 
   BackColor       =   &H00000000&
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Yoshi!"
   ClientHeight    =   2805
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   3960
   ControlBox      =   0   'False
   ForeColor       =   &H00000000&
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2805
   ScaleWidth      =   3960
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer tmrYoshi 
      Interval        =   1
      Left            =   1800
      Top             =   2280
   End
   Begin VB.CommandButton cOK 
      Cancel          =   -1  'True
      Caption         =   "Skip"
      Height          =   375
      Left            =   2520
      TabIndex        =   1
      Top             =   2280
      Width           =   1215
   End
   Begin VB.CommandButton cNext 
      Caption         =   "Next"
      Default         =   -1  'True
      Height          =   375
      Left            =   240
      TabIndex        =   0
      Top             =   2280
      Width           =   1215
   End
   Begin VB.Label lbYoshi 
      Alignment       =   2  'Center
      BackColor       =   &H00000000&
      BorderStyle     =   1  'Fixed Single
      ForeColor       =   &H00FFFFFF&
      Height          =   1815
      Left            =   1440
      TabIndex        =   2
      Top             =   240
      Width           =   2295
   End
   Begin VB.Image imgYoshi 
      Height          =   1440
      Index           =   0
      Left            =   240
      Picture         =   "fYoshi.frx":0000
      Top             =   480
      Width           =   1020
   End
End
Attribute VB_Name = "fYoshi"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim Cnt As Integer

Private Sub cNext_Click()
Dim T$, Done%, Tip%, Doy As Single
Const MaxTips = 13
  PlayYoshi
  Done = 0
  Select Case YoshiHelp
    Case yhMOTD:
      Doy = CSng(Date - #12/30/1999# + 365)
      Tip = Val(GetSetting("ChUB Resurrection", "Yoshi!", "Tip#", "0"))
      Tip = Tip + 1
      T = "Tip of the Day: "
'Redo:
      Select Case Tip
        Case 1: T = T + "Kamek could strike at any moment. You must always be prepared."
        Case 2: T = T + "Quick doesn't have any effect on moves that have any chance at all of giving Quick. In other words, you can't Quick yourself and then instantly Quick someone else."
        Case 3: T = T + "If you use a Chibot Counter and it misses or fails to disable the attacker, you'll be left wide open for his move!"
        Case 4: T = T + "To play ChUB Resurrection without using AOL, simply sign off and close AOL, then re-load ChUB. You'll have a special output window you can use to play against some CPU's."
        Case 5: T = T + "CPU's are very clever. However, they are not without their weaknesses."
        Case 6: T = T + "I like to play Pong. It's a cool game."
        Case 7: T = T + "There have been rumored to be even more runes than the regular Runes. But how do you get them?"
        Case 8: T = T + "A .CH2 manifestation of Kamek is far less powerful than the real McKoopa."
        Case 9: T = T + "Sorry, I don't have a tip for you today because Bowser destroyed my Tip of the Day list."
        Case 10: T = T + "In cases where a move hits multiple targets who are counterattacking, the counterattacks are executed in the order that the players originally joined, but those waiting to counterattack do not receive blocking benefits."
        Case 11: T = T + "If all else fails, /rest."
        Case 12: T = T + "Do not taunt the Happy Fun Mystery Menu Option."
        Case 13: T = T + "If there is no frag limit in Respawn FFA, the game will never end."
        Case 14: T = T + "Kamek's such a lazy-ass... err, hello! I mean, uh, welcome to Chibot Ultra Battle! Heh heh... uhh...."
        Case 15: T = T + "I hate swear words. Please, don't swear in ChUB SE, ok?"
      End Select
      If Tip >= MaxTips Then Tip = 0
      SaveSetting "ChUB Resurrection", "Yoshi!", "Tip#", TrimStr(Tip)
      Done = True
    Case yh1stTime:
      Select Case Cnt
        Case 0: T = "Hello! I'm Yoshi. Welcome to Chibot Ultra Battle Resurrection!"
        Case 1: T = "Since we share a common enemy (Kamek), I'll help you around the game and explain some of the new features."
        Case 2: T = "If you're already experienced at hosting older versions of ChUB Resurrection, this version isn't much different in most of the features."
        Case 3: T = "To skip what I'm saying at any time, hit the Skip button below."
        Case 4: T = "If you don't want to hear my tips, then uncheck the ""New User Help"" option in the Help menu."
        Case 5: T = "To ask me what a particular button or option does, in most cases you can hold SHIFT and right-click the item. If a right-click doesn't work, try SHIFT+left click. Don't be afraid to ask! I'm here to help you."
        Case 6: T = "Hold on! Some of the stuff I might tell you is outdated, since it was written for ChUB 2000."
        Case 7: T = "Make sure you read the README file for updated information! And do not ask Kamek a question that's been answered in the README. He HATES that."
        Case 8: T = "Anyway, enjoy ChUB Resurrection!"
          Done = True
      End Select
    Case yhNoMoreYoshi:
      Select Case Cnt
        Case 0: T = "OK, I won't bother you any longer. Don't forget that if you need advice on a certain part of the game, you can hold SHIFT and right-click it, or SHIFT+left click if right doesn't work."
        Case 1: T = "However, there are certain boxes I cannot help you with, due to Kamek's evil magic. They are usually self-explanatory."
        Case 2: T = "If you need my assistance again, just choose the New User Tips option again. Enjoy ChUB!"
          Done = True
      End Select
    Case yhWelcomeBack:
      T = "So you DO need me, after all! If you get bored of me again, choose the New User Tips option from the Help menu."
      Done = True
    Case yhNoTips:
      T = "OK, I won't share my daily brain farts with you. To change this setting, choose Tip of the Day from the Help menu."
      Done = True
    Case yhTipsBack:
      T = "OK, I'll give you a tip every time you load ChUB. To change this setting, choose Tip of the Day from the Help menu."
      Done = True
    Case yhAFKBot:
      Select Case Cnt
        Case 0: T = "This is the AFK Bot. If you need to leave your keyboard for an extended period of time, this is the perfect tool for you."
        Case 1: T = "To go AFK, type your reason for being AFK in the ""Reason"" box and hit ""Start""."
        Case 2: T = "The ""Frequency"" slider changes the interval at which ChUB SE scrolls a notice that you are AFK."
        Case 3: T = "If you want the AFK Bot to take messages, click on the Take Messages checkbox. Messages recorded will appear in the large box below."
        Case 4: T = "To clear the messages recorded, click the ""Clear"" button."
          Done = True
      End Select
    Case yhAFK_Reason:
      T = "This box allows you to change the reason ChUB will give for your absence from the keyboard."
      Done = True
    Case yhAFK_Freq:
      T = "This slider allows you to change how often ChUB will give a reminder that you are AFK. The number to the right indicates the interval in seconds."
      Done = True
    Case yhAFK_Msg:
      T = "Check this box if you want ChUB to allow others to leave messages on your computer. The messages appear in the list box below the checkbox."
      Done = True
    Case yhAFK_Start:
      T = "Duh! This starts or stops the AFK bot."
      Done = True
    Case yhAFK_Msgs:
      T = "This box shows any messages taken while you were AFK."
      Done = True
    Case yhAFK_Clear:
      T = "Another no-brainer. This clears the box of messages."
      Done = True
    Case yhCharEdit:
      Select Case Cnt
        Case 0: T = "This is the Character Editor. In this window, you can manually add or remove playing characters."
        Case 1: T = "The only options that shound need explaining here are CPU, EZTeams, QuickCPU, and Random."
        Case 2: T = "To make a player CPU-controlled, click on the CPU button. You'll pop up a window allowing you to change the CPU's attributes."
        Case 3: T = "If you don't want to mess with the CPU options, you can click QuickCPU and it will make you a CPU with the most popular settings."
        Case 4: T = """Random Char"" picks a character at random from the character list, and ""Random CPU"" will pick a random character and make it a CPU."
        Case 5: T = "To set up teams quickly, hit the EZTeams button and tell it how many teams you want, and it will attempt to divide the players evenly, but it doesn't always work perfectly."
        Case 6: T = "The two boxes at the bottom are the Team Name editors. To change a team name, put the Team Identifier in the small box and the team's name in the bigger box."
          Done = True
      End Select
    Case yhChar_ScrNam:
      T = "Umm... You type... their screen name... What else can I say? Oh yeah, it must be entered exact case or it won't work. If a CPU is playing this character, change this box to whatever name you want for it."
      Done = True
    Case yhChar_CPU:
      T = "Click this button to make/edit a computer intelligence for this character."
      Done = True
    Case yhChar_AllCPUs:
      T = "You found a secret button! Click this and random CPU's will be filled in every character slot."
      Done = True
    Case yhChar_TeamID:
      T = "Enter their team number/letter/symbol here. It can be any of the 255 ASCII characters, but for simplicity, make it a letter. NOTE: Team ""A"" is NOT the same as Team ""a""!"
      Done = True
    Case yhChar_EZTeams:
      T = "This button will attempt to even out the characters. Tell it the number of teams that you want, and it will divide them as evenly as possible. If the distribution is uneven, it will tell you."
      Done = True
    Case yhChar_QwikCPU:
      T = "Quickly makes a CPU for this character without having to go through the CPU menu."
      Done = True
    Case yhChar_RandChar:
      T = "Uhh... Random char. If you can't guess that it picks a random character, then not even I can help you!"
      Done = True
    Case yhChar_RandCPU:
      T = "Click this button to make a random CPU."
      Done = True
    Case yhChar_tN:
      T = "Put a number in this box to edit that player. The scroll bar allows you to switch between characters."
      Done = True
    Case yhChar_TeamName:
      T = "This allows you to edit a team's name. Put the team's identifier into the small box, and edit the big box to change their team name."
      Done = True
    Case yhCPUEdit:
      Select Case Cnt
        Case 0: T = "This window allows you to edit the CPU's attributes. Click the CPU-controlled box to make the character a CPU."
        Case 1: T = "Goodwill changes how often the CPU will heal allies in danger. 0=never, 100=always."
        Case 2: T = "Greed changes how often the CPU will grab a /get item. 0=never, 100=always, if one exists."
        Case 3: T = "Wrath changes how often the CPU will go after the last person who attacked it. 0=never, 100=always."
        Case 4: T = "Arrogance changes how often the CPU taunts. A CPU will never taunt if they aren't close to full health. 0=never, 100=1 in 5 chance."
          Done = True
      End Select
    Case yhCPU_IsCPU:
      T = "If it's checked, it's a CPU. If not, then it's not."
      Done = True
    Case yhCPU_Goodwill:
      T = "Determines how often a CPU will heal/aid allies in danger."
      Done = True
    Case yhCPU_Greed:
      T = "Determines how often a CPU will /get an item if there is one."
      Done = True
    Case yhCPU_Wrath:
      T = "Determines how often a CPU will attack the last person to attack it."
      Done = True
    Case yhCPU_Arrogance:
      T = "Determines how often a CPU will taunt the other players. CPUs will never taunt unless they are almost at full health."
      Done = True
    Case yhMidi:
      Select Case Cnt
        Case 0: T = "This is the Millenium Midi Player. You can play MIDI files from anywhere on your computer here. Just pick the midi you want and hit Play."
        Case 1: T = "You may also double-click the file name instead of pressing Play."
          Done = True
      End Select
    Case yhMidi_DirBox:
      T = "This allows you to select a directory to play MIDI files from."
      Done = True
    Case yhMidi_File:
      T = "Pick a MIDI file from this list, and hit Play or double-click the file to play the midi."
      Done = True
    Case yhMidi_Lup:
      T = "If the Loop box is checked, the MIDI will play endlessly. If the Loop box is NOT checked, a new random MIDI will begin at the end of the currently playing MIDI."
      Done = True
    Case yhMidi_Play:
      T = "Play? I don't understand what you mean. Play ball? Play bowling? Play the trumpet? Play Pokémon? Play ChUB? Play what?"
      Done = True
    Case yhMidi_Stop:
      Select Case Rand(1, 5)
        Case 1: T = "... in the name of love."
        Case 2: T = "... sign."
        Case 3: T = "... your whining."
        Case 4: T = "... the train!"
        Case 5: T = "... being an idiot!"
      End Select
      Done = True
    Case yhMidi_Pause:
      T = "Allows you to pause and unpause the music."
      Done = True
    Case yhMidi_Hide:
      T = "This will hide the MIDI player so that you can play music in the background without seeing that annoying window."
      Done = True
    Case yhChat_Chat:
      T = "Type stuff here and it will appear in either AOL (if online) or the output window (if offline). If a /-command is entered, it will be processed immediately. You can control most of the game functions from here -- type ~commands in the chat line to see how."
      Done = True
    Case yhOptions:
      Select Case Cnt
        Case 0: T = "This is the Options Window, where most of the game settings may be changed. For more information on an option, hold SHIFT and right-click it."
        Case 1: T = "Note: One G.S. is one game second, which is equivalent (in seconds) to the value of the Lagometer divided by 1000. So if the Lagometer is 4000, one G.S. = 4 real seconds."
          Done = True
      End Select
    Case yhOpt_SameChar:
      T = "If this option is checked, more than one person will be able to select the same character. If unchecked, anyone attempting to join as a character already taken will not be allowed to."
      Done = True
    Case yhOpt_Weapons:
      T = "If checked, weapons are allowed. If unchecked, weapons are disabled and cannot be selected, and weapons will not be randomly dropped during battle."
      Done = True
    Case yhOpt_Runes:
      T = "If checked, runes are enabled. If unchecked, runes are disabled."
      Done = True
    Case yhOpt_Respawn:
      Select Case Cnt
        Case 0: T = "Respawn means to come back to life. It puts a twist on the game, allowing people to play longer. If this is enabled, dead people will come back in 30 seconds."
        Case 1: T = "If you turn Respawn on, make sure to set a Frag Limit or the game will go forever."
          Done = True
      End Select
    Case yhOpt_CTF:
      Select Case Cnt
        Case 0: T = "In Capture the Flag mode, two teams are formed at random by the computer, the Red Team and the Blue Team. One person on each team gets a flag -- but nobody knows who has it."
        Case 1: T = "In order to win, you must find the person on the other team who has the flag. When the person with the flag is killed, the killer gets the flag held by the dead fighter."
        Case 2: T = "The killer must hold the flag for a set amount of time (Flag Time Limit) in order for their team to win. But if the killer is killed, then the other team gets their flag back."
        Case 3: T = "Don't set the Flag Time Limit too high or else it will be harder, perhaps impossible to win."
          Done = True
      End Select
    Case yhOpt_MultiTarget:
      T = "Some characters have moves that hit all enemies or one enemy team. If this is disabled, players will not be able to do those moves."
      Done = True
    Case yhOpt_MDITop:
      T = "The main ChUB window will, by default, stay on top of other applications. If this bothers you, you may turn it off here. This is a matter of personal preference. I prefer to turn this option off and use the chat line commands (~commands)."
      Done = True
    Case yhOpt_ExtendedSupers:
      T = "By default, the maximum Super level is 3. If this is enabled, the Level 4 and Level 5 (random damage) super levels will be enabled. (Confidentially, I heard there was a Level 6, but keep it a secret, mmkay?)"
      Done = True
    Case yhOpt_FragLimit:
      Select Case Cnt
        Case 0: T = "You can change the number of frags (kills) required to complete the game. Useful for Respawn games."
        Case 1: T = "If the limit is 0, then Respawn games will go forever and non-Respawn games continue until one team is left."
        Case 2: T = "If the limit is less than 0, a Fatality Limit is set, and the game will end when this many Fatalities are scored."
        Case 3: T = "If the limit is greater than 0, the game will end when that number of Frags is reached."
        Case 4: T = "In non-Respawn games, the game will end regardless of the frag limit if all of a team's or person's opposition is defeated."
          Done = True
      End Select
    Case yhOpt_GetRate:
      T = "This changes the rate that new items and/or weapons appear."
      Done = True
    Case yhOpt_FlagLimit:
      T = "Changes the amount of time of holding the enemy's flag required to win in Capture the Flag mode."
      Done = True
    Case yhOpt_TLimit:
      T = "Changes the time limit of a game. During the last 60 G.S. of a game with a time limit, Sudden Death mode is engaged, where cheese meters are disabled and attacks do 2.5x damage. When time runs out, the game is a draw."
      Done = True
    Case yhOpt_DmgMult:
      T = "This allows you to change the amount of damage that moves do. 100=normal damage, 50=half damage, 200=double, etc. I recommend you change this setting if you change the Maximum HP setting."
      Done = True
    Case yhOpt_MaxHP:
      T = "Allows players to have more or less HP. If you change this drastically (by 200 HP or more), I recommend changing the Damage Multiplier to an appropriate value."
      Done = True
    Case yhOpt_MaxSP:
      T = "Allows players to hold more or less Super Points. If the value is 300, no Supers above a Level 3 will be allowed. If the value is 0, no supers or counterattacks (except with Rune of Counterattacks) are allowed."
      Done = True
    Case yhOpt_NoJoin:
      T = "Prevents people from joining during the battle. Unlike previous versions of ChUB, this does NOT block people from joining during selection. Stop the selection if you want to stop people from joining until the battle starts."
      Done = True
    Case yhOpt_Status:
      T = "Enables/disables the /status command."
      Done = True
    Case yhOpt_Moves:
      T = "Enables/disables the /moves command."
      Done = True
    Case yhOpt_CPU:
      T = "Enables/disables the /cpu command, which switches control of your character to the CPU. By default, this is off, because it can be abused, but you can turn it on if you like."
      Done = True
    Case yhOpt_Attacking:
      T = "Enables/disables the /attacking command."
      Done = True
    Case yhOpt_Version:
      T = "Enables/disables the /version command."
      Done = True
    Case yhOpt_Help:
      T = "Enables/disables the /help command."
      Done = True
    Case yhOpt_Defect:
      T = "Enables/disables team commands, including /defect, /newteam, and so on. The older ""Disable Defects"" option was removed, and this option is now used to turn them off or on."
      Done = True
    Case yhOpt_Type:
      T = "Enables/disables the /type command."
      Done = True
    Case yhOpt_FragCount:
      T = "Enables/disables the /fragcount command."
      Done = True
    Case yhOpt_LearnedMove:
      T = "Enables/disables the /learnedmove command."
      Done = True
    Case yhOpt_RuneCmd:
      T = "If checked, players can use /rune to see what rune they or others have. This does not change the ability of newcomers to get a random rune by typing /rune."
      Done = True
    Case yhOpt_GetRune:
      T = "If checked, players can use /rune to pick a random rune if they don't have one. If unchecked, newcomers cannot grab runes."
      Done = True
    Case yhOpt_Weapon:
      T = "Enables/disables the /weapon command."
      Done = True
    Case yhOpt_WpnList:
      T = "Enables/disables the /wpnlist command."
      Done = True
    Case yhOpt_OutScroll:
      T = "Changes the interval at which ChUB sends output to the chat room or output window. This rate is in milliseconds."
      Done = True
    Case yhOpt_ScRa:
      T = "ChUB will send this many lines at each scroll interval."
      Done = True
    Case yhOpt_MaxPlayers:
      T = "You can change the maximum amount of players allowed in the game. I don't recommend a value any higher than 12 -- this causes the output to ""lag behind"" the actual action. If this happens, you can change the Lagometer."
      Done = True
    Case yhOpt_Lag:
      T = "This changes the speed of the battle. 1000 is normal speed, which is kinda fast, so I recommend at least 2000, or 3000 for games with at least 8 players."
      Done = True
    Case yhOpt_StatusX:
      T = "If you turn this off, status such as Bless, Stun, etc. are disabled. Only HP-changing moves will work."
      Done = True
    Case yhRant:
      Select Case Cnt
        Case 0: T = "This is Kamek's favorite part of the game, the Rant section! You can use this window to display various messages in boldface-italicized-underlined red text."
        Case 1: T = "You may click any of the presets, or type your own rant or raving in the textbox."
        Done = True
      End Select
    Case yhTwit:
      Select Case Cnt
        Case 0: T = "This is a user-friendly version of the TWIT.TXT editor (compared to a text editor). You can use this window to twit people without having to restart the program."
        Case 1: T = "To add someone, type their name into the Screen Name box and hit the ADD button pointing to the appropriate twit list."
        Case 2: T = "The TWIT list will prevent the listed names from playing or using any commands."
        Case 3: T = "The BACKTWIT list will allow only those people to play or do any commands."
        Case 4: T = "Remember, this editor does NOT automatically save the new TWIT.TXT to disk! Click the Sav button to save the list and preserve it for the next session."
        Done = True
      End Select
    Case yhWhat:
      T = "Huh???... I don't know what this window is for. Lemme know if you figure it out."
      Done = True
    Case yhGraph:
      T = "Hey, cool, you found a graph thingy..."
      Done = True
    Case yhIniLoad:
      T = "Select a dataset from the ones shown. If you want to make your own dataset, read DATASET.TXT! But don't ask me, I can't figure it out. I'm only a Yoshi..."
      Done = True
    Case yhPotty:
      Select Case Rand(1, 11)
        Case 1: T = "::gasp:: Potty mouth!"
        Case 2: T = "Do you EAT with that mouth, " + YourSN + "?"
        Case 3: T = "Someone's got a potty mouth..."
        Case 4: T = "Now that's not very nice!"
        Case 5: T = "That doesn't make me happy, " + YourSN + "."
        Case 6: T = "Oh, stop that, " + YourSN + "!"
        Case 7: T = "I haven't heard such foul language since I kicked Kamek's butt!"
        Case 8: T = "Do you kiss your mother with that mouth?"
        Case 9: T = "Now you go wash your mouth out with soap, buster!"
        Case 10: T = "GADZOOKS!!!"
        Case 11: T = "Sorry, but this is game has a PG rating."
      End Select
      Done = True
    Case yhGiveUp:
      T = "Fine! You win!... I don't give a rat's ass if you cuss any more. Sheeeeeeiat!...."
      SaveSetting "ChUB Resurrection", "Yoshi!", "Cuss", "NoMore"
      Done = True
  End Select
  Cnt = Cnt + 1
  lbYoshi.Caption = T
  If Done Then
    cNext.Enabled = False
    cOK.Caption = "OK!"
    cOK.Default = True
  End If
End Sub

Private Sub cOK_Click()
  If YoshiHelp = yhGiveUp Then Unload fShootYoshi
  YoshiHelp = yhDone
  Unload Me
End Sub

Private Sub PlayYoshi()
Dim Ret%
  Ret = mciSendString("close YoshiSound", 0&, 0, 0)
  Ret = mciSendString("open " + App.Path + "\sounds\yoshi.wav type waveaudio alias YoshiSound", 0&, 0, 0)
  Ret = mciSendString("play YoshiSound", 0&, 0, 0)
End Sub
Private Sub Form_Load()
  SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3
  Cnt = 0
  'cNext.Enabled = False
  cNext_Click
End Sub

Private Sub imgYoshi_Click(index As Integer)
  If imgYoshi(index).MousePointer = 2 Then
    YoshiHelp = yhDone
    Unload Me
    fShootYoshi.Show
  End If
End Sub

Private Sub imgYoshi_MouseMove(index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Shift And YoshiHelp = yhPotty Then
    imgYoshi(index).MousePointer = 2
  Else
    imgYoshi(index).MousePointer = 0
  End If
End Sub

Private Sub tmrYoshi_Timer()
Static Yr As Integer
Static a As Integer
Const LowerBound = -15
Const UpperBound = 15
  If Yr = 0 And a = 0 Then
    Yr = LowerBound
    a = 1
  End If
  imgYoshi(0).Top = imgYoshi(0).Top + Yr
  Yr = Yr + a
  If Yr > UpperBound Then a = -1
  If Yr < LowerBound Then a = 1
End Sub
