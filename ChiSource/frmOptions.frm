VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Begin VB.Form frmOptions 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Options"
   ClientHeight    =   4920
   ClientLeft      =   2565
   ClientTop       =   1500
   ClientWidth     =   6150
   Icon            =   "frmOptions.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4920
   ScaleWidth      =   6150
   ShowInTaskbar   =   0   'False
   Begin VB.PictureBox picOptions 
      Height          =   3780
      Index           =   4
      Left            =   -20000
      ScaleHeight     =   3720
      ScaleWidth      =   5625
      TabIndex        =   71
      TabStop         =   0   'False
      Top             =   480
      Width           =   5685
      Begin VB.CommandButton cRecordsWipe 
         Caption         =   "Erase Records"
         Height          =   615
         Left            =   4200
         TabIndex        =   84
         Top             =   3000
         Width           =   1335
      End
      Begin VB.TextBox tLag 
         Height          =   285
         Left            =   3720
         TabIndex        =   77
         Text            =   "2000"
         Top             =   840
         Width           =   495
      End
      Begin VB.TextBox tMaxPlayers 
         Height          =   285
         Left            =   3720
         TabIndex        =   74
         Text            =   "10"
         Top             =   240
         Width           =   375
      End
      Begin ComctlLib.Slider sldMaxPlayers 
         Height          =   375
         Left            =   1440
         TabIndex        =   73
         Top             =   240
         Width           =   2295
         _ExtentX        =   4048
         _ExtentY        =   661
         _Version        =   327682
         Min             =   2
         Max             =   25
         SelStart        =   10
         Value           =   10
      End
      Begin ComctlLib.Slider sldLag 
         Height          =   375
         Left            =   1440
         TabIndex        =   76
         Top             =   840
         Width           =   2295
         _ExtentX        =   4048
         _ExtentY        =   661
         _Version        =   327682
         LargeChange     =   100
         SmallChange     =   10
         Min             =   100
         Max             =   8000
         SelStart        =   2000
         TickFrequency   =   800
         Value           =   2000
      End
      Begin VB.Label Label2 
         Alignment       =   1  'Right Justify
         Caption         =   "Battle Lag-O-Meter"
         Height          =   495
         Left            =   120
         TabIndex        =   75
         Top             =   840
         Width           =   1095
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "Maximum # of Players in a game"
         Height          =   615
         Left            =   120
         TabIndex        =   72
         Top             =   120
         Width           =   1095
      End
   End
   Begin VB.PictureBox picOptions 
      Height          =   3780
      Index           =   0
      Left            =   240
      ScaleHeight     =   3720
      ScaleWidth      =   5625
      TabIndex        =   4
      TabStop         =   0   'False
      Top             =   480
      Width           =   5685
      Begin VB.CheckBox cStatusE 
         Caption         =   "Status"
         Height          =   255
         Left            =   2640
         TabIndex        =   78
         Top             =   840
         Width           =   855
      End
      Begin VB.CheckBox cMDITop 
         Caption         =   "Keep main window on top of other windows"
         Height          =   375
         Left            =   3600
         TabIndex        =   70
         ToolTipText     =   "The main ChUB window will stay on top"
         Top             =   600
         Width           =   1935
      End
      Begin VB.TextBox tFragLimit 
         Height          =   285
         Left            =   4320
         TabIndex        =   35
         ToolTipText     =   "Game will end when this many frags is achieved. Use negative value for Fatalities instead"
         Top             =   1080
         Width           =   735
      End
      Begin VB.TextBox tGetRate 
         Height          =   285
         Left            =   4320
         TabIndex        =   32
         Top             =   1440
         Width           =   735
      End
      Begin VB.TextBox tFlagLimit 
         Height          =   285
         Left            =   4320
         TabIndex        =   29
         Top             =   1800
         Width           =   735
      End
      Begin VB.TextBox tTLimit 
         Height          =   285
         Left            =   4320
         TabIndex        =   26
         Top             =   2160
         Width           =   735
      End
      Begin VB.TextBox tDmgMult 
         Height          =   285
         Left            =   4320
         TabIndex        =   23
         ToolTipText     =   "100 = Normal Damage. 50=Half Damage. Recomend you change this if you change Max HP"
         Top             =   2520
         Width           =   735
      End
      Begin VB.CheckBox cCTF 
         Caption         =   "Capture the Flag Mode"
         Height          =   255
         Left            =   2760
         TabIndex        =   20
         ToolTipText     =   "Kill the person with the flag and hold onto it for a set amount of time to win"
         Top             =   360
         Width           =   1935
      End
      Begin VB.TextBox tMaxSP 
         Height          =   285
         Left            =   4320
         TabIndex        =   19
         Top             =   3240
         Width           =   735
      End
      Begin VB.TextBox tMaxHP 
         Height          =   285
         Left            =   4320
         TabIndex        =   18
         Top             =   2880
         Width           =   735
      End
      Begin ComctlLib.Slider sldMaxHP 
         Height          =   375
         Left            =   1920
         TabIndex        =   16
         Top             =   2880
         Width           =   2415
         _ExtentX        =   4260
         _ExtentY        =   661
         _Version        =   327682
         LargeChange     =   100
         SmallChange     =   10
         Min             =   100
         Max             =   5000
         SelStart        =   600
         TickFrequency   =   1000
         Value           =   600
      End
      Begin VB.CheckBox cExtendedSupers 
         Caption         =   "Allow Level 4, 5 Super Attacks"
         Height          =   255
         Left            =   120
         TabIndex        =   13
         ToolTipText     =   "Enables Level 4 and Level 5 Super Attacks"
         Top             =   840
         Width           =   2535
      End
      Begin VB.CheckBox cMultiTarget 
         Caption         =   "Allow Moves that Hit More Than One Target"
         Height          =   255
         Left            =   120
         TabIndex        =   12
         ToolTipText     =   "Disable to remove cheap AllFoe attacks"
         Top             =   600
         Width           =   3495
      End
      Begin VB.CheckBox cRespawn 
         Caption         =   "Respawn after 30 G.S. of death"
         Height          =   255
         Left            =   120
         TabIndex        =   11
         ToolTipText     =   "Off = No Respawning"
         Top             =   360
         Width           =   2655
      End
      Begin VB.CheckBox cRunes 
         Caption         =   "Enable Runes"
         Height          =   255
         Left            =   4080
         TabIndex        =   10
         ToolTipText     =   "Turn runes on/off"
         Top             =   120
         Width           =   1455
      End
      Begin VB.CheckBox cWeapons 
         Caption         =   "Enable Weapons"
         Height          =   255
         Left            =   2400
         TabIndex        =   9
         ToolTipText     =   "Turn weapons on/off"
         Top             =   120
         Width           =   1575
      End
      Begin VB.CheckBox cSameChar 
         Caption         =   "Allow Duplicate Characters"
         Height          =   255
         Left            =   120
         TabIndex        =   8
         ToolTipText     =   "More than one person can pick the same character"
         Top             =   120
         Width           =   2295
      End
      Begin ComctlLib.Slider sldMaxSP 
         Height          =   375
         Left            =   1920
         TabIndex        =   17
         Top             =   3240
         Width           =   2415
         _ExtentX        =   4260
         _ExtentY        =   661
         _Version        =   327682
         LargeChange     =   100
         SmallChange     =   10
         Max             =   9999
         SelStart        =   600
         TickFrequency   =   1000
         Value           =   600
      End
      Begin ComctlLib.Slider sldDmgMult 
         Height          =   375
         Left            =   1920
         TabIndex        =   22
         ToolTipText     =   "100 = Normal Damage. 50=Half Damage. Recomend you change this if you change Max HP"
         Top             =   2520
         Width           =   2415
         _ExtentX        =   4260
         _ExtentY        =   661
         _Version        =   327682
         LargeChange     =   100
         SmallChange     =   10
         Min             =   10
         Max             =   1000
         SelStart        =   100
         TickFrequency   =   100
         Value           =   100
      End
      Begin ComctlLib.Slider sldTLimit 
         Height          =   375
         Left            =   1920
         TabIndex        =   25
         Top             =   2160
         Width           =   2415
         _ExtentX        =   4260
         _ExtentY        =   661
         _Version        =   327682
         LargeChange     =   100
         SmallChange     =   10
         Max             =   3600
         TickFrequency   =   300
      End
      Begin ComctlLib.Slider sldFlagLimit 
         Height          =   375
         Left            =   1920
         TabIndex        =   28
         Top             =   1800
         Width           =   2415
         _ExtentX        =   4260
         _ExtentY        =   661
         _Version        =   327682
         LargeChange     =   30
         SmallChange     =   10
         Max             =   960
         SelStart        =   30
         TickFrequency   =   10
         Value           =   30
      End
      Begin ComctlLib.Slider sldGetRate 
         Height          =   375
         Left            =   1920
         TabIndex        =   31
         Top             =   1440
         Width           =   2415
         _ExtentX        =   4260
         _ExtentY        =   661
         _Version        =   327682
         LargeChange     =   50
         SmallChange     =   10
         Max             =   200
         SelStart        =   30
         TickFrequency   =   50
         Value           =   30
      End
      Begin ComctlLib.Slider sldFragLimit 
         Height          =   375
         Left            =   1920
         TabIndex        =   34
         ToolTipText     =   "Game will end when this many frags is achieved. Use negative value for Fatalities instead"
         Top             =   1080
         Width           =   2415
         _ExtentX        =   4260
         _ExtentY        =   661
         _Version        =   327682
         LargeChange     =   50
         SmallChange     =   10
         Min             =   -20
         Max             =   20
         TickFrequency   =   5
      End
      Begin VB.Label lblFragLimit 
         Alignment       =   1  'Right Justify
         Caption         =   "Frags Req'd. To Win"
         Height          =   255
         Left            =   120
         TabIndex        =   33
         ToolTipText     =   "A number indicating the rate of new items/weapons to spawn into battle"
         Top             =   1200
         Width           =   1695
      End
      Begin VB.Label lblGetRate 
         Alignment       =   1  'Right Justify
         Caption         =   "Item Spawn Rate"
         Height          =   255
         Left            =   120
         TabIndex        =   30
         ToolTipText     =   "A number indicating the rate of new items/weapons to spawn into battle"
         Top             =   1560
         Width           =   1695
      End
      Begin VB.Label lblFlagLimit 
         Alignment       =   1  'Right Justify
         Caption         =   "Flag Time Limit (G.S.)"
         Height          =   255
         Left            =   120
         TabIndex        =   27
         ToolTipText     =   "G.S. = Game Second. 1 G.S.=Lagometer Value/1000 seconds"
         Top             =   1920
         Width           =   1695
      End
      Begin VB.Label lblTLimit 
         Alignment       =   1  'Right Justify
         Caption         =   "Game Time Limit (G.S.)"
         Height          =   255
         Left            =   120
         TabIndex        =   24
         ToolTipText     =   "G.S. = Game Second. 1 G.S.=Lagometer Value/1000 seconds"
         Top             =   2280
         Width           =   1695
      End
      Begin VB.Label lblDmgMult 
         Alignment       =   1  'Right Justify
         Caption         =   "Damage Multiplier (%)"
         Height          =   255
         Left            =   120
         TabIndex        =   21
         ToolTipText     =   "100 = Normal Damage. 50=Half Damage. Recomend you change this if you change Max HP"
         Top             =   2640
         Width           =   1695
      End
      Begin VB.Label lblMaxSP 
         Alignment       =   1  'Right Justify
         Caption         =   "Maximum Super Points"
         Height          =   255
         Left            =   120
         TabIndex        =   15
         Top             =   3360
         Width           =   1695
      End
      Begin VB.Label lblMaxHP 
         Alignment       =   1  'Right Justify
         Caption         =   "Maximum Hit Points"
         Height          =   255
         Left            =   120
         TabIndex        =   14
         Top             =   3000
         Width           =   1695
      End
   End
   Begin VB.PictureBox picOptions 
      Height          =   3780
      Index           =   3
      Left            =   -20000
      ScaleHeight     =   3720
      ScaleWidth      =   5625
      TabIndex        =   7
      TabStop         =   0   'False
      Top             =   480
      Width           =   5685
      Begin VB.CommandButton cRandAna 
         Caption         =   "Random"
         Height          =   255
         Left            =   2880
         TabIndex        =   86
         Top             =   3240
         Width           =   855
      End
      Begin VB.CommandButton cShow 
         Caption         =   "Show"
         Height          =   255
         Left            =   1920
         TabIndex        =   79
         Top             =   3240
         Width           =   615
      End
      Begin VB.CommandButton cmdNext 
         Height          =   495
         Left            =   4680
         Picture         =   "frmOptions.frx":000C
         Style           =   1  'Graphical
         TabIndex        =   69
         Top             =   3120
         Width           =   735
      End
      Begin VB.CommandButton cmdPrev 
         Height          =   495
         Left            =   240
         Picture         =   "frmOptions.frx":0316
         Style           =   1  'Graphical
         TabIndex        =   68
         Top             =   3120
         Width           =   735
      End
      Begin VB.Label lblDesc 
         Alignment       =   2  'Center
         Height          =   255
         Index           =   9
         Left            =   120
         TabIndex        =   67
         Top             =   2880
         Width           =   5415
      End
      Begin VB.Label lblDesc 
         Alignment       =   2  'Center
         Height          =   255
         Index           =   8
         Left            =   120
         TabIndex        =   66
         Top             =   2640
         Width           =   5415
      End
      Begin VB.Label lblDesc 
         Alignment       =   2  'Center
         Height          =   255
         Index           =   7
         Left            =   120
         TabIndex        =   65
         Top             =   2400
         Width           =   5415
      End
      Begin VB.Label lblDesc 
         Alignment       =   2  'Center
         Height          =   255
         Index           =   6
         Left            =   120
         TabIndex        =   64
         Top             =   2160
         Width           =   5415
      End
      Begin VB.Label lblDesc 
         Alignment       =   2  'Center
         Height          =   255
         Index           =   5
         Left            =   120
         TabIndex        =   63
         Top             =   1920
         Width           =   5415
      End
      Begin VB.Label lblDesc 
         Alignment       =   2  'Center
         Height          =   255
         Index           =   4
         Left            =   120
         TabIndex        =   62
         Top             =   1680
         Width           =   5415
      End
      Begin VB.Label lblDesc 
         Alignment       =   2  'Center
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   61
         Top             =   1440
         Width           =   5415
      End
      Begin VB.Label lblDesc 
         Alignment       =   2  'Center
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   60
         Top             =   1200
         Width           =   5415
      End
      Begin VB.Label lblDesc 
         Alignment       =   2  'Center
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   59
         Top             =   960
         Width           =   5415
      End
      Begin VB.Label lblDesc 
         Alignment       =   2  'Center
         Caption         =   "Load a dataset before changing your arena..."
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   58
         Top             =   720
         Width           =   5415
      End
      Begin VB.Label lblArenaName 
         Alignment       =   2  'Center
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Hey!"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   18
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   495
         Left            =   120
         TabIndex        =   57
         Top             =   120
         Width           =   5415
      End
   End
   Begin VB.PictureBox picOptions 
      Height          =   3780
      Index           =   2
      Left            =   -24000
      ScaleHeight     =   3720
      ScaleWidth      =   5625
      TabIndex        =   6
      TabStop         =   0   'False
      Top             =   480
      Width           =   5685
      Begin VB.CommandButton Command1 
         Caption         =   "Install ChUB into mIRC"
         Height          =   615
         Left            =   3840
         TabIndex        =   85
         Top             =   1320
         Width           =   1095
      End
      Begin VB.TextBox tmIRC 
         Height          =   285
         Left            =   1800
         TabIndex        =   82
         Text            =   "mIRC32 - ChUB"
         Top             =   1200
         Width           =   1815
      End
      Begin VB.TextBox tmIRCChannel 
         Height          =   285
         Left            =   1800
         TabIndex        =   81
         Text            =   "#ChUB"
         Top             =   1680
         Width           =   1815
      End
      Begin VB.TextBox tScRa 
         Height          =   285
         Left            =   3360
         TabIndex        =   56
         Text            =   "1"
         Top             =   600
         Width           =   615
      End
      Begin VB.TextBox tOutScroll 
         Height          =   285
         Left            =   3360
         TabIndex        =   55
         Text            =   "4000"
         Top             =   120
         Width           =   615
      End
      Begin ComctlLib.Slider sldScRa 
         Height          =   480
         Left            =   1680
         TabIndex        =   51
         Top             =   600
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   847
         _Version        =   327682
         LargeChange     =   1
         Min             =   1
         Max             =   4
         SelStart        =   1
         Value           =   1
      End
      Begin ComctlLib.Slider sldOutScroll 
         Height          =   480
         Left            =   1680
         TabIndex        =   52
         Top             =   120
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   847
         _Version        =   327682
         LargeChange     =   100
         SmallChange     =   10
         Min             =   1
         Max             =   4000
         SelStart        =   1
         TickFrequency   =   500
         Value           =   1
      End
      Begin VB.Label Label4 
         Alignment       =   1  'Right Justify
         Caption         =   "mIRC Window Title"
         Height          =   375
         Left            =   240
         TabIndex        =   83
         Top             =   1200
         Width           =   1335
      End
      Begin VB.Label Label3 
         Alignment       =   1  'Right Justify
         Caption         =   "Output Channel"
         Height          =   255
         Left            =   240
         TabIndex        =   80
         Top             =   1680
         Width           =   1335
      End
      Begin VB.Label lblOutRate 
         Alignment       =   1  'Right Justify
         Caption         =   "Output Scroll Delay"
         Height          =   255
         Left            =   120
         TabIndex        =   54
         Top             =   120
         Width           =   1455
      End
      Begin VB.Label lblScRa 
         Alignment       =   1  'Right Justify
         Caption         =   "Lines Scrolled Per Interval"
         Height          =   495
         Left            =   120
         TabIndex        =   53
         Top             =   600
         Width           =   1455
      End
   End
   Begin VB.PictureBox picOptions 
      Height          =   3780
      Index           =   1
      Left            =   -20000
      ScaleHeight     =   3720
      ScaleWidth      =   5625
      TabIndex        =   5
      TabStop         =   0   'False
      Top             =   480
      Width           =   5685
      Begin VB.CheckBox cWpnList 
         Caption         =   "Allow /wpnlist"
         Height          =   255
         Left            =   120
         TabIndex        =   50
         Top             =   3480
         Width           =   3135
      End
      Begin VB.CheckBox cWeapon 
         Caption         =   "Allow /weapon to see what weapon you have"
         Height          =   255
         Left            =   120
         TabIndex        =   49
         Top             =   3240
         Width           =   3855
      End
      Begin VB.CheckBox cGetRune 
         Caption         =   "Allow /rune to get a rune if you don't have one"
         Height          =   255
         Left            =   120
         TabIndex        =   48
         Top             =   3000
         Width           =   3615
      End
      Begin VB.CheckBox cRuneCmd 
         Caption         =   "Allow /rune to see what rune you have"
         Height          =   255
         Left            =   120
         TabIndex        =   47
         Top             =   2760
         Width           =   3135
      End
      Begin VB.CheckBox cLearnedMove 
         Caption         =   "Allow /learnedmove"
         Height          =   255
         Left            =   120
         TabIndex        =   46
         Top             =   2520
         Width           =   3135
      End
      Begin VB.CheckBox cFragCount 
         Caption         =   "Allow /fragcount"
         Height          =   255
         Left            =   120
         TabIndex        =   45
         Top             =   2280
         Width           =   3135
      End
      Begin VB.CheckBox cType 
         Caption         =   "Allow /type"
         Height          =   255
         Left            =   120
         TabIndex        =   44
         Top             =   2040
         Width           =   3135
      End
      Begin VB.CheckBox cDefect 
         Caption         =   "Allow team commands (defect, etc.)"
         Height          =   255
         Left            =   120
         TabIndex        =   43
         Top             =   1800
         Width           =   3135
      End
      Begin VB.CheckBox cHelp 
         Caption         =   "Allow /help"
         Height          =   255
         Left            =   120
         TabIndex        =   42
         Top             =   1560
         Width           =   1695
      End
      Begin VB.CheckBox cVersion 
         Caption         =   "Allow /version"
         Height          =   255
         Left            =   120
         TabIndex        =   41
         Top             =   1320
         Width           =   1695
      End
      Begin VB.CheckBox cAttacking 
         Caption         =   "Allow /attacking"
         Height          =   255
         Left            =   120
         TabIndex        =   40
         Top             =   1080
         Width           =   1695
      End
      Begin VB.CheckBox cCPU 
         Caption         =   "Allow /cpu"
         Height          =   255
         Left            =   120
         TabIndex        =   39
         Top             =   840
         Width           =   1695
      End
      Begin VB.CheckBox cMoves 
         Caption         =   "Allow /moves"
         Height          =   255
         Left            =   120
         TabIndex        =   38
         Top             =   600
         Width           =   1695
      End
      Begin VB.CheckBox cStatus 
         Caption         =   "Allow /status"
         Height          =   255
         Left            =   120
         TabIndex        =   37
         Top             =   360
         Width           =   1695
      End
      Begin VB.CheckBox cNoJoin 
         Caption         =   "Allow Joining"
         Height          =   255
         Left            =   120
         TabIndex        =   36
         Top             =   120
         Width           =   1695
      End
   End
   Begin VB.CommandButton cmdApply 
      Caption         =   "Apply"
      Height          =   375
      Left            =   4920
      TabIndex        =   3
      Top             =   4455
      Width           =   1095
   End
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   375
      Left            =   3720
      TabIndex        =   2
      Top             =   4455
      Width           =   1095
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "OK"
      Height          =   375
      Left            =   2490
      TabIndex        =   1
      Top             =   4455
      Width           =   1095
   End
   Begin ComctlLib.TabStrip tbsOptions 
      Height          =   4365
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   5895
      _ExtentX        =   10398
      _ExtentY        =   7699
      _Version        =   327682
      BeginProperty Tabs {0713E432-850A-101B-AFC0-4210102A8DA7} 
         NumTabs         =   5
         BeginProperty Tab1 {0713F341-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Battle"
            Key             =   "grpBattle"
            Object.Tag             =   ""
            Object.ToolTipText     =   "Set Battle Options"
            ImageVarType    =   2
         EndProperty
         BeginProperty Tab2 {0713F341-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Commands"
            Key             =   "grpCommands"
            Object.Tag             =   ""
            Object.ToolTipText     =   "Toggle allowable commands"
            ImageVarType    =   2
         EndProperty
         BeginProperty Tab3 {0713F341-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Output"
            Key             =   "grpOutput"
            Object.Tag             =   ""
            Object.ToolTipText     =   "Set Output Options"
            ImageVarType    =   2
         EndProperty
         BeginProperty Tab4 {0713F341-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Arena"
            Key             =   "grpArena"
            Object.Tag             =   ""
            Object.ToolTipText     =   "Change Arena"
            ImageVarType    =   2
         EndProperty
         BeginProperty Tab5 {0713F341-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Misc"
            Key             =   "grpMisc"
            Object.Tag             =   ""
            Object.ToolTipText     =   "Battle Lag-O-Meter, # of Maximum Players, etc"
            ImageVarType    =   2
         EndProperty
      EndProperty
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
End
Attribute VB_Name = "frmOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim cArena%

Private Sub UpdatePreview()
Dim R%, G%, B%
  On Error Resume Next
  'lblPreview.Font.name = cboFontName.List(cboFontName.ListIndex)
  'lblPreview.ForeColor = RGB(sldRed.Value, sldGreen.Value, sldBlue.Value)
  'tHTMLValue.Text = MakeHTMLColor(sldRed.Value, sldGreen.Value, sldBlue.Value)
End Sub

Private Sub cAttacking_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_Attacking)
End Sub

Private Sub cboFontName_Change()
  UpdatePreview
End Sub

Private Sub cboFontName_Click()
  cboFontName_Change
End Sub

Private Sub cCPU_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_CPU)
End Sub

Private Sub cCTF_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_CTF)
End Sub

Private Sub cDefect_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_Defect)
End Sub

Private Sub cExtendedSupers_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_ExtendedSupers)
End Sub

Private Sub cFragCount_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_FragCount)
End Sub

Private Sub cGetRune_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_GetRune)
End Sub

Private Sub cHelp_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_Help)
End Sub

Private Sub cLearnedMove_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_LearnedMove)
End Sub

Private Sub cmdApply_Click()
  SaveChanges
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cMDITop_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_MDITop)
End Sub

Private Sub cmdNext_Click()
  cArena = cArena + 1
  If (cArena = MaxArena) Then
    cmdNext.Enabled = False
  End If
  If (cArena > 1) Then
    cmdPrev.Enabled = True
  End If
  UpdateArenaWind
End Sub

Private Sub cmdOK_Click()
  SaveChanges
  Unload Me
End Sub
Private Sub cmdPrev_Click()
  cArena = cArena - 1
  If (cArena = 1) Then
    cmdPrev.Enabled = False
  End If
  If (cArena < MaxArena) Then
    cmdNext.Enabled = True
  End If
  UpdateArenaWind
End Sub

Private Sub cMoves_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_Moves)
End Sub

Private Sub cMultiTarget_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_MultiTarget)
End Sub

Private Sub cNoJoin_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_NoJoin)
End Sub

Private Sub Command1_Click()
  If kDlgBoxfn("Before proceeding, ensure that mIRC is loaded and you have configured the mIRC Window Title and desired output channel.", vbYesNo + vbCritical, "Wait!") = vbYes Then
    mIRCStatusSend "/load -rs """ + App.Path + "\chub.mrc"""
    mIRCStatusSend "/set %ChUBChan " + mIRCChannel
    kDlgBox "ChUB Resurrection has been installed into mIRC.", vbOKOnly + vbInformation, "Installed"
  End If
End Sub

Private Sub cRandAna_Click()
  cArena = Rand(1, MaxArena)
  UpdateArenaWind
End Sub

Private Sub cRecordsWipe_Click()
Dim x%, L%, ChUBRecords As Variant
  On Error Resume Next
  x% = kDlgBoxfn("Warning. This will erase ALL recorded frag counts, the current champion, and the Graveyard. Are you ABSOLUTELY SURE?", vbYesNo + vbExclamation, "WARNING!")
  If x = 6 Then
    ChUBRecords = GetAllSettings("ChUB Resurrection W/L", "Wins")
    For L = LBound(ChUBRecords, 1) To UBound(ChUBRecords, 1)
      DeleteSetting "ChUB Resurrection W/L", "Wins", ChUBRecords(L, 0)
    Next L
    ChUBRecords = GetAllSettings("ChUB Resurrection W/L", "Losses")
    For L = LBound(ChUBRecords, 1) To UBound(ChUBRecords, 1)
      DeleteSetting "ChUB Resurrection W/L", "Losses", ChUBRecords(L, 0)
    Next L
    ChUBRecords = GetAllSettings("ChUB Resurrection W/L", "SNWins")
    For L = LBound(ChUBRecords, 1) To UBound(ChUBRecords, 1)
      DeleteSetting "ChUB Resurrection W/L", "SNWins", ChUBRecords(L, 0)
    Next L
    ChUBRecords = GetAllSettings("ChUB Resurrection W/L", "SNLosses")
    For L = LBound(ChUBRecords, 1) To UBound(ChUBRecords, 1)
      DeleteSetting "ChUB Resurrection W/L", "SNLosses", ChUBRecords(L, 0)
    Next L
    DeleteSetting "ChUB Resurrection", "Graveyard", "Tombs"
    DeleteSetting "ChUB Resurrection", "ChampFrags", "Name"
    DeleteSetting "ChUB Resurrection", "ChampFrags", "Wins"
    kDlgBox "The requested data has been erased.", vbOKOnly + vbInformation, "Gone"
  End If
End Sub

Private Sub cRespawn_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_Respawn)
End Sub

Private Sub cRuneCmd_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_RuneCmd)
End Sub

Private Sub cRunes_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_Runes)
End Sub

Private Sub cSameChar_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_SameChar)
End Sub

Private Sub cShow_Click()
Dim x
  If NumSenshi > 0 Then
    Send "Arena: " + Arena(cArena).name
    For x = 1 To 10
      If Arena(cArena).Desc(x) <> "" Then Send "[ " + Arena(cArena).Desc(x)
    Next x
  End If
End Sub

Private Sub cStatus_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_Status)
End Sub

Private Sub cStatusE_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi yhOpt_StatusX
End Sub

Private Sub cType_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_Type)
End Sub

Private Sub cVersion_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_Version)
End Sub

Private Sub cWeapon_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_Weapon)
  
End Sub

Private Sub cWeapons_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_Weapons)
End Sub

Private Sub cWpnList_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_WpnList)
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
    Dim I As Integer
    'handle ctrl+tab to move to the next tab
    If Shift = vbCtrlMask And KeyCode = vbKeyTab Then
        I = tbsOptions.SelectedItem.index
        If I = tbsOptions.Tabs.Count Then
            'last tab so we need to wrap to tab 1
            Set tbsOptions.SelectedItem = tbsOptions.Tabs(1)
        Else
            'increment the tab
            Set tbsOptions.SelectedItem = tbsOptions.Tabs(I + 1)
        End If
    End If
End Sub

Private Sub Form_Load()
  Dim x%
  'center the form
  Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
  If Config.MDIOnTop Then
    SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3
  Else
    SetWindowPos Me.hwnd, -2, 0, 0, 0, 0, 3
  End If
  picOptions(0).Left = 240
  picOptions(1).Left = -20000
  picOptions(2).Left = -20000
  picOptions(3).Left = -20000
  picOptions(4).Left = -20000
  'For X% = 1 To Screen.FontCount
  '  cboFontName.AddItem Screen.Fonts(X)
  'Next X%
  UpdateOptionWind
  Me.Show
  If Config.NewUser Then Yoshi (yhOptions)
End Sub

Private Sub picOptions_Click(index As Integer)
Static Strikes%
  'Strikes = Strikes + 1
  'Select Case Strikes:
  '  Case 1: kDlgBox "You missed. Strike one!", 16, "ChUB Resurrection"
  '  Case 2: kDlgBox "Steeeerrrrrrike two! (One more strike and you're out!)", 16, "ChUB Resurrection"
  '  Case 3:
  '    kDlgBox "Strike three! Yer outta here!", 16, "ChUB Resurrection"
  '    ScrollSend1 (YourSN + " struck out of the game!")
  '    End
  'End Select
End Sub

Private Sub sldBlue_Change()
  'tBlue.Text = TrimStr(sldBlue.Value)
  UpdatePreview
End Sub

Private Sub sldBlue_Scroll()
  ''sldBlue_Change
End Sub

Private Sub sldDmgMult_Change()
  tDmgMult.Text = TrimStr(sldDmgMult.Value)
End Sub

Private Sub sldDmgMult_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  tDmgMult_MouseUp Button, Shift, x, Y
End Sub

Private Sub sldDmgMult_Scroll()
  sldDmgMult_Change
End Sub

Private Sub sldFlagLimit_Change()
  tFlagLimit.Text = TrimStr(sldFlagLimit.Value)
End Sub

Private Sub sldFlagLimit_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  tFlagLimit_MouseUp Button, Shift, x, Y
End Sub

Private Sub sldFlagLimit_Scroll()
  sldFlagLimit_Change
End Sub

Private Sub sldFragLimit_Change()
  tFragLimit.Text = TrimStr(sldFragLimit.Value)
End Sub

Private Sub sldFragLimit_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  tFragLimit_MouseUp Button, Shift, x, Y
End Sub

Private Sub sldFragLimit_Scroll()
  sldFragLimit_Change
End Sub

Private Sub sldGetRate_Change()
  tGetRate.Text = TrimStr(sldGetRate.Value)
End Sub

Private Sub sldGetRate_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  tGetRate_MouseUp Button, Shift, x, Y
End Sub

Private Sub sldGetRate_Scroll()
  sldGetRate_Change
End Sub

Private Sub sldGreen_Change()
  'tGreen.Text = sldGreen.Value
  UpdatePreview
End Sub

Private Sub sldGreen_Scroll()
  sldGreen_Change
End Sub

Private Sub sldLag_Change()
  tLag.Text = TrimStr(sldLag.Value)
End Sub

Private Sub sldLag_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_Lag)
End Sub

Private Sub sldLag_Scroll()
  sldLag_Change
End Sub

Private Sub sldMaxHP_Change()
  tMaxHP.Text = TrimStr(sldMaxHP.Value)
End Sub

Private Sub sldMaxHP_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  tMaxHP_MouseUp Button, Shift, x, Y
End Sub

Private Sub sldMaxHP_Scroll()
  sldMaxHP_Change
End Sub

Private Sub sldMaxPlayers_Change()
  tMaxPlayers.Text = TrimStr(sldMaxPlayers.Value)
End Sub

Private Sub sldMaxPlayers_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_MaxPlayers)
End Sub

Private Sub sldMaxPlayers_Scroll()
  sldMaxPlayers_Change
End Sub

Private Sub sldMaxSP_Change()
  tMaxSP.Text = TrimStr(sldMaxSP.Value)
End Sub

Private Sub sldMaxSP_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  tMaxSP_MouseUp Button, Shift, x, Y
End Sub

Private Sub sldMaxSP_Scroll()
  sldMaxSP_Change
End Sub

Private Sub sldOutScroll_Change()
  tOutScroll.Text = TrimStr(sldOutScroll.Value)
End Sub

Private Sub sldOutScroll_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_OutScroll)
End Sub

Private Sub sldOutScroll_Scroll()
  sldOutScroll_Change
End Sub

Private Sub sldRed_Change()
  'tRed.Text = TrimStr(sldRed.Value)
  UpdatePreview
End Sub

Private Sub sldRed_Scroll()
  sldRed_Change
End Sub

Private Sub sldScRa_Change()
  tScRa.Text = TrimStr(sldScRa.Value)
End Sub

Private Sub sldScRa_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_ScRa)
End Sub

Private Sub sldScRa_Scroll()
  sldScRa_Change
End Sub

Private Sub sldTLimit_Change()
  tTLimit.Text = TrimStr(sldTLimit.Value)
End Sub

Private Sub sldTLimit_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  tTLimit_MouseUp Button, Shift, x, Y
End Sub

Private Sub sldTLimit_Scroll()
  sldTLimit_Change
End Sub

Private Sub tBlue_Change()
  'If Val(tBlue.Text) >= sldBlue.Min And Val(tBlue.Text) <= sldBlue.Max Then sldBlue.Value = Val(tBlue.Text)
  UpdatePreview
End Sub

Private Sub tbsOptions_Click()
    
    Dim I As Integer
    'show and enable the selected tab's controls
    'and hide and disable all others
    For I = 0 To tbsOptions.Tabs.Count - 1
        If I = tbsOptions.SelectedItem.index - 1 Then
            picOptions(I).Left = 240
            picOptions(I).Enabled = True
        Else
            picOptions(I).Left = -20000
            picOptions(I).Enabled = False
        End If
    Next
    
End Sub

Private Sub tDmgMult_Change()
  If Val(tDmgMult.Text) >= sldDmgMult.Min And Val(tDmgMult.Text) <= sldDmgMult.Max Then sldDmgMult.Value = Val(tDmgMult.Text)
End Sub

Private Sub tDmgMult_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_DmgMult)
End Sub

Private Sub tFlagLimit_Change()
  If Val(tFlagLimit.Text) >= sldFlagLimit.Min And Val(tFlagLimit.Text) >= sldFlagLimit.Min Then sldFlagLimit.Value = Val(tFlagLimit.Text)
End Sub

Private Sub tFlagLimit_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_FlagLimit)
End Sub

Private Sub tFragLimit_Change()
  If Val(tFragLimit.Text) >= sldFragLimit.Min And Val(tFragLimit.Text) >= sldFragLimit.Min Then sldFragLimit.Value = Val(tFragLimit.Text)
End Sub

Private Sub tFragLimit_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_FragLimit)
End Sub

Private Sub tGetRate_Change()
  If Val(tGetRate.Text) >= sldGetRate.Min And Val(tGetRate.Text) <= sldGetRate.Max Then sldGetRate.Value = Val(tGetRate.Text)
End Sub

Private Sub tGetRate_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_GetRate)
End Sub

Private Sub tGreen_Change()
'  If Val(tGreen.Text) >= sldGreen.Min And Val(tGreen.Text) <= sldGreen.Max Then sldGreen.Value = Val(tGreen.Text)
  UpdatePreview
End Sub

Private Sub tHTMLValue_LostFocus()
Dim R As Long, G As Long, B As Long, x As Long
  'X = Val(tHTMLValue.Text)
  On Error GoTo Quits
  B = (x - (x Mod 65536)) / 65536
  G = ((x Mod 65536) - (x Mod 256)) / 256
  R = (x Mod 65536) Mod 256
  'sldRed.Value = R
  'sldGreen.Value = R
  'sldBlue.Value = R
  'UpdatePreview
Quits:
  Exit Sub
End Sub

Private Sub tLag_Change()
  If Val(tLag.Text) >= sldLag.Min And Val(tLag.Text) <= sldLag.Max Then sldLag.Value = Val(tLag.Text)
End Sub

Private Sub tMaxHP_Change()
  If Val(tMaxHP.Text) >= sldMaxHP.Min And Val(tMaxHP.Text) <= sldMaxHP.Max Then sldMaxHP.Value = Val(tMaxHP.Text)
End Sub

Private Sub tMaxHP_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_MaxHP)
End Sub

Private Sub tMaxPlayers_Change()
  If Val(tMaxPlayers.Text) >= sldMaxPlayers.Min And Val(tMaxPlayers.Text) <= sldMaxPlayers.Max Then sldMaxPlayers.Value = Val(tMaxPlayers.Text)
End Sub

Private Sub tMaxSP_Change()
  If Val(tMaxSP.Text) >= sldMaxSP.Min And Val(tMaxSP.Text) <= sldMaxSP.Max Then sldMaxSP.Value = Val(tMaxSP.Text)
End Sub

Private Sub tMaxSP_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_MaxSP)
End Sub

Private Sub tOutScroll_Change()
  If Val(tOutScroll.Text) >= sldOutScroll.Min And Val(tOutScroll.Text) <= sldOutScroll.Max Then sldOutScroll.Value = Val(tOutScroll.Text)
End Sub

Private Sub tRed_Change()
'  If Val(tRed.Text) >= sldRed.Min And Val(tRed.Text) <= sldRed.Max Then sldRed.Value = Val(tRed.Text)
  UpdatePreview
End Sub

Private Sub tScRa_Change()
  If Val(tScRa.Text) >= sldScRa.Min And Val(tScRa.Text) <= sldScRa.Max Then sldScRa.Value = Val(tScRa.Text)
End Sub

Private Sub tTLimit_Change()
  If Val(tTLimit.Text) >= sldTLimit.Min And Val(tTLimit.Text) <= sldTLimit.Max Then sldTLimit.Value = Val(tTLimit.Text)
End Sub
Private Sub UpdateArenaWind()
Dim x%
  If NumSenshi > 0 Then
    If (cArena > MaxArena) Then cArena = MaxArena
    If (cArena < 1) Then cArena = 1
    For x = 1 To 10
      lblDesc(x - 1).Caption = Arena(cArena).Desc(x)
    Next x
    lblArenaName.Caption = Arena(cArena).name
  End If
End Sub
Private Sub UpdateOptionWind()
Dim x%
  cArena = CurArena
  UpdateArenaWind
  cNoJoin.Value = 1 - Config.NoJoin
  cSameChar.Value = Config.SameChar
  cDefect.Value = Config.Defect
  sldTLimit.Value = TLimit
  sldTLimit_Change
  sldGetRate.Value = GetRate
  sldGetRate_Change
  'tReason.Text = Config.Reason
  'tHTMLValue.Text = Config.FontColor
  'Cmd.Color = Val("&H" + ToHTML(Config.FontColor))
  'Cmd.FontName = Config.FontName
  'For X% = 1 To Screen.FontCount
  '  cboFontName.AddItem Screen.Fonts(X)
  'Next X%
  'cboFontName.Text = Config.FontName
  'cboFontName_Change
  sldFlagLimit.Value = Config.FlaCon
  sldFlagLimit_Change
  sldMaxHP.Value = MaxHP
  sldMaxHP_Change
  sldMaxSP.Value = MaxSP
  sldMaxSP_Change
  cStatusE = Config.StatusE
  cCTF = Config.Flag
  cRespawn = Config.Respawn
  cExtendedSupers = Config.Fours
  sldFragLimit.Value = FragLimit
  sldFragLimit_Change
  sldDmgMult.Value = DMult
  sldDmgMult_Change
  cMultiTarget.Value = Config.Multi
  'cOldSchool = Config.OldSchool
  cRunes.Value = Config.RuneEnable
  cWeapons.Value = Config.WeaponEnable
  sldOutScroll.Value = fChUBMain.tiOutput.interval
  sldOutScroll_Change
  sldScRa.Value = ScRa
  sldScRa_Change
  cMDITop.Value = Config.MDIOnTop
  cStatus.Value = Config.Status
  cMoves.Value = Config.Moves
  cCPU.Value = Config.CPU
  cAttacking.Value = Config.Attacking
  cLearnedMove.Value = Config.LearnedMove
  cVersion.Value = Config.Version
  cHelp.Value = Config.Help
  cType.Value = Config.TypeCommand
  cFragCount.Value = Config.FragCount
  cRuneCmd.Value = Config.RuneCmd
  cGetRune.Value = Config.GetRune
  cWeapon.Value = Config.Weapon
  cWpnList.Value = Config.WpnList
  sldLag.Value = Config.Lag
  sldLag_Change
  sldMaxPlayers.Value = MaxPlayers
  sldMaxPlayers_Change
  tmIRCChannel.Text = mIRCChannel$
  tmIRC.Text = mIRCWindow$
End Sub

Private Sub SaveChanges()
  If NumSenshi > 0 Then CurArena = cArena
  Config.NoJoin = 1 - cNoJoin.Value
  Config.SameChar = cSameChar.Value
  Config.Defect = cDefect.Value
  If (TLimit <> sldTLimit.Value) And Battle Then
    kDlgBox "Time limit cannot be changed during battle through the options menu.", 16, "Note"
    sldTLimit.Value = TLimit
  Else
    TLimit = sldTLimit.Value
  End If
  GetRate = sldGetRate.Value
  'Config.FontColor = tHTMLValue.Text
  'Config.FontName = cboFontName.Text
  Config.FlaCon = sldFlagLimit.Value
  If ((MaxHP <> sldMaxHP.Value) Or (MaxSP <> sldMaxSP.Value)) And Battle Then
    kDlgBox "Maximum HP and Maximum SP cannot be changed during battle. The old values will be retained.", 16, "Note"
    sldMaxHP.Value = MaxHP
    sldMaxSP.Value = MaxSP
  Else
    MaxHP = sldMaxHP.Value
    MaxSP = sldMaxSP.Value
  End If
  If (cCTF.Value <> Config.Flag) And Battle Then
    kDlgBox "Capture the Flag cannot be enabled or disabled during battle. The old setting will be retained.", 16, "Note"
    cCTF.Value = Config.Flag
  Else
    Config.Flag = cCTF.Value
  End If
  If (Config.Respawn <> cRespawn) And Battle Then
    kDlgBox "Respawn cannot be enabled or disabled during battle. The old setting will be retained.", 16, "Note"
    cRespawn.Value = Config.Respawn
  Else
    Config.Respawn = cRespawn.Value
  End If
  If (Config.Fours <> cExtendedSupers) And Battle Then
    kDlgBox "Extended Super settings cannot be changed during battle. The old setting will be retained.", 16, "Note"
    cExtendedSupers.Value = Config.Fours
  Else
    Config.Fours = cExtendedSupers.Value
  End If
  If (sldFragLimit.Value <> FragLimit) And Battle Then
    If sldFragLimit.Value < 0 Then
      Send ("The frag limit has been changed: " + TrimStr(Abs(sldFragLimit.Value)) + " Fatalities are now required to win.")
    ElseIf sldFragLimit.Value > 0 Then
      Send ("The frag limit has been changed to " + TrimStr(sldFragLimit.Value) + " frags.")
    ElseIf sldFragLimit.Value = 0 Then
      Send ("The frag limit has been removed.")
    Else
      Send ("The frag limit has been changed to " + TrimStr(sldFragLimit.Value))
    End If
  End If
  FragLimit = sldFragLimit.Value
  If (DMult <> sldDmgMult.Value) And Battle Then
    kDlgBox "The Damage Multiplier cannot be changed during battle. The old value will be retained.", 16, "Note"
    sldDmgMult.Value = DMult
  Else
    DMult = sldDmgMult.Value
  End If
  If (Config.Multi <> cMultiTarget.Value) And Battle Then
    kDlgBox "Multi-target moves cannot be enabled or disabled during battle. The old setting will be retained.", 16, "Note"
    cMultiTarget.Value = Config.Multi
  Else
    Config.Multi = cMultiTarget.Value
  End If
  'cOldSchool = Config.OldSchool
  If (Config.RuneEnable <> cRunes.Value) And Battle Then
    kDlgBox "Runes cannot be enabled or disabled during battle. The old setting will be retained.", 16, "Note"
    cRunes.Value = Config.RuneEnable
  Else
    Config.RuneEnable = cRunes.Value
  End If
  If (Config.WeaponEnable <> cWeapons.Value) And Battle Then
    kDlgBox "Weapons cannot be enabled or disabled during battle. The old setting will be retained.", 16, "Note"
    cWeapons.Value = Config.WeaponEnable
  Else
    Config.WeaponEnable = cWeapons.Value
  End If
  fChUBMain.tiOutput.interval = sldOutScroll.Value
  ScRa = sldScRa.Value
  Config.StatusE = cStatusE.Value
  Config.MDIOnTop = cMDITop.Value
  If Config.MDIOnTop Then
    SetWindowPos fChUBMain.hwnd, -1, 0, 0, 0, 0, 3
  Else
    SetWindowPos fChUBMain.hwnd, -2, 0, 0, 0, 0, 3
  End If
  Config.Status = cStatus.Value
  Config.Moves = cMoves.Value
  Config.CPU = cCPU.Value
  Config.Attacking = cAttacking.Value
  Config.Version = cVersion.Value
  Config.Help = cHelp.Value
  Config.TypeCommand = cType.Value
  Config.FragCount = cFragCount.Value
  Config.RuneCmd = cRuneCmd.Value
  Config.GetRune = cGetRune.Value
  Config.Weapon = cWeapon.Value
  Config.WpnList = cWpnList.Value
  Config.Lag = sldLag.Value
  If (MaxPlayers > sldMaxPlayers.Value) And Battle Then
    kDlgBox "You may not decrease the maximum allowable players during a battle. Old setting was restored.", 16, "Note"
    sldMaxPlayers.Value = MaxPlayers
  Else
    ReDim Preserve P(sldMaxPlayers.Value)
    ReDim Vote(sldMaxPlayers.Value)
    MaxPlayers = sldMaxPlayers.Value
  End If
  SaveSetting "ChUB Resurrection", "Settings", "Channel", tmIRCChannel.Text
  mIRCChannel = tmIRCChannel.Text
  mIRCStatusSend "/set %ChUBChan " + mIRCChannel
  SaveSetting "ChUB Resurrection", "Settings", "Window", tmIRC.Text
  mIRCWindow = tmIRC.Text
  SaveSetting "ChUB Resurrection", "Settings", "Join", Str$(Config.NoJoin)
  SaveSetting "ChUB Resurrection", "Settings", "SameChar", Str$(Config.SameChar)
  SaveSetting "ChUB Resurrection", "Settings", "Flag", Str$(Config.Flag)
  SaveSetting "ChUB Resurrection", "Settings", "Fours", Str$(Config.Fours)
  SaveSetting "ChUB Resurrection", "Settings", "Multitarget", Str$(Config.Multi)
  SaveSetting "ChUB Resurrection", "Settings", "Respawn", Str$(Config.Respawn)
  SaveSetting "ChUB Resurrection", "Settings", "Defect", Str$(Config.Defect)
  SaveSetting "ChUB Resurrection", "Settings", "RuneEnable", Str$(Config.RuneEnable)
  SaveSetting "ChUB Resurrection", "Settings", "WeaponEnable", Str$(Config.WeaponEnable)
  SaveSetting "ChUB Resurrection", "Settings", "FontName", Config.FontName
  SaveSetting "ChUB Resurrection", "Settings", "FontColor", Config.FontColor
  SaveSetting "ChUB Resurrection", "Settings", "FlaCon", Str$(Config.FlaCon)
  SaveSetting "ChUB Resurrection", "Settings", "DMult", Str$(DMult)
  SaveSetting "ChUB Resurrection", "Settings", "MaxHP", Str$(MaxHP)
  SaveSetting "ChUB Resurrection", "Settings", "MaxSP", Str$(MaxSP)
  SaveSetting "ChUB Resurrection", "Settings", "GetRate", Str$(GetRate)
  SaveSetting "ChUB Resurrection", "Settings", "FragLimit", Str$(FragLimit)
  SaveSetting "ChUB Resurrection", "Settings", "MDIOnTop", Str$(Config.MDIOnTop)
  SaveSetting "ChUB Resurrection", "Settings", "Status", Str$(Config.Status)
  SaveSetting "ChUB Resurrection", "Settings", "Moves", Str$(Config.Moves)
  SaveSetting "ChUB Resurrection", "Settings", "CPU", Str$(Config.CPU)
  SaveSetting "ChUB Resurrection", "Settings", "Version", Str$(Config.Version)
  SaveSetting "ChUB Resurrection", "Settings", "Help", Str$(Config.Help)
  SaveSetting "ChUB Resurrection", "Settings", "TypeCommand", Str$(Config.TypeCommand)
  SaveSetting "ChUB Resurrection", "Settings", "FragCount", Str$(Config.FragCount)
  SaveSetting "ChUB Resurrection", "Settings", "RuneCmd", Str$(Config.RuneCmd)
  SaveSetting "ChUB Resurrection", "Settings", "GetRune", Str$(Config.GetRune)
  SaveSetting "ChUB Resurrection", "Settings", "Weapon", Str$(Config.Weapon)
  SaveSetting "ChUB Resurrection", "Settings", "WpnList", Str$(Config.WpnList)
  SaveSetting "ChUB Resurrection", "Settings", "Lag-O-Meter", Str$(Config.Lag)
  SaveSetting "ChUB Resurrection", "Settings", "MaxPlayers", Str$(MaxPlayers)
  SaveSetting "ChUB Resurrection", "Settings", "Attacking", Str$(Config.Attacking)
  SaveSetting "ChUB Resurrection", "Settings", "LearnedMove", Str$(Config.LearnedMove)
  SaveSetting "ChUB Resurrection", "Settings", "OutScroll", Str$(fChUBMain.tiOutput.interval)
  SaveSetting "ChUB Resurrection", "Settings", "ScRa", Str$(ScRa)
  SaveSetting "ChUB Resurrection", "Settings", "StatusE", Str$(Config.StatusE)
  'If (Not Battle) Then BalancingAct
End Sub

Private Sub tTLimit_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhOpt_TLimit)
End Sub
