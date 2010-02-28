VERSION 5.00
Begin VB.Form fMoveEdit 
   Appearance      =   0  'Flat
   BackColor       =   &H00004000&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Edit Move"
   ClientHeight    =   6375
   ClientLeft      =   375
   ClientTop       =   825
   ClientWidth     =   11085
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H80000008&
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6375
   ScaleWidth      =   11085
   StartUpPosition =   2  'CenterScreen
   Begin VB.FileListBox File1 
      Height          =   3210
      Left            =   9000
      Pattern         =   "*.m2k"
      TabIndex        =   129
      Top             =   1800
      Width           =   1935
   End
   Begin VB.DirListBox Dir1 
      Height          =   1215
      Left            =   9000
      TabIndex        =   128
      Top             =   360
      Width           =   1935
   End
   Begin VB.TextBox tR1 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   124
      Top             =   5640
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.TextBox tR2 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   5280
      TabIndex        =   123
      Top             =   5640
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.TextBox tScarecrow 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   5280
      TabIndex        =   122
      Top             =   5400
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.TextBox tCharm 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   121
      Top             =   5400
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Frame frSuper 
      Appearance      =   0  'Flat
      BackColor       =   &H00004000&
      Caption         =   "Super?"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   1335
      Left            =   6240
      TabIndex        =   111
      Top             =   240
      Width           =   2655
      Begin VB.OptionButton oCanSuper 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Must Lv. >=4"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   5
         Left            =   1200
         TabIndex        =   118
         Top             =   960
         Width           =   1335
      End
      Begin VB.OptionButton oCanSuper 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Must Ctr."
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   6
         Left            =   120
         TabIndex        =   117
         Top             =   720
         Width           =   975
      End
      Begin VB.OptionButton oCanSuper 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Must Lv. >=3"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   4
         Left            =   1200
         TabIndex        =   116
         Top             =   720
         Width           =   1335
      End
      Begin VB.OptionButton oCanSuper 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Must Lv. >=2"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   3
         Left            =   1200
         TabIndex        =   115
         Top             =   480
         Width           =   1335
      End
      Begin VB.OptionButton oCanSuper 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Must Super"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   2
         Left            =   1200
         TabIndex        =   114
         Top             =   240
         Width           =   1215
      End
      Begin VB.OptionButton oCanSuper 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Can Super"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   113
         Top             =   480
         Width           =   1095
      End
      Begin VB.OptionButton oCanSuper 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "No Supers"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   112
         Top             =   240
         Width           =   1095
      End
      Begin VB.OptionButton oCanSuper 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Must ChiCtr"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   7
         Left            =   120
         TabIndex        =   119
         Top             =   960
         Width           =   1215
      End
   End
   Begin VB.CommandButton cSave 
      Appearance      =   0  'Flat
      Caption         =   "Save"
      Height          =   255
      Left            =   7560
      TabIndex        =   68
      Top             =   5280
      Width           =   975
   End
   Begin VB.CommandButton cLoad 
      Appearance      =   0  'Flat
      Caption         =   "Load"
      Height          =   255
      Left            =   6360
      TabIndex        =   67
      Top             =   5280
      Width           =   975
   End
   Begin VB.TextBox tElementStr 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   8160
      TabIndex        =   70
      Top             =   5760
      Width           =   495
   End
   Begin VB.TextBox tQuick 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   29
      Top             =   5160
      Width           =   495
   End
   Begin VB.TextBox tBerserk 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   5280
      TabIndex        =   39
      Top             =   5160
      Width           =   495
   End
   Begin VB.TextBox tMushroom 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   28
      Top             =   4920
      Width           =   495
   End
   Begin VB.TextBox tMIA 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   5280
      TabIndex        =   38
      Top             =   4920
      Width           =   495
   End
   Begin VB.TextBox tLife3 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   5280
      TabIndex        =   37
      Top             =   4680
      Width           =   495
   End
   Begin VB.TextBox tSlow 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   5280
      TabIndex        =   36
      Top             =   4440
      Width           =   495
   End
   Begin VB.TextBox tAttDn 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   5280
      TabIndex        =   35
      Top             =   4200
      Width           =   495
   End
   Begin VB.TextBox tWeak 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   5280
      TabIndex        =   34
      Top             =   3960
      Width           =   495
   End
   Begin VB.TextBox tStop 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   27
      Top             =   4680
      Width           =   495
   End
   Begin VB.TextBox tRegen 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   26
      Top             =   4440
      Width           =   495
   End
   Begin VB.TextBox tEleNum 
      Appearance      =   0  'Flat
      Enabled         =   0   'False
      Height          =   285
      Left            =   8160
      TabIndex        =   69
      Top             =   5520
      Width           =   495
   End
   Begin VB.HScrollBar hsMove 
      Height          =   255
      Left            =   120
      Max             =   12
      Min             =   1
      TabIndex        =   98
      Top             =   5160
      Value           =   1
      Width           =   2895
   End
   Begin VB.TextBox tHaste 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   5280
      TabIndex        =   33
      Top             =   3720
      Width           =   495
   End
   Begin VB.TextBox tAttUp 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   5280
      TabIndex        =   32
      Top             =   3480
      Width           =   495
   End
   Begin VB.TextBox tDefUp 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   5280
      TabIndex        =   31
      Top             =   3240
      Width           =   495
   End
   Begin VB.TextBox tStun 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   5280
      TabIndex        =   30
      Top             =   3000
      Width           =   495
   End
   Begin VB.TextBox tBlind 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   25
      Top             =   4200
      Width           =   495
   End
   Begin VB.TextBox tPoison 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   24
      Top             =   3960
      Width           =   495
   End
   Begin VB.TextBox tSleep 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   23
      Top             =   3720
      Width           =   495
   End
   Begin VB.TextBox tFreeze 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   22
      Top             =   3480
      Width           =   495
   End
   Begin VB.TextBox tChaos 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   21
      Top             =   3240
      Width           =   495
   End
   Begin VB.TextBox tMute 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   20
      Top             =   3000
      Width           =   495
   End
   Begin VB.Frame fElement 
      Appearance      =   0  'Flat
      BackColor       =   &H00004000&
      Caption         =   "Element"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   3495
      Left            =   6240
      TabIndex        =   83
      Top             =   1680
      Width           =   2415
      Begin VB.OptionButton oElement 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "66"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   66
         Left            =   600
         TabIndex        =   51
         Top             =   2640
         Visible         =   0   'False
         Width           =   495
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Ghost"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   75
         Left            =   1200
         TabIndex        =   66
         Top             =   3120
         Visible         =   0   'False
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Dirt"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   73
         Left            =   120
         TabIndex        =   53
         Top             =   3120
         Visible         =   0   'False
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Rock"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   72
         Left            =   1200
         TabIndex        =   65
         Top             =   2880
         Visible         =   0   'False
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "92"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   92
         Left            =   120
         TabIndex        =   50
         Top             =   2640
         Visible         =   0   'False
         Width           =   615
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Grass"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   71
         Left            =   120
         TabIndex        =   52
         Top             =   2880
         Visible         =   0   'False
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Wind"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   28
         Left            =   1200
         TabIndex        =   61
         Top             =   1920
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "HP Drain"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   11
         Left            =   120
         TabIndex        =   43
         Top             =   960
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Reveal"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   23
         Left            =   120
         TabIndex        =   45
         Top             =   1440
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "NoDmg"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   46
         Top             =   1680
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Ki"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   29
         Left            =   1200
         TabIndex        =   63
         Top             =   2400
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Light"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   30
         Left            =   1200
         TabIndex        =   62
         Top             =   2160
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Earth"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   27
         Left            =   1200
         TabIndex        =   60
         Top             =   1680
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Heart"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   26
         Left            =   1200
         TabIndex        =   59
         Top             =   1440
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Lightning"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   25
         Left            =   1200
         TabIndex        =   58
         Top             =   1200
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Fire"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   24
         Left            =   1200
         TabIndex        =   57
         Top             =   960
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Water"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   22
         Left            =   1200
         TabIndex        =   56
         Top             =   720
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Shadow"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   21
         Left            =   1200
         TabIndex        =   55
         Top             =   480
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Moon"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   20
         Left            =   1200
         TabIndex        =   54
         Top             =   240
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Demi"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   19
         Left            =   120
         TabIndex        =   47
         Top             =   1920
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Life"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   17
         Left            =   120
         TabIndex        =   44
         Top             =   1200
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Psychic"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   74
         Left            =   1200
         TabIndex        =   64
         Top             =   2640
         Visible         =   0   'False
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Poison"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   70
         Left            =   120
         TabIndex        =   49
         Top             =   2400
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Morph"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   42
         Top             =   720
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Heal"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   41
         Top             =   480
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Physical"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   40
         Top             =   240
         Width           =   1095
      End
      Begin VB.OptionButton oElement 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "LearnMove"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   42
         Left            =   120
         TabIndex        =   48
         Top             =   2160
         Width           =   1215
      End
   End
   Begin VB.Frame fTarget 
      Appearance      =   0  'Flat
      BackColor       =   &H00004000&
      Caption         =   "Target"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   2295
      Left            =   120
      TabIndex        =   82
      Top             =   2760
      Width           =   2415
      Begin VB.OptionButton oOnlySelf 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "Self Only"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   109
         Top             =   240
         Width           =   2175
      End
      Begin VB.OptionButton oEverybody 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "EVERYBODY (MP x3)"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   19
         Top             =   1920
         Width           =   2175
      End
      Begin VB.OptionButton oAllButSelf 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "All But Attacker (MP x3)"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   18
         Top             =   1680
         Width           =   2055
      End
      Begin VB.OptionButton oAllFoe 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "All Enemies (MP x2.5)"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   17
         Top             =   1440
         Width           =   2175
      End
      Begin VB.OptionButton oAllTeam 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "One Enemy Team (MP x2)"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   16
         Top             =   1200
         Width           =   2175
      End
      Begin VB.OptionButton oAllFriend 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "All Allies (MP x2)"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   15
         Top             =   960
         Width           =   2175
      End
      Begin VB.OptionButton oFriend 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "One Ally"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   14
         Top             =   720
         Width           =   2175
      End
      Begin VB.OptionButton oEnemy 
         Appearance      =   0  'Flat
         BackColor       =   &H00004000&
         Caption         =   "One Enemy"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   13
         Top             =   480
         Width           =   2175
      End
   End
   Begin VB.TextBox tSuperMiss 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   2520
      TabIndex        =   12
      Top             =   2400
      Width           =   3615
   End
   Begin VB.TextBox tMiss 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   2520
      TabIndex        =   11
      Top             =   2160
      Width           =   3615
   End
   Begin VB.TextBox tHealMeld 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   2520
      TabIndex        =   10
      Top             =   1920
      Visible         =   0   'False
      Width           =   3615
   End
   Begin VB.TextBox tHealSelf 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   2520
      TabIndex        =   9
      Top             =   1680
      Width           =   3615
   End
   Begin VB.TextBox tSuperHit 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   2520
      TabIndex        =   8
      Top             =   1440
      Width           =   3615
   End
   Begin VB.TextBox tCritHit 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   2520
      TabIndex        =   7
      Top             =   1200
      Width           =   3615
   End
   Begin VB.TextBox tHit 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   2520
      TabIndex        =   6
      Top             =   960
      Width           =   3615
   End
   Begin VB.TextBox tBegin2HealSelf 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   2520
      TabIndex        =   5
      Top             =   720
      Width           =   3615
   End
   Begin VB.TextBox tBegin2SuperAttack 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   2520
      TabIndex        =   4
      Top             =   480
      Width           =   3615
   End
   Begin VB.TextBox tBegin2Attack 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   2520
      TabIndex        =   3
      Top             =   240
      Width           =   3615
   End
   Begin VB.TextBox tCmdKey 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   6480
      TabIndex        =   2
      Top             =   0
      Width           =   1455
   End
   Begin VB.TextBox tName 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   2520
      TabIndex        =   1
      Top             =   0
      Width           =   3615
   End
   Begin VB.Label dj 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Kamek wanted to use this space to apologize for the sloppy way of loading/saving stuff."
      ForeColor       =   &H00FFFFFF&
      Height          =   855
      Left            =   8880
      TabIndex        =   130
      Top             =   5160
      Width           =   2055
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "R1"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   38
      Left            =   2640
      TabIndex        =   127
      Top             =   5640
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "R2"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   37
      Left            =   4200
      TabIndex        =   126
      Top             =   5640
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Scarecrow"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   36
      Left            =   4200
      TabIndex        =   125
      Top             =   5400
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Charm"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   35
      Left            =   2640
      TabIndex        =   120
      Top             =   5400
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.Label StatLine 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Move Editor"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   -120
      TabIndex        =   110
      Top             =   6120
      Width           =   11175
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0FF&
      Caption         =   "REMEMBER NOT TO USE DOUBLE QUOTES ("")"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   495
      Left            =   600
      TabIndex        =   108
      Top             =   5520
      Width           =   1935
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Quick"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   34
      Left            =   2640
      TabIndex        =   107
      Top             =   5160
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Berserk"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   33
      Left            =   4200
      TabIndex        =   106
      Top             =   5160
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Mushroom"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   32
      Left            =   2640
      TabIndex        =   105
      Top             =   4920
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "M.I.A."
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   31
      Left            =   4200
      TabIndex        =   104
      Top             =   4920
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Stop"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   30
      Left            =   2640
      TabIndex        =   103
      Top             =   4680
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Regen"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   29
      Left            =   2640
      TabIndex        =   102
      Top             =   4440
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Life3"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   28
      Left            =   4200
      TabIndex        =   101
      Top             =   4680
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "MBarrier"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   27
      Left            =   4200
      TabIndex        =   100
      Top             =   4200
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Raw Element #"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   26
      Left            =   6720
      TabIndex        =   99
      Top             =   5520
      Width           =   1335
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Move Strength"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   25
      Left            =   6720
      TabIndex        =   97
      Top             =   5760
      Width           =   1335
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Haste"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   24
      Left            =   4200
      TabIndex        =   96
      Top             =   3720
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Curse"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   23
      Left            =   4200
      TabIndex        =   95
      Top             =   3480
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Bless"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   22
      Left            =   4200
      TabIndex        =   94
      Top             =   3240
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Stun"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   21
      Left            =   4200
      TabIndex        =   93
      Top             =   3000
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Slow"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   20
      Left            =   4200
      TabIndex        =   92
      Top             =   4440
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Barrier"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   19
      Left            =   4200
      TabIndex        =   91
      Top             =   3960
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Blindness"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   18
      Left            =   2640
      TabIndex        =   90
      Top             =   4200
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Poison"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   17
      Left            =   2880
      TabIndex        =   89
      Top             =   3960
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Sleep"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   16
      Left            =   2880
      TabIndex        =   88
      Top             =   3720
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Freeze"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   15
      Left            =   2880
      TabIndex        =   87
      Top             =   3480
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Chaos"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   14
      Left            =   2880
      TabIndex        =   86
      Top             =   3240
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "% Chance of causing Status (-1 to cancel status)"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   13
      Left            =   2520
      TabIndex        =   85
      Top             =   2760
      Width           =   3615
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Mute"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   12
      Left            =   2880
      TabIndex        =   84
      Top             =   3000
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Move Missed (Super)"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   11
      Left            =   0
      TabIndex        =   81
      Top             =   2400
      Width           =   2415
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Move Missed (Normal)"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   10
      Left            =   0
      TabIndex        =   80
      Top             =   2160
      Width           =   2415
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Move Hit (MindMeld Heal)"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   9
      Left            =   0
      TabIndex        =   79
      Top             =   1920
      Visible         =   0   'False
      Width           =   2415
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Move Hit (heal self)"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   8
      Left            =   480
      TabIndex        =   78
      Top             =   1680
      Width           =   1935
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Move Hit (Super)"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   7
      Left            =   720
      TabIndex        =   77
      Top             =   1440
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Move Hit (critical)"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   6
      Left            =   720
      TabIndex        =   76
      Top             =   1200
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Move Hit (normal)"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   5
      Left            =   720
      TabIndex        =   75
      Top             =   960
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Pre-Heal Self"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   4
      Left            =   480
      TabIndex        =   74
      Top             =   720
      Width           =   1935
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Pre-Super Attack"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   3
      Left            =   720
      TabIndex        =   73
      Top             =   480
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Pre-Attack String"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   2
      Left            =   720
      TabIndex        =   72
      Top             =   240
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "/"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   1
      Left            =   6360
      TabIndex        =   71
      Top             =   0
      Width           =   135
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Move Name"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   0
      Left            =   720
      TabIndex        =   0
      Top             =   0
      Width           =   1695
   End
End
Attribute VB_Name = "fMoveEdit"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cLoad_Click()
Dim S As String
On Error GoTo Cancel9
  'CmdChr.FileName = "*.m2k"
  'CmdChr.DialogTitle = "Load ChUB 2000 Move"
  'CmdChr.Action = 1
  S = File1.Path + "\" + File1.FileName
  LoadMove S, Num, M
  UpdateWin
  Changed = True
Cancel9:
  On Error GoTo 0
  Exit Sub
End Sub

Private Sub cSave_Click()
Dim S As String
  On Error GoTo DontSave3
  'CmdChr.FileName = First8(Senshi(Num).Moves(M).CmdKey) + ".m2k"
  'CmdChr.DialogTitle = "Save Move"
  'CmdChr.FilterIndex = 2
  'CmdChr.Action = 2
  S = First8(Senshi(Num).Moves(M).CmdKey) + ".m2k"
  SaveMove S, Num, M
  File1.Refresh
DontSave3:
  On Error GoTo 0
  Exit Sub
End Sub

Private Sub DoMP()
  
End Sub

Private Sub Dir1_Change()
  File1.Path = Dir1.Path
End Sub

Private Sub fElement_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Hmm...."
End Sub

Private Sub fElement_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If (Button = 2 And Shift And X > 1000) Then
    If Rot13(UCase(InputBox("What's the password?"))) = "98M`9aNW):" Then
      oElement(66).Visible = True
      oElement(71).Visible = True
      oElement(72).Visible = True
      oElement(73).Visible = True
      oElement(74).Visible = True
      oElement(75).Visible = True
      oElement(92).Visible = True
      tEleNum.Enabled = True
      tEleNum.Visible = True
      Label1(26).Visible = True
      fDSEdit!oWeakness(70).Visible = True
      fDSEdit!oWeakness(71).Visible = True
      fDSEdit!oWeakness(72).Visible = True
      fDSEdit!oWeakness(73).Visible = True
      fDSEdit!oWeakness(74).Visible = True
      fDSEdit!oWeakness(75).Visible = True
      fDSEdit!oResist(70).Visible = True
      fDSEdit!oResist(71).Visible = True
      fDSEdit!oResist(72).Visible = True
      fDSEdit!oResist(73).Visible = True
      fDSEdit!oResist(74).Visible = True
      fDSEdit!oResist(75).Visible = True
      fDSEdit!fWeakTo.Height = 4215
      fDSEdit!fResist.Height = 4215
    Else
      MsgBox "Wrong!"
    End If
  End If
End Sub

Private Sub Form_Load()
  UpdateWin
  hsMove.Value = M
  hsMove.Max = MaxMoves
  Me.Show
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move Editor"
End Sub

Private Sub Form_Unload(Cancel As Integer)
Dim X As Integer
  fDSEdit!cbMoves.Clear
  For X = 1 To MaxMoves
    fDSEdit!cbMoves.AddItem Senshi(Num).Moves(X).Name
  Next X
  fDSEdit!cbMoves.ListIndex = M - 1
End Sub

Private Sub hsMove_Change()
  M = hsMove.Value
  UpdateWin
End Sub

Private Sub Label1_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
Dim Msg$
  If Button = 2 Then
    Label1(26).Visible = True
    tEleNum.Visible = True
    Label1(37).Visible = True
    Label1(38).Visible = True
    tR1.Visible = True
    tR2.Visible = True
  End If
  Select Case Index
    Case 0: Msg = "Name of the attack."
    Case 1: Msg = "Command to execute the move."
    Case 2: Msg = "String shown when the move begins."
    Case 3: Msg = "Shown when a Super attack based on this move begins."
    Case 4: Msg = "For heals, the string shown when the player begins to heal himself. For multiple-target moves, the string shown when the move is completed."
    Case 5: Msg = "String shown when the attack hits the target."
    Case 6: Msg = "String shown for a critical hit."
  End Select
  'MsgBox 64, Msg, "Explanation"
End Sub

Private Sub Label3_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "I'm tellin' ya, you'll screw up your character!"
End Sub

Private Sub Label4_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 Then tWeak.Enabled = True
  If Button = 2 Then tAttDn.Enabled = True
End Sub

Private Sub oAllButSelf_Click()
  Changed = True
  Senshi(Num).Moves(M).Target = AllButSelf
  Senshi(Num).Moves(M).CanSuper = False
  'cCanSuper.Value = 0
  'cCanSuper.Enabled = False
  Label1(4).Caption = "Post-Attack String A"
  Label1(8).Caption = "Post-Attack String B"
  DoMP
End Sub

Private Sub oAllButSelf_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects everyone BUT the player using it."
End Sub

Private Sub oAllFoe_Click()
  Changed = True
  Senshi(Num).Moves(M).Target = AllFoe
  Senshi(Num).Moves(M).CanSuper = False
  'cCanSuper.Value = 0
  'cCanSuper.Enabled = False
  Label1(4).Caption = "Post-Attack String A"
  Label1(8).Caption = "Post-Attack String B"
  DoMP
End Sub

Private Sub oAllFoe_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects all enemies in the battle."
End Sub

Private Sub oAllFriend_Click()
  Changed = True
  Senshi(Num).Moves(M).Target = AllAlly
  Senshi(Num).Moves(M).CanSuper = False
  'cCanSuper.Value = 0
  'cCanSuper.Enabled = False
  Label1(4).Caption = "Pre-Heal Self/Team"
  Label1(8).Caption = "Move Hit (heal self)"
  DoMP
End Sub

Private Sub oAllFriend_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects player's entire team."
End Sub

Private Sub oAllTeam_Click()
  Changed = True
  Senshi(Num).Moves(M).Target = AllTeam
  Senshi(Num).Moves(M).CanSuper = False
  'cCanSuper.Value = 0
  'cCanSuper.Enabled = False
  Label1(4).Caption = "Post-Attack String A"
  Label1(8).Caption = "Post-Attack String B"
  DoMP
End Sub

Private Sub oAllTeam_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects one enemy team."
End Sub

Private Sub oCanSuper_Click(Index As Integer)
  If Index = Senshi(Num).Moves(M).CanSuper Then Exit Sub
  Changed = True
  Select Case Senshi(Num).Moves(M).Element
    Case Phys, Poison, Grass, Rock, Dirt, Psychic, Ghost, MoonE, Shadow, Water, Fire, Lit, Heart, Earth, Wind, Ki, Lum:
      Senshi(Num).Moves(M).CanSuper = Index
    Case Else:
      If (Index >= 1 And Index <= 5) Then
        oCanSuper(Senshi(Num).Moves(M).CanSuper).Value = True
        Exit Sub
      Else
        Senshi(Num).Moves(M).CanSuper = Index
      End If
  End Select
  Select Case Senshi(Num).Moves(M).Target
    Case Enemy:
      Senshi(Num).Moves(M).CanSuper = Index
    Case Else:
      If (Index >= 1 And Index <= 5) Then
        oCanSuper(Senshi(Num).Moves(M).CanSuper).Value = True
        Exit Sub
      Else
        Senshi(Num).Moves(M).CanSuper = Index
      End If
  End Select
End Sub

Private Sub oElement_Click(Index As Integer)
  Changed = True
  Senshi(Num).Moves(M).Element = Index
  tElementStr.Enabled = True
  Select Case Index
    Case NoDmg, Morph, Invin, Life, Demi, Reveal: tElementStr.Enabled = False
  End Select
  'If oEnemy.Value = True Then cCanSuper.Enabled = True
  Select Case Index
    Case NoDmg, Phys, HPTheft, MPTheft, Demi, MoonE, Shadow, Water, Fire, Lit, Heart, Earth, Lum, Reveal, Poison:
      'oEnemy.Value = True
      'oEnemy_Click
      'cCanSuper.Enabled = True
    Case Heal, Life:
      oFriend.Value = True
      oFriend_Click
  End Select
  'Select Case Index
  '  Case NoDmg, Heal, Morph, Invin, Life, Demi, Reveal, HPtheft: cCanSuper.Enabled = False
  'End Select
  tEleNum.Text = TrimStr(Senshi(Num).Moves(M).Element)
  DoMP
  oElement(Index).Value = True
End Sub

Private Sub oElement_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
  'If Button = 2 Then oElement(10).Visible = True
End Sub

Private Sub oElement_MouseMove(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
  Select Case Index
    Case 0: StatLine = "No effect other than what is in the status boxes."
    Case 1: StatLine = "Physical Attack"
    Case 2: StatLine = "Restore HP"
    Case 3: StatLine = "Allows player to change into another player in the current dataset."
    Case 4 To 10, 12, 13 To 16, 18: StatLine = "Obsolete element, has no effect"
    Case 11: StatLine = "Steals HP from target"
    Case 17: StatLine = "Restore life to dead people"
    Case 19: StatLine = "Halves target's HP"
    Case 20: StatLine = "Moon Energy"
    Case 21: StatLine = "Dark Energy"
    Case 22: StatLine = "Water Magic"
    Case 23: StatLine = "Shows cool stuff about target"
    Case 24: StatLine = "Fire Magic"
    Case 25: StatLine = "Lightning Magic"
    Case 26: StatLine = "Heart Power"
    Case 27: StatLine = "Earth Power"
    Case 28: StatLine = "Wind Magic"
    Case 29: StatLine = "Ki Power"
    Case 30: StatLine = "Solar Energy"
    Case 31 To 41, 43 To 69, 76 To 91: StatLine = "No element... yet?"
    Case 70: StatLine = "Poison Magic. Automatically poisons"
    Case 71: StatLine = "Grass-type attack (for Pokémon)"
    Case 72: StatLine = "Rock-type attack (for Pokémon)"
    Case 73: StatLine = "Dirt-type attack (for Pokémon)"
    Case 74: StatLine = "Psychic-type attack (for Pokémon)"
    Case 75: StatLine = "Ghost-type attack (for Pokémon)"
    Case 42: StatLine = "Learn opponent's moves"
    Case 66: StatLine = "Shield!"
    Case 92: StatLine = "TOP SECRET!"
    Case Else: StatLine = "No element"
  End Select
End Sub

Private Sub oEnemy_Click()
  Changed = True
  Senshi(Num).Moves(M).Target = Enemy
  'Select Case Senshi(Num).Moves(M).Element
  '  Case NoDmg, Phys, HPtheft, MPTheft, Demi, MoonE, Shadow, Water, Fire, Lit, Heart, Earth, Lum, Reveal:  cCanSuper.Enabled = True
  'End Select
  Label1(4).Caption = "Pre-Heal Self"
  Label1(8).Caption = "Move Hit (heal self)"
  DoMP
End Sub

Private Sub oEnemy_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move is by default carried out on an enemy."
End Sub

Private Sub oEverybody_Click()
  Changed = True
  Senshi(Num).Moves(M).Target = Everybody
  Senshi(Num).Moves(M).CanSuper = False
  'cCanSuper.Value = 0
  'cCanSuper.Enabled = False
  Label1(4).Caption = "Post-Attack String A"
  Label1(8).Caption = "Post-Attack String B"
  DoMP
End Sub

Private Sub oEverybody_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects everyone in the battle."
End Sub

Private Sub oFriend_Click()
  Changed = True
  Senshi(Num).Moves(M).Target = Ally
  Senshi(Num).Moves(M).CanSuper = False
  'cCanSuper.Value = 0
  'cCanSuper.Enabled = False
  Label1(4).Caption = "Pre-Heal Self"
  Label1(8).Caption = "Move Hit (heal self)"
  DoMP
End Sub

Private Sub oFriend_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move is by default carried out on yourself or a Ally."
End Sub

Private Sub oOnlySelf_Click()
  Changed = True
  Senshi(Num).Moves(M).Target = 8
  Senshi(Num).Moves(M).CanSuper = False
  'cCanSuper.Value = 0
  'cCanSuper.Enabled = False
  Label1(4).Caption = "Pre-Heal Self"
  Label1(8).Caption = "Move Hit (heal self)"
  DoMP
End Sub

Private Sub oOnlySelf_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move will affect only the player using it regardless of who s/he targets."
End Sub

Private Sub StatLine_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Hi, I'm StatLine. I'll tell you what each thing does."
End Sub

Private Sub tAttDn_Change()
  Changed = True
  If (Val(tAttDn) >= -1) And (Val(tAttDn) <= 100) Then Senshi(Num).Moves(M).Status.MBarrier = Val(tAttDn)
  DoMP
End Sub

Private Sub tAttDn_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 Then tAttDn.Enabled = True
End Sub

Private Sub tAttDn_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "A barrier protects from magic attacks."
End Sub

Private Sub tAttUp_Change()
  Changed = True
  If (Val(tAttUp) >= -1) And (Val(tAttUp) <= 100) Then Senshi(Num).Moves(M).Status.Curse = Val(tAttUp)
  DoMP
End Sub

Private Sub tAttUp_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "The target is cursed to attack with less damage."
End Sub

Private Sub tBegin2Attack_Change()
  Changed = True
  Senshi(Num).Moves(M).Begin2Attack = tBegin2Attack
End Sub

Private Sub tBegin2Attack_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "String shown when player starts doing the attack."
End Sub

Private Sub tBegin2HealSelf_Change()
  Changed = True
  Senshi(Num).Moves(M).Begin2HealSelf = tBegin2HealSelf
End Sub

Private Sub tBegin2HealSelf_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If oOnlySelf Or oEnemy Or oFriend Or oAllFriend Then
    StatLine = "String shown when player beings to use a Heal move on themself."
  Else
    StatLine = "String shown when an attack that hits everyone is carried out."
  End If
End Sub

Private Sub tBegin2SuperAttack_Change()
  Changed = True
  Senshi(Num).Moves(M).Begin2SuperAttack = tBegin2SuperAttack
End Sub

Private Sub tBegin2SuperAttack_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "String shown when beginning a Super Move."
End Sub

Private Sub tBerserk_Change()
  Changed = True
  If (Val(tBerserk) >= -1) And (Val(tBerserk) <= 100) Then Senshi(Num).Moves(M).Status.Berserk = Val(tBerserk)
  DoMP
End Sub

Private Sub tBerserk_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target has increased attack power but uncontrolled attacks on random enemies."
End Sub

Private Sub tBlind_Change()
  Changed = True
  If (Val(tBlind) >= -1) And (Val(tBlind) <= 100) Then Senshi(Num).Moves(M).Status.blind = Val(tBlind)
  DoMP
End Sub

Private Sub tBlind_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target cannot see and moves do minimum damage."
End Sub

Private Sub tChaos_Change()
  Changed = True
  If (Val(tChaos) >= -1) And (Val(tChaos) <= 100) Then Senshi(Num).Moves(M).Status.chaos = Val(tChaos)
  DoMP
End Sub

Private Sub tChaos_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target's mind is placed in chaos. Random moves on random people."
End Sub

Private Sub tCharm_Change()
  Changed = True
  If (Val(tCharm) >= -1) And (Val(tCharm) <= 100) Then Senshi(Num).Moves(M).Status.Charm = Val(tCharm)
  DoMP
End Sub

Private Sub tCmdKey_Change()
  Changed = True
  Senshi(Num).Moves(M).CmdKey = tCmdKey.Text
End Sub

Private Sub tCmdKey_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move Command Key"
End Sub

Private Sub tCritHit_Change()
  Changed = True
  Senshi(Num).Moves(M).CritHit = tCritHit
End Sub

Private Sub tCritHit_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "String shown instead of Move Hit Normal if the attack is a Critical Hit"
End Sub

Private Sub tDefUp_Change()
  Changed = True
  If (Val(tDefUp) >= -1) And (Val(tDefUp) <= 100) Then Senshi(Num).Moves(M).Status.Bless = Val(tDefUp)
  DoMP
End Sub

Private Sub tDefUp_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "The target is blessed with power that increases their attack damage."
End Sub

Private Sub tElementStr_Change()
  Changed = True
  Senshi(Num).Moves(M).ElementStr = Val(tElementStr)
  DoMP
End Sub

Private Sub tElementStr_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "How much damage it would do if your Strength and their Defense were the same."
End Sub

Private Sub tEleNum_Change()
On Error GoTo NeverMind
  oElement(Senshi(Num).Moves(M).Element).Value = False
NeverMind: Resume Next
  oElement_Click (Val(tEleNum))
  On Error GoTo IgnoreIt
  oElement(Val(tEleNum)).Enabled = True
IgnoreIt: Exit Sub
End Sub

Private Sub tEleNum_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  oElement_MouseMove Val(tEleNum.Text), Button, Shift, X, Y
End Sub

Private Sub tFreeze_Change()
  Changed = True
  If (Val(tFreeze) >= -1) And (Val(tFreeze) <= 100) Then Senshi(Num).Moves(M).Status.freeze = Val(tFreeze)
  DoMP
End Sub

Private Sub tFreeze_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is frozen solid and cannot move."
End Sub

Private Sub tHaste_Change()
  Changed = True
  If (Val(tHaste) >= -1) And (Val(tHaste) <= 100) Then Senshi(Num).Moves(M).Status.haste = Val(tHaste)
  DoMP
End Sub

Private Sub tHaste_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target moves faster."
End Sub

Private Sub tHealMeld_Change()
  Changed = True
  Senshi(Num).Moves(M).HealMeld = tHealMeld
End Sub

Private Sub tHealMeld_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Huh?"
End Sub

Private Sub tHealSelf_Change()
  Changed = True
  Senshi(Num).Moves(M).HealSelf = tHealSelf
End Sub

Private Sub tHealSelf_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "String shown when a heal on self or team is carried out"
End Sub

Private Sub tHit_Change()
  Changed = True
  Senshi(Num).Moves(M).Hit = tHit
End Sub

Private Sub tHit_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If oOnlySelf Or oEnemy Or oFriend Or oAllFriend Then
    StatLine = "String shown when player's attack hits another player (%T)"
  Else
    StatLine = "String shown for each individual person attacked (%T)"
  End If
End Sub

Private Sub tLife3_Change()
  Changed = True
  If (Val(tLife3) >= -1) And (Val(tLife3) <= 100) Then Senshi(Num).Moves(M).Status.Life3 = Val(tLife3)
  DoMP
End Sub

Private Sub tLife3_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target will come back to life after being killed."
End Sub

Private Sub tMIA_Change()
  Changed = True
  If (Val(tMIA) >= -1) And (Val(tMIA) <= 100) Then Senshi(Num).Moves(M).Status.MIA = Val(tMIA)
  DoMP
End Sub

Private Sub tMIA_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is not present in battle."
End Sub

Private Sub tMiss_Change()
  Changed = True
  Senshi(Num).Moves(M).Miss = tMiss
End Sub

Private Sub tMiss_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If oOnlySelf Or oEnemy Or oFriend Or oAllFriend Then
    StatLine = "Oops! Your move missed or was blocked."
  Else
    StatLine = "Oops! Your move missed or was blocked by %T."
  End If
End Sub

Private Sub tmushroom_Change()
  Changed = True
  If (Val(tMushroom) >= -1) And (Val(tMushroom) <= 100) Then Senshi(Num).Moves(M).Status.Mushroom = Val(tMushroom)
  DoMP
End Sub

Private Sub tMushroom_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is turned into a fungus and restores HP at every turn interval."
End Sub

Private Sub tMute_Change()
  Changed = True
  If (Val(tMute) >= -1) And (Val(tMute) <= 100) Then Senshi(Num).Moves(M).Status.Mute = Val(tMute)
  DoMP
End Sub

Private Sub tMute_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is unable to use moves other than physical moves and Basic moves."
End Sub

Private Sub tName_Change()
  Changed = True
  Senshi(Num).Moves(M).Name = tName
End Sub

Private Sub tName_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Name of move. Used in status window and Super Attack messages."
End Sub

Private Sub tPoison_Change()
  Changed = True
  If (Val(tPoison) >= -1) And (Val(tPoison) <= 100) Then Senshi(Num).Moves(M).Status.Poison = Val(tPoison)
  DoMP
End Sub

Private Sub tPoison_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target gradually loses HP."
End Sub

Private Sub tQuick_Change()
  Changed = True
  If (Val(tQuick) >= -1) And (Val(tQuick) <= 100) Then Senshi(Num).Moves(M).Status.Quick = Val(tQuick)
  DoMP
End Sub

Private Sub tQuick_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target's next non-morph non-life move is carried out immediately."
End Sub

Private Sub tR1_Change()
  Changed = True
  If (Val(tR1) >= -1) And (Val(tR1) <= 100) Then Senshi(Num).Moves(M).Status.R1 = Val(tR1)
  DoMP
End Sub

Private Sub tR2_Change()
  Changed = True
  If (Val(tR2) >= -1) And (Val(tR2) <= 100) Then Senshi(Num).Moves(M).Status.R2 = Val(tR2)
  DoMP
End Sub

Private Sub tRegen_Change()
  Changed = True
  If (Val(tRegen) >= -1) And (Val(tRegen) <= 100) Then Senshi(Num).Moves(M).Status.Regen = Val(tRegen)
  DoMP
End Sub

Private Sub tRegen_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target gradually restores HP."
End Sub

Private Sub tScarecrow_Change()
  Changed = True
  If (Val(tScarecrow) >= -1) And (Val(tScarecrow) <= 100) Then Senshi(Num).Moves(M).Status.Scarecrow = Val(tScarecrow)
  DoMP
End Sub

Private Sub tsleep_Change()
  Changed = True
  If (Val(tSleep) >= -1) And (Val(tSleep) <= 100) Then Senshi(Num).Moves(M).Status.sleep = Val(tSleep)
  DoMP
End Sub

Private Sub tSleep_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is put to sleep and cannot move."
End Sub

Private Sub tSlow_Change()
  Changed = True
  If (Val(tSlow) >= -1) And (Val(tSlow) <= 100) Then Senshi(Num).Moves(M).Status.slow = Val(tSlow)
  DoMP
End Sub

Private Sub tSlow_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target moves slower."
End Sub

Private Sub tStop_Change()
  Changed = True
  If (Val(tStop) >= -1) And (Val(tStop) <= 100) Then Senshi(Num).Moves(M).Status.Stop = Val(tStop)
  DoMP
End Sub

Private Sub tStop_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is placed in stasis and cannot move or act."
End Sub

Private Sub tStun_Change()
  Changed = True
  If (Val(tStun) >= -1) And (Val(tStun) <= 100) Then Senshi(Num).Moves(M).Status.stun = Val(tStun)
  DoMP
End Sub

Private Sub tStun_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is stunned and cannot move."
End Sub

Private Sub tSuperHit_Change()
  Changed = True
  Senshi(Num).Moves(M).SuperHit = tSuperHit
End Sub

Private Sub tSuperHit_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown when the Supered move is carried out"
End Sub

Private Sub tSuperMiss_Change()
  Changed = True
  Senshi(Num).Moves(M).SuperMiss = tSuperMiss
End Sub

Private Sub tSuperMiss_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Oops! You missed. D'OH!"
End Sub

Private Sub tWeak_Change()
  Changed = True
  If (Val(tWeak) >= -1) And (Val(tWeak) <= 100) Then Senshi(Num).Moves(M).Status.Barrier = Val(tWeak)
  DoMP
End Sub

Private Sub tWeak_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 Then tWeak.Enabled = True
End Sub

Private Sub tWeak_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "A barrier protects from physical attacks."
End Sub

Private Sub UpdateWin()
Dim M1 As MoveType
Dim S As MStatusType
Dim X%
Dim Old%
  Old = Changed
  M1 = Senshi(Num).Moves(M)
  tName = M1.Name
  tBegin2Attack = M1.Begin2Attack
  tBegin2SuperAttack = M1.Begin2SuperAttack
  tBegin2HealSelf = M1.Begin2HealSelf
  tHit = M1.Hit
  tCritHit = M1.CritHit
  tSuperHit = M1.SuperHit
  tHealSelf = M1.HealSelf
  tHealMeld = M1.HealMeld
  tMiss = M1.Miss
  tSuperMiss = M1.SuperMiss
  tCmdKey = M1.CmdKey
  On Error Resume Next
  For X = 0 To 30
    oElement(X).Value = 0
  Next X
  On Error GoTo DidntWork
  oElement_Click (M1.Element)
  oElement(M1.Element) = 1
  Label1(26).Visible = False
  tEleNum.Visible = False
  tEleNum.Text = TrimStr(M1.Element)
  GoTo ContinueOn
DidntWork:
  Label1(26).Visible = True
  tEleNum.Visible = False
  Resume ContinueOn
ContinueOn:
  Select Case M1.Target
    Case Enemy: oEnemy_Click
                oEnemy = 1
    Case Ally: oFriend_Click
                 oFriend = 1
    Case AllAlly: oAllFriend_Click
                    oAllFriend = 1
    Case AllTeam: oAllTeam_Click
                  oAllTeam = 1
    Case AllFoe: oAllFoe_Click
                 oAllFoe = 1
    Case AllButSelf: oAllButSelf_Click
                     oAllButSelf = 1
    Case Everybody: oEverybody_Click
                    oEverybody = 1
    Case 8: oOnlySelf_Click
            oOnlySelf = 1
    Case Else: oEnemy_Click
               oEnemy = 1
  End Select
  tElementStr = TrimStr(M1.ElementStr)
  S = M1.Status
  tMute = TrimStr(S.Mute)
  tChaos = TrimStr(S.chaos)
  tFreeze = TrimStr(S.freeze)
  tSleep = TrimStr(S.sleep)
  tPoison = TrimStr(S.Poison)
  tBlind = TrimStr(S.blind)
  tWeak = TrimStr(S.Barrier)
  tSlow = TrimStr(S.slow)
  tStun = TrimStr(S.stun)
  tDefUp = TrimStr(S.Bless)
  tAttUp = TrimStr(S.Curse)
  tHaste = TrimStr(S.haste)
  tRegen = TrimStr(S.Regen)
  tStop = TrimStr(S.Stop)
  tLife3 = TrimStr(S.Life3)
  tMIA = TrimStr(S.MIA)
  tQuick = TrimStr(S.Quick)
  tBerserk = TrimStr(S.Berserk)
  tMushroom = TrimStr(S.Mushroom)
  tAttDn = TrimStr(S.MBarrier)
  tCharm = TrimStr(S.Charm)
  tScarecrow = TrimStr(S.Scarecrow)
  tR1 = TrimStr(S.R1)
  tR2 = TrimStr(S.R2)
  'cCanSuper.Value = M1.CanSuper
  oCanSuper(M1.CanSuper).Value = True
  DoMP
  Changed = Old
End Sub

