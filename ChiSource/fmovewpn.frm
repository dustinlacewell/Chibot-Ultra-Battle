VERSION 2.00
Begin Form fMoveEdit 
   BackColor       =   &H00400040&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Weapon Move Editor"
   ClientHeight    =   6495
   ClientLeft      =   375
   ClientTop       =   825
   ClientWidth     =   8730
   Height          =   6900
   Left            =   315
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   6495
   ScaleWidth      =   8730
   Top             =   480
   Width           =   8850
   Begin Frame fElement 
      BackColor       =   &H00400040&
      Caption         =   "Element"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   3495
      Left            =   6240
      TabIndex        =   116
      Top             =   600
      Width           =   2415
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Physical"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   41
         Top             =   240
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Heal"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   42
         Top             =   480
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Morph"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   43
         Top             =   720
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Poison"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   70
         Left            =   120
         TabIndex        =   50
         Top             =   2400
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Psychic"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   74
         Left            =   1200
         TabIndex        =   65
         Top             =   2640
         Visible         =   0   'False
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Life"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   17
         Left            =   120
         TabIndex        =   45
         Top             =   1200
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Demi"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   19
         Left            =   120
         TabIndex        =   48
         Top             =   1920
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Moon"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   20
         Left            =   1200
         TabIndex        =   55
         Top             =   240
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Shadow"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   21
         Left            =   1200
         TabIndex        =   56
         Top             =   480
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Water"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   22
         Left            =   1200
         TabIndex        =   57
         Top             =   720
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Fire"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   24
         Left            =   1200
         TabIndex        =   58
         Top             =   960
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Lightning"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   25
         Left            =   1200
         TabIndex        =   59
         Top             =   1200
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Heart"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   26
         Left            =   1200
         TabIndex        =   60
         Top             =   1440
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Earth"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   27
         Left            =   1200
         TabIndex        =   61
         Top             =   1680
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Light"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   30
         Left            =   1200
         TabIndex        =   63
         Top             =   2160
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Ki"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   29
         Left            =   1200
         TabIndex        =   64
         Top             =   2400
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "NoDmg"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   47
         Top             =   1680
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Reveal"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   23
         Left            =   120
         TabIndex        =   46
         Top             =   1440
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "HP Drain"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   11
         Left            =   120
         TabIndex        =   44
         Top             =   960
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Wind"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   28
         Left            =   1200
         TabIndex        =   62
         Top             =   1920
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "LearnMove"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   42
         Left            =   120
         TabIndex        =   49
         Top             =   2160
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Grass"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   71
         Left            =   120
         TabIndex        =   53
         Top             =   2880
         Visible         =   0   'False
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "92"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   92
         Left            =   120
         TabIndex        =   51
         Top             =   2640
         Visible         =   0   'False
         Width           =   495
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Rock"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   72
         Left            =   1200
         TabIndex        =   66
         Top             =   2880
         Visible         =   0   'False
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Dirt"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   73
         Left            =   120
         TabIndex        =   54
         Top             =   3120
         Visible         =   0   'False
         Width           =   1095
      End
      Begin OptionButton oElement 
         BackColor       =   &H00400040&
         Caption         =   "Ghost"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   75
         Left            =   1200
         TabIndex        =   67
         Top             =   3120
         Visible         =   0   'False
         Width           =   1095
      End
      Begin OptionButton oElement 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00400040&
         Caption         =   "66"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   66
         Left            =   600
         TabIndex        =   52
         Top             =   2640
         Visible         =   0   'False
         Width           =   495
      End
   End
   Begin CommandButton cLoad 
      Caption         =   "Load"
      Height          =   255
      Left            =   6360
      TabIndex        =   68
      Top             =   4200
      Width           =   975
   End
   Begin CommandButton cSave 
      Caption         =   "Save"
      Height          =   255
      Left            =   7560
      TabIndex        =   69
      Top             =   4200
      Width           =   975
   End
   Begin CommonDialog cmdChr 
      CancelError     =   -1  'True
      DefaultExt      =   "*.chr"
      Filter          =   "ChUB 2000 Moves (*.m2k)|*.m2k"
      FilterIndex     =   1
      Flags           =   4
      Left            =   5880
      Top             =   5040
   End
   Begin CheckBox cReqAllUses 
      BackColor       =   &H00400040&
      Caption         =   "Req. All Uses"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   7440
      TabIndex        =   75
      Top             =   5400
      Width           =   1215
   End
   Begin CheckBox cInstantHit 
      BackColor       =   &H00400040&
      Caption         =   "Instant Hit"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   6360
      TabIndex        =   74
      Top             =   5400
      Width           =   975
   End
   Begin OptionButton oDestroyWeapon 
      BackColor       =   &H00400040&
      Caption         =   "Destroys Weapon"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   3
      Left            =   6360
      TabIndex        =   73
      Top             =   5160
      Width           =   2295
   End
   Begin OptionButton oDestroyWeapon 
      BackColor       =   &H00400040&
      Caption         =   "Weapon is Dropped"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   2
      Left            =   6360
      TabIndex        =   72
      Top             =   4920
      Width           =   2295
   End
   Begin OptionButton oDestroyWeapon 
      BackColor       =   &H00400040&
      Caption         =   "Expends Weapon"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   1
      Left            =   6360
      TabIndex        =   71
      Top             =   4680
      Width           =   2295
   End
   Begin OptionButton oDestroyWeapon 
      BackColor       =   &H00400040&
      Caption         =   "No Effect on Weapon"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   0
      Left            =   6360
      TabIndex        =   70
      Top             =   4440
      Width           =   2295
   End
   Begin TextBox tElementStr 
      Height          =   285
      Left            =   8160
      TabIndex        =   77
      Top             =   5880
      Width           =   495
   End
   Begin TextBox tQuick 
      Height          =   285
      Left            =   3720
      TabIndex        =   30
      Top             =   5160
      Width           =   495
   End
   Begin TextBox tBerserk 
      Height          =   285
      Left            =   5280
      TabIndex        =   40
      Top             =   5160
      Width           =   495
   End
   Begin TextBox tMushroom 
      Height          =   285
      Left            =   3720
      TabIndex        =   29
      Top             =   4920
      Width           =   495
   End
   Begin TextBox tMIA 
      Height          =   285
      Left            =   5280
      TabIndex        =   39
      Top             =   4920
      Width           =   495
   End
   Begin TextBox tLife3 
      Height          =   285
      Left            =   5280
      TabIndex        =   38
      Top             =   4680
      Width           =   495
   End
   Begin TextBox tSlow 
      Height          =   285
      Left            =   5280
      TabIndex        =   37
      Top             =   4440
      Width           =   495
   End
   Begin TextBox tAttDn 
      Height          =   285
      Left            =   5280
      TabIndex        =   36
      Top             =   4200
      Width           =   495
   End
   Begin TextBox tWeak 
      Height          =   285
      Left            =   5280
      TabIndex        =   35
      Top             =   3960
      Width           =   495
   End
   Begin TextBox tStop 
      Height          =   285
      Left            =   3720
      TabIndex        =   28
      Top             =   4680
      Width           =   495
   End
   Begin TextBox tRegen 
      Height          =   285
      Left            =   3720
      TabIndex        =   27
      Top             =   4440
      Width           =   495
   End
   Begin TextBox tEleNum 
      Enabled         =   0   'False
      Height          =   285
      Left            =   8160
      TabIndex        =   76
      Top             =   5640
      Visible         =   0   'False
      Width           =   495
   End
   Begin HScrollBar hsMove 
      Height          =   255
      Left            =   120
      Max             =   5
      Min             =   1
      TabIndex        =   104
      Top             =   5280
      Value           =   1
      Width           =   2895
   End
   Begin TextBox tHaste 
      Height          =   285
      Left            =   5280
      TabIndex        =   34
      Top             =   3720
      Width           =   495
   End
   Begin TextBox tAttUp 
      Height          =   285
      Left            =   5280
      TabIndex        =   33
      Top             =   3480
      Width           =   495
   End
   Begin TextBox tDefUp 
      Height          =   285
      Left            =   5280
      TabIndex        =   32
      Top             =   3240
      Width           =   495
   End
   Begin TextBox tStun 
      Height          =   285
      Left            =   5280
      TabIndex        =   31
      Top             =   3000
      Width           =   495
   End
   Begin TextBox tBlind 
      Height          =   285
      Left            =   3720
      TabIndex        =   26
      Top             =   4200
      Width           =   495
   End
   Begin TextBox tPoison 
      Height          =   285
      Left            =   3720
      TabIndex        =   25
      Top             =   3960
      Width           =   495
   End
   Begin TextBox tSleep 
      Height          =   285
      Left            =   3720
      TabIndex        =   24
      Top             =   3720
      Width           =   495
   End
   Begin TextBox tFreeze 
      Height          =   285
      Left            =   3720
      TabIndex        =   23
      Top             =   3480
      Width           =   495
   End
   Begin TextBox tChaos 
      Height          =   285
      Left            =   3720
      TabIndex        =   22
      Top             =   3240
      Width           =   495
   End
   Begin TextBox tMute 
      Height          =   285
      Left            =   3720
      TabIndex        =   21
      Top             =   3000
      Width           =   495
   End
   Begin Frame fTarget 
      BackColor       =   &H00400040&
      Caption         =   "Target"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   2295
      Left            =   120
      TabIndex        =   89
      Top             =   2760
      Width           =   2415
      Begin OptionButton oOnlySelf 
         BackColor       =   &H00400040&
         Caption         =   "Self Only"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   114
         Top             =   240
         Width           =   2175
      End
      Begin OptionButton oEverybody 
         BackColor       =   &H00400040&
         Caption         =   "EVERYBODY (MP x3)"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   20
         Top             =   1920
         Width           =   2175
      End
      Begin OptionButton oAllButSelf 
         BackColor       =   &H00400040&
         Caption         =   "All But Attacker (MP x3)"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   19
         Top             =   1680
         Width           =   2055
      End
      Begin OptionButton oAllFoe 
         BackColor       =   &H00400040&
         Caption         =   "All Enemies (MP x2.5)"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   18
         Top             =   1440
         Width           =   2175
      End
      Begin OptionButton oAllTeam 
         BackColor       =   &H00400040&
         Caption         =   "One Enemy Team (MP x2)"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   17
         Top             =   1200
         Width           =   2175
      End
      Begin OptionButton oAllFriend 
         BackColor       =   &H00400040&
         Caption         =   "All Allies (MP x2)"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   16
         Top             =   960
         Width           =   2175
      End
      Begin OptionButton oFriend 
         BackColor       =   &H00400040&
         Caption         =   "One Ally"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   15
         Top             =   720
         Width           =   2175
      End
      Begin OptionButton oEnemy 
         BackColor       =   &H00400040&
         Caption         =   "One Enemy"
         FontBold        =   0   'False
         FontItalic      =   0   'False
         FontName        =   "MS Sans Serif"
         FontSize        =   8.25
         FontStrikethru  =   0   'False
         FontUnderline   =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   120
         TabIndex        =   14
         Top             =   480
         Width           =   2175
      End
   End
   Begin TextBox tSuperMiss 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   2520
      TabIndex        =   13
      Top             =   2400
      Width           =   3615
   End
   Begin TextBox tMiss 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   2520
      TabIndex        =   12
      Top             =   2160
      Width           =   3615
   End
   Begin TextBox tHealMeld 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   2520
      TabIndex        =   11
      Top             =   1920
      Visible         =   0   'False
      Width           =   3615
   End
   Begin TextBox tHealSelf 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   2520
      TabIndex        =   10
      Top             =   1680
      Width           =   3615
   End
   Begin TextBox tSuperHit 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   2520
      TabIndex        =   9
      Top             =   1440
      Width           =   3615
   End
   Begin TextBox tCritHit 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   2520
      TabIndex        =   8
      Top             =   1200
      Width           =   3615
   End
   Begin TextBox tHit 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   2520
      TabIndex        =   7
      Top             =   960
      Width           =   3615
   End
   Begin TextBox tBegin2HealSelf 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   2520
      TabIndex        =   6
      Top             =   720
      Width           =   3615
   End
   Begin TextBox tBegin2SuperAttack 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   2520
      TabIndex        =   5
      Top             =   480
      Width           =   3615
   End
   Begin TextBox tBegin2Attack 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   2520
      TabIndex        =   4
      Top             =   240
      Width           =   3615
   End
   Begin CheckBox cCanSuper 
      BackColor       =   &H00400040&
      Caption         =   "Can be Supered"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   6360
      TabIndex        =   3
      Top             =   360
      Width           =   1695
   End
   Begin TextBox tCmdKey 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   6480
      TabIndex        =   2
      Top             =   0
      Width           =   1455
   End
   Begin TextBox tName 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   2520
      TabIndex        =   1
      Top             =   0
      Width           =   3615
   End
   Begin Label StatLine 
      Alignment       =   2  'Center
      BackColor       =   &H00C0C0C0&
      Caption         =   "Move Editor"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   -120
      TabIndex        =   115
      Top             =   6240
      Width           =   8895
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Quick"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   34
      Left            =   2640
      TabIndex        =   113
      Top             =   5160
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Berserk"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   33
      Left            =   4200
      TabIndex        =   112
      Top             =   5160
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Mushroom"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   32
      Left            =   2640
      TabIndex        =   111
      Top             =   4920
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "M.I.A."
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   31
      Left            =   4200
      TabIndex        =   110
      Top             =   4920
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Stop"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   30
      Left            =   2640
      TabIndex        =   109
      Top             =   4680
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Regen"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   29
      Left            =   2640
      TabIndex        =   108
      Top             =   4440
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Life3"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   28
      Left            =   4200
      TabIndex        =   107
      Top             =   4680
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "MBarrier"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   27
      Left            =   4200
      TabIndex        =   106
      Top             =   4200
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Raw Element #"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   26
      Left            =   6720
      TabIndex        =   105
      Top             =   5640
      Visible         =   0   'False
      Width           =   1335
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Move Strength"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   25
      Left            =   6720
      TabIndex        =   103
      Top             =   5880
      Width           =   1335
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Haste"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   24
      Left            =   4200
      TabIndex        =   102
      Top             =   3720
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Curse"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   23
      Left            =   4200
      TabIndex        =   101
      Top             =   3480
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Bless"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   22
      Left            =   4200
      TabIndex        =   100
      Top             =   3240
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Stun"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   21
      Left            =   4200
      TabIndex        =   99
      Top             =   3000
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Slow"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   20
      Left            =   4200
      TabIndex        =   98
      Top             =   4440
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Barrier"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   19
      Left            =   4200
      TabIndex        =   97
      Top             =   3960
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Blindness"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   18
      Left            =   2760
      TabIndex        =   96
      Top             =   4200
      Width           =   855
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Poison"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   17
      Left            =   2880
      TabIndex        =   95
      Top             =   3960
      Width           =   735
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Sleep"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   16
      Left            =   2880
      TabIndex        =   94
      Top             =   3720
      Width           =   735
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Freeze"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   15
      Left            =   2880
      TabIndex        =   93
      Top             =   3480
      Width           =   735
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Chaos"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   14
      Left            =   2880
      TabIndex        =   92
      Top             =   3240
      Width           =   735
   End
   Begin Label Label1 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "% Chance of causing Status (-1 to cancel status)"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   13
      Left            =   2640
      TabIndex        =   91
      Top             =   2760
      Width           =   3615
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Mute"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   12
      Left            =   2880
      TabIndex        =   90
      Top             =   3000
      Width           =   735
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Move Missed (Super)"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   11
      Left            =   0
      TabIndex        =   88
      Top             =   2400
      Width           =   2415
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Move Missed (Normal)"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   10
      Left            =   0
      TabIndex        =   87
      Top             =   2160
      Width           =   2415
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Move Hit (MindMeld Heal)"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   9
      Left            =   0
      TabIndex        =   86
      Top             =   1920
      Visible         =   0   'False
      Width           =   2415
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Move Hit (heal self)"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   8
      Left            =   480
      TabIndex        =   85
      Top             =   1680
      Width           =   1935
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Move Hit (Super)"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   7
      Left            =   720
      TabIndex        =   84
      Top             =   1440
      Width           =   1695
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Move Hit (critical)"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   6
      Left            =   720
      TabIndex        =   83
      Top             =   1200
      Width           =   1695
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Move Hit (normal)"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   5
      Left            =   720
      TabIndex        =   82
      Top             =   960
      Width           =   1695
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Pre-Heal Self"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   4
      Left            =   480
      TabIndex        =   81
      Top             =   720
      Width           =   1935
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Pre-Super Attack"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   3
      Left            =   720
      TabIndex        =   80
      Top             =   480
      Width           =   1695
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Pre-Attack String"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   2
      Left            =   720
      TabIndex        =   79
      Top             =   240
      Width           =   1695
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "/"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   1
      Left            =   6360
      TabIndex        =   78
      Top             =   0
      Width           =   135
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Move Name"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   0
      Left            =   720
      TabIndex        =   0
      Top             =   0
      Width           =   1695
   End
End
Option Explicit

Sub cCanSuper_Click ()
  Changed = True
  Wpn(Num).Moves(M).CanSuper = cCanSuper
End Sub

Sub cCanSuper_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move can be supered. Not all types of attacks may be supered."
End Sub

Sub cInstantHit_Click ()
  Changed = True
  Wpn(Num).Moves(M).InstantHit = cInstantHit.Value
End Sub

Sub cInstantHit_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "The move takes effect immediately. Only works if weapon is fully expended or destroyed"
End Sub

Sub cLoad_Click ()
Dim S As String
On Error GoTo Cancel9
  CmdChr.Filename = "*.m2k"
  CmdChr.DialogTitle = "Load ChUB 2000 Move"
  CmdChr.Action = 1
  S = CmdChr.Filename
  LoadMove S, Num, M
  UpdateWin
  Changed = True
Cancel9:
  On Error GoTo 0
  Exit Sub
End Sub

Sub cReqAllUses_Click ()
  Changed = True
  Wpn(Num).Moves(M).ReqAllUses = cReqAllUses.Value
End Sub

Sub cReqAllUses_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = ("Requires that the weapon has a 'full charge', with no uses decremented.")
End Sub

Sub cSave_Click ()
Dim S As String
  On Error GoTo DontSave3
  CmdChr.Filename = First8(Wpn(Num).Moves(M).CmdKey) + ".m2k"
  CmdChr.DialogTitle = "Save Move"
  CmdChr.FilterIndex = 2
  CmdChr.Action = 2
  S = CmdChr.Filename
  SaveMove S, Num, M
DontSave3:
  On Error GoTo 0
  Exit Sub
End Sub

Sub fElement_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Hmm...."
End Sub

Sub fElement_MouseUp (Button As Integer, Shift As Integer, X As Single, Y As Single)
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
    Else
      MsgBox "Wrong!"
    End If
  End If
End Sub

Sub Form_Load ()
  UpdateWin
  hsMove.Value = M
  hsMove.Max = MaxMoves
  Me.Show
End Sub

Sub Form_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move Editor"
End Sub

Sub Form_Unload (Cancel As Integer)
Dim X As Integer
  fWpnMake!cbMoves.Clear
  For X = 1 To MaxMoves
    fWpnMake!cbMoves.AddItem Wpn(Num).Moves(X).Name
  Next X
  fWpnMake!cbMoves.ListIndex = M - 1
End Sub

Sub hsMove_Change ()
  M = hsMove.Value
  UpdateWin
End Sub

Sub Label1_MouseDown (Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
Dim Msg$
  If Button = 2 Then
    Label1(26).Visible = True
    tEleNum.Visible = True
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

Sub Label3_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "I'm tellin' ya, you'll screw up your character!"
End Sub

Sub Label4_MouseDown (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 Then tWeak.Enabled = True
  If Button = 2 Then tAttDn.Enabled = True
End Sub

Sub oAllButSelf_Click ()
  Changed = True
  Wpn(Num).Moves(M).Target = AllButSelf
  Wpn(Num).Moves(M).CanSuper = False
  cCanSuper.Value = 0
  cCanSuper.Enabled = False
  Label1(4).Caption = "Post-Attack String A"
  Label1(8).Caption = "Post-Attack String B"
End Sub

Sub oAllButSelf_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects everyone BUT the player using it."
End Sub

Sub oAllFoe_Click ()
  Changed = True
  Wpn(Num).Moves(M).Target = AllFoe
  Wpn(Num).Moves(M).CanSuper = False
  cCanSuper.Value = 0
  cCanSuper.Enabled = False
  Label1(4).Caption = "Post-Attack String A"
  Label1(8).Caption = "Post-Attack String B"

End Sub

Sub oAllFoe_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects all enemies in the battle."
End Sub

Sub oAllFriend_Click ()
  Changed = True
  Wpn(Num).Moves(M).Target = Allfriend
  Wpn(Num).Moves(M).CanSuper = False
  cCanSuper.Value = 0
  cCanSuper.Enabled = False
  Label1(4).Caption = "Pre-Heal Self/Team"
  Label1(8).Caption = "Move Hit (heal self)"

End Sub

Sub oAllFriend_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects player's entire team."
End Sub

Sub oAllTeam_Click ()
  Changed = True
  Wpn(Num).Moves(M).Target = AllTeam
  Wpn(Num).Moves(M).CanSuper = False
  cCanSuper.Value = 0
  cCanSuper.Enabled = False
  Label1(4).Caption = "Post-Attack String A"
  Label1(8).Caption = "Post-Attack String B"

End Sub

Sub oAllTeam_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects one enemy team."
End Sub

Sub oDestroyWeapon_Click (Index As Integer)
  Changed = True
  Wpn(Num).Moves(M).DestroyWeapon = Index
End Sub

Sub oDestroyWeapon_MouseMove (Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
  Select Case Index
    Case 0: StatLine = "Weapon is not affected by the move"
    Case 1: StatLine = "# of uses decreased. When # uses = 0, weapon is destroyed"
    Case 2: StatLine = "The weapon is dropped, # of uses returns to max"
    Case 3: StatLine = "Weapon is destroyed, regardless of the # of uses remaining."
  End Select
End Sub

Sub oElement_Click (Index As Integer)
  Changed = True
  Wpn(Num).Moves(M).Element = Index
  tElementStr.Enabled = True
  Select Case Index
    Case NoDmg, Morph, Invin, Life, Demi, Reveal: tElementStr.Enabled = False
  End Select
  If oEnemy.Value = True Then cCanSuper.Enabled = True
  Select Case Index
    Case NoDmg, Phys, HPtheft, MPTheft, Demi, MoonE, Shadow, Water, Fire, Lit, Heart, Earth, Lum, Reveal, Poison:
      'oEnemy.Value = True
      'oEnemy_Click
      cCanSuper.Enabled = True
    Case Heal, Life:
      oFriend.Value = True
      oFriend_Click
  End Select
  Select Case Index
    Case NoDmg, Heal, Morph, Invin, Life, Demi, Reveal, HPtheft, MPTheft: cCanSuper.Enabled = False
  End Select
  tEleNum.Text = trimstr(Wpn(Num).Moves(M).Element)

  oElement(Index).Value = True
End Sub

Sub oElement_MouseDown (Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 Then oElement(10).Visible = True
End Sub

Sub oElement_MouseMove (Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
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

Sub oEnemy_Click ()
  Changed = True
  Wpn(Num).Moves(M).Target = Enemy
  Select Case Wpn(Num).Moves(M).Element
    Case NoDmg, Phys, HPtheft, MPTheft, Demi, MoonE, Shadow, Water, Fire, Lit, Heart, Earth, Lum, Reveal:  cCanSuper.Enabled = True
  End Select
  Label1(4).Caption = "Pre-Heal Self"
  Label1(8).Caption = "Move Hit (heal self)"

End Sub

Sub oEnemy_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move is by default carried out on an enemy."
End Sub

Sub oEverybody_Click ()
  Changed = True
  Wpn(Num).Moves(M).Target = Everybody
  Wpn(Num).Moves(M).CanSuper = False
  cCanSuper.Value = 0
  cCanSuper.Enabled = False
  Label1(4).Caption = "Post-Attack String A"
  Label1(8).Caption = "Post-Attack String B"

End Sub

Sub oEverybody_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects everyone in the battle."
End Sub

Sub oFriend_Click ()
  Changed = True
  Wpn(Num).Moves(M).Target = Friend
  Wpn(Num).Moves(M).CanSuper = False
  cCanSuper.Value = 0
  cCanSuper.Enabled = False
  Label1(4).Caption = "Pre-Heal Self"
  Label1(8).Caption = "Move Hit (heal self)"

End Sub

Sub oFriend_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move is by default carried out on yourself or a friend."
End Sub

Sub oOnlySelf_Click ()
  Changed = True
  Wpn(Num).Moves(M).Target = 8
  Wpn(Num).Moves(M).CanSuper = False
  cCanSuper.Value = 0
  cCanSuper.Enabled = False
  Label1(4).Caption = "Pre-Heal Self"
  Label1(8).Caption = "Move Hit (heal self)"

End Sub

Sub oOnlySelf_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move will affect only the player using it regardless of who s/he targets."
End Sub

Sub StatLine_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Hi, I'm StatLine. I'll tell you what each thing does."
End Sub

Sub tAttDn_Change ()
  Changed = True
  If (Val(tAttDn) >= -1) And (Val(tAttDn) <= 100) Then Wpn(Num).Moves(M).Status.MBarrier = Val(tAttDn)

End Sub

Sub tAttDn_MouseDown (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 Then tAttDn.Enabled = True
End Sub

Sub tAttDn_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "A barrier protects from magic attacks."
End Sub

Sub tAttUp_Change ()
  Changed = True
  If (Val(tattup) >= -1) And (Val(tattup) <= 100) Then Wpn(Num).Moves(M).Status.Curse = Val(tattup)

End Sub

Sub tAttUp_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "The target is cursed to attack with less damage."
End Sub

Sub tBegin2Attack_Change ()
  Changed = True
  Wpn(Num).Moves(M).Begin2Attack = tBegin2Attack
End Sub

Sub tBegin2Attack_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "String shown when player starts doing the attack."
End Sub

Sub tBegin2HealSelf_Change ()
  Changed = True
  Wpn(Num).Moves(M).Begin2HealSelf = tBegin2HealSelf
End Sub

Sub tBegin2HealSelf_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If oOnlySelf Or oEnemy Or oFriend Or oAllFriend Then
    StatLine = "String shown when player beings to use a Heal move on themself."
  Else
    StatLine = "String shown when an attack that hits everyone is carried out."
  End If
End Sub

Sub tBegin2SuperAttack_Change ()
  Changed = True
  Wpn(Num).Moves(M).Begin2SuperAttack = tBegin2SuperAttack
End Sub

Sub tBegin2SuperAttack_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "String shown when beginning a Super Move."
End Sub

Sub tBerserk_Change ()
  Changed = True
  If (Val(tberserk) >= -1) And (Val(tberserk) <= 100) Then Wpn(Num).Moves(M).Status.Berserk = Val(tberserk)

End Sub

Sub tBerserk_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target has increased attack power but uncontrolled attacks on random enemies."
End Sub

Sub tBlind_Change ()
  Changed = True
  If (Val(tblind) >= -1) And (Val(tblind) <= 100) Then Wpn(Num).Moves(M).Status.blind = Val(tblind)

End Sub

Sub tBlind_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target cannot see and moves do minimum damage."
End Sub

Sub tChaos_Change ()
  Changed = True
  If (Val(tchaos) >= -1) And (Val(tchaos) <= 100) Then Wpn(Num).Moves(M).Status.chaos = Val(tchaos)

End Sub

Sub tChaos_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target's mind is placed in chaos. Random moves on random people."
End Sub

Sub tCmdKey_Change ()
  Changed = True
  Wpn(Num).Moves(M).CmdKey = tCmdkey.Text
End Sub

Sub tCmdKey_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move Command Key"
End Sub

Sub tCritHit_Change ()
  Changed = True
  Wpn(Num).Moves(M).CritHit = tCritHit
End Sub

Sub tCritHit_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "String shown instead of Move Hit Normal if the attack is a Critical Hit"
End Sub

Sub tDefUp_Change ()
  Changed = True
  If (Val(tdefup) >= -1) And (Val(tdefup) <= 100) Then Wpn(Num).Moves(M).Status.Bless = Val(tdefup)

End Sub

Sub tDefUp_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "The target is blessed with power that increases their attack damage."
End Sub

Sub tElementStr_Change ()
  Changed = True
  Wpn(Num).Moves(M).ElementStr = Val(tElementStr)

End Sub

Sub tElementStr_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "How much damage it would do if your Strength and their Defense were the same."
End Sub

Sub tEleNum_Change ()
On Error GoTo NeverMind
  oElement(Wpn(Num).Moves(M).Element).Value = False
NeverMind: Resume Next
  oElement_Click (Val(tEleNum))
  On Error GoTo IgnoreIt
  oElement(Val(tEleNum)).Enabled = True
IgnoreIt: Exit Sub
End Sub

Sub tEleNum_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  oElement_MouseMove Val(tEleNum.Text), Button, Shift, X, Y
End Sub

Sub tFreeze_Change ()
  Changed = True
  If (Val(tfreeze) >= -1) And (Val(tfreeze) <= 100) Then Wpn(Num).Moves(M).Status.freeze = Val(tfreeze)

End Sub

Sub tFreeze_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is frozen solid and cannot move."
End Sub

Sub tHaste_Change ()
  Changed = True
  If (Val(thaste) >= -1) And (Val(thaste) <= 100) Then Wpn(Num).Moves(M).Status.haste = Val(thaste)

End Sub

Sub tHaste_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target moves faster."
End Sub

Sub tHealMeld_Change ()
  Changed = True
  Wpn(Num).Moves(M).HealMeld = tHealMeld
End Sub

Sub tHealMeld_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Huh?"
End Sub

Sub tHealSelf_Change ()
  Changed = True
  Wpn(Num).Moves(M).HealSelf = tHealSelf
End Sub

Sub tHealSelf_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "String shown when a heal on self or team is carried out"
End Sub

Sub tHit_Change ()
  Changed = True
  Wpn(Num).Moves(M).Hit = tHit
End Sub

Sub tHit_MouseDown (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If oOnlySelf Or oEnemy Or oFriend Or oAllFriend Then
    StatLine = "String shown when player's attack hits another player (%T)"
  Else
    StatLine = "String shown for each individual person attacked (%T)"
  End If
End Sub

Sub tLife3_Change ()
  Changed = True
  If (Val(tLife3) >= -1) And (Val(tLife3) <= 100) Then Wpn(Num).Moves(M).Status.Life3 = Val(tLife3)

End Sub

Sub tLife3_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target will come back to life after being killed."
End Sub

Sub tMIA_Change ()
  Changed = True
  If (Val(tmia) >= -1) And (Val(tmia) <= 100) Then Wpn(Num).Moves(M).Status.MIA = Val(tmia)

End Sub

Sub tMIA_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is not present in battle."
End Sub

Sub tMiss_Change ()
  Changed = True
  Wpn(Num).Moves(M).Miss = tMiss
End Sub

Sub tMiss_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If oOnlySelf Or oEnemy Or oFriend Or oAllFriend Then
    StatLine = "Oops! Your move missed or was blocked."
  Else
    StatLine = "Oops! Your move missed or was blocked by %T."
  End If
End Sub

Sub tMushroom_Change ()
  Changed = True
  If (Val(tmushroom) >= -1) And (Val(tmushroom) <= 100) Then Wpn(Num).Moves(M).Status.Mushroom = Val(tmushroom)

End Sub

Sub tMushroom_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is turned into a fungus and restores HP at every turn interval."
End Sub

Sub tMute_Change ()
  Changed = True
  If (Val(tMute) >= -1) And (Val(tMute) <= 100) Then Wpn(Num).Moves(M).Status.Mute = Val(tMute)

End Sub

Sub tMute_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is unable to use moves other than physical moves and Basic moves."
End Sub

Sub tName_Change ()
  Changed = True
  Wpn(Num).Moves(M).Name = tName
End Sub

Sub tName_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Name of move. Used in status window and Super Attack messages."
End Sub

Sub tPoison_Change ()
  Changed = True
  If (Val(tpoison) >= -1) And (Val(tpoison) <= 100) Then Wpn(Num).Moves(M).Status.Poison = Val(tpoison)

End Sub

Sub tPoison_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target gradually loses HP."
End Sub

Sub tQuick_Change ()
  Changed = True
  If (Val(tquick) >= -1) And (Val(tquick) <= 100) Then Wpn(Num).Moves(M).Status.Quick = Val(tquick)

End Sub

Sub tQuick_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target's next non-morph non-life move is carried out immediately."
End Sub

Sub tRegen_Change ()
  Changed = True
  If (Val(tregen) >= -1) And (Val(tregen) <= 100) Then Wpn(Num).Moves(M).Status.Regen = Val(tregen)

End Sub

Sub tRegen_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target gradually restores HP."
End Sub

Sub tSleep_Change ()
  Changed = True
  If (Val(tsleep) >= -1) And (Val(tsleep) <= 100) Then Wpn(Num).Moves(M).Status.sleep = Val(tsleep)

End Sub

Sub tSleep_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is put to sleep and cannot move."
End Sub

Sub tSlow_Change ()
  Changed = True
  If (Val(tslow) >= -1) And (Val(tslow) <= 100) Then Wpn(Num).Moves(M).Status.slow = Val(tslow)

End Sub

Sub tSlow_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target moves slower."
End Sub

Sub tStop_Change ()
  Changed = True
  If (Val(tstop) >= -1) And (Val(tstop) <= 100) Then Wpn(Num).Moves(M).Status.Stop = Val(tstop)

End Sub

Sub tStop_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is placed in stasis and cannot move or act."
End Sub

Sub tStun_Change ()
  Changed = True
  If (Val(tstun) >= -1) And (Val(tstun) <= 100) Then Wpn(Num).Moves(M).Status.stun = Val(tstun)

End Sub

Sub tStun_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is stunned and cannot move."
End Sub

Sub tSuperHit_Change ()
  Changed = True
  Wpn(Num).Moves(M).SuperHit = tSuperHit
End Sub

Sub tSuperHit_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown when the Supered move is carried out"
End Sub

Sub tSuperMiss_Change ()
  Changed = True
  Wpn(Num).Moves(M).SuperMiss = tSuperMiss
End Sub

Sub tSuperMiss_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Oops! You missed. D'OH!"
End Sub

Sub tWeak_Change ()
  Changed = True
  If (Val(tWeak) >= -1) And (Val(tWeak) <= 100) Then Wpn(Num).Moves(M).Status.Barrier = Val(tWeak)

End Sub

Sub tWeak_MouseDown (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 Then tWeak.Enabled = True
End Sub

Sub tWeak_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "A barrier protects from physical attacks."
End Sub

Sub UpdateWin ()
Dim M1 As MoveType
Dim S As MStatustype
Dim X%
Dim Old%
  Old = Changed
  M1 = Wpn(Num).Moves(M)
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
  tCmdkey = M1.CmdKey
  oDestroyWeapon(M1.DestroyWeapon) = 1
  If M1.InstantHit <> 0 Then cInstantHit.Value = 1 Else cInstantHit.Value = 0
  If M1.ReqAllUses <> 0 Then cReqAllUses.Value = 1 Else cReqAllUses.Value = 0
  On Error Resume Next
  For X = 0 To 30
    oElement(X).Value = 0
  Next X
  On Error GoTo DidntWork
  oElement_Click (M1.Element)
  oElement(M1.Element) = 1
  Label1(26).Visible = False
  tEleNum.Visible = False
  tEleNum.Text = trimstr(M1.Element)
  GoTo ContinueOn
DidntWork:
  Label1(26).Visible = True
  tEleNum.Visible = False
  Resume ContinueOn
ContinueOn:
  Select Case M1.Target
    Case Enemy: oEnemy_Click
                oEnemy = 1
    Case Friend: oFriend_Click
                 oFriend = 1
    Case Allfriend: oAllFriend_Click
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
  tElementStr = trimstr(M1.ElementStr)
  S = M1.Status
  tMute = trimstr(S.Mute)
  tchaos = trimstr(S.chaos)
  tfreeze = trimstr(S.freeze)
  tsleep = trimstr(S.sleep)
  tpoison = trimstr(S.Poison)
  tblind = trimstr(S.blind)
  tWeak = trimstr(S.Barrier)
  tslow = trimstr(S.slow)
  tstun = trimstr(S.stun)
  tdefup = trimstr(S.Bless)
  tattup = trimstr(S.Curse)
  thaste = trimstr(S.haste)
  tregen = trimstr(S.Regen)
  tstop = trimstr(S.Stop)
  tLife3 = trimstr(S.Life3)
  tmia = trimstr(S.MIA)
  tquick = trimstr(S.Quick)
  tberserk = trimstr(S.Berserk)
  tmushroom = trimstr(S.Mushroom)
  tAttDn = trimstr(S.MBarrier)
  cCanSuper.Value = M1.CanSuper

  Changed = Old
End Sub

