VERSION 5.00
Begin VB.Form fCheat 
   Appearance      =   0  'Flat
   BackColor       =   &H00882826&
   Caption         =   "What the hell are you doing here?"
   ClientHeight    =   5325
   ClientLeft      =   2325
   ClientTop       =   1230
   ClientWidth     =   5880
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
   Icon            =   "FCHEAT.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   355
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   392
   Begin VB.CommandButton cAddStatus 
      Caption         =   "Add Status"
      Height          =   495
      Left            =   4440
      TabIndex        =   76
      Top             =   3720
      Width           =   1215
   End
   Begin VB.ComboBox cStatusToAdd 
      Height          =   315
      ItemData        =   "FCHEAT.frx":0CCA
      Left            =   4320
      List            =   "FCHEAT.frx":0CD1
      Style           =   2  'Dropdown List
      TabIndex        =   75
      Top             =   3240
      Width           =   1455
   End
   Begin VB.TextBox tCharges 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3000
      TabIndex        =   69
      Top             =   3120
      Width           =   495
   End
   Begin VB.TextBox tWeapon 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3000
      TabIndex        =   65
      Top             =   2880
      Width           =   495
   End
   Begin VB.TextBox tRuneTemp2 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3000
      TabIndex        =   71
      Top             =   2640
      Width           =   495
   End
   Begin VB.TextBox tRuneTemp 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3000
      TabIndex        =   67
      Top             =   2400
      Width           =   495
   End
   Begin VB.TextBox tRune 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3000
      TabIndex        =   64
      Top             =   2160
      Width           =   495
   End
   Begin VB.TextBox tCharging 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3000
      TabIndex        =   73
      Top             =   1920
      Width           =   495
   End
   Begin VB.TextBox tScroller 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   56
      Top             =   1440
      Width           =   495
   End
   Begin VB.TextBox tBehavior 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3600
      TabIndex        =   61
      Top             =   4920
      Width           =   495
   End
   Begin VB.CommandButton bSave 
      Appearance      =   0  'Flat
      Caption         =   "Save Changes"
      Height          =   375
      Left            =   3600
      TabIndex        =   59
      Top             =   2760
      Width           =   1455
   End
   Begin VB.CommandButton bUpdate 
      Appearance      =   0  'Flat
      Caption         =   "Update Fields"
      Height          =   375
      Left            =   3600
      TabIndex        =   58
      Top             =   2280
      Width           =   1455
   End
   Begin VB.CheckBox cAuto 
      Appearance      =   0  'Flat
      BackColor       =   &H00882826&
      Caption         =   "Auto-Update"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3720
      TabIndex        =   57
      Top             =   1920
      Width           =   1335
   End
   Begin VB.Timer Timer1 
      Interval        =   1000
      Left            =   4440
      Top             =   1320
   End
   Begin VB.CheckBox cDefect 
      Appearance      =   0  'Flat
      BackColor       =   &H00882826&
      Caption         =   "Yes"
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
      Left            =   1320
      TabIndex        =   49
      Top             =   4560
      Width           =   1215
   End
   Begin VB.TextBox tFatalFrags 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   42
      Top             =   4320
      Width           =   495
   End
   Begin VB.TextBox tFrags 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   41
      Top             =   4080
      Width           =   495
   End
   Begin VB.TextBox tTarget 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   40
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox tMoveStart 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   38
      Top             =   3360
      Width           =   855
   End
   Begin VB.TextBox tCurMove 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   37
      Top             =   3120
      Width           =   495
   End
   Begin VB.TextBox tMagDef 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   36
      Top             =   2880
      Width           =   495
   End
   Begin VB.TextBox tMagStr 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   35
      Top             =   2640
      Width           =   495
   End
   Begin VB.TextBox tPhysDef 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   34
      Top             =   2400
      Width           =   495
   End
   Begin VB.TextBox tPhysStr 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   33
      Top             =   2160
      Width           =   495
   End
   Begin VB.TextBox tSuper 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   32
      Top             =   1920
      Width           =   495
   End
   Begin VB.TextBox tMP 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   31
      Top             =   1680
      Width           =   495
   End
   Begin VB.TextBox tHP 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   29
      Top             =   1440
      Width           =   495
   End
   Begin VB.ComboBox cbSenshiID 
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
      Height          =   315
      Left            =   1320
      Style           =   2  'Dropdown List
      TabIndex        =   53
      Top             =   480
      Width           =   4455
   End
   Begin VB.CheckBox cSmart 
      Appearance      =   0  'Flat
      BackColor       =   &H00882826&
      Caption         =   "Yes"
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
      Left            =   3600
      TabIndex        =   52
      Top             =   4680
      Width           =   615
   End
   Begin VB.TextBox tArrogance 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3600
      TabIndex        =   47
      Top             =   4440
      Width           =   495
   End
   Begin VB.TextBox tWrath 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3600
      TabIndex        =   46
      Top             =   4200
      Width           =   495
   End
   Begin VB.TextBox tGreed 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3600
      TabIndex        =   45
      Top             =   3960
      Width           =   495
   End
   Begin VB.TextBox tGoodwill 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3600
      TabIndex        =   44
      Top             =   3720
      Width           =   495
   End
   Begin VB.CheckBox cCPU 
      Appearance      =   0  'Flat
      BackColor       =   &H00882826&
      Caption         =   "Yes"
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
      Left            =   3600
      TabIndex        =   51
      Top             =   3480
      Width           =   615
   End
   Begin VB.CheckBox cDraw 
      Appearance      =   0  'Flat
      BackColor       =   &H00882826&
      Caption         =   "Yes"
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
      Left            =   1320
      TabIndex        =   50
      Top             =   5040
      Width           =   1215
   End
   Begin VB.TextBox tAttackedMe 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   43
      Top             =   4800
      Width           =   495
   End
   Begin VB.TextBox tSuperNum 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   39
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox tMaxHP 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1920
      TabIndex        =   30
      Top             =   1440
      Width           =   495
   End
   Begin VB.TextBox tScrNam 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   27
      Top             =   0
      Width           =   4455
   End
   Begin VB.ComboBox cbOldSenshiID 
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
      Height          =   315
      Left            =   1320
      Style           =   2  'Dropdown List
      TabIndex        =   54
      Top             =   840
      Width           =   4455
   End
   Begin VB.TextBox tTeamID 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   28
      Top             =   1200
      Width           =   375
   End
   Begin VB.CheckBox cGod 
      Appearance      =   0  'Flat
      BackColor       =   &H00882826&
      Caption         =   "True"
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
      Left            =   1320
      TabIndex        =   48
      Top             =   240
      Width           =   1215
   End
   Begin VB.Label Label37 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Idiot Rating (for Idiot Killer)"
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
      Height          =   375
      Left            =   2520
      TabIndex        =   74
      Top             =   1440
      Width           =   1095
   End
   Begin VB.Label Label36 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Charging"
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
      Left            =   2160
      TabIndex        =   72
      Top             =   1920
      Width           =   735
   End
   Begin VB.Label Label35 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "RuneTemp2"
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
      Left            =   1920
      TabIndex        =   70
      Top             =   2640
      Width           =   975
   End
   Begin VB.Label Label34 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Charges"
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
      Left            =   2160
      TabIndex        =   68
      Top             =   3120
      Width           =   735
   End
   Begin VB.Label Label33 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "RuneTemp"
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
      Left            =   2040
      TabIndex        =   66
      Top             =   2400
      Width           =   855
   End
   Begin VB.Label Label32 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Weapon"
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
      Left            =   2160
      TabIndex        =   63
      Top             =   2880
      Width           =   735
   End
   Begin VB.Label Label31 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Rune"
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
      Left            =   2160
      TabIndex        =   62
      Top             =   2160
      Width           =   735
   End
   Begin VB.Label Label30 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Behavior Type"
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
      Left            =   2280
      TabIndex        =   60
      Top             =   4920
      Width           =   1215
   End
   Begin VB.Label Label28 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "/"
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
      Left            =   1800
      TabIndex        =   55
      Top             =   1440
      Width           =   135
   End
   Begin VB.Label Label27 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Wants a Draw?"
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
      Left            =   0
      TabIndex        =   26
      Top             =   5040
      Width           =   1215
   End
   Begin VB.Label Label26 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Last Attacked By"
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
      Left            =   0
      TabIndex        =   25
      Top             =   4800
      Width           =   1215
   End
   Begin VB.Label Label25 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Allowing Defects"
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
      Left            =   0
      TabIndex        =   24
      Top             =   4560
      Width           =   1215
   End
   Begin VB.Label Label24 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Smart CPU"
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
      Left            =   2280
      TabIndex        =   23
      Top             =   4680
      Width           =   1215
   End
   Begin VB.Label Label23 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "CPU Arrogance"
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
      Left            =   2280
      TabIndex        =   22
      Top             =   4440
      Width           =   1215
   End
   Begin VB.Label Label22 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "CPU Wrath"
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
      Left            =   2280
      TabIndex        =   21
      Top             =   4200
      Width           =   1215
   End
   Begin VB.Label Label21 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "CPU Greed"
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
      Left            =   2280
      TabIndex        =   20
      Top             =   3960
      Width           =   1215
   End
   Begin VB.Label Label20 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "CPU Goodwill"
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
      Left            =   2280
      TabIndex        =   19
      Top             =   3720
      Width           =   1215
   End
   Begin VB.Label Label19 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "CPU-Controlled"
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
      Left            =   2280
      TabIndex        =   18
      Top             =   3480
      Width           =   1215
   End
   Begin VB.Label Label18 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Fatal Frags"
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
      Left            =   0
      TabIndex        =   17
      Top             =   4320
      Width           =   1215
   End
   Begin VB.Label Label17 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Frags"
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
      Left            =   0
      TabIndex        =   16
      Top             =   4080
      Width           =   1215
   End
   Begin VB.Label Label16 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Target ID"
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
      Left            =   0
      TabIndex        =   15
      Top             =   3840
      Width           =   1215
   End
   Begin VB.Label Label15 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Super Atk #"
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
      Left            =   0
      TabIndex        =   14
      Top             =   3600
      Width           =   1215
   End
   Begin VB.Label Label14 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Move Started"
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
      Left            =   0
      TabIndex        =   13
      Top             =   3360
      Width           =   1215
   End
   Begin VB.Label Label13 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Current Move ID"
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
      Left            =   0
      TabIndex        =   12
      Top             =   3120
      Width           =   1215
   End
   Begin VB.Label Label12 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "MagDef"
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
      Left            =   0
      TabIndex        =   11
      Top             =   2880
      Width           =   1215
   End
   Begin VB.Label Label11 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "MagStr"
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
      Left            =   0
      TabIndex        =   10
      Top             =   2640
      Width           =   1215
   End
   Begin VB.Label Label10 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "PhysDef"
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
      Left            =   0
      TabIndex        =   9
      Top             =   2400
      Width           =   1215
   End
   Begin VB.Label Label9 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "PhysStr"
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
      Left            =   0
      TabIndex        =   8
      Top             =   2160
      Width           =   1215
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Super Meter"
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
      Left            =   0
      TabIndex        =   7
      Top             =   1920
      Width           =   1215
   End
   Begin VB.Label Label7 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Cheese Meter"
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
      Left            =   0
      TabIndex        =   6
      Top             =   1680
      Width           =   1215
   End
   Begin VB.Label Label6 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "HP/MaxHP"
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
      Left            =   0
      TabIndex        =   5
      Top             =   1440
      Width           =   1215
   End
   Begin VB.Label Label5 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Team ID"
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
      Left            =   0
      TabIndex        =   4
      Top             =   1200
      Width           =   1215
   End
   Begin VB.Label Label4 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Old SenshiID"
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
      Left            =   0
      TabIndex        =   3
      Top             =   840
      Width           =   1215
   End
   Begin VB.Label Label3 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "SenshiID"
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
      Left            =   0
      TabIndex        =   2
      Top             =   480
      Width           =   1215
   End
   Begin VB.Label Label2 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "God Flag"
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
      Left            =   0
      TabIndex        =   1
      Top             =   240
      Width           =   1215
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Player Name"
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
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   1215
   End
End
Attribute VB_Name = "fCheat"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Const MyName = "fCheat2"

Private Sub bSave_Click()
  SaveChanges
End Sub

Private Sub bUpdate_Click()
  UpdateCheat
End Sub

Private Sub cAddStatus_Click()
Dim Almighty(sMaxStatus) As Integer
  If cStatusToAdd.ListIndex > 0 Then
    Send "The host does something odd."
    Almighty(cStatusToAdd.ListIndex) = 100
    CheckStatus Almighty(), Temp4, 0, 1
  End If
End Sub

Private Sub Form_Load()
Dim x As Integer, Msg$
  LoadPosition Me, MyName
  For x = 0 To HighSenshi
    cbSenshiID.AddItem Senshi(x).FullName
    cbOldSenshiID.AddItem Senshi(x).FullName
  Next x
  For x = 0 To sMaxStatus
    Select Case x:
      Case sMute: Msg = "Mute"
      Case sChaos: Msg = "Chaos"
      Case sFreeze: Msg = "Freeze"
      Case sPoison: Msg = "Poison"
      Case sBlind: Msg = "Blindness"
      Case sInvin: Msg = "Invin"
      Case sHaste: Msg = "Haste"
      Case sMorph: Msg = "Morph"
      Case sSlow: Msg = "Slow"
      Case sStun: Msg = "Stun"
      Case sReraise: Msg = "Reraise"
      Case sRegen: Msg = "Regen"
      Case sStop: Msg = "Stop"
      Case sMushroom: Msg = "Mushroom"
      Case sMIA: Msg = "MIA"
      Case sQuick: Msg = "Quick"
      Case sBerserk: Msg = "Berserk"
      Case sSleep: Msg = "Sleep"
      Case sVirus: Msg = "Virus"
      Case sBarrier: Msg = "Barrier"
      Case sMBarrier: Msg = "MBarrier"
      Case sBless: Msg = "Bless"
      Case sCurse: Msg = "Curse"
      Case sPMS: Msg = "PMS"
      Case sZombie: Msg = "Zombie"
      Case sHamedo: Msg = "Hamedo"
      Case Else: Msg = "??? (" + TrimStr(x) + ")"
    End Select
    cStatusToAdd.AddItem Msg, x
  Next x
  UpdateCheat
  If Config.NewUser Then Yoshi yhWhat
End Sub

Private Sub Form_Unload(Cancel As Integer)
  SavePosition Me, MyName
End Sub

Private Sub SaveChanges()
  P(Temp4).ScrNam = tScrNam.Text
  If cGod = 1 Then P(Temp4).God = True Else P(Temp4).God = False
  P(Temp4).CharID = cbSenshiID.ListIndex
  ToChUBBot ("CHUBPLAYERS " + TrimStr(NumPlaying))
  P(Temp4).OldCharID = cbOldSenshiID.ListIndex
  P(Temp4).TeamID = tTeamID.Text
  P(Temp4).HP = Val(tHP)
  P(Temp4).MaxHP = Val(tMaxHP)
  P(Temp4).Cheese = Val(tMP)
  'P(Temp4).MP = Val(tMP)
  'P(Temp4).MaxMP = Val(tMaxMP)
  P(Temp4).Super = Val(tSuper)
  P(Temp4).PhysStr = Val(tPhysStr)
  P(Temp4).PhysDef = Val(tPhysDef)
  P(Temp4).MagStr = Val(tMagStr)
  P(Temp4).MagDef = Val(tMagDef)
  P(Temp4).CurMove = Val(tCurMove)
  P(Temp4).MoveStart = Val(tMoveStart)
  P(Temp4).SuperNum = Val(tSuperNum)
  P(Temp4).Target = Val(tTarget)
  P(Temp4).Frags = Val(tFrags)
  P(Temp4).FatalFrags = Val(tFatalFrags)
  If cDefect = 1 Then P(Temp4).Defect = True Else P(Temp4).Defect = False
  P(Temp4).AttackedMe = Val(tAttackedMe)
  If cDraw = 1 Then P(Temp4).Draw = True Else P(Temp4).Draw = False
  If cCPU = 1 Then P(Temp4).CPU = True Else P(Temp4).CPU = False
  P(Temp4).Goodwill = Val(tGoodwill)
  P(Temp4).Greed = Val(tGreed)
  P(Temp4).Wrath = Val(tWrath)
  P(Temp4).Arrogance = Val(tArrogance)
  P(Temp4).Scroller = Val(tScroller)
  P(Temp4).Charging = Val(tCharging)
  P(Temp4).Rune = Val(tRune)
  P(Temp4).RuneTemp = Val(tRuneTemp)
  P(Temp4).RuneTemp2 = Val(tRuneTemp2)
  P(Temp4).Weapon = Val(tWeapon)
  P(Temp4).WpnUsesLeft = Val(tCharges)
  InitMoves (Temp4)
End Sub

Private Sub Timer1_Timer()
  If cAuto = 1 Then UpdateCheat
End Sub

Private Sub UpdateCheat()
  tScrNam.Text = P(Temp4).ScrNam
  If P(Temp4).God Then cGod = 1 Else cGod = 0
  cbSenshiID.ListIndex = P(Temp4).CharID
  cbOldSenshiID.ListIndex = P(Temp4).OldCharID
  tTeamID.Text = P(Temp4).TeamID
  tHP = TrimStr(P(Temp4).HP)
  tMaxHP = TrimStr(P(Temp4).MaxHP)
  tMP = TrimStr(P(Temp4).Cheese)
  'tMP = TrimStr(p(Temp4).MP)
  'tMaxMP = TrimStr(p(Temp4).MaxMP)
  tSuper = TrimStr(P(Temp4).Super)
  tPhysStr = TrimStr(P(Temp4).PhysStr)
  tPhysDef = TrimStr(P(Temp4).PhysDef)
  tMagStr = TrimStr(P(Temp4).MagStr)
  tMagDef = TrimStr(P(Temp4).MagDef)
  tCurMove = TrimStr(P(Temp4).CurMove)
  tMoveStart = TrimStr(P(Temp4).MoveStart)
  tSuperNum = TrimStr(P(Temp4).SuperNum)
  tTarget = TrimStr(P(Temp4).Target)
  tFrags = TrimStr(P(Temp4).Frags)
  tFatalFrags = TrimStr(P(Temp4).FatalFrags)
  If P(Temp4).Defect Then cDefect = 1 Else cDefect = 0
  tAttackedMe = TrimStr(P(Temp4).AttackedMe)
  If P(Temp4).Draw Then cDraw = 1 Else cDraw = 0
  If P(Temp4).CPU Then cCPU = 1 Else cCPU = 0
  tGoodwill = TrimStr(P(Temp4).Goodwill)
  tGreed = TrimStr(P(Temp4).Greed)
  tWrath = TrimStr(P(Temp4).Wrath)
  tArrogance = TrimStr(P(Temp4).Arrogance)
  tRune = TrimStr(P(Temp4).Rune)
  tWeapon = TrimStr(P(Temp4).Weapon)
  tRuneTemp = TrimStr(P(Temp4).RuneTemp)
  tRuneTemp2 = TrimStr(P(Temp4).RuneTemp2)
  tCharges = TrimStr(P(Temp4).WpnUsesLeft)
  tCharging = TrimStr(P(Temp4).Charging)
  tScroller = TrimStr(P(Temp4).Scroller)
End Sub

