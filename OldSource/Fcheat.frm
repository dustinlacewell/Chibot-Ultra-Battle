VERSION 2.00
Begin Form fCheat 
   BackColor       =   &H00882826&
   Caption         =   "I Spy With My Little Eye..."
   ClientHeight    =   5325
   ClientLeft      =   2325
   ClientTop       =   1230
   ClientWidth     =   5145
   Height          =   5730
   Icon            =   FCHEAT.FRX:0000
   Left            =   2265
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   5325
   ScaleWidth      =   5145
   Top             =   885
   Width           =   5265
   Begin TextBox tCharges 
      Height          =   285
      Left            =   3000
      TabIndex        =   69
      Top             =   3120
      Width           =   495
   End
   Begin TextBox tWeapon 
      Height          =   285
      Left            =   3000
      TabIndex        =   65
      Top             =   2880
      Width           =   495
   End
   Begin TextBox tRuneTemp2 
      Height          =   285
      Left            =   3000
      TabIndex        =   71
      Top             =   2640
      Width           =   495
   End
   Begin TextBox tRuneTemp 
      Height          =   285
      Left            =   3000
      TabIndex        =   67
      Top             =   2400
      Width           =   495
   End
   Begin TextBox tRune 
      Height          =   285
      Left            =   3000
      TabIndex        =   64
      Top             =   2160
      Width           =   495
   End
   Begin TextBox tCharging 
      Height          =   285
      Left            =   3000
      TabIndex        =   73
      Top             =   1920
      Width           =   495
   End
   Begin TextBox tScroller 
      Height          =   285
      Left            =   3720
      TabIndex        =   56
      Top             =   1440
      Width           =   495
   End
   Begin TextBox tBehavior 
      Height          =   285
      Left            =   4560
      TabIndex        =   61
      Top             =   4800
      Width           =   495
   End
   Begin CommandButton bSave 
      Caption         =   "Save Changes"
      Height          =   375
      Left            =   3600
      TabIndex        =   59
      Top             =   2760
      Width           =   1455
   End
   Begin CommandButton bUpdate 
      Caption         =   "Update Fields"
      Height          =   375
      Left            =   3600
      TabIndex        =   58
      Top             =   2280
      Width           =   1455
   End
   Begin CheckBox cAuto 
      BackColor       =   &H00882826&
      Caption         =   "Auto-Update"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3720
      TabIndex        =   57
      Top             =   1920
      Width           =   1335
   End
   Begin Timer Timer1 
      Interval        =   1000
      Left            =   4680
      Top             =   120
   End
   Begin CheckBox cDefect 
      BackColor       =   &H00882826&
      Caption         =   "Yes"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1320
      TabIndex        =   49
      Top             =   4560
      Width           =   1215
   End
   Begin TextBox tFatalFrags 
      Height          =   285
      Left            =   1320
      TabIndex        =   42
      Top             =   4320
      Width           =   495
   End
   Begin TextBox tFrags 
      Height          =   285
      Left            =   1320
      TabIndex        =   41
      Top             =   4080
      Width           =   495
   End
   Begin TextBox tTarget 
      Height          =   285
      Left            =   1320
      TabIndex        =   40
      Top             =   3840
      Width           =   495
   End
   Begin TextBox tMoveStart 
      Height          =   285
      Left            =   1320
      TabIndex        =   38
      Top             =   3360
      Width           =   855
   End
   Begin TextBox tCurMove 
      Height          =   285
      Left            =   1320
      TabIndex        =   37
      Top             =   3120
      Width           =   495
   End
   Begin TextBox tMagDef 
      Height          =   285
      Left            =   1320
      TabIndex        =   36
      Top             =   2880
      Width           =   495
   End
   Begin TextBox tMagStr 
      Height          =   285
      Left            =   1320
      TabIndex        =   35
      Top             =   2640
      Width           =   495
   End
   Begin TextBox tPhysDef 
      Height          =   285
      Left            =   1320
      TabIndex        =   34
      Top             =   2400
      Width           =   495
   End
   Begin TextBox tPhysStr 
      Height          =   285
      Left            =   1320
      TabIndex        =   33
      Top             =   2160
      Width           =   495
   End
   Begin TextBox tSuper 
      Height          =   285
      Left            =   1320
      TabIndex        =   32
      Top             =   1920
      Width           =   495
   End
   Begin TextBox tMP 
      Height          =   285
      Left            =   1320
      TabIndex        =   31
      Top             =   1680
      Width           =   495
   End
   Begin TextBox tHP 
      Height          =   285
      Left            =   1320
      TabIndex        =   29
      Top             =   1440
      Width           =   495
   End
   Begin ComboBox cbSenshiID 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   315
      Left            =   1320
      Style           =   2  'Dropdown List
      TabIndex        =   53
      Top             =   480
      Width           =   3135
   End
   Begin CheckBox cSmart 
      BackColor       =   &H00882826&
      Caption         =   "Yes"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   4560
      TabIndex        =   52
      Top             =   4560
      Width           =   495
   End
   Begin TextBox tArrogance 
      Height          =   285
      Left            =   4560
      TabIndex        =   47
      Top             =   4320
      Width           =   495
   End
   Begin TextBox tWrath 
      Height          =   285
      Left            =   4560
      TabIndex        =   46
      Top             =   4080
      Width           =   495
   End
   Begin TextBox tGreed 
      Height          =   285
      Left            =   4560
      TabIndex        =   45
      Top             =   3840
      Width           =   495
   End
   Begin TextBox tGoodwill 
      Height          =   285
      Left            =   4560
      TabIndex        =   44
      Top             =   3600
      Width           =   495
   End
   Begin CheckBox cCPU 
      BackColor       =   &H00882826&
      Caption         =   "Yes"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   4560
      TabIndex        =   51
      Top             =   3360
      Width           =   495
   End
   Begin CheckBox cDraw 
      BackColor       =   &H00882826&
      Caption         =   "Yes"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1320
      TabIndex        =   50
      Top             =   5040
      Width           =   1215
   End
   Begin TextBox tAttackedMe 
      Height          =   285
      Left            =   1320
      TabIndex        =   43
      Top             =   4800
      Width           =   495
   End
   Begin TextBox tSuperNum 
      Height          =   285
      Left            =   1320
      TabIndex        =   39
      Top             =   3600
      Width           =   495
   End
   Begin TextBox tMaxHP 
      Height          =   285
      Left            =   1920
      TabIndex        =   30
      Top             =   1440
      Width           =   495
   End
   Begin TextBox tScrNam 
      Height          =   285
      Left            =   1320
      TabIndex        =   27
      Top             =   0
      Width           =   3135
   End
   Begin ComboBox cbOldSenshiID 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   315
      Left            =   1320
      Style           =   2  'Dropdown List
      TabIndex        =   54
      Top             =   840
      Width           =   3135
   End
   Begin TextBox tTeamID 
      Height          =   285
      Left            =   1320
      TabIndex        =   28
      Top             =   1200
      Width           =   375
   End
   Begin CheckBox cGod 
      BackColor       =   &H00882826&
      Caption         =   "True"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1320
      TabIndex        =   48
      Top             =   240
      Width           =   1215
   End
   Begin Label Label37 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Idiot Rating (for Idiot Killer)"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   375
      Left            =   2520
      TabIndex        =   74
      Top             =   1440
      Width           =   1095
   End
   Begin Label Label36 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Charging"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2160
      TabIndex        =   72
      Top             =   1920
      Width           =   735
   End
   Begin Label Label35 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "RuneTemp2"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1920
      TabIndex        =   70
      Top             =   2640
      Width           =   975
   End
   Begin Label Label34 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Charges"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2160
      TabIndex        =   68
      Top             =   3120
      Width           =   735
   End
   Begin Label Label33 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "RuneTemp"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2040
      TabIndex        =   66
      Top             =   2400
      Width           =   855
   End
   Begin Label Label32 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Weapon"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2160
      TabIndex        =   63
      Top             =   2880
      Width           =   735
   End
   Begin Label Label31 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Rune"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2160
      TabIndex        =   62
      Top             =   2160
      Width           =   735
   End
   Begin Label Label30 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Behavior Type"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3240
      TabIndex        =   60
      Top             =   4800
      Width           =   1215
   End
   Begin Label Label28 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "/"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1800
      TabIndex        =   55
      Top             =   1440
      Width           =   135
   End
   Begin Label Label27 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Wants a Draw?"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   26
      Top             =   5040
      Width           =   1215
   End
   Begin Label Label26 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Last Attacked By"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   25
      Top             =   4800
      Width           =   1215
   End
   Begin Label Label25 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Allowing Defects"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   24
      Top             =   4560
      Width           =   1215
   End
   Begin Label Label24 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Smart CPU"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3240
      TabIndex        =   23
      Top             =   4560
      Width           =   1215
   End
   Begin Label Label23 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "CPU Arrogance"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3240
      TabIndex        =   22
      Top             =   4320
      Width           =   1215
   End
   Begin Label Label22 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "CPU Wrath"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3240
      TabIndex        =   21
      Top             =   4080
      Width           =   1215
   End
   Begin Label Label21 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "CPU Greed"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3240
      TabIndex        =   20
      Top             =   3840
      Width           =   1215
   End
   Begin Label Label20 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "CPU Goodwill"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3240
      TabIndex        =   19
      Top             =   3600
      Width           =   1215
   End
   Begin Label Label19 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "CPU-Controlled"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3240
      TabIndex        =   18
      Top             =   3360
      Width           =   1215
   End
   Begin Label Label18 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Fatal Frags"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   17
      Top             =   4320
      Width           =   1215
   End
   Begin Label Label17 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Frags"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   16
      Top             =   4080
      Width           =   1215
   End
   Begin Label Label16 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Target ID"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   15
      Top             =   3840
      Width           =   1215
   End
   Begin Label Label15 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Super Atk #"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   14
      Top             =   3600
      Width           =   1215
   End
   Begin Label Label14 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Move Started"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   13
      Top             =   3360
      Width           =   1215
   End
   Begin Label Label13 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Current Move ID"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   12
      Top             =   3120
      Width           =   1215
   End
   Begin Label Label12 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "MagDef"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   11
      Top             =   2880
      Width           =   1215
   End
   Begin Label Label11 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "MagStr"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   10
      Top             =   2640
      Width           =   1215
   End
   Begin Label Label10 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "PhysDef"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   9
      Top             =   2400
      Width           =   1215
   End
   Begin Label Label9 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "PhysStr"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   8
      Top             =   2160
      Width           =   1215
   End
   Begin Label Label8 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Super Meter"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   7
      Top             =   1920
      Width           =   1215
   End
   Begin Label Label7 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Cheese Meter"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   6
      Top             =   1680
      Width           =   1215
   End
   Begin Label Label6 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "HP/MaxHP"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   5
      Top             =   1440
      Width           =   1215
   End
   Begin Label Label5 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Team ID"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   4
      Top             =   1200
      Width           =   1215
   End
   Begin Label Label4 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Old SenshiID"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   3
      Top             =   840
      Width           =   1215
   End
   Begin Label Label3 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "SenshiID"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   2
      Top             =   480
      Width           =   1215
   End
   Begin Label Label2 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "God Flag"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   1
      Top             =   240
      Width           =   1215
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Player Name"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   1215
   End
End
Option Explicit

Const MyName = "fCheat2"

Sub bSave_Click ()
  SaveChanges
End Sub

Sub bUpdate_Click ()
  UpdateCheat
End Sub

Sub Form_Load ()
Dim X As Integer
  LoadPosition Me, MyName
  For X = 0 To NumSenshi
    cbSenshiID.AddItem Senshi(X).FullName
    cbOldSenshiID.AddItem Senshi(X).FullName
  Next X
  UpdateCheat
End Sub

Sub Form_Unload (Cancel As Integer)
  SavePosition Me, MyName

End Sub

Sub SaveChanges ()
  p(Temp4).Scrnam = tScrNam.Text
  If cGod = 1 Then p(Temp4).God = True Else p(Temp4).God = False
  p(Temp4).CharID = cbSenshiID.ListIndex
  p(Temp4).OldCharID = cbOldSenshiID.ListIndex
  p(Temp4).TeamID = tTeamID.Text
  p(Temp4).HP = Val(tHP)
  p(Temp4).MaxHP = Val(tMaxHP)
  p(Temp4).Cheese = Val(tMP)
  'p(Temp4).MP = Val(tMP)
  'p(Temp4).MaxMP = Val(tMaxMP)
  p(Temp4).Super = Val(tSuper)
  p(Temp4).PhysStr = Val(tPhysStr)
  p(Temp4).PhysDef = Val(tPhysDef)
  p(Temp4).MagStr = Val(tMagStr)
  p(Temp4).MagDef = Val(tMagDef)
  p(Temp4).CurMove = Val(tCurMove)
  p(Temp4).MoveStart = Val(tMoveStart)
  p(Temp4).SuperNum = Val(tSuperNum)
  p(Temp4).Target = Val(tTarget)
  p(Temp4).Frags = Val(tFrags)
  p(Temp4).FatalFrags = Val(tFatalFrags)
  If cDefect = 1 Then p(Temp4).Defect = True Else p(Temp4).Defect = False
  p(Temp4).AttackedMe = Val(tAttackedMe)
  If cDraw = 1 Then p(Temp4).Draw = True Else p(Temp4).Draw = False
  If cCPU = 1 Then p(Temp4).CPU = True Else p(Temp4).CPU = False
  p(Temp4).Goodwill = Val(tGoodwill)
  p(Temp4).Greed = Val(tGreed)
  p(Temp4).Wrath = Val(tWrath)
  p(Temp4).Arrogance = Val(tArrogance)
  p(Temp4).Scroller = Val(tScroller)
  p(Temp4).Charging = Val(tCharging)
  p(Temp4).Rune = Val(tRune)
  p(Temp4).RuneTemp = Val(tRuneTemp)
  p(Temp4).RuneTemp2 = Val(tRuneTemp2)
  p(Temp4).Weapon = Val(tWeapon)
  p(Temp4).WpnUsesLeft = Val(tCharges)
  InitMoves (Temp4)
End Sub

Sub Timer1_Timer ()
  If cAuto = 1 Then UpdateCheat
End Sub

Sub UpdateCheat ()
  tScrNam.Text = p(Temp4).Scrnam
  If p(Temp4).God Then cGod = 1 Else cGod = 0
  cbSenshiID.ListIndex = p(Temp4).CharID
  cbOldSenshiID.ListIndex = p(Temp4).OldCharID
  tTeamID.Text = p(Temp4).TeamID
  tHP = TrimStr(p(Temp4).HP)
  tMaxHP = TrimStr(p(Temp4).MaxHP)
  tMP = TrimStr(p(Temp4).Cheese)
  'tMP = TrimStr(p(Temp4).MP)
  'tMaxMP = TrimStr(p(Temp4).MaxMP)
  tSuper = TrimStr(p(Temp4).Super)
  tPhysStr = TrimStr(p(Temp4).PhysStr)
  tPhysDef = TrimStr(p(Temp4).PhysDef)
  tMagStr = TrimStr(p(Temp4).MagStr)
  tMagDef = TrimStr(p(Temp4).MagDef)
  tCurMove = TrimStr(p(Temp4).CurMove)
  tMoveStart = TrimStr(p(Temp4).MoveStart)
  tSuperNum = TrimStr(p(Temp4).SuperNum)
  tTarget = TrimStr(p(Temp4).Target)
  tFrags = TrimStr(p(Temp4).Frags)
  tFatalFrags = TrimStr(p(Temp4).FatalFrags)
  If p(Temp4).Defect Then cDefect = 1 Else cDefect = 0
  tAttackedMe = TrimStr(p(Temp4).AttackedMe)
  If p(Temp4).Draw Then cDraw = 1 Else cDraw = 0
  If p(Temp4).CPU Then cCPU = 1 Else cCPU = 0
  tGoodwill = TrimStr(p(Temp4).Goodwill)
  tGreed = TrimStr(p(Temp4).Greed)
  tWrath = TrimStr(p(Temp4).Wrath)
  tArrogance = TrimStr(p(Temp4).Arrogance)
  tRune = TrimStr(p(Temp4).Rune)
  tWeapon = TrimStr(p(Temp4).Weapon)
  tRuneTemp = TrimStr(p(Temp4).RuneTemp)
  tRuneTemp2 = TrimStr(p(Temp4).RuneTemp2)
  tCharges = TrimStr(p(Temp4).WpnUsesLeft)
  tCharging = TrimStr(p(Temp4).Charging)
  tScroller = TrimStr(p(Temp4).Scroller)
End Sub

