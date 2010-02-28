VERSION 5.00
Begin VB.Form fDSEdit 
   Appearance      =   0  'Flat
   BackColor       =   &H0050017E&
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "ChUB 2000 CharEdit"
   ClientHeight    =   6480
   ClientLeft      =   450
   ClientTop       =   1410
   ClientWidth     =   11535
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
   Icon            =   "fche.frx":0000
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6480
   ScaleWidth      =   11535
   StartUpPosition =   2  'CenterScreen
   Begin VB.DirListBox Dir 
      Height          =   1665
      Left            =   7800
      TabIndex        =   94
      Top             =   4560
      Width           =   1935
   End
   Begin VB.CommandButton cmdEnc 
      Caption         =   "Encrypt Current"
      Height          =   255
      Left            =   9840
      TabIndex        =   93
      Top             =   3960
      Width           =   1455
   End
   Begin VB.CommandButton cmdSave 
      Caption         =   "Save Current"
      Height          =   255
      Left            =   9840
      TabIndex        =   92
      Top             =   3720
      Width           =   1455
   End
   Begin VB.CommandButton cmdLoad 
      Caption         =   "Load Char"
      Height          =   255
      Left            =   9840
      TabIndex        =   91
      Top             =   3480
      Width           =   1455
   End
   Begin VB.FileListBox File 
      Height          =   1845
      Left            =   9840
      Pattern         =   "*.ch?"
      TabIndex        =   90
      Top             =   4320
      Width           =   1575
   End
   Begin VB.Frame fResist 
      Appearance      =   0  'Flat
      BackColor       =   &H0050017E&
      Caption         =   "Resistance"
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
      Height          =   2775
      Left            =   8400
      TabIndex        =   88
      Top             =   240
      Width           =   1335
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Moon"
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
         Index           =   20
         Left            =   120
         TabIndex        =   47
         Top             =   240
         Width           =   975
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Shadow"
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
         Index           =   21
         Left            =   120
         TabIndex        =   48
         Top             =   480
         Value           =   -1  'True
         Width           =   975
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Fire"
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
         Index           =   24
         Left            =   120
         TabIndex        =   49
         Top             =   720
         Width           =   975
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Water"
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
         Index           =   22
         Left            =   120
         TabIndex        =   50
         Top             =   960
         Width           =   975
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Lightning"
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
         Index           =   25
         Left            =   120
         TabIndex        =   51
         Top             =   1200
         Width           =   1035
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Wind"
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
         Index           =   28
         Left            =   120
         TabIndex        =   52
         Top             =   1440
         Width           =   975
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Earth"
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
         Index           =   27
         Left            =   120
         TabIndex        =   53
         Top             =   1680
         Width           =   975
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Ki"
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
         Index           =   29
         Left            =   120
         TabIndex        =   54
         Top             =   1920
         Width           =   975
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Light"
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
         Index           =   30
         Left            =   120
         TabIndex        =   55
         Top             =   2160
         Width           =   975
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Heart"
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
         Index           =   26
         Left            =   120
         TabIndex        =   56
         Top             =   2400
         Width           =   975
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Poison"
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
         Index           =   70
         Left            =   120
         TabIndex        =   57
         Top             =   2640
         Visible         =   0   'False
         Width           =   975
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Grass"
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
         Index           =   71
         Left            =   120
         TabIndex        =   58
         Top             =   2880
         Visible         =   0   'False
         Width           =   975
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Rock"
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
         Index           =   72
         Left            =   120
         TabIndex        =   59
         Top             =   3120
         Visible         =   0   'False
         Width           =   975
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Dirt"
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
         Index           =   73
         Left            =   120
         TabIndex        =   60
         Top             =   3360
         Visible         =   0   'False
         Width           =   975
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Psychic"
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
         Index           =   74
         Left            =   120
         TabIndex        =   61
         Top             =   3600
         Visible         =   0   'False
         Width           =   975
      End
      Begin VB.OptionButton oResist 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Ghost"
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
         Index           =   75
         Left            =   120
         TabIndex        =   89
         Top             =   3840
         Visible         =   0   'False
         Width           =   975
      End
   End
   Begin VB.Frame fWeakTo 
      Appearance      =   0  'Flat
      BackColor       =   &H0050017E&
      Caption         =   "Weak To"
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
      Height          =   2775
      Left            =   6960
      TabIndex        =   87
      Top             =   240
      Width           =   1215
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Earth"
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
         Index           =   27
         Left            =   120
         TabIndex        =   37
         Top             =   1680
         Width           =   975
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Ki"
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
         Index           =   29
         Left            =   120
         TabIndex        =   38
         Top             =   1920
         Width           =   975
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Light"
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
         Index           =   30
         Left            =   120
         TabIndex        =   39
         Top             =   2160
         Width           =   975
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Heart"
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
         Index           =   26
         Left            =   120
         TabIndex        =   40
         Top             =   2400
         Width           =   975
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Poison"
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
         Index           =   70
         Left            =   120
         TabIndex        =   41
         Top             =   2640
         Visible         =   0   'False
         Width           =   975
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Grass"
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
         Index           =   71
         Left            =   120
         TabIndex        =   42
         Top             =   2880
         Visible         =   0   'False
         Width           =   975
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Rock"
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
         Index           =   72
         Left            =   120
         TabIndex        =   43
         Top             =   3120
         Visible         =   0   'False
         Width           =   975
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Dirt"
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
         Index           =   73
         Left            =   120
         TabIndex        =   44
         Top             =   3360
         Visible         =   0   'False
         Width           =   975
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Psychic"
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
         Index           =   74
         Left            =   120
         TabIndex        =   45
         Top             =   3600
         Visible         =   0   'False
         Width           =   975
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Ghost"
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
         Index           =   75
         Left            =   120
         TabIndex        =   46
         Top             =   3840
         Visible         =   0   'False
         Width           =   975
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Wind"
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
         Index           =   28
         Left            =   120
         TabIndex        =   36
         Top             =   1440
         Width           =   975
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Lightning"
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
         Index           =   25
         Left            =   120
         TabIndex        =   35
         Top             =   1200
         Width           =   1035
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Water"
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
         Index           =   22
         Left            =   120
         TabIndex        =   34
         Top             =   960
         Width           =   975
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Fire"
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
         Index           =   24
         Left            =   120
         TabIndex        =   33
         Top             =   720
         Width           =   975
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Shadow"
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
         Index           =   21
         Left            =   120
         TabIndex        =   32
         Top             =   480
         Value           =   -1  'True
         Width           =   975
      End
      Begin VB.OptionButton oWeakness 
         Appearance      =   0  'Flat
         BackColor       =   &H0050017E&
         Caption         =   "Moon"
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
         Index           =   20
         Left            =   120
         TabIndex        =   31
         Top             =   240
         Width           =   975
      End
   End
   Begin VB.TextBox tFatalMove 
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
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   22
      Top             =   4200
      Width           =   4935
   End
   Begin VB.TextBox tPreFatal 
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
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   21
      Top             =   3960
      Width           =   4935
   End
   Begin VB.TextBox tFatalCmdkey 
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
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   20
      Top             =   3720
      Width           =   4935
   End
   Begin VB.TextBox tDeathStr 
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
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   19
      Top             =   3480
      Width           =   4935
   End
   Begin VB.TextBox tKillStr 
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
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   17
      Top             =   3240
      Width           =   4935
   End
   Begin VB.TextBox tTaunt 
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
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   15
      Top             =   3000
      Width           =   4935
   End
   Begin VB.TextBox tBlockYes 
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
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   13
      Top             =   2760
      Width           =   4935
   End
   Begin VB.TextBox tBlockFail 
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
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   12
      Top             =   2520
      Width           =   4935
   End
   Begin VB.TextBox tBlock 
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
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   11
      Top             =   2280
      Width           =   4935
   End
   Begin VB.TextBox tRest 
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
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   10
      Top             =   2040
      Width           =   4935
   End
   Begin VB.TextBox tSelectJoin 
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
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   9
      Top             =   1800
      Width           =   4935
   End
   Begin VB.TextBox tSelectStr 
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
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   8
      Top             =   1560
      Width           =   4935
   End
   Begin VB.TextBox tDesc 
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
      Index           =   4
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   7
      Top             =   1320
      Width           =   4935
   End
   Begin VB.TextBox tDesc 
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
      Index           =   3
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   6
      Top             =   1080
      Width           =   4935
   End
   Begin VB.TextBox tDesc 
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
      Index           =   2
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   5
      Top             =   840
      Width           =   4935
   End
   Begin VB.TextBox tDesc 
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
      Index           =   1
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   4
      Top             =   600
      Width           =   4935
   End
   Begin VB.TextBox tPickMe 
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
      Left            =   4320
      MaxLength       =   120
      TabIndex        =   3
      Top             =   360
      Width           =   1935
   End
   Begin VB.TextBox tSenshiID 
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
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   2
      Top             =   360
      Width           =   1935
   End
   Begin VB.Timer Timer1 
      Interval        =   1000
      Left            =   840
      Top             =   1080
   End
   Begin VB.VScrollBar vsDeathStr 
      Height          =   255
      Left            =   1680
      Max             =   5
      Min             =   1
      TabIndex        =   18
      Top             =   3480
      Value           =   1
      Width           =   135
   End
   Begin VB.VScrollBar vsKillStr 
      Height          =   255
      Left            =   1680
      Max             =   5
      Min             =   1
      TabIndex        =   16
      Top             =   3240
      Value           =   1
      Width           =   135
   End
   Begin VB.CommandButton cEdit 
      Appearance      =   0  'Flat
      Caption         =   "Edit"
      Height          =   375
      Left            =   3240
      TabIndex        =   63
      Top             =   5400
      Width           =   735
   End
   Begin VB.ComboBox cbMoves 
      Appearance      =   0  'Flat
      Height          =   315
      Left            =   120
      Style           =   2  'Dropdown List
      TabIndex        =   62
      Top             =   5400
      Width           =   3015
   End
   Begin VB.TextBox tMagDef 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   30
      Text            =   "0"
      Top             =   5040
      Width           =   495
   End
   Begin VB.TextBox tMagStr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3720
      TabIndex        =   28
      Text            =   "0"
      Top             =   4800
      Width           =   495
   End
   Begin VB.TextBox tPhysDef 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   2040
      TabIndex        =   26
      Text            =   "0"
      Top             =   5040
      Width           =   495
   End
   Begin VB.TextBox tPhysStr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   2040
      TabIndex        =   24
      Text            =   "0"
      Top             =   4800
      Width           =   495
   End
   Begin VB.HScrollBar hsMagDef 
      Height          =   255
      LargeChange     =   10
      Left            =   2640
      Max             =   100
      TabIndex        =   29
      Top             =   5040
      Width           =   1095
   End
   Begin VB.HScrollBar hsMagStr 
      Height          =   255
      LargeChange     =   10
      Left            =   2640
      Max             =   100
      TabIndex        =   27
      Top             =   4800
      Width           =   1095
   End
   Begin VB.HScrollBar hsPhysDef 
      Height          =   255
      LargeChange     =   10
      Left            =   960
      Max             =   100
      TabIndex        =   25
      Top             =   5040
      Width           =   1095
   End
   Begin VB.HScrollBar hsPhysStr 
      Height          =   255
      LargeChange     =   10
      Left            =   960
      Max             =   100
      TabIndex        =   23
      Top             =   4800
      Width           =   1095
   End
   Begin VB.VScrollBar vsTaunt 
      Height          =   255
      Left            =   1680
      Max             =   5
      Min             =   1
      TabIndex        =   14
      Top             =   3000
      Value           =   1
      Width           =   135
   End
   Begin VB.TextBox tFullName 
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
      Left            =   1920
      MaxLength       =   120
      TabIndex        =   1
      Top             =   0
      Width           =   3135
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Description"
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
      Left            =   120
      TabIndex        =   86
      Top             =   600
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Block Successful"
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
      Left            =   120
      TabIndex        =   85
      Top             =   2760
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Stat Avg."
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
      Left            =   4320
      TabIndex        =   84
      Top             =   4560
      Width           =   1095
   End
   Begin VB.Label InfoLine 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
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
      Left            =   120
      TabIndex        =   64
      Top             =   5760
      Width           =   5295
   End
   Begin VB.Label StatLine 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Chibot CharEdit"
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
      Left            =   0
      TabIndex        =   83
      Top             =   6240
      Width           =   11535
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "When Killed 1"
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
      Left            =   120
      TabIndex        =   82
      Top             =   3480
      Width           =   1455
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "After a Kill 1"
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
      Left            =   120
      TabIndex        =   81
      Top             =   3240
      Width           =   1455
   End
   Begin VB.Label LTotal 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   24
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   -1  'True
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   495
      Left            =   4320
      TabIndex        =   65
      Top             =   4800
      Width           =   975
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Defense"
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
      Left            =   120
      TabIndex        =   66
      Top             =   5040
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Strength"
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
      Left            =   120
      TabIndex        =   67
      Top             =   4800
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Magical"
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
      Left            =   2640
      TabIndex        =   68
      Top             =   4560
      Width           =   1095
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Physical"
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
      Left            =   960
      TabIndex        =   69
      Top             =   4560
      Width           =   1095
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Fatality Execution"
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
      Left            =   120
      TabIndex        =   80
      Top             =   4200
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Fatality Pre-Attack"
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
      Left            =   120
      TabIndex        =   79
      Top             =   3960
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Fatality /"
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
      Left            =   240
      TabIndex        =   78
      Top             =   3720
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Taunt 1"
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
      Left            =   120
      TabIndex        =   77
      Top             =   3000
      Width           =   1455
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Block Failed"
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
      Left            =   120
      TabIndex        =   76
      Top             =   2520
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "/Block String"
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
      Index           =   13
      Left            =   120
      TabIndex        =   75
      Top             =   2280
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Rest String"
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
      Left            =   120
      TabIndex        =   74
      Top             =   2040
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Join during Battle"
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
      Left            =   120
      TabIndex        =   73
      Top             =   1800
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Selection String"
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
      Left            =   120
      TabIndex        =   72
      Top             =   1560
      Width           =   1695
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "ID (Short Name)"
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
      Left            =   120
      TabIndex        =   71
      Top             =   360
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
      Index           =   7
      Left            =   4200
      TabIndex        =   70
      Top             =   360
      Width           =   135
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0FF&
      Caption         =   "REMEMBER NOT TO USE DOUBLE QUOTES ("")"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   495
      Left            =   5520
      TabIndex        =   0
      Top             =   5760
      Width           =   2055
   End
   Begin VB.Menu mChar 
      Caption         =   "&File"
      Begin VB.Menu mClearChar 
         Caption         =   "&New"
         Shortcut        =   ^N
      End
      Begin VB.Menu mDash 
         Caption         =   "-"
      End
      Begin VB.Menu mLoadChar 
         Caption         =   "&Load..."
         Shortcut        =   ^L
      End
      Begin VB.Menu mSaveChar 
         Caption         =   "&Save..."
         Shortcut        =   ^S
      End
      Begin VB.Menu mEncrypt 
         Caption         =   "&Encrypt..."
         Shortcut        =   ^E
      End
      Begin VB.Menu mDash2 
         Caption         =   "-"
      End
      Begin VB.Menu mExit 
         Caption         =   "E&xit"
         Shortcut        =   ^X
      End
   End
End
Attribute VB_Name = "fDSEdit"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim Temp1 As Integer

Private Sub cEdit_Click()
  'Changed = True
  M = cbMoves.ListIndex + 1
  Load fMoveEdit
End Sub

Private Sub cEdit_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Edit Moves"
End Sub

Private Function ChecksOkay(S As SenshiType) As Integer
Dim X As Integer
Dim Y As Integer
  If S.PhysStr + S.PhysDef + S.MagStr + S.MagDef > 220 Then
    InfoLine = "Error: Stat Average must be less than 220."
    ChecksOkay = 0
    Exit Function
  End If
  ChecksOkay = 1
End Function

Private Sub CTotal()
Dim T As Integer
Dim X As Integer
  T = Senshi(Num).PhysStr + Senshi(Num).PhysDef + Senshi(Num).MagStr + Senshi(Num).MagDef
  Select Case T
    Case 1 To 219: LTotal.ForeColor = &H80FF80
    Case 220: LTotal.ForeColor = &HFFFFFF
    Case Is > 220: LTotal.ForeColor = &H8080FF
  End Select
  LTotal.Caption = Trim(Str$(T))
  For X = 1 To MaxMoves
    Senshi(Num).Moves(X).MPReq = CALCMP(Num, X)
  Next X
End Sub

Private Sub cmdEnc_Click()
Dim S$
  If ChecksOkay(Senshi(1)) Then
    On Error GoTo DontSave3
    'CmdChr.FileName = First8(Senshi(Num).SenshiID) + ".che"
    'CmdChr.DialogTitle = "Save Encrypted Character"
    'CmdChr.FilterIndex = 2
    'CmdChr.Action = 2
    S = File.Path + "\" + First8(Senshi(Num).SenshiID) + ".che"
    SaveEncryptedChar S, Num
    Changed = False
    Temp1 = 0
    InfoLine = "Character " + S + " successfully encrypted."
    File.Refresh
DontSave3:
    On Error GoTo 0
    Exit Sub
  Else
    Temp1 = 1
  End If
  File.Refresh
End Sub

Private Sub cmdLoad_Click()
Dim S As String
Dim X As Integer
  On Error GoTo Cancell1
  S = File.Path + "\" + File.FileName
  Call LoadChar(S, 1)
  UpdateFields
  Changed = False
  InfoLine = "Character " + S + " successfully loaded."
Cancell1:
  On Error GoTo 0
  Exit Sub
End Sub

Private Sub cmdSave_Click()
Dim S As String
  If ChecksOkay(Senshi(1)) Then
    On Error GoTo DontSave1
    'CmdChr.FileName = First8(Senshi(Num).SenshiID) + ".ch2"
    'CmdChr.DialogTitle = "Save Character"
    'CmdChr.FilterIndex = 2
    'CmdChr.Action = 2
    S = File.Path + "\" + First8(Senshi(Num).SenshiID) + ".ch2"
    If Len(S) > 4 Then
      If UCase(Right$(S, 3) = "CHR") Then
        SaveChar S, Num
      Else
        SaveNewChar S, Num
      End If
    Else
      SaveNewChar S, Num
    End If
    Changed = False
    Temp1 = 0
    InfoLine = "Character " + S + " successfully saved."
    File.Refresh
DontSave1:
    On Error GoTo 0
    Exit Sub
  Else
    Temp1 = 1
  End If
End Sub

Private Sub Dir_Change()
  File.Path = Dir.Path
End Sub

Private Sub Form_Load()
Dim X As Integer
Dim S As String
  Me.Show
  ReDim Senshi(1)
  If (Command$ <> "") Then
    S = Command$
    LoadChar S, 1
    Fil = S
    UpdateFields
    Changed = False
    InfoLine = "Character " + S + " successfully loaded."
  Else
    Fil = ""
    ClearChar (1)
    UpdateFields
  End If
  Changed = False
  M = 1
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Chibot CharEdit"
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
  If (Changed) Then
    Select Case MsgBox("Save changes?", 35, "ChUB 2000 CharEdit")
      Case 2: Cancel = 1
      Case 6: mSaveChar_Click
              Cancel = Temp1
      Case 7: Cancel = 0
    End Select
  End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
  End
End Sub

Private Sub hsMagDef_Change()
  Changed = True
  Senshi(Num).MagDef = hsMagDef.Value
  tMagDef = TrimStr(hsMagDef.Value)
  CTotal
End Sub

Private Sub hsMagDef_Scroll()
  hsMagDef_Change
End Sub

Private Sub hsMagStr_Change()
  Changed = True
  Senshi(Num).MagStr = hsMagStr.Value
  tMagStr = TrimStr(hsMagStr.Value)
  CTotal
End Sub

Private Sub hsMagStr_Scroll()
  hsMagStr_Change
End Sub

Private Sub hsPhysDef_Change()
  Changed = True
  Senshi(Num).PhysDef = hsPhysDef.Value
  tPhysDef = TrimStr(hsPhysDef.Value)
  CTotal
End Sub

Private Sub hsPhysDef_Scroll()
  hsPhysDef_Change
End Sub

Private Sub hsPhysStr_Change()
  Changed = True
  Senshi(Num).PhysStr = hsPhysStr.Value
  tPhysStr = TrimStr(hsPhysStr.Value)
  CTotal
End Sub

Private Sub hsPhysStr_Scroll()
  hsPhysStr_Change
End Sub

Private Sub Label2_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Failure to do so will result in screwed-up characters."
End Sub

Private Sub mClearChar_Click()
  If MsgBox("Erase this character?", 36, "CharEdit") = 6 Then
    ClearChar (Num)
    UpdateAll
  End If
End Sub

Private Sub mEncrypt_Click()
Dim S As String
  cmdEnc_Click
End Sub

Private Sub mExit_Click()
  Unload Me
End Sub

Private Sub mLoadChar_Click()
  cmdLoad_Click
End Sub

Private Sub mSaveChar_Click()
cmdSave_Click
End Sub

Private Sub oResist_Click(Index As Integer)
  Changed = True
  Senshi(Num).Resist = Index
End Sub

Private Sub oResist_MouseMove(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "This character will take less damage against the selected element."
End Sub

Private Sub oWeakness_Click(Index As Integer)
  Changed = True
  Senshi(Num).WeakTo = Index
End Sub

Private Sub oWeakness_MouseMove(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Change this character's Magic Weakness. If someone attacks this character with their weakness, damage increases."
End Sub

Private Sub StatLine_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Ooga Booga."
End Sub

Private Sub tBlock_Change()
  Senshi(Num).Block = tBlock
  Changed = True
End Sub

Private Sub tBlock_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown when player blocks"
End Sub

Private Sub tBlockFail_Change()
  Senshi(Num).BlockFail = tBlockFail
  Changed = True
End Sub

Private Sub tBlockFail_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown when a block fails"
End Sub

Private Sub tBlockYes_Change()
  Senshi(Num).BlockYes = tBlockYes
  Changed = True
End Sub

Private Sub tBlockYes_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown if the block was successful in warding off an attack"
End Sub

Private Sub tDeathStr_Change()
  Senshi(Num).DeathStr(vsDeathStr.Value) = tDeathStr.Text
  Changed = True
End Sub

Private Sub tDeathStr_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  'StatLine = "Shown when character
End Sub

Private Sub tDesc_Change(Index As Integer)
  Senshi(Num).Desc(Index) = tDesc(Index)
  Changed = True
End Sub

Private Sub tFatalCmdkey_Change()
  Senshi(Num).Fatality.CmdKey = tFatalCmdkey
  Changed = True
End Sub

Private Sub tFatalCmdkey_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Fatality Command Key"
End Sub

Private Sub tFatalMove_Change()
  Senshi(Num).Fatality.FatalMove = tFatalMove
  Changed = True
End Sub

Private Sub tFatalMove_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Fatality Finish"
End Sub

Private Sub tFullName_Change()
Dim X As Integer
  Senshi(Num).FullName = tFullName
  Changed = True
End Sub

Private Sub tFullName_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Character's Name"
End Sub

Private Sub Timer1_Timer()
  If Timer1.Interval = 10000 Then
    InfoLine = ""
    Timer1.Interval = 1000
  End If
  If InfoLine <> "" Then
    Timer1.Interval = 10000
  End If
End Sub

Private Sub Timer2_Timer()
  Dir.Refresh
  File.Refresh
End Sub

Private Sub tKillStr_Change()
  Senshi(Num).KillStr(vsKillStr.Value) = tKillStr.Text
  Changed = True
End Sub

Private Sub tKillStr_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown after character makes a kill"
End Sub

Private Sub tMagDef_Change()
  Changed = True
  If (Val(tMagDef) >= 0) And (Val(tMagDef) <= 100) Then
    hsMagDef = tMagDef
    Senshi(Num).MagDef = tMagDef
    CTotal
  End If
End Sub

Private Sub tMagStr_Change()
  Changed = True
  If (Val(tMagStr) >= 0) And (Val(tMagStr) <= 100) Then
    hsMagStr = tMagStr
    Senshi(Num).MagStr = tMagStr
    CTotal
  End If
End Sub

Private Sub tPhysDef_Change()
  Changed = True
  If (Val(tPhysDef) >= 0) And (Val(tPhysDef) <= 100) Then
    hsPhysDef = tPhysDef
    Senshi(Num).PhysDef = tPhysDef
    CTotal
  End If
End Sub

Private Sub tPhysStr_Change()
  Changed = True
  If (Val(tPhysStr) >= 0) And (Val(tPhysStr) <= 100) Then
    hsPhysStr = tPhysStr
    Senshi(Num).PhysStr = tPhysStr
    CTotal
  End If
End Sub

Private Sub tPickMe_Change()
  Senshi(Num).PickMe = tPickMe
  Changed = True
End Sub

Private Sub tPickMe_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Selection String"
End Sub

Private Sub tPreFatal_Change()
  Senshi(Num).Fatality.PreFatal = tPreFatal
  Changed = True
End Sub

Private Sub tPreFatal_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Fatality Start"
End Sub

Private Sub tRest_Change()
  Senshi(Num).Rest = tRest
  Changed = True
End Sub

Private Sub tRest_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown when player rests"
End Sub

Private Sub tSelectJoin_Change()
  Senshi(Num).SelectJoin = tSelectJoin
  Changed = True
End Sub

Private Sub tSelectJoin_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown when character picked during battle"
End Sub

Private Sub tSelectStr_Change()
  Senshi(Num).SelectStr = tSelectStr
  Changed = True
End Sub

Private Sub tSelectStr_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown when character picked at selection"
End Sub

Private Sub tSenshiID_Change()
  Senshi(Num).SenshiID = tSenshiID
  Changed = True
End Sub

Private Sub tSenshiID_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Targeting Identifier"
End Sub

Private Sub tSpecies_Change()
  Changed = True
End Sub

Private Sub tTaunt_Change()
  Senshi(Num).Taunt(vsTaunt.Value) = tTaunt.Text
  Changed = True
End Sub

Private Sub tTaunt_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown at /taunt"
End Sub

Private Sub UpdateAll()
Dim X As Integer
  UpdateFields
End Sub

Private Sub UpdateFields()
Dim S As SenshiType
Dim X As Integer
  S = Senshi(1)
  tFullName = S.FullName
  tPickMe = S.PickMe
  tSenshiID = S.SenshiID
  tSelectStr = S.SelectStr
  tSelectJoin = S.SelectJoin
  tRest = S.Rest
  tBlock = S.Block
  tBlockFail = S.BlockFail
  tBlockYes = S.BlockYes
  tTaunt.Text = S.Taunt(vsTaunt.Value)
  tKillStr.Text = S.KillStr(vsKillStr.Value)
  tDeathStr.Text = S.DeathStr(vsDeathStr.Value)
  tFatalCmdkey = S.Fatality.CmdKey
  tPreFatal = S.Fatality.PreFatal
  tFatalMove = S.Fatality.FatalMove
  hsPhysStr = S.PhysStr
  hsPhysDef = S.PhysDef
  hsMagStr = S.MagStr
  hsMagDef = S.MagDef
TryAgain:
  On Error GoTo Defa
  oWeakness(S.WeakTo).Value = True
  GoTo NoProb
Defa:
  S.WeakTo = Shadow
  Resume TryAgain
NoProb:
  On Error GoTo Df2
  oResist(S.Resist).Value = True
  GoTo Np2
Df2:
  S.Resist = Shadow
  Resume TryAgain
Np2:
  CTotal
  cbMoves.Clear
  For X = 1 To MaxMoves
    cbMoves.AddItem S.Moves(X).Name
  Next X
  For X = 1 To 4
    tDesc(X).Text = S.Desc(X)
  Next X
  cbMoves.ListIndex = 0
End Sub

Private Sub vsDeathStr_Change()
  Label1(1).Caption = "When Killed " + Trim(Str$(vsDeathStr.Value))
  tDeathStr.Text = Senshi(Num).DeathStr(vsDeathStr.Value)
End Sub

Private Sub vsKillStr_Change()
  Label1(0).Caption = "After a Kill " + Trim(Str$(vsKillStr.Value))
  tKillStr.Text = Senshi(Num).KillStr(vsKillStr.Value)
End Sub

Private Sub vsTaunt_Change()
  Label1(15).Caption = "Taunt " + Trim(Str$(vsTaunt.Value))
  tTaunt.Text = Senshi(Num).Taunt(vsTaunt.Value)
End Sub

