VERSION 5.00
Begin VB.Form fNeoStat 
   Appearance      =   0  'Flat
   BackColor       =   &H00400040&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Character Statistics"
   ClientHeight    =   5760
   ClientLeft      =   660
   ClientTop       =   1455
   ClientWidth     =   7095
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
   ScaleHeight     =   384
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   473
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   15
      Left            =   360
      Picture         =   "FNEOSTAT.frx":0000
      ScaleHeight     =   240
      ScaleWidth      =   255
      TabIndex        =   66
      Top             =   4440
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   30
      Left            =   2640
      Picture         =   "FNEOSTAT.frx":058A
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   60
      Top             =   3360
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   29
      Left            =   4080
      Picture         =   "FNEOSTAT.frx":09CC
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   59
      Top             =   4320
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   28
      Left            =   4080
      Picture         =   "FNEOSTAT.frx":1296
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   58
      Top             =   3840
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   20
      Left            =   3600
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   57
      Top             =   3840
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   480
      Index           =   8
      Left            =   3600
      Picture         =   "FNEOSTAT.frx":1B60
      ScaleHeight     =   480
      ScaleWidth      =   480
      TabIndex        =   56
      Top             =   4320
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Timer tHMS 
      Interval        =   100
      Left            =   4080
      Top             =   4920
   End
   Begin VB.PictureBox pRed 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   600
      Picture         =   "FNEOSTAT.frx":242A
      ScaleHeight     =   465
      ScaleWidth      =   465
      TabIndex        =   45
      Top             =   2400
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pBlue 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   120
      Picture         =   "FNEOSTAT.frx":2734
      ScaleHeight     =   465
      ScaleWidth      =   465
      TabIndex        =   44
      Top             =   2400
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pCPU 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   360
      Picture         =   "FNEOSTAT.frx":2A3E
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   38
      Top             =   720
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   480
      Index           =   26
      Left            =   3120
      Picture         =   "FNEOSTAT.frx":2E80
      ScaleHeight     =   480
      ScaleWidth      =   480
      TabIndex        =   41
      Top             =   3840
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   25
      Left            =   2640
      Picture         =   "FNEOSTAT.frx":32C2
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   40
      Top             =   3840
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   6
      Left            =   120
      Picture         =   "FNEOSTAT.frx":3B8C
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   39
      Top             =   0
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   4
      Left            =   1680
      Picture         =   "FNEOSTAT.frx":3FCE
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   18
      Top             =   3360
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   18
      Left            =   1680
      Picture         =   "FNEOSTAT.frx":4898
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   37
      Top             =   3840
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   17
      Left            =   3120
      Picture         =   "FNEOSTAT.frx":4CDA
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   36
      Top             =   4320
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   14
      Left            =   2640
      Picture         =   "FNEOSTAT.frx":511C
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   34
      Top             =   4320
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   13
      Left            =   2160
      Picture         =   "FNEOSTAT.frx":555E
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   33
      Top             =   3360
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   12
      Left            =   2160
      Picture         =   "FNEOSTAT.frx":5E28
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   32
      Top             =   3840
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   11
      Left            =   240
      Picture         =   "FNEOSTAT.frx":75AA
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   31
      Top             =   3840
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   10
      Left            =   1680
      Picture         =   "FNEOSTAT.frx":7E74
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   30
      Top             =   4320
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pAttDn 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   8040
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   29
      Top             =   3240
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pDefDn 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   7440
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   28
      Top             =   3240
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pAttUp 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   8040
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   27
      Top             =   3840
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   7
      Left            =   2160
      Picture         =   "FNEOSTAT.frx":9F9E
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   26
      Top             =   4320
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pDefUp 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   7440
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   25
      Top             =   3840
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   5
      Left            =   720
      Picture         =   "FNEOSTAT.frx":A3E0
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   24
      Top             =   3840
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   19
      Left            =   240
      Picture         =   "FNEOSTAT.frx":A6EA
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   23
      Top             =   3360
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   3
      Left            =   1200
      Picture         =   "FNEOSTAT.frx":A9F4
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   22
      Top             =   4320
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   2
      Left            =   720
      Picture         =   "FNEOSTAT.frx":AE36
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   21
      Top             =   4320
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   1
      Left            =   1200
      Picture         =   "FNEOSTAT.frx":B700
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   20
      Top             =   3840
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pL3 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   2640
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   9
      Top             =   6720
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pL2 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   2040
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   5
      Top             =   6720
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pL1 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   1440
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   10
      Top             =   6720
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.ListBox Peepz 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
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
      Height          =   4530
      Left            =   4920
      TabIndex        =   6
      Top             =   0
      Width           =   2055
   End
   Begin VB.PictureBox pDead 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   720
      Picture         =   "FNEOSTAT.frx":BA0A
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   19
      Top             =   3360
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.PictureBox pStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      FillColor       =   &H0050017E&
      ForeColor       =   &H80000008&
      Height          =   495
      Index           =   16
      Left            =   1200
      Picture         =   "FNEOSTAT.frx":C2D4
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   35
      Top             =   3360
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   22
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   5280
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   21
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   5040
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   20
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   4800
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   19
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   4560
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   18
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   4320
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   17
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   4080
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   16
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   3840
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   15
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   3600
      Width           =   195
   End
   Begin VB.Label LSP4 
      Appearance      =   0  'Flat
      BackColor       =   &H00800080&
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
      Left            =   3720
      TabIndex        =   65
      Top             =   3000
      Visible         =   0   'False
      Width           =   930
   End
   Begin VB.Label LSP3 
      Appearance      =   0  'Flat
      BackColor       =   &H00000080&
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
      Left            =   2760
      TabIndex        =   64
      Top             =   3000
      Visible         =   0   'False
      Width           =   930
   End
   Begin VB.Label LSP2 
      Appearance      =   0  'Flat
      BackColor       =   &H00008080&
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
      TabIndex        =   63
      Top             =   3000
      Visible         =   0   'False
      Width           =   930
   End
   Begin VB.Label LHP 
      Appearance      =   0  'Flat
      BackColor       =   &H00800000&
      Caption         =   "9999/9999"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFC0C0&
      Height          =   255
      Left            =   1320
      TabIndex        =   62
      Top             =   720
      Width           =   3255
   End
   Begin VB.Label StatLine 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Neo*Status 2000"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   0
      TabIndex        =   61
      Top             =   5520
      Width           =   7095
   End
   Begin VB.Label LWeapon 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Weapon o' Mass Destruction (999 uses)"
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
      Height          =   615
      Left            =   4920
      TabIndex        =   55
      Top             =   4800
      Width           =   2055
   End
   Begin VB.Label Label14 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Weapon:"
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
      Left            =   4920
      TabIndex        =   54
      Top             =   4560
      Width           =   2055
   End
   Begin VB.Label LRune 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Rune of Shit Happens"
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
      Height          =   495
      Left            =   3240
      TabIndex        =   53
      Top             =   3360
      Width           =   1335
   End
   Begin VB.Label LSP 
      Appearance      =   0  'Flat
      BackColor       =   &H00008000&
      Caption         =   "9999"
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
      Left            =   840
      TabIndex        =   52
      Top             =   3000
      Width           =   930
   End
   Begin VB.Label Label10 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "MBarrier"
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
      TabIndex        =   51
      Top             =   5160
      Width           =   615
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   14
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   3360
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   13
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   3120
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   12
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   2880
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   11
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   2640
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   10
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   2400
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   9
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   2160
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   8
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   1920
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   7
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   1680
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   6
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   1440
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   5
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   1200
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   4
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   960
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   3
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   720
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   2
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   480
      Width           =   195
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   1
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   240
      Width           =   195
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   18
      Left            =   3000
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   17
      Left            =   2880
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   16
      Left            =   2760
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   15
      Left            =   2640
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   14
      Left            =   2520
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   13
      Left            =   2400
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   12
      Left            =   2280
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   11
      Left            =   2160
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   10
      Left            =   2040
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   9
      Left            =   1920
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   8
      Left            =   1800
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   7
      Left            =   1680
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   6
      Left            =   1560
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   5
      Left            =   1440
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   4
      Left            =   1320
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   3
      Left            =   1200
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   2
      Left            =   1080
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   1
      Left            =   960
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   0
      Left            =   840
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape MBarrier 
      BackColor       =   &H00000080&
      BorderColor     =   &H000000FF&
      Height          =   255
      Index           =   20
      Left            =   3120
      Top             =   5160
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   19
      Left            =   3120
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   18
      Left            =   3000
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   17
      Left            =   2880
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   16
      Left            =   2760
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   15
      Left            =   2640
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   14
      Left            =   2520
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   13
      Left            =   2400
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   12
      Left            =   2280
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   11
      Left            =   2160
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   10
      Left            =   2040
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   9
      Left            =   1920
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   8
      Left            =   1800
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   7
      Left            =   1680
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   6
      Left            =   1560
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   5
      Left            =   1440
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   4
      Left            =   1320
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   3
      Left            =   1200
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   2
      Left            =   1080
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   1
      Left            =   960
      Top             =   4920
      Width           =   135
   End
   Begin VB.Shape PBarrier 
      BackColor       =   &H00882826&
      BorderColor     =   &H00FF0000&
      Height          =   255
      Index           =   0
      Left            =   840
      Top             =   4920
      Width           =   135
   End
   Begin VB.Label LAttacking 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Anti-Chibot"
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
      TabIndex        =   43
      Top             =   1920
      Width           =   3135
   End
   Begin VB.Label Label12 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Attacking Me:"
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
      TabIndex        =   50
      Top             =   1920
      Width           =   1215
   End
   Begin VB.Label LChar 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Nobob of Arabia"
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
      TabIndex        =   46
      Top             =   480
      Width           =   3135
   End
   Begin VB.Label Label13 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Character:"
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
      Left            =   480
      TabIndex        =   49
      Top             =   480
      Width           =   735
   End
   Begin VB.Label LTID 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Nuke Iraq!!! (Team ?)"
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
      Width           =   3135
   End
   Begin VB.Label Label11 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Team:"
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
      Left            =   720
      TabIndex        =   47
      Top             =   240
      Width           =   495
   End
   Begin VB.Shape sHitsIn 
      BackColor       =   &H0000C000&
      BorderColor     =   &H0000FF00&
      Height          =   255
      Index           =   0
      Left            =   4680
      Shape           =   4  'Rounded Rectangle
      Top             =   0
      Width           =   195
   End
   Begin VB.Label LName 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Chibot Hoser"
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
      TabIndex        =   7
      Top             =   0
      Width           =   3150
   End
   Begin VB.Label Label9 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "PBarrier"
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
      TabIndex        =   42
      Top             =   4920
      Width           =   615
   End
   Begin VB.Label LCP 
      Appearance      =   0  'Flat
      BackColor       =   &H00000040&
      Caption         =   "9999"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00C0C0FF&
      Height          =   255
      Left            =   1320
      TabIndex        =   8
      Top             =   1200
      Width           =   3255
   End
   Begin VB.Label LLast 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Anti-Chibot"
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
      TabIndex        =   17
      Top             =   1680
      Width           =   3135
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "LastAttackedBy:"
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
      Top             =   1680
      Width           =   1215
   End
   Begin VB.Label LFrags 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "99/99"
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
      TabIndex        =   15
      Top             =   1440
      Width           =   495
   End
   Begin VB.Label Label7 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Frags/Fatals:"
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
      Left            =   240
      TabIndex        =   14
      Top             =   1440
      Width           =   975
   End
   Begin VB.Label LMP 
      Appearance      =   0  'Flat
      BackColor       =   &H00004000&
      Caption         =   "9999/9999"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   255
      Left            =   1320
      TabIndex        =   13
      Top             =   960
      Visible         =   0   'False
      Width           =   3255
   End
   Begin VB.Label Label6 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "MP:"
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
      Left            =   960
      TabIndex        =   12
      Top             =   960
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label LStatus 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Jacking Off"
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
      Height          =   735
      Left            =   1320
      TabIndex        =   11
      Top             =   2160
      Width           =   3135
   End
   Begin VB.Label Label5 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Currently:"
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
      Left            =   480
      TabIndex        =   4
      Top             =   2160
      Width           =   735
   End
   Begin VB.Label Label4 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Super:"
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
      Left            =   240
      TabIndex        =   3
      Top             =   3000
      Width           =   495
   End
   Begin VB.Label Label3 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "CP:"
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
      Left            =   840
      TabIndex        =   2
      Top             =   1200
      Width           =   375
   End
   Begin VB.Label Label2 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "HP:"
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
      Left            =   840
      TabIndex        =   1
      Top             =   720
      Width           =   375
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Name:"
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
      Left            =   720
      TabIndex        =   0
      Top             =   0
      Width           =   495
   End
End
Attribute VB_Name = "fNeoStat"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Const Gray = &H404040

Const MyName = "fNeoOld"

Private Sub Form_Load()
Dim x As Integer
  LoadPosition Me, MyName
  For x = 1 To MaxPlayers
    Peepz.AddItem P(x).ScrNam
  Next x
  Peepz.ListIndex = 0
  If Config.Flag <> 0 Then
    If P(Peepz.ListIndex + 1).TeamID = "R" Then Me.BackColor = RGB(150, 0, 0)
    If P(Peepz.ListIndex + 1).TeamID = "B" Then Me.BackColor = RGB(0, 0, 150)
  End If
  On Error Resume Next
  'fNeoStat!iC.Picture = LoadPicture(App.Path + "\pics\" + First8(Senshi(p(1).CharID).CharID) + ".BMP")
  'If Err <> 0 Then fNeoStat!iC.Picture = LoadPicture()
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 Then
    Graph = Peepz.ListIndex + 1
    Unload fGraph
    fGraph.Show
  End If
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Neo*Status 2000"
End Sub

Private Sub Form_Unload(Cancel As Integer)
  SavePosition Me, MyName

End Sub

Private Sub Label10_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "MBarrier: Take less damage from magic attacks."
End Sub

Private Sub Label9_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "PBarrier: Take less damage from physical attacks."
End Sub

Private Sub LAttacking_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "People attacking this player"
End Sub

Private Sub LChar_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Player's Character"
End Sub

Private Sub LCP_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Cheese Meter"
End Sub

Private Sub LFrags_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Frags / Fatalities"
End Sub

Private Sub LHitsIn_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Time left for this move"
End Sub

Private Sub LHP_Click()
Dim x As Integer
Dim Msg As String
Dim index%
  index% = Peepz.ListIndex
  If Godmode Then
    x = Val(kDlgBoxInput("Enter new HP for " + P(index + 1).ScrNam + " (Type -1 to kill):", "SMK God Mode HP Changer", Trim(Str$(P(index + 1).HP))))
    If x = 0 Then x = P(index + 1).HP
    If x = -1 Then
      P(index + 1).HP = -666
      Select Case Int(Rnd * 6) + 1
        Case 1: Msg = "A SuperNova strikes " + P(index + 1).ScrNam + ", instantly killing them."
        Case 2: Msg = P(index + 1).ScrNam + " is struck by a lightning bolt and instantly disintegrated."
        Case 3: Msg = "Doom Gaze comes and casts 'Doom' on " + P(index + 1).ScrNam + "."
        Case 4: Msg = "With a wave of Kamek's wand, " + P(index + 1).ScrNam + " is instantly killed."
        Case 5: Msg = "Cthulhu rises up out of the ground and eats " + P(index + 1).ScrNam + "."
        Case 6: Msg = "Sub-Zero comes and rips out " + P(index + 1).ScrNam + "'s spine with the head still attached."
      End Select
      Send (Msg)
    Else
      P(index + 1).HP = x
    End If
  End If
End Sub

Private Sub LHP_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Current HP"
End Sub

Private Sub LLast_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Last player to attack this player"
End Sub

Private Sub LName_Click()
  If Godmode Then
    Temp4 = Peepz.ListIndex + 1
    fCheat.Show
  End If
End Sub

Private Sub LName_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Player's Screen Name"
End Sub

Private Sub LRune_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Player's Rune"
End Sub

Private Sub LSP_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Super Points"
End Sub

Private Sub LStatus_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "This player's current move"
End Sub

Private Sub LTID_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Player's Team"
End Sub

Private Sub LWeapon_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Player's Weapon and # of charges remaining"
End Sub

Private Sub pAttDn_Click()
  kDlgBox "AttDn: Does less damage with attacks and heals less with heals.", 64, "Explanation"
End Sub

Private Sub pAttUp_Click()
  kDlgBox "AttUp: Damage dealt out and amount healed with healing moves is increased.", 64, "Explanation"
End Sub

Private Sub pBerserk_Click()
  kDlgBox "Berserk: Target cannot control attacks or targets, however their strength is greatly increased.", 64, "Explanation"
End Sub

Private Sub pBlind_Click()
  kDlgBox "Blind: Cannot see very well. Most attacks miss.", 64, "Explanation"
End Sub

Private Sub pBlue_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "This player has the Blue Flag."
End Sub

Private Sub pChaos_Click()
  kDlgBox "Chaos: Uncontrollable random attacks at random targets.", 64, "Explanation"
End Sub

Private Sub pCPU_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "CPU: Target is controlled by the (less-than-smart) ChUB Artificial Stupidity routine."
End Sub

Private Sub pDead_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Oh my God! They killed " + P(Peepz.ListIndex + 1).ScrNam + "! You bastards!"
End Sub

Private Sub pDefDn_Click()
  kDlgBox "DefDn: Will take more damage from attacks.", 64, "Explanation"
End Sub

Private Sub pDefUp_Click()
  kDlgBox "DefUp: Is more resistant to oncoming attacks.", 64, "Explanation"
End Sub

Private Sub Peepz_Click()
Dim x%
  UpdateGameStat
  If Config.Flag <> 0 Then
    If P(Peepz.ListIndex + 1).TeamID = "R" Then Me.BackColor = RGB(150, 0, 0)
    If P(Peepz.ListIndex + 1).TeamID = "B" Then Me.BackColor = RGB(0, 0, 150)
  End If
  'For X = 0 To 19
  '  pHP(X).BackColor = Gray
  '  pMP(X).BackColor = Gray
  '  pSP(X).BackColor = Gray
  'Next X
  'For X = 20 To 26
  '  pSP(X).BackColor = Gray
  'Next X
  On Error Resume Next
  'fNeoStat!iC.Picture = LoadPicture(App.Path + "\pics\" + First8(Senshi(p(Peepz.ListIndex + 1).CharID).CharID) + ".BMP")
  'If Err <> 0 Then fNeoStat!iC.Picture = LoadPicture()
End Sub

Private Sub Peepz_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Lookit all the people playing!"
End Sub

Private Sub pFreeze_Click()
  kDlgBox "Frozen: Frozen solid. Cannot act. Can be melted with a fire attack.", 64, "Explanation"
End Sub

Private Sub pHaste_Click()
  kDlgBox "Haste: Attacks take 10 seconds to hit instead of 15.", 64, "Explanation"
End Sub

Private Sub pHP_MouseMove(index As Integer, Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Neato torpedo HP bar!"
End Sub

Private Sub pL1_Click()
  kDlgBox "Super Level 1: Requires 100 Super Points.", 64, "Explanation"
End Sub

Private Sub pL2_Click()
  kDlgBox "Super Level 2: Requires 200 Super Points.", 64, "Explanation"
End Sub

Private Sub pL3_Click()
  kDlgBox "Super Level 3: Requires 300 Super Points. Maximum possible level on Super Meter.", 64, "Explanation"
End Sub

Private Sub pMIA_Click()
  kDlgBox "MIA: Missing In Action. Cannot act, cannot be targeted. Will return to whatever they were doing when they are returned to Earth.", 64, "Explanation"
End Sub

Private Sub pMP_MouseMove(index As Integer, Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Neato torpedo Cheese Meter bar!"
End Sub

Private Sub pMushroom_Click()
  kDlgBox "Mushroom: Target is turned into a fungus. They restore HP gradually.", 64, "Explanation"
End Sub

Private Sub pMute_Click()
  kDlgBox "Mute: Cannot do any attacks other than physical attacks.", 64, "Explanation"
End Sub

Private Sub pPoison_Click()
  kDlgBox "Poisoned: Gradually lose HP until effect wears off.", 64, "Explanation"
End Sub

Private Sub pQuick_Click()
  kDlgBox "Quick: Next non-morph non-life move hits instantly.", 64, "Explanation"
End Sub

Private Sub pRed_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "This player has the Red Flag."
End Sub

Private Sub pRegen_Click()
  kDlgBox "Regen: Gradually restores HP.", 64, "Explanation"
End Sub

Private Sub pReRaise_Click()
  kDlgBox "ReRaise: Once HP reaches zero, they return to life with half normal HP.", 64, "Explanation"
End Sub

Private Sub pSleep_Click()
  kDlgBox "Asleep: Cannot act. A Physical Attack can wake them up.", 64, "Explanation"
End Sub

Private Sub pSlow_Click()
  kDlgBox "Slow: Moves take 20 seconds to hit instead of 15.", 64, "Explanation"
End Sub

Private Sub pSP_MouseMove(index As Integer, Button As Integer, Shift As Integer, x As Single, Y As Single)
  StatLine = "Neato torpedo SP bar!"
End Sub

Private Sub pStatus_Click(index As Integer)
Dim Q$, Msg$
  If Godmode Then
    Select Case index
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
    End Select
    Q$ = kDlgBoxInput("Enter new value for status " + Msg, P(Peepz.ListIndex + 1).ScrNam, TrimStr(P(Peepz.ListIndex + 1).Status(index)))
    If Q$ <> "" Then P(Peepz.ListIndex + 1).Status(index) = Val(Q$)
  End If
End Sub

Private Sub pStatus_MouseMove(index As Integer, Button As Integer, Shift As Integer, x As Single, Y As Single)
Dim S$
  Select Case index
    Case sMute: S = "Muted. Can only do physical and basic moves."
    Case sChaos: S = "Chaosed. Uncontrollable moves at random people."
    Case sFreeze: S = "Frozen. Cannot move."
    Case sPoison: S = "Poisoned. Steadily lose HP."
    Case sBlind: S = "Blinded. Cannot see, attack goes to zero."
    Case sInvin: S = "Shielded. Evade most attacks, take less damage from Super Moves."
    Case sHaste: S = "Hasted. Less time inbetween attacks."
    Case sMorph: S = "This person is morphed from " + Senshi(P(Peepz.ListIndex + 1).OldCharID).FullName
    Case sSlow: S = "Slowed. More time to wait for an attack."
    Case sStun: S = "Stunned. Cannot move."
    Case sReraise: S = "Life 3. One free resurrection from the dead."
    Case sRegen: S = "Regen. Gradually regain HP."
    Case sStop: S = "Frozen in time. Cannot move or act but will continue with previous action after unfrozen."
    Case sMushroom: S = "Mushroom. Cannot do anything but regain HP at every turn interval."
    Case sMIA: S = "Missing In Action. Player is not present."
    Case sQuick: S = "Quick. Next non-morph non-life move hits instantly."
    Case sBerserk: S = "Berserked. Strength up, random attacks at random enemies."
    Case sSleep: S = "Sleeping. A good hit will wake them up."
    Case sVirus: S = "Virused. Contagious version of poison, does more damage and random effects."
    Case sBarrier: S = "Barrier. Take less damage from Physical attacks."
    Case sMBarrier: S = "MBarrier. Take less damage from Magical attacks."
    Case sBless: S = "Bless. Damage done by attacks increased."
    Case sCurse: S = "Cursed. Damage done by attacks decreased."
    Case sPMS: S = "PMSed. Effects of berserk + haste"
    Case sZombie: S = "Zombified. Attacks do nil. Harmed by cure and life moves."
    Case sHamedo:
      On Error Resume Next
      S = "Chibot Countering. Person will do " + P(Peepz.ListIndex + 1).Moves(P(Peepz.ListIndex + 1).Target).name + " before being attacked."
      On Error GoTo 0
    Case Else: S = "Unknown Status Effect."
  End Select
  StatLine = S
End Sub

Private Sub pStop_Click()
  kDlgBox "Stop: Literally frozen in time. They are still in action, except their moves don't hit until the effect wears off.", 64, "Explanation"
End Sub

Private Sub pStun_Click()
  kDlgBox "Stun: Cannot move. A physical attack can knock them back to their senses.", 64, "Explanation"
End Sub

Private Sub tHMS_Timer()
Dim Px%, HpS%, MpS%, Sps%, x%
Dim C As Long
Dim HP%, MP%, SP%, Bow&
  Bow = RGB(Rand(50, 255), Rand(50, 255), Rand(50, 255))
  Px = Peepz.ListIndex + 1
  HP = P(Px).HP
  MP = P(Px).Cheese
  SP = P(Px).Super
  LSP4.BackColor = Bow
  'On Error Resume Next
  'HpS = Int(15 * HP / P(Px).MaxHP)
  'Select Case HpS
  '  Case Is <= 5: C = RGB(255, 0, 0)
  '  Case 6 To 10: C = RGB(255, 255, 0)
  '  Case Is >= 11: C = RGB(64, 255, 64)
  'End Select
  'For X = 0 To 19
  ' If HpS - 1 >= X Then
  '    If X > 14 Then
  '      pHP(X).BackColor = Bow
  '    ElseIf pHP(X).BackColor <> C Then
  '      pHP(X).BackColor = C
  '    End If
  '  Else
  '    If pHP(X).BackColor <> Gray Then
  '      pHP(X).BackColor = Gray
  '    End If
  '  End If
  'Next X
  'MpS = Int(15 * MP / 1500)
  'Select Case MpS
  '  Case Is >= 10: C = RGB(255, 0, 0)
  '  Case 5 To 9: C = RGB(255, 255, 0)
  '  Case Is <= 4: C = RGB(64, 255, 64)
  'End Select
  'For X = 0 To 19
  '  If MpS - 1 >= X Then
  '    If X > 9 Then
  '      pMP(X).BackColor = Bow
  '    ElseIf pMP(X).BackColor <> C Then
  '      pMP(X).BackColor = C
  '    End If
  '  Else
  '    If pMP(X).BackColor <> Gray Then
  '      pMP(X).BackColor = Gray
  '    End If
  '  End If
  'Next X
  'Sps = Int(27 * SP / 400)
  'For X = 0 To 26
  '  If Sps - 1 >= X Then
  '    Select Case X
  '      Case 0 To 5: C = RGB(64, 255, 64)
  '      Case 6: C = RGB(128, 255, 128)
  '      Case 7 To 12: C = RGB(255, 255, 0)
  '      Case 13: C = RGB(255, 255, 128)
  '      Case 14 To 19: C = RGB(255, 64, 64)
  '      Case 20: C = RGB(255, 128, 128)
  '      Case 21 To 25: C = RGB(255, 255, 255)
  '    End Select
  '    If Sps >= 26 Then C = Bow
  '    If pSP(X).BackColor <> C Then
  '      pSP(X).BackColor = C
  '    End If
  '  Else
  '    If pSP(X).BackColor <> Gray Then
  '      pSP(X).BackColor = Gray
  '    End If
  '  End If
  'Next X
End Sub

