VERSION 2.00
Begin Form fItemEd 
   BackColor       =   &H00400040&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "ChUB 2000 Item Editor"
   ClientHeight    =   4110
   ClientLeft      =   1155
   ClientTop       =   1935
   ClientWidth     =   6045
   Height          =   4800
   Icon            =   FITEMED.FRX:0000
   Left            =   1095
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   4110
   ScaleWidth      =   6045
   Top             =   1305
   Width           =   6165
   Begin TextBox tR2 
      Height          =   285
      Left            =   5520
      MaxLength       =   4
      TabIndex        =   61
      Top             =   3360
      Visible         =   0   'False
      Width           =   495
   End
   Begin TextBox tR1 
      Height          =   285
      Left            =   5520
      MaxLength       =   4
      TabIndex        =   60
      Top             =   3120
      Visible         =   0   'False
      Width           =   495
   End
   Begin TextBox tCharm 
      Height          =   285
      Left            =   5520
      MaxLength       =   4
      TabIndex        =   59
      Top             =   2880
      Width           =   495
   End
   Begin TextBox tScarecrow 
      Height          =   285
      Left            =   5520
      MaxLength       =   4
      TabIndex        =   58
      Top             =   2640
      Width           =   495
   End
   Begin TextBox tBerserk 
      Height          =   285
      Left            =   5520
      MaxLength       =   4
      TabIndex        =   57
      Top             =   2400
      Width           =   495
   End
   Begin TextBox tQuick 
      Height          =   285
      Left            =   5520
      MaxLength       =   4
      TabIndex        =   56
      Top             =   2160
      Width           =   495
   End
   Begin TextBox tMIA 
      Height          =   285
      Left            =   3960
      MaxLength       =   4
      TabIndex        =   49
      Top             =   3360
      Width           =   495
   End
   Begin TextBox tMushroom 
      Height          =   285
      Left            =   3960
      MaxLength       =   4
      TabIndex        =   48
      Top             =   3120
      Width           =   495
   End
   Begin TextBox tStop 
      Height          =   285
      Left            =   3960
      MaxLength       =   4
      TabIndex        =   47
      Top             =   2880
      Width           =   495
   End
   Begin TextBox tRegen 
      Height          =   285
      Left            =   3960
      MaxLength       =   4
      TabIndex        =   46
      Top             =   2640
      Width           =   495
   End
   Begin TextBox tLife3 
      Height          =   285
      Left            =   3960
      MaxLength       =   4
      TabIndex        =   45
      Top             =   2400
      Width           =   495
   End
   Begin TextBox tStun 
      Height          =   285
      Left            =   3960
      MaxLength       =   4
      TabIndex        =   44
      Top             =   2160
      Width           =   495
   End
   Begin TextBox tSlow 
      Height          =   285
      Left            =   2400
      MaxLength       =   4
      TabIndex        =   37
      Top             =   3360
      Width           =   495
   End
   Begin TextBox tMBarrier 
      Height          =   285
      Left            =   2400
      MaxLength       =   4
      TabIndex        =   36
      Top             =   3120
      Width           =   495
   End
   Begin TextBox tBarrier 
      Height          =   285
      Left            =   2400
      MaxLength       =   4
      TabIndex        =   35
      Top             =   2880
      Width           =   495
   End
   Begin TextBox tCurse 
      Height          =   285
      Left            =   2400
      MaxLength       =   4
      TabIndex        =   34
      Top             =   2640
      Width           =   495
   End
   Begin TextBox tHaste 
      Height          =   285
      Left            =   2400
      MaxLength       =   4
      TabIndex        =   33
      Top             =   2400
      Width           =   495
   End
   Begin TextBox tBless 
      Height          =   285
      Left            =   2400
      MaxLength       =   4
      TabIndex        =   32
      Top             =   2160
      Width           =   495
   End
   Begin TextBox tBlind 
      Height          =   285
      Left            =   840
      MaxLength       =   4
      TabIndex        =   25
      Top             =   3360
      Width           =   495
   End
   Begin TextBox tPoison 
      Height          =   285
      Left            =   840
      MaxLength       =   4
      TabIndex        =   24
      Top             =   3120
      Width           =   495
   End
   Begin TextBox tSleep 
      Height          =   285
      Left            =   840
      MaxLength       =   4
      TabIndex        =   23
      Top             =   2880
      Width           =   495
   End
   Begin TextBox tFreeze 
      Height          =   285
      Left            =   840
      MaxLength       =   4
      TabIndex        =   22
      Top             =   2640
      Width           =   495
   End
   Begin TextBox tChaos 
      Height          =   285
      Left            =   840
      MaxLength       =   4
      TabIndex        =   21
      Top             =   2400
      Width           =   495
   End
   Begin TextBox tMute 
      Height          =   285
      Left            =   840
      MaxLength       =   4
      TabIndex        =   20
      Top             =   2160
      Width           =   495
   End
   Begin Timer Timer1 
      Interval        =   1000
      Left            =   0
      Top             =   480
   End
   Begin CommonDialog cmd 
      CancelError     =   -1  'True
      Filter          =   "Items (*.itm)|*.itm"
      FilterIndex     =   1
      Flags           =   6
      Left            =   0
      Top             =   0
   End
   Begin TextBox tMP 
      Height          =   285
      Left            =   3960
      MaxLength       =   4
      TabIndex        =   12
      Top             =   1320
      Width           =   495
   End
   Begin TextBox tHP 
      Height          =   285
      Left            =   1920
      MaxLength       =   4
      TabIndex        =   10
      Top             =   1320
      Width           =   495
   End
   Begin TextBox tGet 
      Height          =   285
      Left            =   1320
      TabIndex        =   8
      Top             =   960
      Width           =   4695
   End
   Begin TextBox tTeleFrag 
      Height          =   285
      Left            =   1320
      TabIndex        =   5
      Top             =   600
      Width           =   4695
   End
   Begin TextBox tSpawn 
      Height          =   285
      Left            =   1320
      TabIndex        =   3
      Top             =   360
      Width           =   4695
   End
   Begin TextBox tName 
      Height          =   285
      Left            =   1320
      TabIndex        =   1
      Top             =   120
      Width           =   4695
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "R1"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   23
      Left            =   4440
      TabIndex        =   55
      Top             =   3120
      Visible         =   0   'False
      Width           =   975
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Charm"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   22
      Left            =   4440
      TabIndex        =   54
      Top             =   2880
      Width           =   975
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Scarecrow"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   21
      Left            =   4440
      TabIndex        =   53
      Top             =   2640
      Width           =   975
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Berserk"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   20
      Left            =   4440
      TabIndex        =   52
      Top             =   2400
      Width           =   975
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Quick"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   19
      Left            =   4440
      TabIndex        =   51
      Top             =   2160
      Width           =   975
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "R2"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   18
      Left            =   4440
      TabIndex        =   50
      Top             =   3360
      Visible         =   0   'False
      Width           =   975
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "MIA"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   17
      Left            =   2880
      TabIndex        =   43
      Top             =   3360
      Width           =   975
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Mushroom"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   16
      Left            =   2880
      TabIndex        =   42
      Top             =   3120
      Width           =   975
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Stop"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   15
      Left            =   2880
      TabIndex        =   41
      Top             =   2880
      Width           =   975
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Regen"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   14
      Left            =   2880
      TabIndex        =   40
      Top             =   2640
      Width           =   975
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Life3"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   13
      Left            =   2880
      TabIndex        =   39
      Top             =   2400
      Width           =   975
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Stun"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   12
      Left            =   2880
      TabIndex        =   38
      Top             =   2160
      Width           =   975
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Slow"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   11
      Left            =   1440
      TabIndex        =   31
      Top             =   3360
      Width           =   855
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "MBarrier"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   10
      Left            =   1440
      TabIndex        =   30
      Top             =   3120
      Width           =   855
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "PBarrier"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   9
      Left            =   1440
      TabIndex        =   29
      Top             =   2880
      Width           =   855
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Curse"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   8
      Left            =   1440
      TabIndex        =   28
      Top             =   2640
      Width           =   855
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Haste"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   7
      Left            =   1440
      TabIndex        =   27
      Top             =   2400
      Width           =   855
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Bless"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   6
      Left            =   1440
      TabIndex        =   26
      Top             =   2160
      Width           =   855
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Blind"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   5
      Left            =   0
      TabIndex        =   19
      Top             =   3360
      Width           =   735
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Poison"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   4
      Left            =   0
      TabIndex        =   18
      Top             =   3120
      Width           =   735
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Sleep"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   3
      Left            =   0
      TabIndex        =   17
      Top             =   2880
      Width           =   735
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Freeze"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   2
      Left            =   0
      TabIndex        =   16
      Top             =   2640
      Width           =   735
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Chaos"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   1
      Left            =   0
      TabIndex        =   15
      Top             =   2400
      Width           =   735
   End
   Begin Label Label4 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Status Caused (% Chance) (-1 to cancel)"
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
      Top             =   1680
      Width           =   6015
   End
   Begin Label LStat 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Mute"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   0
      Left            =   0
      TabIndex        =   13
      Top             =   2160
      Width           =   735
   End
   Begin Label Stat 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Blah."
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   6
      Top             =   3840
      Width           =   6015
   End
   Begin Label Label7 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Effect on SP:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   2640
      TabIndex        =   11
      Top             =   1320
      Width           =   1215
   End
   Begin Label Label6 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Effect on HP:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   600
      TabIndex        =   9
      Top             =   1320
      Width           =   1215
   End
   Begin Label Label5 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Get String"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   7
      Top             =   960
      Width           =   1215
   End
   Begin Label Label3 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Telefrag"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   4
      Top             =   600
      Width           =   1215
   End
   Begin Label Label2 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Spawn"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   2
      Top             =   360
      Width           =   1215
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Name"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   0
      Top             =   120
      Width           =   1215
   End
   Begin Menu mFile 
      Caption         =   "&File"
      Begin Menu mNew 
         Caption         =   "&New"
         Shortcut        =   ^N
      End
      Begin Menu mDash 
         Caption         =   "-"
      End
      Begin Menu mLoad 
         Caption         =   "&Load..."
         Shortcut        =   ^L
      End
      Begin Menu mSave 
         Caption         =   "&Save"
         Shortcut        =   ^S
      End
      Begin Menu mSaveAs 
         Caption         =   "Save &As..."
         Shortcut        =   ^A
      End
      Begin Menu mDash1 
         Caption         =   "-"
      End
      Begin Menu mExit 
         Caption         =   "E&xit"
         Shortcut        =   ^X
      End
   End
End
Option Explicit

Dim Fil As String
Dim Changed As Integer
                   
Dim Temp2 As Integer

Sub Form_Load ()
  If Command$ <> "" Then
    LoadItem (Command$)
    Fil = Command$
  Else
    Fil = ""
    ClearItem
  End If
  UpdateAll
  Changed = False
End Sub

Sub Form_QueryUnload (Cancel As Integer, UnloadMode As Integer)
  If Changed Then
    Select Case MsgBox("Save changes to " + Fil$ + "?", 35, "ChUB 2000 ItemEdit")
      Case 6:
        mSaveAs_Click
        If (Temp2 <> 0) Then Cancel = 1
      Case 2: Cancel = 1
      Case 7:
    End Select
  End If
End Sub

Sub Form_Unload (Cancel As Integer)
  End
End Sub

Sub LStat_MouseDown (Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = 2 Then
    LStat(23).Visible = True
    LStat(18).Visible = True
    tR1.Visible = True
    tR2.Visible = True
  End If
End Sub

Sub mExit_Click ()
  Unload Me
End Sub

Sub mLoad_Click ()
  If Changed Then
    If MsgBox("Save changes to " + Fil$ + "?", 36, "ChUB 2000 ItemEdit") = 6 Then
      mSaveAs_Click
      If (Temp2 <> 0) Then Exit Sub
    End If
  End If
  cmd.Filename = "*.itm"
  On Error GoTo CancelLoad
    cmd.Action = 1
    Fil = cmd.Filename
    LoadItem (Fil)
    Changed = False
    UpdateAll
  Exit Sub
CancelLoad:
  Exit Sub
End Sub

Sub mNew_Click ()
  If Changed Then
    If MsgBox("Save changes to " + Fil$ + "?", 36, "ChUB 2000 ItemEdit") = 6 Then
      mSaveAs_Click
      If (Temp2 <> 0) Then Exit Sub
    End If
  End If
  ClearItem
  UpdateAll
  Fil = ""
  Changed = False
End Sub

Sub mSave_Click ()
  If (Fil = "") Then
    mSaveAs_Click
  Else
    SaveItem (Fil)
    Changed = 0
    Stat = "Saved item as " + Fil + "."
  End If
End Sub

Sub mSaveAs_Click ()
  cmd.Filename = Fil
  On Error GoTo CancelSave
    cmd.Action = 2
    Fil = cmd.Filename
    SaveItem (Fil)
    Changed = False
    UpdateAll
    Stat = "Saved item as " + Fil + "."
    Temp2 = 0
  Exit Sub
CancelSave:
  Temp2 = 1
  Exit Sub
End Sub

Sub tBarrier_Change ()
  Changed = True
  Item.PlayerStat.Barrier = Val(tBarrier)

End Sub

Sub tBerserk_Change ()
  Changed = True
  Item.PlayerStat.Berserk = Val(tBerserk)

End Sub

Sub tBless_Change ()
  Changed = True
  Item.PlayerStat.Bless = Val(tBless)

End Sub

Sub tBlind_Change ()
  Changed = True
  Item.PlayerStat.Blind = Val(tBlind)

End Sub

Sub tChaos_Change ()
  Changed = True
  Item.PlayerStat.Chaos = Val(tChaos)

End Sub

Sub tCharm_Change ()
  Changed = True
  Item.PlayerStat.Charm = Val(tCharm)

End Sub

Sub tCurse_Change ()
  Changed = True
  Item.PlayerStat.Curse = Val(tCurse)

End Sub

Sub tFreeze_Change ()
  Changed = True
  Item.PlayerStat.Freeze = Val(tFreeze)

End Sub

Sub tGet_Change ()
  Changed = True
  Item.PlayerGet = tGet
End Sub

Sub tHaste_Change ()
  Changed = True
  Item.PlayerStat.Haste = Val(tHaste)

End Sub

Sub tHP_Change ()
  Changed = True
  Item.PlayerHP = Val(tHP)
End Sub

Sub Timer1_Timer ()
  If Timer1.Interval = 10000 Then
    Stat = ""
    Timer1.Interval = 1000
  End If
  If Stat <> "" Then
    Timer1.Interval = 10000
  End If
End Sub

Sub tLife3_Change ()
  Changed = True
  Item.PlayerStat.Life3 = Val(tLife3)

End Sub

Sub tMBarrier_Change ()
  Changed = True
  Item.PlayerStat.MBarrier = Val(tMBarrier)

End Sub

Sub tMIA_Change ()
  Changed = True
  Item.PlayerStat.MIA = Val(tMIA)

End Sub

Sub tMP_Change ()
  Changed = True
  Item.PlayerMP = Val(tMP)
End Sub

Sub tMushroom_Change ()
  Changed = True
  Item.PlayerStat.Mushroom = Val(tMushroom)

End Sub

Sub tMute_Change ()
  Changed = True
  Item.PlayerStat.Mute = Val(tMute)
End Sub

Sub tName_Change ()
  Changed = True
  Item.Name = tName
End Sub

Sub tPoison_Change ()
  Changed = True
  Item.PlayerStat.Poison = Val(tPoison)

End Sub

Sub tQuick_Change ()
  Changed = True
  Item.PlayerStat.Quick = Val(tQuick)

End Sub

Sub tR1_Change ()
  Changed = True
  Item.PlayerStat.R1 = Val(tR1)

End Sub

Sub tR2_Change ()
  Changed = True
  Item.PlayerStat.R2 = Val(tR2)

End Sub

Sub tRegen_Change ()
  Changed = True
  Item.PlayerStat.Regen = Val(tRegen)

End Sub

Sub tScarecrow_Change ()
  Changed = True
  Item.PlayerStat.Scarecrow = Val(tScarecrow)

End Sub

Sub tSleep_Change ()
  Changed = True
  Item.PlayerStat.Sleep = Val(tSleep)

End Sub

Sub tSlow_Change ()
  Changed = True
  Item.PlayerStat.Slow = Val(tSlow)

End Sub

Sub tSpawn_Change ()
  Changed = True
  Item.Spawn = tSpawn
End Sub

Sub tStop_Change ()
  Changed = True
  Item.PlayerStat.Stop = Val(tStop)

End Sub

Sub tStun_Change ()
  Changed = True
  Item.PlayerStat.Stun = Val(tStun)

End Sub

Sub tTeleFrag_Change ()
  Changed = True
  Item.Telefrag = tTelefrag
End Sub

Sub UpdateAll ()
Dim M As MStatusType
Dim s As String
Dim HP As Integer
Dim MP As Integer
Dim t%
  t% = Changed
  tName = Item.Name
  tSpawn = Item.Spawn
  tTelefrag = Item.Telefrag
  s = Item.PlayerGet
  HP = Item.PlayerHP
  MP = Item.PlayerMP
  M = Item.PlayerStat
  tGet = s
  tHP = TrimStr(HP)
  tMP = TrimStr(MP)
  tMute = TrimStr(M.Mute)
  tChaos = TrimStr(M.Chaos)
  tFreeze = TrimStr(M.Freeze)
  tSleep = TrimStr(M.Sleep)
  tPoison = TrimStr(M.Poison)
  tBlind = TrimStr(M.Blind)
  tCurse = TrimStr(M.Curse)
  tSlow = TrimStr(M.Slow)
  tStun = TrimStr(M.Stun)
  tBarrier = TrimStr(M.Barrier)
  tBless = TrimStr(M.Bless)
  tHaste = TrimStr(M.Haste)
  tStop = TrimStr(M.Stop)
  tRegen = TrimStr(M.Regen)
  tMBarrier = TrimStr(M.MBarrier)
  tQuick = TrimStr(M.Quick)
  tScarecrow = TrimStr(M.Scarecrow)
  tCharm = TrimStr(M.Charm)
  tR1 = TrimStr(M.R1)
  tR2 = TrimStr(M.R2)
  tMushroom = TrimStr(M.Mushroom)
  tMIA = TrimStr(M.MIA)
  tBerserk = TrimStr(M.Berserk)
  tLife3 = TrimStr(M.Life3)
  Changed = t%
End Sub

