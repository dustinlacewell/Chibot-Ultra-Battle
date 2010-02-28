VERSION 2.00
Begin Form fItemEd 
   BackColor       =   &H00400040&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "ChUB 2000 Item Editor"
   ClientHeight    =   3810
   ClientLeft      =   1095
   ClientTop       =   1770
   ClientWidth     =   5625
   Height          =   4500
   Icon            =   FITEMED.FRX:0000
   Left            =   1035
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   3810
   ScaleWidth      =   5625
   Top             =   1140
   Width           =   5745
   Begin Timer Timer1 
      Interval        =   1000
      Left            =   0
      Top             =   480
   End
   Begin CheckBox Check6 
      BackColor       =   &H00400040&
      Caption         =   "Berserk"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H0080FF80&
      Height          =   255
      Left            =   4320
      TabIndex        =   25
      Top             =   3240
      Width           =   1215
   End
   Begin CheckBox Check5 
      BackColor       =   &H00400040&
      Caption         =   "M.I.A."
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H0080FF80&
      Height          =   255
      Left            =   4320
      TabIndex        =   32
      Top             =   3000
      Width           =   1215
   End
   Begin CheckBox Check4 
      BackColor       =   &H00400040&
      Caption         =   "Mushroom"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H0080FF80&
      Height          =   255
      Left            =   4320
      TabIndex        =   31
      Top             =   2760
      Width           =   1215
   End
   Begin CheckBox cRegen 
      BackColor       =   &H00400040&
      Caption         =   "Regen"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Left            =   4320
      TabIndex        =   29
      Top             =   2520
      Width           =   1215
   End
   Begin CheckBox cQuick 
      BackColor       =   &H00400040&
      Caption         =   "Quick"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Left            =   4320
      TabIndex        =   30
      Top             =   2280
      Width           =   1215
   End
   Begin CheckBox cLife3 
      BackColor       =   &H00400040&
      Caption         =   "Adds Life3"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Left            =   2760
      TabIndex        =   28
      Top             =   3240
      Width           =   1215
   End
   Begin CheckBox cHaste 
      BackColor       =   &H00400040&
      Caption         =   "Haste"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Left            =   2760
      TabIndex        =   18
      Top             =   3000
      Width           =   1215
   End
   Begin CheckBox cAttUp 
      BackColor       =   &H00400040&
      Caption         =   "Blessing"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Left            =   2760
      TabIndex        =   19
      Top             =   2760
      Width           =   1215
   End
   Begin CheckBox cMBarrier 
      BackColor       =   &H00400040&
      Caption         =   "Mag. Barrier"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Left            =   2760
      TabIndex        =   27
      Top             =   2520
      Width           =   1215
   End
   Begin CheckBox cDefUp 
      BackColor       =   &H00400040&
      Caption         =   "Phys. Barrier"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FF8080&
      Height          =   255
      Left            =   2760
      TabIndex        =   20
      Top             =   2280
      Width           =   1215
   End
   Begin CheckBox cBlind 
      BackColor       =   &H00400040&
      Caption         =   "Blinds"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H008080FF&
      Height          =   255
      Left            =   1440
      TabIndex        =   21
      Top             =   3240
      Width           =   1215
   End
   Begin CheckBox cStop 
      BackColor       =   &H00400040&
      Caption         =   "Time Freeze"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H008080FF&
      Height          =   255
      Left            =   1440
      TabIndex        =   26
      Top             =   3000
      Width           =   1215
   End
   Begin CheckBox cStun 
      BackColor       =   &H00400040&
      Caption         =   "Stuns"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H008080FF&
      Height          =   255
      Left            =   1440
      TabIndex        =   15
      Top             =   2760
      Width           =   1215
   End
   Begin CheckBox cSlow 
      BackColor       =   &H00400040&
      Caption         =   "Slow"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H008080FF&
      Height          =   255
      Left            =   1440
      TabIndex        =   16
      Top             =   2520
      Width           =   1215
   End
   Begin CheckBox cWeak 
      BackColor       =   &H00400040&
      Caption         =   "Adds a Curse"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H008080FF&
      Height          =   255
      Left            =   1440
      TabIndex        =   17
      Top             =   2280
      Width           =   1215
   End
   Begin CheckBox cSleep 
      BackColor       =   &H00400040&
      Caption         =   "Sleep"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H008080FF&
      Height          =   255
      Left            =   240
      TabIndex        =   23
      Top             =   3000
      Width           =   1215
   End
   Begin CheckBox cFreeze 
      BackColor       =   &H00400040&
      Caption         =   "Freezes"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H008080FF&
      Height          =   255
      Left            =   240
      TabIndex        =   24
      Top             =   2760
      Width           =   1215
   End
   Begin CheckBox cChaos 
      BackColor       =   &H00400040&
      Caption         =   "Chaoses"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H008080FF&
      Height          =   255
      Left            =   240
      TabIndex        =   14
      Top             =   2520
      Width           =   1215
   End
   Begin CheckBox cMute 
      BackColor       =   &H00400040&
      Caption         =   "Mutes"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H008080FF&
      Height          =   255
      Left            =   240
      TabIndex        =   13
      Top             =   2280
      Width           =   1215
   End
   Begin CommonDialog cmd 
      CancelError     =   -1  'True
      Filter          =   "Items (*.itm)|*.itm"
      FilterIndex     =   1
      Flags           =   6
      Left            =   0
      Top             =   0
   End
   Begin CheckBox cPoison 
      BackColor       =   &H00400040&
      Caption         =   "Poisons"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H008080FF&
      Height          =   255
      Left            =   240
      TabIndex        =   22
      Top             =   3240
      Width           =   1215
   End
   Begin TextBox tMP 
      Height          =   285
      Left            =   3960
      MaxLength       =   4
      TabIndex        =   12
      Top             =   1560
      Width           =   495
   End
   Begin TextBox tHP 
      Height          =   285
      Left            =   1920
      MaxLength       =   4
      TabIndex        =   10
      Top             =   1560
      Width           =   495
   End
   Begin TextBox tGet 
      Height          =   285
      Left            =   1320
      TabIndex        =   8
      Top             =   960
      Width           =   4215
   End
   Begin TextBox tTeleFrag 
      Height          =   285
      Left            =   1320
      TabIndex        =   5
      Top             =   600
      Width           =   4215
   End
   Begin TextBox tSpawn 
      Height          =   285
      Left            =   1320
      TabIndex        =   3
      Top             =   360
      Width           =   4215
   End
   Begin TextBox tName 
      Height          =   285
      Left            =   1320
      TabIndex        =   1
      Top             =   120
      Width           =   4215
   End
   Begin Label Stat 
      BackStyle       =   0  'Transparent
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   240
      TabIndex        =   6
      Top             =   3480
      Width           =   5175
   End
   Begin Label Label7 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Effect on MP:"
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
      Top             =   1560
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
      Top             =   1560
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

Sub cAttUp_Click ()
  Changed = True
  Item.PlayerStat.Bless = cAttUp * 100
End Sub

Sub cBlind_Click ()
  Changed = True
  Item.PlayerStat.Blind = cBlind * 100
End Sub

Sub cChaos_Click ()
  Changed = True
  Item.PlayerStat.Chaos = cChaos * 100
End Sub

Sub cDefUp_Click ()
  Changed = True
  Item.PlayerStat.Barrier = cDefUp * 100
End Sub

Sub cFreeze_Click ()
  Changed = True
  Item.PlayerStat.Freeze = cFreeze * 100
End Sub

Sub cHaste_Click ()
  Changed = True
  Item.PlayerStat.Haste = cHaste * 100
End Sub

Sub Check4_Click ()
  Changed = True
  Item.PlayerStat.Mushroom = Check4 * 100
End Sub

Sub Check5_Click ()
  Changed = True
  Item.PlayerStat.Mia = Check5 * 100
End Sub

Sub Check6_Click ()
  Changed = True
  Item.PlayerStat.Berserk = Check6 * 100
End Sub

Sub cLife3_Click ()
  Changed = True
  Item.PlayerStat.Life3 = cLife3 * 100
End Sub

Sub cMBarrier_Click ()
  Changed = True
  Item.PlayerStat.MBarrier = cMBarrier * 100
End Sub

Sub cMute_Click ()
  Changed = True
  Item.PlayerStat.Mute = cMute * 100
End Sub

Sub cPoison_Click ()
  Changed = True
  Item.PlayerStat.Poison = cPoison * 100
End Sub

Sub cQuick_Click ()
  Changed = True
  Item.PlayerStat.Quick = cQuick * 100
End Sub

Sub cRegen_Click ()
  Changed = True
  Item.PlayerStat.Regen = cRegen * 100
End Sub

Sub cSleep_Click ()
  Changed = True
  Item.PlayerStat.Sleep = cSleep * 100
End Sub

Sub cSlow_Click ()
  Changed = True
  Item.PlayerStat.Slow = cSlow * 100
End Sub

Sub cStop_Click ()
  Changed = True
  Item.PlayerStat.Stop = cStop * 100
End Sub

Sub cStun_Click ()
  Changed = True
  Item.PlayerStat.Stun = cStun * 100
End Sub

Sub cWeak_Click ()
  Changed = True
  Item.PlayerStat.Curse = cWeak * 100
End Sub

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

Sub tGet_Change ()
  Changed = True
  Item.PlayerGet = tGet
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

Sub tMP_Change ()
  Changed = True
  Item.PlayerMP = Val(tMP)
End Sub

Sub tName_Change ()
  Changed = True
  Item.Name = tName
End Sub

Sub tSpawn_Change ()
  Changed = True
  Item.Spawn = tSpawn
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
Dim T%
  T% = Changed
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
  cMute = -(M.Mute <> 0)
  cChaos = -(M.Chaos <> 0)
  cFreeze = -(M.Freeze <> 0)
  cSleep = -(M.Sleep <> 0)
  cPoison = -(M.Poison <> 0)
  cBlind = -(M.Blind <> 0)
  cWeak = -(M.Curse <> 0)
  cSlow = -(M.Slow <> 0)
  cStun = -(M.Stun <> 0)
  cDefUp = -(M.Barrier <> 0)
  cAttUp = -(M.Bless <> 0)
  cHaste = -(M.Haste <> 0)
  cStop = -(M.Stop <> 0)
  cRegen = -(M.Regen <> 0)
  cMBarrier = -(M.MBarrier <> 0)
  cQuick = -(M.Quick <> 0)
  Check4 = -(M.Mushroom <> 0)
  Check5 = -(M.Mia <> 0)
  Check6 = -(M.Berserk <> 0)
  Changed = T%
End Sub

