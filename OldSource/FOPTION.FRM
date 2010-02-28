VERSION 2.00
Begin Form fOption 
   BackColor       =   &H00400000&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Battle Settings"
   ClientHeight    =   3345
   ClientLeft      =   855
   ClientTop       =   1500
   ClientWidth     =   5040
   ControlBox      =   0   'False
   ForeColor       =   &H00000000&
   Height          =   3750
   Icon            =   FOPTION.FRX:0000
   Left            =   795
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   3345
   ScaleWidth      =   5040
   Top             =   1155
   Width           =   5160
   Begin CheckBox cOldSchool 
      BackColor       =   &H00400000&
      Caption         =   "OldSchool Kombat"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   36
      Top             =   720
      Width           =   1695
   End
   Begin CheckBox cMulti 
      BackColor       =   &H00400000&
      Caption         =   "Multitarget Moves"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   35
      Top             =   1680
      Width           =   1695
   End
   Begin TextBox tFragLimit 
      Height          =   285
      Left            =   3120
      TabIndex        =   34
      Text            =   "0"
      Top             =   1800
      Width           =   615
   End
   Begin CheckBox cFours 
      BackColor       =   &H00400000&
      Caption         =   "Extended Supers"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   13
      Top             =   1920
      Width           =   1575
   End
   Begin CheckBox cFlag 
      BackColor       =   &H00400000&
      Caption         =   "Capture the Flag"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   14
      Top             =   960
      Width           =   1575
   End
   Begin CheckBox cRespawn 
      BackColor       =   &H00400000&
      Caption         =   "Respawn"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   32
      Top             =   1200
      Width           =   1575
   End
   Begin CheckBox cDefect 
      BackColor       =   &H00400000&
      Caption         =   "Defects"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   5
      Top             =   1440
      Width           =   1575
   End
   Begin CheckBox cSameChar 
      BackColor       =   &H00400000&
      Caption         =   "Pick Same Char."
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   1
      Top             =   480
      Width           =   1575
   End
   Begin CheckBox Check1 
      BackColor       =   &H00400000&
      Caption         =   "Zombie Mode"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   18
      Top             =   240
      Width           =   1575
   End
   Begin TextBox tSP 
      Height          =   285
      Left            =   4560
      TabIndex        =   31
      Text            =   "1000"
      Top             =   1320
      Width           =   495
   End
   Begin TextBox tHP 
      Height          =   285
      Left            =   4560
      TabIndex        =   30
      Text            =   "600"
      Top             =   1080
      Width           =   495
   End
   Begin TextBox tDmg 
      Height          =   315
      Left            =   3120
      TabIndex        =   25
      Text            =   "100"
      Top             =   1080
      Width           =   495
   End
   Begin TextBox tFlaCon 
      Height          =   285
      Left            =   3120
      TabIndex        =   23
      Text            =   "60"
      Top             =   1440
      Width           =   615
   End
   Begin CommandButton cLog 
      Caption         =   "Log File"
      Height          =   255
      Left            =   3840
      TabIndex        =   21
      Top             =   3480
      Visible         =   0   'False
      Width           =   1215
   End
   Begin ComboBox cUnit 
      Height          =   315
      Left            =   3720
      Style           =   2  'Dropdown List
      TabIndex        =   9
      Top             =   720
      Width           =   1215
   End
   Begin TextBox tbLimit 
      Height          =   315
      Left            =   3120
      TabIndex        =   8
      Text            =   "0"
      Top             =   720
      Width           =   615
   End
   Begin ComboBox tFontName 
      Height          =   315
      Left            =   2040
      Sorted          =   -1  'True
      TabIndex        =   20
      Text            =   "Arial"
      Top             =   2160
      Width           =   2055
   End
   Begin CommandButton cPickColor 
      Caption         =   "Pick Color..."
      Height          =   255
      Left            =   2040
      TabIndex        =   19
      Top             =   2760
      Width           =   1215
   End
   Begin CommonDialog cmd 
      CancelError     =   -1  'True
      Flags           =   11
      FontName        =   "Arial"
      FontSize        =   12
      Left            =   4320
      Max             =   12
      Min             =   12
      Top             =   0
   End
   Begin CommandButton bShow 
      Caption         =   "Show &Options"
      Height          =   255
      Left            =   0
      TabIndex        =   3
      Top             =   2760
      Width           =   1815
   End
   Begin CommandButton cArena 
      Caption         =   "Pick Arena..."
      Height          =   255
      Left            =   0
      TabIndex        =   2
      Top             =   2520
      Width           =   1815
   End
   Begin CheckBox cNoJoin 
      BackColor       =   &H00400000&
      Caption         =   "Disable Joining"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   1575
   End
   Begin TextBox tReason 
      Height          =   285
      Left            =   2520
      TabIndex        =   16
      Text            =   "none"
      Top             =   0
      Width           =   1695
   End
   Begin CommandButton cTest 
      Caption         =   "Test"
      Height          =   495
      Left            =   3360
      TabIndex        =   15
      Top             =   2520
      Width           =   735
   End
   Begin TextBox tFontColor 
      Height          =   285
      Left            =   2040
      TabIndex        =   12
      Top             =   2520
      Width           =   1215
   End
   Begin TextBox tGetRate 
      Height          =   285
      Left            =   3120
      TabIndex        =   11
      Text            =   "0"
      Top             =   360
      Width           =   615
   End
   Begin CommandButton bDone 
      Caption         =   "&Done"
      Default         =   -1  'True
      Height          =   495
      Left            =   4200
      TabIndex        =   4
      Top             =   2520
      Width           =   735
   End
   Begin Label StatLine 
      Alignment       =   2  'Center
      BackColor       =   &H00C0C0C0&
      Caption         =   "Status Bar."
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial Narrow"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   0
      TabIndex        =   28
      Top             =   3120
      Width           =   5055
   End
   Begin Label Label3 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Frags To Win:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1920
      TabIndex        =   33
      Top             =   1800
      Width           =   1095
   End
   Begin Label LArenaName 
      Alignment       =   2  'Center
      BackColor       =   &H00400000&
      Caption         =   "Boring"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   6
      Top             =   2280
      Width           =   1815
   End
   Begin Label Label11 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Max SP"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3840
      TabIndex        =   29
      Top             =   1320
      Width           =   615
   End
   Begin Label Label9 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Max HP"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3840
      TabIndex        =   27
      Top             =   1080
      Width           =   615
   End
   Begin Label Label8 
      BackStyle       =   0  'Transparent
      Caption         =   "%"
      FontBold        =   -1  'True
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   12
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3600
      TabIndex        =   26
      Top             =   1080
      Width           =   255
   End
   Begin Label Label7 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Dmg. Multiplier"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1920
      TabIndex        =   24
      Top             =   1080
      Width           =   1095
   End
   Begin Label Label6 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Flag Time Limit (in seconds):"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   375
      Left            =   1920
      TabIndex        =   22
      Top             =   1320
      Width           =   1095
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Time Limit:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   12
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1800
      TabIndex        =   7
      Top             =   720
      Width           =   1215
   End
   Begin Label Label5 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Reason:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   1800
      TabIndex        =   17
      Top             =   0
      Width           =   615
   End
   Begin Label Label2 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Item Rate:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   12
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   375
      Left            =   1800
      TabIndex        =   10
      Top             =   360
      Width           =   1215
   End
End

' FOPTION.FRM
' Happy-Fun-Options Menu!!!

Const MyName = "fOption"

Option Explicit

Sub bDone_Click ()
Dim P1$, P2$, P4$, PO%
  Me.Hide
  P1$ = "Millenium"
  P2$ = "Join"
  If Config.NoJoin = 0 Then
    P4$ = "1"
  Else
    P4$ = "0"
  End If
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "SameChar"
  If Config.SameChar <> 0 Then
    P4$ = "1"
  Else
    P4$ = "0"
  End If
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  'P2$ = "InfMP"
  'If Config.InfMP <> 0 Then
  '  P4$ = "1"
  'Else
  '  P4$ = "0"
  'End If
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "Flag"
  If Config.Flag <> 0 Then
    P4$ = "1"
  Else
    P4$ = "0"
  End If
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "Fours"
  If Config.Fours <> 0 Then
    P4$ = "1"
  Else
    P4$ = "0"
  End If
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "Multitarget"
  If Config.Multi <> 0 Then
    P4$ = "1"
  Else
    P4$ = "0"
  End If
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "Respawn"
  If Config.Respawn <> 0 Then
    P4$ = "1"
  Else
    P4$ = "0"
  End If
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "Defect"
  If Config.Defect <> 0 Then
    P4$ = "1"
  Else
    P4$ = "0"
  End If
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "Zombie"
  If ZombieM = 1 Then
    P4$ = "1"
  ElseIf ZombieM = 0 Then
    P4$ = "0"
  End If
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "OldSchool"
  If Config.OldSchool = 1 Then
    P4$ = "1"
  ElseIf Config.OldSchool = 0 Then
    P4$ = "0"
  End If
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "FontName"
  P4$ = Config.FontName
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "FontColor"
  P4$ = Config.FontColor
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "WhyNoJoin"
  P4$ = Config.Reason
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "FlaCon"
  P4$ = tFlaCon.Text
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "DMult"
  P4$ = tDmg.Text
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "MaxHP"
  P4$ = tHP.Text
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  'P2$ = "MaxMP"
  'P4$ = tMP.Text
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "MaxSP"
  P4$ = tSP.Text
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "GetRate"
  P4$ = tGetRate.Text
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  P2$ = "FragLimit"
  P4$ = tFragLimit.Text
  PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  BalancingAct
End Sub

Sub bDone_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Save changes and exit"
End Sub

Sub bShow_Click ()
Dim XX%, XX5%
Dim Msg As String
  Send ("Chibot Phude Settings:")
  Send ("Version: [" + VerID + "]")
  If Config.NoJoin Then
    Send ("Join: [Nobody else may join]")
  Else
    Send ("Join: [Anyone may join until the game is full]")
  End If
  If Config.SameChar Then
    Send ("Pick Same Character: [Dupes of same character allowed]")
  Else
    Send ("Pick Same Character: [No two people may be the same character]")
  End If
  If Config.OldSchool <> 0 Then Msg = "Battle: OldSchool " Else Msg = "Battle: Enhanced "
  If Config.Flag = 0 Then
    If Config.Respawn <> 0 Then Msg = Msg + "Respawn "
    If Config.Defect <> 0 Then Msg = Msg + "with Defects "
    If FragLimit < 0 Then
      Msg = Msg + " [" + TrimStr(Abs(FragLimit)) + " Fatalities to win]"
    ElseIf FragLimit > 0 Then
      Msg = Msg + " [" + TrimStr(FragLimit) + " Frags to Win]"
    Else
      Msg = Msg + " [No Frag Limit]"
    End If
  Else
    Msg = Msg + "Capture the Flag"
  End If
  Send (Msg)
  'If Config.InfMP Then
  '  Send ("Infinite MP: [All moves are free!]")
  'Else
  '  Send ("Infinite MP: [NO]")
  'End If
  Send ("Max HP: [" + TrimStr(MaxHP) + "] Max SP: [" + TrimStr(MaxSP) + "]")
  Send ("Damage Multiplier: [" + TrimStr(DMult) + "%]")
  If (Config.Fours = 0) Then
    Send ("Level 4 and 5 Supers: [DISABLED]")
  Else
    Send ("Level 4 and 5 Supers: [ENABLED]")
  End If
  If (ZombieM = 0) Then
    Send ("First to Die: [Dies]")
  Else
    Send ("First to Die: [Becomes a Zombie]")
  End If
  If (GetRate = 0) Then
    Send ("Item Spawn Rate: [Items Disabled]")
  Else
    Send ("Item Spawn Rate: [" + TrimStr(GetRate) + "]")
  End If
  If TLimit = 0 Then
    Send ("Time Limit: [None]")
  Else
    Msg = "Time Limit: ["
    XX = TLimit
    If (TLimit > 60) Then
      If (TLimit > 60 * 60) Then
        XX5% = Int(XX / 60 / 60)
        Msg = Msg + TrimStr(XX5%) + " hours "
        XX = XX - (XX5% * 60 * 60)
      End If
      XX5% = Int(XX / 60)
      Msg = Msg + TrimStr(XX5%) + " minutes "
      XX = XX - (XX5% * 60)
    End If
    Msg = Msg + TrimStr(XX) + " seconds]"
    Send (Msg)
  End If
  ShowArena
End Sub

Sub bShow_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shows options to chatroom"
End Sub

Sub cArena_Click ()
  fArena.Show
End Sub

Sub cArena_MouseDown (Button As Integer, Shift As Integer, X As Single, Y As Single)
Static T%
  If Button = 2 Then T = T + 1
  If T = 10 Then
    Send ("Langolier arena unlocked.")
    MaxArena = MaxArena + 1
    ReDim Preserve Arena(MaxArena)
    Arena(MaxArena) = Langolier
    Lang = MaxArena
  End If
End Sub

Sub cArena_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Change Arena"
End Sub

Sub cDefect_Click ()
  Config.Defect = cDefect.Value
End Sub

Sub cDefect_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Allows team changes"
End Sub

Sub cFlag_Click ()
  Config.Flag = cFlag.Value
  If (Config.Flag = 1) Then
    Config.Respawn = 1
    cRespawn.Value = 1
    cRespawn.Enabled = False
    Config.Defect = 0
    cDefect.Value = 0
    cDefect.Enabled = False
  Else
    cRespawn.Enabled = True
    cDefect.Enabled = True
  End If
End Sub

Sub cFlag_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Enables Capture the Flag battle"
End Sub

Sub cFours_Click ()
  Config.Fours = cFours.Value
End Sub

Sub cFours_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Enables Level 4 and 5 Supers"
End Sub

Sub Check1_Click ()
  ZombieM = Check1
End Sub

Sub Check1_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "First person to die becomes Zombie"
End Sub

Sub cLog_Click ()
  If Trim(Config.WLog) = "" Then
    Config.WLog = kDlgBoxInput("Enter log filename:", "Battle Log", "chibat.txt")
    LogFileOpen (Config.WLog)
  Else
    LogFileClose
    kDlgBox "Log file closed.", 64, "ChUB 2000"
    Config.WLog = ""
  End If
End Sub

Sub cMulti_Click ()
  Config.Multi = cMulti.Value
End Sub

Sub cMulti_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Allows moves that hit more than one player"
End Sub

Sub cNoJoin_Click ()
  Config.NoJoin = cNoJoin.Value
End Sub

Sub cNoJoin_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Allows joining during battle"
End Sub

Sub cOldSchool_Click ()
  Config.OldSchool = cOldSchool.Value
  Select Case Config.OldSchool
    Case 0: Send ("OldSchool Kombat mode disabled!")
    Case 1: Send ("OldSchool Kombat mode enabled! (No Runes or Weapons)")
  End Select
End Sub

Sub cOldSchool_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "OldSchool Kombat. No Runes or Weapons."
End Sub

Sub cPickColor_Click ()
On Error GoTo NoWayC
  cmd.Action = 3
  Config.FontColor = ToHTML(Hex$(cmd.Color))
  tFontColor = ToHTML(Hex$(cmd.Color))
  'If YourSN = "Player" Then
  '  fOffline!tDebug.FontName = Config.FontName
  '  fOffline!tDebug.ForeColor = Val("&H" + ToHTML(Config.FontColor))
  '  fOffline!tDebug.BackColor = 16777215 Xor Me.BackColor
  'End If
NoWayC:
  Exit Sub
End Sub

Sub cPickColor_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Pick font color from color wheel"
End Sub

Sub cPickFont_Click ()
  On Error GoTo NoWayF
  cmd.FontName = Trim(Config.FontName)
  cmd.FontSize = 12
  cmd.Action = 4
  If cmd.FontBold Or cmd.FontItalic Or cmd.FontUnderLine Then
    kDlgBox "Note that you cannot set the bold, italic, or underline attributes for the output font.", 64, "Chibot Font Settings"
    cmd.FontBold = False
    cmd.FontItalic = False
    cmd.FontUnderLine = False
  End If
  Config.FontName = cmd.FontName
  tFontName = cmd.FontName
NoWayF:
  Exit Sub
End Sub

Sub cRespawn_Click ()
  Config.Respawn = cRespawn.Value
End Sub

Sub cRespawn_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Come back to life after 30 game seconds"
End Sub

Sub cSameChar_Click ()
  Config.SameChar = cSameChar.Value
End Sub

Sub cSameChar_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Allows more than one person to pick the same char"
End Sub

Sub cTest_Click ()
  ScrollSend ("This is font " + Trim(Config.FontName) + " and color " + Trim(Config.FontColor) + ".")
End Sub

Sub cTest_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Test current font"
End Sub

Sub cUnit_Change ()
  tbLimit_Change
End Sub

Sub Form_Load ()
Dim X%
  On Error Resume Next
  LoadPosition Me, MyName
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3             ' Always on top
  LArenaName.Caption = Arena(CurArena).Name
  cNoJoin.Value = Config.NoJoin
  cSameChar.Value = Config.SameChar
  cDefect.Value = Config.Defect
  'Config.InfMP = 1
  Select Case ZombieM
    Case 1, 2: Check1 = 1
    Case 0: Check1 = 0
  End Select
  tbLimit.Text = TrimStr(TLimit)
  cUnit.AddItem "seconds"
  cUnit.AddItem "minutes"
  cUnit.AddItem "hours???"
  cUnit.ListIndex = 0
  tGetRate.Text = TrimStr(GetRate)
  tReason.Text = Config.Reason
  tFontColor = Config.FontColor
  cmd.Color = Val("&H" + ToHTML(Config.FontColor))
  cmd.FontName = Config.FontName
  For X% = 1 To Screen.FontCount
    tFontName.AddItem Screen.Fonts(X)
  Next X%
  tFontName.Text = Config.FontName
  Config.FontColor = tFontColor
  tFlaCon.Text = TrimStr(Config.FlaCon)
  tHP.Text = TrimStr(MaxHP)
  tSP.Text = TrimStr(MaxSP)
  cFlag = Config.Flag
  cRespawn = Config.Respawn
  cFours = Config.Fours
  tFragLimit.Text = TrimStr(FragLimit)
  tDmg.Text = TrimStr(DMult)
  cMulti = Config.Multi
  cOldSchool = Config.OldSchool
End Sub

Sub Form_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Battle Settings"
End Sub

Sub Form_Unload (Cancel As Integer)
  SavePosition Me, MyName

End Sub

Sub LArenaName_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Current Arena"
End Sub

Sub tbLimit_Change ()
  Select Case cUnit.ListIndex
    Case 0: TLimit = Val(tbLimit)
    Case 1: TLimit = Val(tbLimit) * 60
    Case 2: TLimit = Val(tbLimit) * 60 * 60
  End Select
End Sub

Sub tbLimit_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Change time limit for match"
End Sub

Sub tDmg_Change ()
  DMult = Val(tDmg)
End Sub

Sub tDmg_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Changes amount of damage done. 200=Double 50=Half"
End Sub

Sub tFlaCon_Change ()
  Config.FlaCon = Val(tFlaCon.Text)
End Sub

Sub tFlaCon_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Keep enemy flag for this long in Capture the Flag to win"
End Sub

Sub tFontColor_Change ()
  Config.FontColor = tFontColor
  'Me.BackColor = cmd.Color
  'Label1.ForeColor = 16777215 Xor Me.BackColor
  'Label2.ForeColor = 16777215 Xor Me.BackColor
  'Label3.ForeColor = 16777215 Xor Me.BackColor
  'Label4.ForeColor = 16777215 Xor Me.BackColor
  'Label5.ForeColor = 16777215 Xor Me.BackColor
  'Check1.ForeColor = 16777215 Xor Me.BackColor
  'Check1.BackColor = Me.BackColor
  'LArenaName.ForeColor = 16777215 Xor Me.BackColor
  'LArenaName.BackColor = Me.BackColor
  On Error Resume Next
  'If YourSN = "Player" Then
  '  fOffline!tDebug.FontName = Config.FontName
  '  fOffline!tDebug.ForeColor = Val("&H" + ToHTML(Config.FontColor))
  '  fOffline!tDebug.BackColor = 16777215 Xor fOffline!tDebug.ForeColor
  'End If
NoWay5:
End Sub

Sub tFontColor_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Change font color"
End Sub

Sub tFontName_Change ()
  Config.FontName = tFontName
  On Error GoTo Nope1
  tFontName.FontName = tFontName
  'If YourSN = "Player" Then
  '  fOffline!tDebug.FontName = Config.FontName
  '  fOffline!tDebug.ForeColor = Val("&H" + ToHTML(Config.FontColor))
  '  fOffline!tDebug.BackColor = 16777215 Xor fOffline!tDebug.ForeColor
  'End If
Nope1: Exit Sub
End Sub

Sub tFontName_Click ()
  Config.FontName = tFontName
  On Error GoTo Nope
  tFontName.FontName = tFontName
  'If YourSN = "Player" Then
  '  fOffline!tDebug.FontName = Config.FontName
  '  fOffline!tDebug.ForeColor = Val("&H" + ToHTML(Config.FontColor))
  '  fOffline!tDebug.BackColor = 16777215 Xor Me.BackColor
  'End If
Nope: Exit Sub
End Sub

Sub tFragLimit_Change ()
  FragLimit = Val(tFragLimit)
End Sub

Sub tFragLimit_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Frag limit: Positive value = Kills, Negative value = Fatalities"
End Sub

Sub tGetRate_Change ()
  GetRate = Val(tGetRate)
End Sub

Sub tGetRate_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Item respawn rate. Good value: 100"
End Sub

Sub tHP_Change ()
Dim X%
  MaxHP = Val(tHP)
  For X = 1 To Maxplayers
    p(X).MaxHP = MaxHP
  Next X
  BalancingAct
End Sub

Sub tHP_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Changes maximum HP allowed. Also affects global move strength and danger HP level"
End Sub

Sub tReason_Change ()
  Config.Reason = tReason
End Sub

Sub tReason_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Change reason displayed for not allowing joining"
End Sub

Sub tSP_Change ()
  MaxSP = Val(tSP)
End Sub

Sub tSP_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Maximum amount of Super points allowed"
End Sub

