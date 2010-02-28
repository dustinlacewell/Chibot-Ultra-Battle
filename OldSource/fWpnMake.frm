VERSION 2.00
Begin Form fWpnMake 
   BackColor       =   &H00812577&
   BorderStyle     =   3  'Fixed Double
   Caption         =   "ChUB 2000 Weapon Editor"
   ClientHeight    =   2175
   ClientLeft      =   1185
   ClientTop       =   1470
   ClientWidth     =   7095
   Height          =   2865
   Icon            =   FWPNMAKE.FRX:0000
   Left            =   1125
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   ScaleHeight     =   2175
   ScaleWidth      =   7095
   Top             =   840
   Width           =   7215
   Begin TextBox tNumUses 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   5160
      TabIndex        =   6
      Top             =   1080
      Width           =   615
   End
   Begin TextBox tDesc 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Index           =   2
      Left            =   1560
      TabIndex        =   5
      Top             =   720
      Width           =   5535
   End
   Begin TextBox tDesc 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Index           =   1
      Left            =   1560
      TabIndex        =   4
      Top             =   480
      Width           =   5535
   End
   Begin TextBox tSelectStr 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   1560
      TabIndex        =   3
      Top             =   240
      Width           =   5535
   End
   Begin TextBox tPickMe 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   4920
      TabIndex        =   2
      Top             =   0
      Width           =   2175
   End
   Begin TextBox tFullName 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   1560
      TabIndex        =   1
      Top             =   0
      Width           =   3135
   End
   Begin Timer Timer1 
      Interval        =   1000
      Left            =   120
      Top             =   2280
   End
   Begin CommonDialog cmdChr 
      CancelError     =   -1  'True
      DefaultExt      =   "*.chr"
      Filter          =   "Weapons (*.w2k)|*.w2k"
      FilterIndex     =   1
      Flags           =   4
      Left            =   600
      Top             =   2280
   End
   Begin CommandButton cEdit 
      Caption         =   "Edit"
      Height          =   375
      Left            =   3240
      TabIndex        =   8
      Top             =   1080
      Width           =   735
   End
   Begin ComboBox cbMoves 
      Height          =   315
      Left            =   120
      Style           =   2  'Dropdown List
      TabIndex        =   7
      Top             =   1080
      Width           =   3015
   End
   Begin Label Label2 
      Alignment       =   2  'Center
      BackColor       =   &H00C0C0FF&
      Caption         =   "Warning: Using "" will make your weapon screwy!"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H000000FF&
      Height          =   495
      Left            =   5040
      TabIndex        =   16
      Top             =   1440
      Width           =   2055
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "# of Uses"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   3
      Left            =   4080
      TabIndex        =   0
      Top             =   1080
      Width           =   975
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Desc. Line 2"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   2
      Left            =   0
      TabIndex        =   15
      Top             =   720
      Width           =   1455
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Desc. Line 1"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   1
      Left            =   0
      TabIndex        =   14
      Top             =   480
      Width           =   1455
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Weapon Name"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Index           =   0
      Left            =   120
      TabIndex        =   10
      Top             =   0
      Width           =   1335
   End
   Begin Label InfoLine 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial Narrow"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   495
      Left            =   240
      TabIndex        =   9
      Top             =   1440
      Width           =   4695
   End
   Begin Label StatLine 
      Alignment       =   2  'Center
      BackColor       =   &H00C0C0C0&
      Caption         =   "Chibot Weapon Editor"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   0
      TabIndex        =   11
      Top             =   1920
      Width           =   7095
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Selection String"
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
      TabIndex        =   12
      Top             =   240
      Width           =   1455
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
      Index           =   7
      Left            =   4800
      TabIndex        =   13
      Top             =   0
      Width           =   135
   End
   Begin Menu mFile 
      Caption         =   "&File"
      Begin Menu mExit 
         Caption         =   "E&xit"
         Shortcut        =   ^X
      End
   End
   Begin Menu mChar 
      Caption         =   "&Weapon"
      Begin Menu mLoadChar 
         Caption         =   "&Load..."
         Shortcut        =   ^L
      End
      Begin Menu mSaveChar 
         Caption         =   "&Save..."
         Shortcut        =   ^S
      End
      Begin Menu mClearChar 
         Caption         =   "&Clear"
         Shortcut        =   ^C
      End
   End
End
Option Explicit

Dim Temp1 As Integer

Sub cEdit_Click ()
  'Changed = True
  M = cbMoves.ListIndex + 1
  Load fMoveEdit
End Sub

Sub cEdit_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Edit Moves"
End Sub

Sub Form_Load ()
Dim X As Integer
Dim S As String
  Me.Show
  ClearWeapon
  If (Command$ <> "") Then
    S = Command$
    LoadWeapon S, 1
    Fil = S
    UpdateFields
    Changed = False
    InfoLine = "Weapon " + S + " successfully loaded."
  Else
    Fil = ""
    UpdateFields
  End If
  Changed = False
  M = 1
End Sub

Sub Form_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Chibot Weapon Editor"
End Sub

Sub Form_QueryUnload (Cancel As Integer, UnloadMode As Integer)
  If (Changed) Then
    Select Case MsgBox("Save changes?", 35, "ChUB 2000 Weapon Editor")
      Case 2: Cancel = 1
      Case 6: mSaveChar_Click
              Cancel = Temp1
      Case 7: Cancel = 0
    End Select
  End If
End Sub

Sub Form_Unload (Cancel As Integer)
  End
End Sub

Sub Label2_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Failure to do so will result in screwed-up weapons."
End Sub

Sub mClearChar_Click ()
  If MsgBox("Erase this weapon?", 36, "CharEdit") = 6 Then
    ClearWeapon
    UpdateAll
  End If
End Sub

Sub mExit_Click ()
  Unload Me
End Sub

Sub mLoadChar_Click ()
Dim S As String
Dim X As Integer
  On Error GoTo Cancell1
  CmdChr.Filename = "*.w??"
  CmdChr.DialogTitle = "Load Weapon"
  CmdChr.Action = 1
  S = CmdChr.Filename
  LoadWeapon S, Num
  UpdateFields
  Changed = False
  InfoLine = "Weapon " + S + " successfully loaded."
Cancell1:
  On Error GoTo 0
  Exit Sub
End Sub

Sub mSaveChar_Click ()
Dim S As String
  On Error GoTo DontSave1
  CmdChr.Filename = First8(Wpn(Num).Pickme) + ".w2k"
  CmdChr.DialogTitle = "Save Weapon"
  CmdChr.FilterIndex = 2
  CmdChr.Action = 2
  S = CmdChr.Filename
  SaveWeapon S, Num
  Changed = False
  Temp1 = 0
  InfoLine = "Weapon " + S + " successfully saved."
DontSave1:
  On Error GoTo 0
  Exit Sub
End Sub

Sub StatLine_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Ooga Booga."
End Sub

Sub tDesc_Change (Index As Integer)
  Wpn(Num).Desc(Index) = tDesc(Index).Text
End Sub

Sub tDesc_MouseMove (Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Weapon Description"
End Sub

Sub tFullName_Change ()
Dim X As Integer
  Wpn(Num).Name = tFullName
  Changed = True
End Sub

Sub tFullName_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Weapon Name"
End Sub

Sub Timer1_Timer ()
  If Timer1.Interval = 10000 Then
    InfoLine = ""
    Timer1.Interval = 1000
  End If
  If InfoLine <> "" Then
    Timer1.Interval = 10000
  End If
End Sub

Sub tNumUses_Change ()
  Changed = True
  Wpn(Num).NumUses = Val(tNumUses.Text)
End Sub

Sub tNumUses_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "# of times the weapon can be used. Only affected by certain moves (see move editor)"
End Sub

Sub tPickMe_Change ()
  Wpn(Num).Pickme = tPickme
  Changed = True
End Sub

Sub tPickMe_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Selection String"
End Sub

Sub tSelectStr_Change ()
  Wpn(Num).SelectStr = tSelectStr
  Changed = True
End Sub

Sub tSelectStr_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown when weapon is picked"
End Sub

Sub UpdateAll ()
Dim X As Integer
  UpdateFields
End Sub

Sub UpdateFields ()
Dim S As WeaponType
Dim X As Integer
  S = Wpn(1)
  tFullName = S.Name
  tPickme = S.Pickme
  tSelectStr = S.SelectStr
  tNumUses = TrimStr(S.NumUses)
  For X = 1 To 2
    tDesc(X) = S.Desc(X)
  Next X
  cbMoves.Clear
  For X = 1 To 5
    cbMoves.AddItem S.Moves(X).Name
  Next X
  cbMoves.ListIndex = 0
End Sub

