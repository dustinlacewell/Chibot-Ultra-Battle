VERSION 2.00
Begin Form fArenaEd 
   BackColor       =   &H00808080&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "ChUB 2000 Arena Editor"
   ClientHeight    =   5820
   ClientLeft      =   105
   ClientTop       =   840
   ClientWidth     =   9660
   Height          =   6510
   Icon            =   FAREDIT.FRX:0000
   Left            =   45
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   5820
   ScaleWidth      =   9660
   Top             =   210
   Width           =   9780
   Begin Timer Timer1 
      Interval        =   1000
      Left            =   120
      Top             =   1680
   End
   Begin CheckBox cbAll 
      BackColor       =   &H00808080&
      Caption         =   "Hits Everyone"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   300
      Left            =   7320
      TabIndex        =   29
      Top             =   3120
      Width           =   1575
   End
   Begin TextBox teStr 
      Height          =   285
      Left            =   6840
      MaxLength       =   5
      TabIndex        =   24
      Top             =   4800
      Width           =   615
   End
   Begin TextBox teMiss 
      Height          =   495
      Left            =   6120
      MultiLine       =   -1  'True
      TabIndex        =   23
      Top             =   4200
      Width           =   3375
   End
   Begin TextBox teHit 
      Height          =   495
      Left            =   6120
      MultiLine       =   -1  'True
      TabIndex        =   22
      Top             =   3600
      Width           =   3375
   End
   Begin TextBox teFrequency 
      Height          =   285
      Left            =   6720
      MaxLength       =   2
      TabIndex        =   21
      Top             =   3120
      Width           =   495
   End
   Begin TextBox teName 
      Height          =   285
      Left            =   6720
      TabIndex        =   20
      Top             =   2760
      Width           =   2775
   End
   Begin CommandButton cEdit 
      Caption         =   "Edit >>"
      Height          =   375
      Left            =   3480
      TabIndex        =   19
      Top             =   5160
      Width           =   1455
   End
   Begin ListBox LEvents 
      Height          =   1395
      Left            =   3480
      TabIndex        =   18
      Top             =   3480
      Width           =   1455
   End
   Begin TextBox tEffect 
      Height          =   285
      Left            =   2640
      MaxLength       =   5
      TabIndex        =   17
      Top             =   4320
      Width           =   495
   End
   Begin ListBox LElements 
      Height          =   810
      Left            =   360
      TabIndex        =   16
      Top             =   4680
      Width           =   2175
   End
   Begin TextBox tGradualHP 
      Height          =   285
      Left            =   2280
      MaxLength       =   3
      TabIndex        =   15
      Top             =   3600
      Width           =   495
   End
   Begin TextBox tRestHighHP 
      Height          =   285
      Left            =   2400
      MaxLength       =   5
      TabIndex        =   14
      Top             =   3000
      Width           =   615
   End
   Begin TextBox tRestLowHP 
      Height          =   285
      Left            =   1440
      MaxLength       =   5
      TabIndex        =   13
      Top             =   3000
      Width           =   615
   End
   Begin TextBox tDesc 
      Height          =   285
      Index           =   9
      Left            =   1320
      MaxLength       =   75
      TabIndex        =   12
      Top             =   2520
      Width           =   3975
   End
   Begin TextBox tDesc 
      Height          =   285
      Index           =   8
      Left            =   1320
      MaxLength       =   75
      TabIndex        =   11
      Top             =   2280
      Width           =   3975
   End
   Begin TextBox tDesc 
      Height          =   285
      Index           =   7
      Left            =   1320
      MaxLength       =   75
      TabIndex        =   10
      Top             =   2040
      Width           =   3975
   End
   Begin TextBox tDesc 
      Height          =   285
      Index           =   6
      Left            =   1320
      MaxLength       =   75
      TabIndex        =   9
      Top             =   1800
      Width           =   3975
   End
   Begin TextBox tDesc 
      Height          =   285
      Index           =   5
      Left            =   1320
      MaxLength       =   75
      TabIndex        =   8
      Top             =   1560
      Width           =   3975
   End
   Begin TextBox tDesc 
      Height          =   285
      Index           =   4
      Left            =   1320
      MaxLength       =   75
      TabIndex        =   7
      Top             =   1320
      Width           =   3975
   End
   Begin TextBox tDesc 
      Height          =   285
      Index           =   3
      Left            =   1320
      MaxLength       =   75
      TabIndex        =   6
      Top             =   1080
      Width           =   3975
   End
   Begin TextBox tDesc 
      Height          =   285
      Index           =   2
      Left            =   1320
      MaxLength       =   75
      TabIndex        =   5
      Top             =   840
      Width           =   3975
   End
   Begin TextBox tDesc 
      Height          =   285
      Index           =   1
      Left            =   1320
      MaxLength       =   75
      TabIndex        =   3
      Top             =   600
      Width           =   3975
   End
   Begin TextBox tDesc 
      Height          =   285
      Index           =   0
      Left            =   1320
      MaxLength       =   75
      TabIndex        =   2
      Top             =   360
      Width           =   3975
   End
   Begin TextBox tName 
      Height          =   285
      Left            =   1320
      TabIndex        =   1
      Top             =   0
      Width           =   3975
   End
   Begin CommonDialog cmd 
      CancelError     =   -1  'True
      Copies          =   1
      Filter          =   "KUB FiNAL Arenas (*.ana)|*.ana|KUBFF1m Arenas (*.an2)|*.an2"
      FilterIndex     =   1
      Flags           =   6
      Left            =   120
      Top             =   1080
   End
   Begin Label Stat 
      BackStyle       =   0  'Transparent
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Arial Narrow"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   27
      Top             =   5520
      Width           =   5295
   End
   Begin Label Label15 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "HP Damage (if 0, display Miss string)"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   975
      Left            =   5520
      TabIndex        =   32
      Top             =   4800
      Width           =   1215
   End
   Begin Label Label14 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Miss"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   375
      Left            =   5520
      TabIndex        =   36
      Top             =   4320
      Width           =   495
   End
   Begin Label Label13 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Hit"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   375
      Left            =   5520
      TabIndex        =   35
      Top             =   3600
      Width           =   495
   End
   Begin Label Label11 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Frequency"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   375
      Left            =   5520
      TabIndex        =   34
      Top             =   3120
      Width           =   1095
   End
   Begin Label Label10 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Event Name"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   5520
      TabIndex        =   33
      Top             =   2760
      Width           =   1095
   End
   Begin Line Line3 
      X1              =   5400
      X2              =   5400
      Y1              =   0
      Y2              =   6510
   End
   Begin Label Label9 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Events"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3480
      TabIndex        =   31
      Top             =   4920
      Width           =   1455
   End
   Begin Label Label8 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Affects ""All Attacks"" By:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   375
      Left            =   120
      TabIndex        =   30
      Top             =   4320
      Width           =   2415
   End
   Begin Label Label6 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Gradual HP Recovery:"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   375
      Left            =   120
      TabIndex        =   28
      Top             =   3600
      Width           =   2055
   End
   Begin Label Label4 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "% MaxHP"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   3000
      TabIndex        =   26
      Top             =   3000
      Width           =   975
   End
   Begin Line Line1 
      X1              =   2160
      X2              =   2280
      Y1              =   3120
      Y2              =   3120
   End
   Begin Label Label3 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "/rest restores"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   375
      Left            =   120
      TabIndex        =   25
      Top             =   3000
      Width           =   1215
   End
   Begin Label Label2 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Description"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   375
      Left            =   0
      TabIndex        =   4
      Top             =   360
      Width           =   1215
   End
   Begin Label Label1 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Arena Name"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   9.75
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   1215
   End
   Begin Menu mFile 
      Caption         =   "&File"
      Begin Menu mNew 
         Caption         =   "&New"
         Shortcut        =   ^N
      End
      Begin Menu mDash1 
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
      Begin Menu mDash2 
         Caption         =   "-"
      End
      Begin Menu mExit 
         Caption         =   "E&xit"
         Shortcut        =   ^X
      End
   End
End
Option Explicit

Dim Fil$
Dim Changed As Integer
Dim Temp1 As Integer
Dim Ev As Integer

Sub cbAll_Click ()
  Arena.Event(Ev).HitsAll = cbAll
  Changed = True
End Sub

Sub cEdit_Click ()
  If cEdit.Caption = "Edit >>" Then
    fArenaEd.Width = 9795
    cEdit.Caption = "<< Hide"
  Else
    fArenaEd.Width = Line3.X1
    cEdit.Caption = "Edit >>"
  End If
End Sub

Sub Form_Load ()
  If Command$ <> "" Then
    Fil$ = Command$
    LoadArena (Fil$)
  Else
    ClearArena
    Fil = ""
  End If
  lElements.AddItem "All Attacks"
  lElements.AddItem "Physical"
  lElements.AddItem "Moon"
  lElements.AddItem "Shadow"
  lElements.AddItem "Water"
  lElements.AddItem "Fire"
  lElements.AddItem "Lightning"
  lElements.AddItem "Heart"
  lElements.AddItem "Wind"
  lElements.AddItem "Earth"
  lElements.AddItem "Ki"
  lElements.AddItem "Light"
  lElements.AddItem "Poison (the element)"
  lElements.AddItem "Grass"
  lElements.AddItem "Rock"
  lElements.AddItem "Dirt"
  lElements.AddItem "Psychic"
  lElements.AddItem "Ghost"
  lElements.AddItem "Gravity"
  UpdateAll
  Temp1 = 0
  fArenaEd.Width = Line3.X1
  Ev = 1
  lElements.ListIndex = 0
  lEvents.ListIndex = 0
  Changed = False
End Sub

Sub Form_QueryUnload (Cancel As Integer, UnloadMode As Integer)
  If Changed Then
    Select Case MsgBox("Save changes to " + Fil$ + "?", 35, "ChUB 2000 ArenaEd")
      Case 6:
        mSaveAs_Click
        If (Temp1 <> 0) Then Cancel = 1
      Case 2: Cancel = 1
      Case 7:
    End Select
  End If
End Sub

Sub LElements_Click ()
  Label8.Caption = "Affects " + Chr$(34) + lElements.List(lElements.ListIndex) + Chr$(34) + " By:"
  Select Case lElements.ListIndex
    Case 0: tEffect = TrimStr(Arena.AllAttacks)
    Case 1: tEffect = TrimStr(Arena.Effect(Phys))
    Case 2: tEffect = TrimStr(Arena.Effect(MoonE))
    Case 3: tEffect = TrimStr(Arena.Effect(Shadow))
    Case 4: tEffect = TrimStr(Arena.Effect(Water))
    Case 5: tEffect = TrimStr(Arena.Effect(Fire))
    Case 6: tEffect = TrimStr(Arena.Effect(Lit))
    Case 7: tEffect = TrimStr(Arena.Effect(Heart))
    Case 8: tEffect = TrimStr(Arena.Effect(RandE))
    Case 9: tEffect = TrimStr(Arena.Effect(Earth))
    Case 10: tEffect = TrimStr(Arena.Effect(Mult))
    Case 11: tEffect = TrimStr(Arena.Effect(Lum))
    Case 12: tEffect = TrimStr(Arena.Effect(Poison))
    Case 13: tEffect = TrimStr(Arena.Effect(Grass))
    Case 14: tEffect = TrimStr(Arena.Effect(Rock))
    Case 15: tEffect = TrimStr(Arena.Effect(Dirt))
    Case 16: tEffect = TrimStr(Arena.Effect(Psychic))
    Case 17: tEffect = TrimStr(Arena.Effect(Ghost))
    Case 18: tEffect = TrimStr(Arena.Effect(Gravity))
  End Select
End Sub

Sub LEvents_Click ()
Dim X As Integer
  Ev = lEvents.ListIndex + 1
  X = Changed
  UpdateEvent (Ev)
  Changed = X
End Sub

Sub mExit_Click ()
  Unload Me
End Sub

Sub mLoad_Click ()
  If Changed Then
    If MsgBox("Save changes to " + Fil$ + "?", 36, "ChUB 2000 ArenaEd") = 6 Then
      mSaveAs_Click
      If (Temp1 <> 0) Then Exit Sub
    End If
  End If
  cmd.Filename = "*.ana"
  On Error GoTo CancelLoad
    cmd.Action = 1
    Fil = cmd.Filename
    LoadArena (Fil)
    UpdateAll
    Changed = False
  Exit Sub
CancelLoad:
  Exit Sub
End Sub

Sub mNew_Click ()
  If Changed Then
    If MsgBox("Save changes to " + Fil$ + "?", 36, "ChUB 2000 ArenaEd") = 6 Then
      mSaveAs_Click
      If (Temp1 <> 0) Then Exit Sub
    End If
  End If
  ClearArena
  UpdateAll
  Fil = ""
  Changed = False
End Sub

Sub mSave_Click ()
  If (Fil = "") Then
    mSaveAs_Click
  Else
    SaveArena (Fil)
    Stat = "Saved arena as " + Fil + "."
  End If
  Changed = False
End Sub

Sub mSaveAs_Click ()
  cmd.Filename = "*.an2"
  On Error GoTo CancelSave
    cmd.Action = 2
    Fil = cmd.Filename
    SaveArena (Fil)
    UpdateAll
    Stat = "Saved arena as " + Fil + "."
    Changed = False
    Temp1 = 0
  Exit Sub
CancelSave:
  Temp1 = 1
  Exit Sub
End Sub

Sub tDesc_Change (Index As Integer)
  Arena.Desc(Index + 1) = tDesc(Index)
  Changed = True
End Sub

Sub tDesc_KeyUp (Index As Integer, KeyCode As Integer, Shift As Integer)
Dim X%
  If Len(tDesc(Index).Text) = tDesc(Index).MaxLength And Index < 9 Then
    For X = Len(tDesc(Index).Text) To 1 Step -1
      If Mid$(tDesc(Index).Text, X, 1) = " " Then
        tDesc(Index + 1).Text = Right$(tDesc(Index).Text, Len(tDesc(Index).Text) - X) + tDesc(Index + 1).Text
        '+ Chr$(KeyCode)
        tDesc(Index).Text = Left$(tDesc(Index).Text, X - 1)
        tDesc(Index + 1).SetFocus
        tDesc(Index + 1).SelStart = Len(tDesc(Index + 1).Text)
        'tDesc(Index + 1).Text = tDesc(Index + 1).Text + Chr$(KeyCode)
        Exit Sub
      End If
    Next X
  End If
End Sub

Sub tEffect_Change ()
Dim X2 As Integer
  Changed = True
  X2 = Val(tEffect)
  Select Case lElements.ListIndex
    Case 0: Arena.AllAttacks = X2
    Case 1: Arena.Effect(Phys) = X2
    Case 2: Arena.Effect(MoonE) = X2
    Case 3: Arena.Effect(Shadow) = X2
    Case 4: Arena.Effect(Water) = X2
    Case 5: Arena.Effect(Fire) = X2
    Case 6: Arena.Effect(Lit) = X2
    Case 7: Arena.Effect(Heart) = X2
    Case 8: Arena.Effect(RandE) = X2
    Case 9: Arena.Effect(Earth) = X2
    Case 10: Arena.Effect(Mult) = X2
    Case 11: Arena.Effect(Lum) = X2
    Case 12: Arena.Effect(Poison) = X2
    Case 13: Arena.Effect(Grass) = X2
    Case 14: Arena.Effect(Rock) = X2
    Case 15: Arena.Effect(Dirt) = X2
    Case 16: Arena.Effect(Psychic) = X2
    Case 17: Arena.Effect(Ghost) = X2
    Case 18: Arena.Effect(Gravity) = X2
  End Select
End Sub

Sub teFrequency_Change ()
  Arena.Event(Ev).Frequency = Val(teFrequency)
  Changed = True
End Sub

Sub teHit_Change ()
  Arena.Event(Ev).Hit = teHit
  Changed = True
End Sub

Sub teMiss_Change ()
  Arena.Event(Ev).Miss = teMiss
  Changed = True
End Sub

Sub teName_Change ()
Dim X2 As Integer
  Arena.Event(Ev).Name = teName
  Changed = True
End Sub

Sub teName_LostFocus ()
Dim X2 As Integer
  lEvents.Clear
  For X2 = 1 To 10
    lEvents.AddItem Arena.Event(X2).Name
  Next X2
End Sub

Sub teStr_Change ()
  Arena.Event(Ev).ElementStr = Val(teStr)
  Changed = True
End Sub

Sub tGradualHP_Change ()
  Arena.GradualHP = Val(tGradualHP)
  Changed = True
End Sub

Sub tGradualMP_Change ()
  'Arena.GradualMP = Val(tGradualMP)
  'Changed = True
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

Sub tName_Change ()
  Arena.Name = tName
  Changed = True
End Sub

Sub tRestHighHP_Change ()
  Arena.RestHighHP = Val(tRestHighHP)
  Changed = True
End Sub

Sub tRestHighMP_Change ()
  'Arena.RestHighMP = Val(tRestHighMP)
  'Changed = True
End Sub

Sub tRestLowHP_Change ()
  Arena.RestLowHP = Val(tRestLowHP)
  Changed = True
End Sub

Sub tRestLowMP_Change ()
  'Arena.RestLowMP = Val(tRestLowMP)
  'Changed = True
End Sub

Sub UpdateAll ()
Dim X2 As Integer
  LElements_Click
  tName = Arena.Name
  For X2 = 1 To 10
    tDesc(X2 - 1) = Arena.Desc(X2)
  Next X2
  tRestLowHP = TrimStr(Arena.RestLowHP)
  tRestHighHP = TrimStr(Arena.RestHighHP)
  'tRestLowMP = TrimStr(Arena.RestLowMP)
  'tRestHighMP = TrimStr(Arena.RestHighMP)
  tGradualHP = TrimStr(Arena.GradualHP)
  'tGradualMP = TrimStr(Arena.GradualMP)
  UpdateEvent (Ev)
  lEvents.Clear
  For X2 = 1 To 10
    lEvents.AddItem Arena.Event(X2).Name
  Next X2
End Sub

Sub UpdateEvent (ByVal E As Integer)
  teName = Arena.Event(E).Name
  teFrequency = TrimStr(Arena.Event(E).Frequency)
  teHit = Arena.Event(E).Hit
  teMiss = Arena.Event(E).Miss
  teStr = TrimStr(Arena.Event(E).ElementStr)
  cbAll = Arena.Event(E).HitsAll
End Sub

