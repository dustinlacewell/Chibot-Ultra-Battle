' FCHE.FRM
Option Explicit
Dim m001C As Integer
Const mc004E = 1 ' &H1%

Sub cEdit_Click ()
  gv0310 = True
  gv030A = cbMoves.ListIndex + 1
  Load fMoveEdit
End Sub

Sub cEdit_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Edit Moves"
End Sub

Function fn012F (p003E As T0FF3) As Integer
Dim l0040 As Integer
  If  p003E.M1049 + p003E.M1054 + p003E.M105F + p003E.M1069 > 220 Then
    MsgBox "Error: " + gv0314(l0040).M1001 + "'s PhysStr, PhysDef, MagStr, and MagDef combined must be less than a total of 220. Please fix it.", 16, "KUB FiNAL CharEdit"
    fn012F = 0
    Exit Function
  End If
  fn012F = 1
End Function

Sub sub013D ()
Dim l004C As Integer
' Const mc004E = 1 ' &H1%
  l004C = gv0314(mc004E).M1049 + gv0314(mc004E).M1054 + gv0314(mc004E).M105F + gv0314(mc004E).M1069
  Select Case l004C
    Case 1 To 219: LTotal.ForeColor = &H80FF80&
    Case 220: LTotal.ForeColor = &HFFFFFF&
    Case Is > 220: LTotal.ForeColor = &H8080FF&
  End Select
  LTotal.Caption = Trim(Str$(l004C))
End Sub

Sub Form_Load ()
Dim l0058 As String
  Me.Show
  If  (Command$ <> "") Then
    l0058 = Command$
    sub00BD l0058, 1
    gv030C = l0058
    sub0154
    gv0310 = False
    MsgBox "Character " + l0058 + " successfully loaded.", 64, "KUB FiNAL CharEdit"
  Else
    gv030C = ""
    sub008B (1)
    sub0154
  End If
  gv0310 = False
  gv030A = 1
End Sub

Sub Form_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "CharEdit Version 9 Gazillion"
End Sub

Sub Form_QueryUnload (Cancel As Integer, UnloadMode As Integer)
  If  (gv0310) Then
    Select Case MsgBox("Save changes?", 35, "KUB FiNAL CharEdit")
      Case 2: Cancel = 1
      Case 6: mSaveChar_Click
              Cancel = m001C
      Case 7: Cancel = 0
    End Select
  End If
End Sub

Sub Form_Unload (Cancel As Integer)
  End
End Sub

Sub hsMagDef_Change ()
  gv0310 = True
  gv0314(mc004E).M1069 = hsMagDef.Value
  tMagDef = fn00EF(hsMagDef.Value)
  sub013D
End Sub

Sub hsMagDef_Scroll ()
  hsMagDef_Change
End Sub

Sub hsMagStr_Change ()
  gv0310 = True
  gv0314(mc004E).M105F = hsMagStr.Value
  tMagStr = fn00EF(hsMagStr.Value)
  sub013D
End Sub

Sub hsMagStr_Scroll ()
  hsMagStr_Change
End Sub

Sub hsPhysDef_Change ()
  gv0310 = True
  gv0314(mc004E).M1054 = hsPhysDef.Value
  tPhysDef = fn00EF(hsPhysDef.Value)
  sub013D
End Sub

Sub hsPhysDef_Scroll ()
  hsPhysDef_Change
End Sub

Sub hsPhysStr_Change ()
  gv0310 = True
  gv0314(mc004E).M1049 = hsPhysStr.Value
  tPhysStr = fn00EF(hsPhysStr.Value)
  sub013D
End Sub

Sub hsPhysStr_Scroll ()
  hsPhysStr_Change
End Sub

Sub Label2_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Failure to do so will result in screwed-up characters."
End Sub

Sub mClearChar_Click ()
  If  MsgBox("Erase this character?", 36, "CharEdit") = 6 Then
    sub008B (mc004E)
    sub0270
  End If
End Sub

Sub mExit_Click ()
  Unload Me
End Sub

Sub mLoadChar_Click ()
Dim l00A4 As String
  On Error GoTo L41E8
  Select Case cmdChr.FilterIndex
    Case 1: cmdChr.Filename = "*.chr"
    Case 2: cmdChr.Filename = "*.ch2"
    Case Else: cmdChr.Filename = "*.ch2"
  End Select
  cmdChr.DialogTitle = "Load Character"
  cmdChr.Action = 1
  l00A4 = cmdChr.Filename
  Call sub00BD(l00A4, 1)
  sub0154
  gv0310 = False
  MsgBox "Character " + l00A4 + " successfully loaded.", 64, "KUB FiNAL CharEdit"
L41E8:
  On Error GoTo 0
  Exit Sub
End Sub

Sub mSaveChar_Click ()
Dim l00AE As String
  If  fn012F(gv0314(1)) Then
    On Error GoTo L4332
    cmdChr.Filename = fn00A1(gv0314(mc004E).M100D) + ".ch2"
    cmdChr.DialogTitle = "Save Character"
    cmdChr.FilterIndex = 2
    cmdChr.Action = 2
    l00AE = cmdChr.Filename
    sub00E0 l00AE, mc004E
    gv0310 = False
    m001C = 0
    MsgBox "Character " + l00AE + " successfully saved.", 64, "KUB FiNAL CharEdit"
L4332:
    On Error GoTo 0
    Exit Sub
  Else
    m001C = 1
  End If
End Sub

Sub StatLine_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Ooga Booga."
End Sub

Sub tBlock_Change ()
  gv0314(mc004E).M1085 = tBlock
  gv0310 = True
End Sub

Sub tBlock_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown when player blocks"
End Sub

Sub tBlockFail_Change ()
  gv0314(mc004E).M108E = tBlockFail
  gv0310 = True
End Sub

Sub tBlockFail_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown when a block fails"
End Sub

Sub tDeathStr_Change ()
  gv0314(mc004E).M10B9(vsDeathStr.Value) = tDeathStr.Text
  gv0310 = True
End Sub

Sub tDeathStr_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
End Sub

Sub tFatalCmdkey_Change ()
  gv0314(mc004E).M10A4.M0EEF = tFatalCmdkey
  gv0310 = True
End Sub

Sub tFatalCmdkey_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Fatality Command Key"
End Sub

Sub tFatalMove_Change ()
  gv0314(mc004E).M10A4.M0FE6 = tFatalMove
  gv0310 = True
End Sub

Sub tFatalMove_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Fatality Finish"
End Sub

Sub tFullName_Change ()
  gv0314(mc004E).M1001 = tFullName
  gv0310 = True
End Sub

Sub tFullName_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Character's Name"
End Sub

Sub tKillStr_Change ()
  gv0314(mc004E).M10C5(vsKillStr.Value) = tKillStr.Text
  gv0310 = True
End Sub

Sub tKillStr_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown after character makes a kill"
End Sub

Sub tMagDef_Change ()
  gv0310 = True
  If  (Val(tMagDef) >= 0) And (Val(tMagDef) <= 100) Then
    hsMagDef = tMagDef
    gv0314(mc004E).M1069 = tMagDef
    sub013D
  End If
End Sub

Sub tMagStr_Change ()
  gv0310 = True
  If  (Val(tMagStr) >= 0) And (Val(tMagStr) <= 100) Then
    hsMagStr = tMagStr
    gv0314(mc004E).M105F = tMagStr
    sub013D
  End If
End Sub

Sub tPhysDef_Change ()
  gv0310 = True
  If  (Val(tPhysDef) >= 0) And (Val(tPhysDef) <= 100) Then
    hsPhysDef = tPhysDef
    gv0314(mc004E).M1054 = tPhysDef
    sub013D
  End If
End Sub

Sub tPhysStr_Change ()
  gv0310 = True
  If  (Val(tPhysStr) >= 0) And (Val(tPhysStr) <= 100) Then
    hsPhysStr = tPhysStr
    gv0314(mc004E).M1049 = tPhysStr
    sub013D
  End If
End Sub

Sub tPickMe_Change ()
  gv0314(mc004E).M1024 = tPickMe
  gv0310 = True
End Sub

Sub tPickMe_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Selection String"
End Sub

Sub tPreFatal_Change ()
  gv0314(mc004E).M10A4.M0FDA = tPreFatal
  gv0310 = True
End Sub

Sub tPreFatal_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Fatality Start"
End Sub

Sub tRest_Change ()
  gv0314(mc004E).M107D = tRest
  gv0310 = True
End Sub

Sub tRest_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown when player rests"
End Sub

Sub tSelectJoin_Change ()
  gv0314(mc004E).M103B = tSelectJoin
  gv0310 = True
End Sub

Sub tSelectJoin_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown when character picked during battle"
End Sub

Sub tSelectStr_Change ()
  gv0314(mc004E).M102E = tSelectStr
  gv0310 = True
End Sub

Sub tSelectStr_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown when character picked at selection"
End Sub

Sub tSenshiID_Change ()
  gv0314(mc004E).M100D = tSenshiID
  gv0310 = True
End Sub

Sub tSenshiID_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Targeting Identifier"
End Sub

Sub sub052A ()
  gv0310 = True
End Sub

Sub tTaunt_Change ()
  gv0314(mc004E).M109B(vsTaunt.Value) = tTaunt.Text
  gv0310 = True
End Sub

Sub tTaunt_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown at /taunt"
End Sub

Sub sub0270 ()
  sub0154
End Sub

Sub sub0154 ()
Dim l0196 As T0FF3
Dim l0198
  l0196 = gv0314(1)
  tFullName = l0196.M1001
  tPickMe = l0196.M1024
  tSenshiID = l0196.M100D
  tSelectStr = l0196.M102E
  tSelectJoin = l0196.M103B
  tRest = l0196.M107D
  tBlock = l0196.M1085
  tBlockFail = l0196.M108E
  tTaunt.Text = l0196.M109B(vsTaunt.Value)
  tKillStr.Text = l0196.M10C5(vsKillStr.Value)
  tDeathStr.Text = l0196.M10B9(vsDeathStr.Value)
  tFatalCmdkey = l0196.M10A4.M0EEF
  tPreFatal = l0196.M10A4.M0FDA
  tFatalMove = l0196.M10A4.M0FE6
  hsPhysStr = l0196.M1049
  hsPhysDef = l0196.M1054
  hsMagStr = l0196.M105F
  hsMagDef = l0196.M1069
  sub013D
  cbMoves.Clear
  For l0198 = 1 To 12
    cbMoves.AddItem l0196.M10B0(l0198).M0EE7
  Next l0198
  cbMoves.ListIndex = 0
End Sub

Sub vsDeathStr_Change ()
  Label1(1).Caption = "When Killed " + Trim(Str$(vsDeathStr.Value))
  tDeathStr.Text = gv0314(mc004E).M10B9(vsDeathStr.Value)
End Sub

Sub vsKillStr_Change ()
  Label1(0).Caption = "After a Kill " + Trim(Str$(vsKillStr.Value))
  tKillStr.Text = gv0314(mc004E).M10C5(vsKillStr.Value)
End Sub

Sub vsTaunt_Change ()
  Label1(15).Caption = "Taunt " + Trim(Str$(vsTaunt.Value))
  tTaunt.Text = gv0314(mc004E).M109B(vsTaunt.Value)
End Sub
