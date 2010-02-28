Option Explicit

Global dlgText As String
Global dlgTitle$
Global dlgType As Integer
Global dlgIcon As Integer

Global dlgResponse As Variant

Sub kDlgBox (ByVal Msg$, ByVal Typ%, ByVal Title$)
Dim X%
  X = kDlgBoxfn(Msg$, Typ%, Title$)
End Sub

Function kDlgBoxfn (ByVal Msg$, ByVal Typ%, ByVal Title$)
Dim X%
  dlgText = Msg$
  dlgTitle = Title$
  dlgType = Typ
  If Typ - 64 >= 0 Then
    dlgIcon = 4
    dlgType = Typ - 64
  ElseIf Typ - 48 >= 0 Then
    dlgIcon = 3
    dlgType = Typ - 48
  ElseIf Typ - 32 >= 0 Then
    dlgIcon = 2
    dlgType = Typ - 32
  ElseIf Typ - 16 >= 0 Then
    dlgIcon = 1
    dlgType = Typ - 16
  Else
    dlgIcon = 0
    dlgType = Typ - 0
  End If
  fDialog!iStop.Visible = False
  fDialog!iQuestion.Visible = False
  fDialog!iWarning.Visible = False
  fDialog!iInfo.Visible = False
  Select Case dlgIcon:
    Case 1:
      fDialog!iStop.Visible = True
      Playwav ("stop")
    Case 2:
      fDialog!iQuestion.Visible = True
      Playwav ("question")
    Case 3:
      fDialog!iWarning.Visible = True
      Playwav ("warning")
    Case 4:
      fDialog!iInfo.Visible = True
      Playwav ("info")
  End Select
  For X = 1 To 6
    fDialog!cbX(X).Visible = False
  Next X
  fDialog.Show
  Select Case dlgType:
    Case 1:
      fDialog!cbX(4).Visible = True
      fDialog!cbX(5).Visible = True
      fDialog!cbX(4).Caption = "&OK"
      fDialog!cbX(5).Caption = "&Cancel"
      fDialog!cbX(4).Default = True
      fDialog!cbX(5).Cancel = True
      fDialog!cbX(4).SetFocus
    Case 2:
      For X = 1 To 3
        fDialog!cbX(X).Visible = True
      Next X
      fDialog!cbX(1).Caption = "&Abort"
      fDialog!cbX(2).Caption = "&Retry"
      fDialog!cbX(3).Caption = "&Ignore"
      fDialog!cbX(1).SetFocus
    Case 3:
      For X = 1 To 3
        fDialog!cbX(X).Visible = True
      Next X
      fDialog!cbX(1).Caption = "&Yes"
      fDialog!cbX(2).Caption = "&No"
      fDialog!cbX(3).Caption = "&Cancel"
      fDialog!cbX(1).Default = True
      fDialog!cbX(3).Cancel = True
      fDialog!cbX(1).SetFocus
    Case 4:
      fDialog!cbX(4).Visible = True
      fDialog!cbX(5).Visible = True
      fDialog!cbX(4).Caption = "&Yes"
      fDialog!cbX(5).Caption = "&No"
      fDialog!cbX(4).Default = True
      fDialog!cbX(5).Cancel = True
      fDialog!cbX(4).SetFocus
    Case 5:
      fDialog!cbX(4).Visible = True
      fDialog!cbX(5).Visible = True
      fDialog!cbX(4).Caption = "&Retry"
      fDialog!cbX(5).Caption = "&Cancel"
      fDialog!cbX(4).Default = True
      fDialog!cbX(5).Cancel = True
      fDialog!cbX(4).SetFocus
    Case Else:
      fDialog!cbX(6).Visible = True
      fDialog!cbX(6).Caption = "&OK"
      fDialog!cbX(6).Default = True
      fDialog!cbX(6).SetFocus
  End Select
  fDialog.Caption = dlgTitle
  fDialog!LText.Caption = dlgText
  dlgResponse = -1
  Do
    DoEvents
  Loop Until dlgResponse <> -1
  kDlgBoxfn = dlgResponse
End Function

Function kDlgBoxInput (ByVal Msg$, ByVal Title$, ByVal Def$)
Dim X%, XP%, YP%
  fDialog!tInputBox.Text = Def$
  dlgText = Msg$
  dlgTitle = Title$
  dlgIcon = 2
  dlgType = 1
  fDialog!iStop.Visible = False
  fDialog!iQuestion.Visible = True
  fDialog!iWarning.Visible = False
  fDialog!iInfo.Visible = False
  fDialog!tInputBox.Visible = True
  Playwav ("question")
  For X = 1 To 6
    fDialog!cbX(X).Visible = False
  Next X
  fDialog!cbX(4).Visible = True
  fDialog!cbX(5).Visible = True
  fDialog!cbX(4).Caption = "&OK"
  fDialog!cbX(5).Caption = "&Cancel"
  fDialog!cbX(4).Default = True
  fDialog!cbX(5).Cancel = True
  fDialog.Caption = dlgTitle
  fDialog!LText.Caption = dlgText
  fDialog.Height = 2220
  XP = Int(Screen.Width / 2) - Int(fDialog.Width / 2)
  YP = Int(Screen.Height / 2) - Int(fDialog.Height / 2)
  fDialog.Left = XP
  fDialog.Top = YP
  fDialog.Show
  fDialog!tInputBox.SelStart = 0
  fDialog!tInputBox.SelLength = Len(fDialog!tInputBox.Text)
  fDialog!tInputBox.SetFocus
  dlgResponse = -1
  Do
    DoEvents
    If VarType(dlgResponse) = 8 Then Exit Do
  Loop Until dlgResponse <> -1
  If (VarType(dlgResponse) <> 8) Then
    kDlgBoxInput = ""
  Else
    kDlgBoxInput = dlgResponse
  End If
End Function

