Attribute VB_Name = "IM"

Function AOLChild()
Dim Aol%, MDI%, Child%
Aol% = FindWindow("AOL Frame25", 0&)
MDI% = FindChildByClass(Aol%, "MDIClient")
Child% = FindChildByClass(MDI%, "AOL Child")
AOLChild = Child%
End Function

Function AOLMDI()
Aol = FindWindow("AOL Frame25", vbNullString)
AOLMDI = FindChildByClass(Aol, "MDIClient")
End Function

Sub center(formname As Form)
On Error Resume Next
formname.Top = (Screen.Height - formname.Height) / 2
formname.Left = (Screen.Width - formname.Width) / 2
End Sub

Sub Enter(edt%)
X = SendMessage(edt%, WM_CHAR, 13, 0)
End Sub

Function GetTextFromRICHCNTL(hWindow As Integer)
Aol% = FindWindow("AOL Frame25", 0&)
MDI% = FindChildByClass(Aol%, "MDIClient")
Msg$ = String$(255, 0)
XX% = SendMessage(hWindow, WM_ProGGer, 254, Msg$)
Msg$ = Trim$(Msg$)
GetTextFromRICHCNTL = Msg$

End Function

Sub IMControl(ByVal S$)
  AOLTitle ("America  Online")
  AppActivate "America  Online"
  SendKeys "^i", True
  Delay (0.75)
  'TimeOut (.75)
  SendKeys "$IM_" + S$, True
  SendKeys "{TAB}", True
  SendKeys "{TAB}", True
  SendKeys "{TAB}", True
  SendKeys " ", True
  Delay (2)
  'TimeOut (2)
  SendKeys "{ENTER}"
  SendKeys "^{F4}"
  Delay (2)
  Send ("Instant Messages are now " + S + ".")
End Sub

Sub waitforok()
Do
DoEvents
okw% = FindWindow("#32770", 0&)
DoEvents
If okw% <> 0 Then Exit Do
Loop
    OKB = FindChildByTitle(okw%, "OK")
    Okd = SendMessage(OKB, WM_LBUTTONDOWN, 0, 0&)
    oku = SendMessage(OKB, WM_LBUTTONUP, 0, 0&)
End Sub

