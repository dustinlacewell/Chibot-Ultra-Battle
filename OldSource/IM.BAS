
Function AOLChild ()
Dim Aol%, MDI%, Child%
Aol% = FindWindow("AOL Frame25", 0&)
MDI% = FindChildByClass(Aol%, "MDIClient")
Child% = FindChildByClass(MDI%, "AOL Child")
AOLChild = Child%
End Function

Function AOLMDI ()
Aol = FindWindow("AOL Frame25", vbNullString)
AOLMDI = FindChildByClass(Aol, "MDIClient")
End Function

Sub center (formname As Form)
On Error Resume Next
formname.Top = (Screen.Height - formname.Height) / 2
formname.Left = (Screen.Width - formname.Width) / 2
End Sub

Sub Enter (edt%)
X = SendMessage(edt%, WM_CHAR, 13, 0)
End Sub

Function GetTextFromRICHCNTL (hWindow As Integer)
Aol% = FindWindow("AOL Frame25", 0&)
MDI% = FindChildByClass(Aol%, "MDIClient")
Msg$ = String$(255, 0)
XX% = SendMessage(hWindow, WM_ProGGer, 254, Msg$)
Msg$ = Trim$(Msg$)
GetTextFromRICHCNTL = Msg$

End Function

Sub hit_menu (Mnu_str$)
Aol% = FindWindow("AOL Frame25", 0&)
mnu% = GetMenu(Aol%)
MNU_Count% = GetMenuItemCount(mnu%)
For Top_Level% = 0 To MNU_Count% - 1
    Sub_Mnu% = GetSubMenu(mnu%, Top_Level%)
    Sub_Count% = GetMenuItemCount(Sub_Mnu%)
    For Sub_level% = 0 To Sub_Count% - 1
        Buff$ = Space$(50)
        junk% = GetMenuString(Sub_Mnu%, Sub_level%, Buff$, 50, MF_BYPOSITION)  'As Integer
        Buff$ = Trim$(Buff$): Buff$ = Left(Buff$, Len(Buff$) - 1)
        If Buff$ = "" Then Buff$ = " -"
        If InStr(Buff$, Mnu_str$) Then
            Mnu_ID% = GetMenuItemID(Sub_Mnu%, Sub_level%)
            junk% = SendMessage(Aol%, WM_COMMAND, Mnu_ID%, 0)
        End If
    Next Sub_level%
Next Top_Level%
End Sub

Sub hit_menu_by_string (Mnu_str$)
Aol% = FindWindow("AOL Frame25", 0&)
mnu% = GetMenu(Aol%)
MNU_Count% = GetMenuItemCount(mnu%)
For Top_Level% = 0 To MNU_Count% - 1
    Sub_Mnu% = GetSubMenu(mnu%, Top_Level%)
    Sub_Count% = GetMenuItemCount(Sub_Mnu%)
    For Sub_level% = 0 To Sub_Count% - 1
        Buff$ = Space$(50)
        junk% = GetMenuString(Sub_Mnu%, Sub_level%, Buff$, 50, MF_BYPOSITION)  'As Integer
        Buff$ = Trim$(Buff$): Buff$ = Left(Buff$, Len(Buff$) - 1)
        If Buff$ = "" Then Buff$ = " -"
        If InStr(Buff$, Mnu_str$) Then
            Mnu_ID% = GetMenuItemID(Sub_Mnu%, Sub_level%)
            junk% = SendMessage(Aol%, WM_COMMAND, Mnu_ID%, 0)
        End If
    Next Sub_level%
Next Top_Level%
End Sub

Sub IMControl (ByVal S$)
  AOLTitle ("America  Online")
  AppActivate "America  Online"
  SendKeys "^i", True
  Delay (.75)
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

Sub waitforok ()
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

