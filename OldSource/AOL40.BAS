
Dim DMF1 As Long
Dim DMF2 As Long
Dim DMF3 As Long

Option Explicit

Function AOLGetChat ()
Dim AOL%, f%, b%, Child%, GetTrim%, GetString%, TrimSpace$
AOL = FindWindow("AOL Frame25", 0&)
If AOL = 0 Then Exit Function
f = FindChildByClass(AOL, "MDIClient")
b = FindChatroom1()

Child = FindChildByClass(b, "RICHCNTL")

GetTrim = SendMessage(Child, 14, 0&, 0&)
TrimSpace$ = Space$(GetTrim)
GetString = SendMessage(Child, 13, GetTrim + 1, TrimSpace$)

AOLGetChat = GetString
End Function

Function AOLGetUser ()
Dim AOL%, MDI%, Welcome%, WelcomeLength%, WelcomeTitle$
Dim A%, User$
On Error Resume Next
AOL% = FindWindow("AOL Frame25", 0&)
MDI% = FindChildByClass(AOL%, "MDIClient")
Welcome% = FindChildByTitle(MDI%, "Welcome, ")
WelcomeLength% = GetWindowTextLength(Welcome%)
WelcomeTitle$ = String$(200, 0)
A% = GetWindowText(Welcome%, WelcomeTitle$, (WelcomeLength% + 1))
User = Mid$(WelcomeTitle$, 10, (InStr(WelcomeTitle$, "!") - 10))
AOLGetUser = User
End Function

Sub ChatSend2 (Text)
Dim AOL%, f%, b%, C%, FNL%, One1%, Two2%, Three3%, Four4%
Dim Five5%, Six6%, Seve7%, Eight8%, lSnd%, lEnter%
AOL = FindWindow("AOL Frame25", 0&)
If AOL = 0 Then Exit Sub
f = FindChildByClass(AOL, "MDIClient")
b = FindChatroom1()

start:
C = FindChildByClass(b, "RICHCNTL")
If C = 0 Then GoTo nextwnd
nextwnd:
FNL% = FindChildByClassX(b, "RICHCNTL")
One1% = GetWindow(FNL%, 2)
Two2% = GetWindow(One1%, 2)
Three3% = GetWindow(Two2%, 2)
Four4% = GetWindow(Three3%, 2)
Five5% = GetWindow(Four4%, 2)
Six6% = GetWindow(Five5%, 2)
Seve7% = GetWindow(Six6%, 2)
Eight8% = GetWindow(Seve7%, 2)

lSnd% = SendMessageS(Six6%, 12, Len(Text), Text)
Click (Seve7%)
lEnter% = SendMessageL(Six6%, WM_CHAR, 13, 0&)
lEnter% = SendMessageL(Six6%, WM_CHAR, 13, 0&)
End Sub

Function FindChildByClass (parentw, childhand)
Dim firs%, firss%, room%
firs% = GetWindow(parentw, 5)
If UCase(Mid(GetClass(firs%), 1, Len(childhand))) Like UCase(childhand) Then GoTo bone
firs% = GetWindow(parentw, GW_CHILD)
If UCase(Mid(GetClass(firs%), 1, Len(childhand))) Like UCase(childhand) Then GoTo bone

While firs%
firss% = GetWindow(parentw, 5)
If UCase(Mid(GetClass(firss%), 1, Len(childhand))) Like UCase(childhand) Then GoTo bone
firs% = GetWindow(firs%, 2)
If UCase(Mid(GetClass(firs%), 1, Len(childhand))) Like UCase(childhand) Then GoTo bone
Wend
FindChildByClass = 0

bone:
room% = firs%
FindChildByClass = room%

End Function

Function FindChildByClassX (parentw, childhand)
Dim firs%, firss%, room%
firs% = GetWindow(parentw, 5)
If UCase(Mid(GetClass(firs%), 1, Len(childhand))) Like UCase(childhand) And (firs% <> DMF1) And (firs% <> DMF2) And (firs% <> DMF3) Then GoTo bone2
firs% = GetWindow(parentw, GW_CHILD)
If UCase(Mid(GetClass(firs%), 1, Len(childhand))) Like UCase(childhand) And (firs% <> DMF1) And (firs% <> DMF2) And (firs% <> DMF3) Then GoTo bone2

While firs%
firss% = GetWindow(parentw, 5)
If UCase(Mid(GetClass(firss%), 1, Len(childhand))) Like UCase(childhand) And (firs% <> DMF1) And (firs% <> DMF2) And (firs% <> DMF3) Then GoTo bone2
firs% = GetWindow(firs%, 2)
If UCase(Mid(GetClass(firs%), 1, Len(childhand))) Like UCase(childhand) And (firs% <> DMF1) And (firs% <> DMF2) And (firs% <> DMF3) Then GoTo bone2
Wend
FindChildByClassX = 0

bone2:
room% = firs%
FindChildByClassX = room%
End Function

Function FindChildByTitle (parentw, childhand)
Dim firs%, firss%, room%
firs% = GetWindow(parentw, 5)
If UCase(GetCaption(firs%)) Like UCase(childhand) Then GoTo bone1
firs% = GetWindow(parentw, GW_CHILD)

While firs%
firss% = GetWindow(parentw, 5)
If UCase(GetCaption(firss%)) Like UCase(childhand) & "*" Then GoTo bone1
firs% = GetWindow(firs%, 2)
If UCase(GetCaption(firs%)) Like UCase(childhand) & "*" Then GoTo bone1
Wend
FindChildByTitle = 0

bone1:
room% = firs%
FindChildByTitle = room%
End Function

Function GetCaption (hWnd)
Dim hwndLength%, hwndTitle$, A%
hwndLength% = GetWindowTextLength(hWnd)
hwndTitle$ = String$(hwndLength%, 0)
A% = GetWindowText(hWnd, hwndTitle$, (hwndLength% + 1))

GetCaption = hwndTitle$
End Function

Function GetClass (Child)
Dim Buffer$, Getclas%
Buffer$ = String$(250, 0)
Getclas% = GetClassName(Child, Buffer$, 250)

GetClass = Buffer$
End Function

Sub pause (interval)
Dim Current As Long
Current = Timer
Do While Timer - Current < Val(interval)
DoEvents
Loop
End Sub

Sub runmenu (menu1 As Integer, menu2 As Integer)
Dim AOLWorks As Long
Static Working As Integer
Dim AOLMenus%, AOLSubMenu%, AOLItemID, ClickAOLMenu

AOLMenus% = GetMenu(FindWindow("AOL Frame25", 0&))
AOLSubMenu% = GetSubMenu(AOLMenus%, menu1)
AOLItemID = GetMenuItemID(AOLSubMenu%, menu2)
AOLWorks = CLng(0) * &H10000 Or Working
ClickAOLMenu = SendMessage(FindWindow("AOL Frame25", 0&), 273, AOLItemID, 0&)

End Sub

