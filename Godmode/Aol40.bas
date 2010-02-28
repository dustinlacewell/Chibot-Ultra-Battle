Attribute VB_Name = "Aol40"
Declare Function FindWindow Lib "User32" Alias "FindWindowA" (ByVal lpClassName As String, ByVal lpWindowName As String) As Long
' GetWindow() Constants
Public Const GW_HWNDFIRST = 0
Public Const GW_HWNDLAST = 1
Public Const GW_HWNDNEXT = 2
Public Const GW_HWNDPREV = 3
Public Const GW_OWNER = 4
Public Const GW_CHILD = 5
Public Const GW_MAX = 5

' Window Messages
Public Const WM_NULL = &H0
Public Const WM_CREATE = &H1
Public Const WM_DESTROY = &H2
Public Const WM_MOVE = &H3
Public Const WM_SIZE = &H5

Public Const WM_ACTIVATE = &H6
'
'  WM_ACTIVATE state values

Public Const WA_INACTIVE = 0
Public Const WA_ACTIVE = 1
Public Const WA_CLICKACTIVE = 2

Public Const WM_SETFOCUS = &H7
Public Const WM_KILLFOCUS = &H8
Public Const WM_ENABLE = &HA
Public Const WM_SETREDRAW = &HB
Public Const WM_SETTEXT = &HC
Public Const WM_GETTEXT = &HD
Public Const WM_GETTEXTLENGTH = &HE
Public Const WM_PAINT = &HF
Public Const WM_CLOSE = &H10
Public Const WM_QUERYENDSESSION = &H11
Public Const WM_QUIT = &H12
Public Const WM_QUERYOPEN = &H13
Public Const WM_ERASEBKGND = &H14
Public Const WM_SYSCOLORCHANGE = &H15
Public Const WM_ENDSESSION = &H16
Public Const WM_SHOWWINDOW = &H18
Public Const WM_WININICHANGE = &H1A
Public Const WM_DEVMODECHANGE = &H1B
Public Const WM_ACTIVATEAPP = &H1C
Public Const WM_FONTCHANGE = &H1D
Public Const WM_TIMECHANGE = &H1E
Public Const WM_CANCELMODE = &H1F
Public Const WM_SETCURSOR = &H20
Public Const WM_MOUSEACTIVATE = &H21
Public Const WM_CHILDACTIVATE = &H22
Public Const WM_QUEUESYNC = &H23

Public Const WM_GETMINMAXINFO = &H24

Public Const WM_PAINTICON = &H26
Public Const WM_ICONERASEBKGND = &H27
Public Const WM_NEXTDLGCTL = &H28
Public Const WM_SPOOLERSTATUS = &H2A
Public Const WM_DRAWITEM = &H2B
Public Const WM_MEASUREITEM = &H2C
Public Const WM_DELETEITEM = &H2D
Public Const WM_VKEYTOITEM = &H2E
Public Const WM_CHARTOITEM = &H2F
Public Const WM_SETFONT = &H30
Public Const WM_GETFONT = &H31
Public Const WM_SETHOTKEY = &H32
Public Const WM_GETHOTKEY = &H33
Public Const WM_QUERYDRAGICON = &H37
Public Const WM_COMPAREITEM = &H39
Public Const WM_COMPACTING = &H41
Public Const WM_OTHERWINDOWCREATED = &H42               '  no longer suported
Public Const WM_OTHERWINDOWDESTROYED = &H43             '  no longer suported
Public Const WM_COMMNOTIFY = &H44                       '  no longer suported

' notifications passed in low word of lParam on WM_COMMNOTIFY messages
Public Const CN_RECEIVE = &H1
Public Const CN_TRANSMIT = &H2
Public Const CN_EVENT = &H4

Public Const WM_WINDOWPOSCHANGING = &H46
Public Const WM_WINDOWPOSCHANGED = &H47

Public Const WM_POWER = &H48
'
'  wParam for WM_POWER window message and DRV_POWER driver notification

Public Const PWR_OK = 1
Public Const PWR_FAIL = (-1)
Public Const PWR_SUSPENDREQUEST = 1
Public Const PWR_SUSPENDRESUME = 2
Public Const PWR_CRITICALRESUME = 3

Public Const WM_COPYDATA = &H4A
Public Const WM_CANCELJOURNAL = &H4B

Type COPYDATASTRUCT
        dwData As Long
        cbData As Long
        lpData As Long
End Type

Public Const WM_NCCREATE = &H81
Public Const WM_NCDESTROY = &H82
Public Const WM_NCCALCSIZE = &H83
Public Const WM_NCHITTEST = &H84
Public Const WM_NCPAINT = &H85
Public Const WM_NCACTIVATE = &H86
Public Const WM_GETDLGCODE = &H87
Public Const WM_NCMOUSEMOVE = &HA0
Public Const WM_NCLBUTTONDOWN = &HA1
Public Const WM_NCLBUTTONUP = &HA2
Public Const WM_NCLBUTTONDBLCLK = &HA3
Public Const WM_NCRBUTTONDOWN = &HA4
Public Const WM_NCRBUTTONUP = &HA5
Public Const WM_NCRBUTTONDBLCLK = &HA6
Public Const WM_NCMBUTTONDOWN = &HA7
Public Const WM_NCMBUTTONUP = &HA8
Public Const WM_NCMBUTTONDBLCLK = &HA9

Public Const WM_KEYFIRST = &H100
Public Const WM_KEYDOWN = &H100
Public Const WM_KEYUP = &H101
Public Const WM_CHAR = &H102
Public Const WM_DEADCHAR = &H103
Public Const WM_SYSKEYDOWN = &H104
Public Const WM_SYSKEYUP = &H105
Public Const WM_SYSCHAR = &H106
Public Const WM_SYSDEADCHAR = &H107
Public Const WM_KEYLAST = &H108
Public Const WM_INITDIALOG = &H110
Public Const WM_COMMAND = &H111
Public Const WM_SYSCOMMAND = &H112
Public Const WM_TIMER = &H113
Public Const WM_HSCROLL = &H114
Public Const WM_VSCROLL = &H115
Public Const WM_INITMENU = &H116
Public Const WM_INITMENUPOPUP = &H117
Public Const WM_MENUSELECT = &H11F
Public Const WM_MENUCHAR = &H120
Public Const WM_ENTERIDLE = &H121

Public Const WM_CTLCOLORMSGBOX = &H132
Public Const WM_CTLCOLOREDIT = &H133
Public Const WM_CTLCOLORLISTBOX = &H134
Public Const WM_CTLCOLORBTN = &H135
Public Const WM_CTLCOLORDLG = &H136
Public Const WM_CTLCOLORSCROLLBAR = &H137
Public Const WM_CTLCOLORSTATIC = &H138

Public Const WM_MOUSEFIRST = &H200
Public Const WM_MOUSEMOVE = &H200
Public Const WM_LBUTTONDOWN = &H201
Public Const WM_LBUTTONUP = &H202
Public Const WM_LBUTTONDBLCLK = &H203
Public Const WM_RBUTTONDOWN = &H204
Public Const WM_RBUTTONUP = &H205
Public Const WM_RBUTTONDBLCLK = &H206
Public Const WM_MBUTTONDOWN = &H207
Public Const WM_MBUTTONUP = &H208
Public Const WM_MBUTTONDBLCLK = &H209
Public Const WM_MOUSELAST = &H209

Public Const WM_PARENTNOTIFY = &H210
Public Const WM_ENTERMENULOOP = &H211
Public Const WM_EXITMENULOOP = &H212
Public Const WM_MDICREATE = &H220
Public Const WM_MDIDESTROY = &H221
Public Const WM_MDIACTIVATE = &H222
Public Const WM_MDIRESTORE = &H223
Public Const WM_MDINEXT = &H224
Public Const WM_MDIMAXIMIZE = &H225
Public Const WM_MDITILE = &H226
Public Const WM_MDICASCADE = &H227
Public Const WM_MDIICONARRANGE = &H228
Public Const WM_MDIGETACTIVE = &H229
Public Const WM_MDISETMENU = &H230
Public Const WM_DROPFILES = &H233
Public Const WM_MDIREFRESHMENU = &H234


Public Const WM_CUT = &H300
Public Const WM_COPY = &H301
Public Const WM_PASTE = &H302
Public Const WM_CLEAR = &H303
Public Const WM_UNDO = &H304
Public Const WM_RENDERFORMAT = &H305
Public Const WM_RENDERALLFORMATS = &H306
Public Const WM_DESTROYCLIPBOARD = &H307
Public Const WM_DRAWCLIPBOARD = &H308
Public Const WM_PAINTCLIPBOARD = &H309
Public Const WM_VSCROLLCLIPBOARD = &H30A
Public Const WM_SIZECLIPBOARD = &H30B
Public Const WM_ASKCBFORMATNAME = &H30C
Public Const WM_CHANGECBCHAIN = &H30D
Public Const WM_HSCROLLCLIPBOARD = &H30E
Public Const WM_QUERYNEWPALETTE = &H30F
Public Const WM_PALETTEISCHANGING = &H310
Public Const WM_PALETTECHANGED = &H311
Public Const WM_HOTKEY = &H312

Public Const WM_PENWINFIRST = &H380
Public Const WM_PENWINLAST = &H38F


Dim DMF1 As Long
Dim DMF2 As Long
Dim DMF3 As Long

Option Explicit

Function AOLGetChat()
Dim AOL%, F%, B%, Child%, GetTrim%, GetString%, TrimSpace$
AOL = FindWindow("AOL Frame25", 0&)
If AOL = 0 Then Exit Function
F = FindChildByClass(AOL, "MDIClient")
B = FindChatroom1()

Child = FindChildByClass(B, "RICHCNTL")

GetTrim = SendMessage(Child, 14, 0&, 0&)
TrimSpace$ = Space$(GetTrim)
GetString = SendMessage(Child, 13, GetTrim + 1, TrimSpace$)

AOLGetChat = GetString
End Function

Function AOLGetUser()
Dim AOL%, MDI%, Welcome%, WelcomeLength%, WelcomeTitle$
Dim a%, User$
On Error Resume Next
AOL% = FindWindow("AOL Frame25", vbNullString)
MDI% = FindChildByClass(AOL%, "MDIClient")
Welcome% = FindChildByTitle(MDI%, "Welcome, ")
WelcomeLength% = GetWindowTextLength(Welcome%)
WelcomeTitle$ = String$(200, 0)
a% = GetWindowText(Welcome%, WelcomeTitle$, (WelcomeLength% + 1))
User = Mid$(WelcomeTitle$, 10, (InStr(WelcomeTitle$, "!") - 10))
AOLGetUser = User
End Function

Sub ChatSend2(Text)
Dim AOL%, F%, B%, C%, FNL%, One1%, Two2%, Three3%, Four4%
Dim Five5%, Six6%, Seve7%, Eight8%, lSnd%, lEnter%
AOL = FindWindow("AOL Frame25", vbNullString)
If AOL = 0 Then Exit Sub
F = FindChildByClass(AOL, "MDIClient")
B = FindChatroom1()

start:
C = FindChildByClass(B, "RICHCNTL")
If C = 0 Then Exit Sub
'DMF1 = C
'FNL% = FindChildByClassX(b, "RICHCNTL")
One1% = GetWindow(C, 2)
Two2% = GetWindow(One1%, 2)
Three3% = GetWindow(Two2%, 2)
Four4% = GetWindow(Three3%, 2)
Five5% = GetWindow(Four4%, 2)
Six6% = GetWindow(Five5%, 2)
Seve7% = GetWindow(Six6%, 2)
Eight8% = GetWindow(Seve7%, 2)

lSnd% = SendMessageByString(Six6%, 12, 0, Text)
'lSnd% = SendMessage(Six6%, WM_SETTEXT, Len(Text), Text)
'lSnd% = SetWindowText(Six6%, Text)
Click (Seve7%)
lEnter% = SendMessage(Six6%, WM_CHAR, 13, 0&)
lEnter% = SendMessage(Six6%, WM_CHAR, 13, 0&)
End Sub

Function FindChildByClass(parentw, childhand)
Dim firs%, firss%, Room%
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
Room% = firs%
FindChildByClass = Room%

End Function

Function FindChildByClassX(parentw, childhand)
Dim firs%, firss%, Room%
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
Room% = firs%
FindChildByClassX = Room%
End Function

Function FindChildByTitle(parentw, childhand)
Dim firs%, firss%, Room%
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
Room% = firs%
FindChildByTitle = Room%
End Function

Function GetCaption(hwnd)
Dim hwndLength%, hwndTitle$, a%
hwndLength% = GetWindowTextLength(hwnd)
hwndTitle$ = String$(hwndLength%, 0)
a% = GetWindowText(hwnd, hwndTitle$, (hwndLength% + 1))

GetCaption = hwndTitle$
End Function

Function GetClass(Child)
Dim Buffer$, Getclas%
Buffer$ = String$(250, 0)
Getclas% = GetClassName(Child, Buffer$, 250)

GetClass = Buffer$
End Function

Sub pause(interval)
Dim Current As Long
Current = Timer
Do While Timer - Current < Val(interval)
DoEvents
Loop
End Sub

Sub runmenu(menu1 As Integer, menu2 As Integer)
Dim AOLWorks As Long
Static Working As Integer
Dim AOLMenus%, AOLSubMenu%, AOLItemID, ClickAOLMenu

AOLMenus% = GetMenu(FindWindow("AOL Frame25", 0&))
AOLSubMenu% = GetSubMenu(AOLMenus%, menu1)
AOLItemID = GetMenuItemID(AOLSubMenu%, menu2)
AOLWorks = CLng(0) * &H10000 Or Working
ClickAOLMenu = SendMessage(FindWindow("AOL Frame25", 0&), 273, AOLItemID, 0&)

End Sub


