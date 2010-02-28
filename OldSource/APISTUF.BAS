Option Explicit

Declare Function FindWindow Lib "User" (ByVal p1$, ByVal p2&) As Integer

Declare Function AOLGetList Lib "311.DLL" (ByVal index%, ByVal Buf$) As Integer
Declare Function AOLGetCombo Lib "311.DLL" (ByVal index%, ByVal Buf$) As Integer

Function AOLCheck () As Integer
Dim X As Integer
Dim NullStr As String
Dim AOL As Integer
Dim MDI As Integer
Dim Child As Integer
Dim X2 As Integer
Dim Y As Integer
  If Findchatroom1() <> 0 Then
    AOLCheck = 444
    Exit Function
  End If
  AOL = FindWindow("AOL Frame25", 0&)       ' Find the AOL Frame25
  MDI = FindChildByClass(AOL, "MDIClient")    ' Find the MDI Client Chat Window
  Child = FindChildByClass(MDI, "AOL Child")
  X = FindChildByClass(Child, "_AOL_EDIT")    ' Finds the Chat Line
  Select Case X
    Case 0:
      X2 = FindChildByClass(Child, "RICHCNTL")
      If X2 <> 0 Then
        AOLCheck = 444
      Else
        'AOL = FindWindow("mIRC32", 0&)
        'MDI = FindChildByClass(AOL, "MDIClient")
        'Child = FindChildByClass(MDI, "channel")
        'x = FindChildByClass(Child, "Edit")
        'If x = 0 Then
          AOLCheck = 0
        'Else
        '  AOLCheck = 32
        'End If
      End If
    Case Else:
      AOLCheck = 333
  End Select
End Function

Sub AOLTitle (ByVal S As String)
Dim X As Integer
  X = FindWindow("AOL Frame25", 0&)
  SetWindowText X, S
End Sub

Sub Click (P As Integer)
' Simulates a mouse click on the current highlighted button
Dim X As Long
  X = SendMessageL(P, &H201, 0, 0)
  X = SendMessageL(P, &H202, 0, 0)
End Sub

Function CountPeeps () As Integer
Dim Frame25 As Integer
Dim MDIClient As Integer
Dim AOLChild As Integer
Dim ListBox As Integer
  'Frame25 = FindWindow("AOL Frame25", 0&)
  'MDIClient = FindChildByClass(Frame25, "MDIClient")
  AOLChild = Findchatroom1()
  ListBox = FindChildByClass(AOLChild, "_AOL_Listbox")
  CountPeeps = SendMessage(ListBox, LB_GETCOUNT, 0, 0)
End Function

Sub Delay (S As Single)
' Will cause a delay for S seconds while calling DoEvents to prevent system
' from halting.
Dim X As Single
  X = Timer + S
  Do While X >= Timer
    DoEvents
  Loop
End Sub

Function Findchatroom1 () As Integer
Dim AOL%
Dim B%, OldB%
Dim C%, D%, E%
Dim MDI%
Dim Ver%
Dim P$, X%, Y%
'Finds the handle of the AOL Chatroom by looking for a
'Window with a ListBox (Chat ScreenNames), Edit Box,
'(Where you type chat text), and an _AOL_VIEW.  If another
'AOL window is present that also has these 3 controls, it
'may find the wrong window.  I have never seen another AOL
'window with these 3 controls at once

AOL = FindWindow("AOL Frame25", 0&)
If AOL = 0 Then Exit Function
MDI% = FindChildByClass(AOL, "MDIClient")
If MDI = 0 Then Exit Function
B = FindChildByClass(MDI, "AOL Child")

Ver = 444
start:
If Ver = 444 Then
  C = FindChildByClass(B, "RICHCNTL")
  If C = 0 Then GoTo NextWnd
Else
  C = FindChildByClass(B, "_AOL_VIEW")
  If C = 0 Then GoTo NextWnd
  D = FindChildByClass(B, "_AOL_EDIT")
  If D = 0 Then GoTo NextWnd
End If
E = FindChildByClass(B, "_AOL_LISTBOX")
If E = 0 Then GoTo NextWnd
'We've found it
P$ = GetCaption(B)
OldB = B
'If GodMode1 = False Then
'  If Left$(P$, 25) = "Arts and Entertainment - " Then B = 0
'  If Left$(P$, 14) = "Town Square - " Then B = 0
'  If Left$(P$, 10) = "Friends - " Then B = 0
'  If Left$(P$, 7) = "Life - " Then B = 0
'  If Left$(P$, 28) = "News, Sports, and Finance - " Then B = 0
'  If Left$(P$, 9) = "Places - " Then B = 0
'  If Left$(P$, 10) = "Romance - " Then B = 0
'  If Left$(P$, 20) = "Special Interests - " Then B = 0
'  If Left$(P$, 10) = "Germany - " Then B = 0
'  If Left$(P$, 20) = "The UK Experience - " Then B = 0
'  If Left$(P$, 9) = "France - " Then B = 0
'  If Left$(P$, 8) = "Japan - " Then B = 0
'  If Left$(P$, 9) = "Canada - " Then B = 0
'  If Left$(P$, 12) = "Australia - " Then B = 0
'  If P$ = "SM Battle" Then B = 0
'End If
Findchatroom1 = B
Exit Function

NextWnd:
B = getnextwindow(B, 2)
If B = Getwindow(B, GW_HWNDLAST) Then Exit Function
GoTo start


End Function

Function GetLineOfChat () As Variant
' Returns the bottom line of chat from the chat window
Dim S As String
Dim Frame25 As Integer
Dim MDIClient As Integer
Dim Parent As Integer
Dim View As Integer
Dim X1, X2 As Long
  Select Case AOLCheck()
    Case 333:
    'Frame25 = FindWindow("AOL FRAME25", 0&)             ' Finds AOL Frame25
    'MDIClient = FindChildByClass(Frame25, "MDIClient")  ' Finds the MDI Client Chat Window
    'Parent = FindChildByClass(MDIClient, "AOL Child")
    Parent = Findchatroom1()
    View = FindChildByClass(Parent, "_AOL_VIEW")        ' Goes to the AOL Chat Window
    X1 = SendMessageL(View, 14, 0, 0)                   ' Prepares to get a line of chat
    S = String(X1 + 200, 0)                             ' X1 is the length of the string
    X2 = SendMessageS(View, 13, X1, S)                  ' S now contains the line of chat
    Case 444:
    'Frame25 = FindWindow("AOL Frame25", 0&)
    'MDIClient = FindChildByClass(Frame25, "MDIClient")
    'Parent = FindChildByClass(MDIClient, "AOL Child")
    Parent = Findchatroom1()
    View = FindChildByClass(Parent, "RICHCNTL")
    X1 = SendMessageL(View, 14, 0, 0)                   ' Prepares to get a line of chat
    S = Space$(X1)                             ' X1 is the length of the string
    X2 = SendMessageS(View, 13, X1 + 1, S)                ' S now contains the line of chat
  End Select
  GetLineOfChat = S
End Function

Function InRoom (ByVal SN$) As Integer
Dim X%, Room%, List%, N$, Tv%
  Room = Findchatroom1()
  If Room = 0 Then Exit Function
  List = FindChildByClass(Room, "_AOL_LISTBOX")
  X = SendMessage(List, WM_SETFOCUS, 0, 0)
  N$ = Space$(10)
  For X = 0 To 23
    Tv% = AOLGetList(X, N$)
    If N$ = SN$ Then
      InRoom = True
      Exit Function
    End If
  Next X
  InRoom = False
End Function

Sub ScrollSend (ByVal S As String)
  DoEvents
  If Len(Trim(StripHTML(S))) > 130 Then
    'ScrollSend ("<font color=#FF0000><b>WARNING: Next line too long, truncated")
    ScrollSend (Left$(S, 130))
    S = Right$(S, Len(S) - 130)
    ScrollSend (S)
    Exit Sub
  End If
  If (OutPtr > outmax) Then OutPtr = outmax
  OutRay(OutPtr) = Trim(S)
  OutPtr = OutPtr + 1
  DoEvents
  If (OutPtr > outmax) Then
    OutPtr = 1
  End If
  'Delay (.25)
End Sub

Sub ScrollSend1 (ByVal S As String)
' Sends a line of text to the AOL chat window without a care of scrolling.
Dim X, X2 As Integer
Dim Z As Long
Dim T As Long
Dim s2 As String
Dim Ns As String
Dim LCV As Integer
  DoEvents
  s2 = StripHiAscii(S)
  If (s2 = "") Or (s2 = "0") Then Exit Sub
  If (Findchatroom1() = 0) And (AOLCheck() <> 32) Then
    s2 = StripHTML(s2)
    'If LineCount(fOffline!tDebug) >= 28 Then fOffline!tDebug = ""
    Z = Len(fOffline!tDebug.Text)
    If Z > 15000 Then fOffline!tDebug.Text = Right$(fOffline!tDebug.Text, 10000)
    fOffline!tDebug.ForeColor = 0
    fOffline!tDebug = fOffline!tDebug + s2 + Chr$(13) + Chr$(10)
    fOffline!tDebug.SelStart = Len(fOffline!tDebug.Text)
    fOffline!tDebug.ForeColor = RGB(255, 255, 255)
  ElseIf AOLCheck() = 32 Then
    s2 = StripHTML(s2)
      Ns = ""
      LCV = 0
      Do
        Do
          LCV = LCV + 1
          Ns = Ns + Mid$(s2, LCV, 1)
        Loop Until Mid$(s2, LCV, 1) = "|" Or LCV >= Len(s2)
        If Mid$(s2, LCV, 1) = "|" Then Ns = Mid$(Ns, 1, Len(Ns) - 1)
        X = FindWindow("mIRC32", 0&)       ' Find the AOL Frame25
        X = FindChildByClass(X, "MDIClient")    ' Find the MDI Client Chat Window
        X = FindChildByClass(X, "channel")
        'X = FindChatroom1()
        X = FindChildByClass(X, "Edit")    ' Finds the Chat Line
        Z = SendMessageS(X, 12, Len(Ns), Ns)       ' Writes the stuff to be sent to the chat line
        Z = SendMessage(X, wm_char, 13, 13)
        Ns = ""
      Loop Until LCV >= Len(s2)
  Else Select Case AOLCheck()
    Case 333:
      s2 = StripHTML(s2)
      Ns = ""
      LCV = 0
      Do
        Do
          LCV = LCV + 1
          Ns = Ns + Mid$(s2, LCV, 1)
        Loop Until Mid$(s2, LCV, 1) = "|" Or LCV >= Len(s2)
        If Mid$(s2, LCV, 1) = "|" Then Ns = Mid$(Ns, 1, Len(Ns) - 1)
        'X = FindWindow("AOL FRAME25", 0&)       ' Find the AOL Frame25
        'X = FindChildByClass(X, "MDIClient")    ' Find the MDI Client Chat Window
        'X = FindChildByClass(X, "AOL Child")
        X = Findchatroom1()
        X = FindChildByClass(X, "_AOL_EDIT")    ' Finds the Chat Line
        Z = SendMessageS(X, 12, Len(Ns), Ns)       ' Writes the stuff to be sent to the chat line
        X2 = getnextwindow(X, 2)                ' Tab over to the Send button
        Delay (0)
        Click (X2)                              ' Click the Send button
        Ns = ""
      Loop Until LCV >= Len(s2)
    Case 444:
      s2 = "<font face=""" + Trim(StripHiAscii(Config.FontName)) + """ color=""#" + Trim(StripHiAscii(Config.FontColor)) + """>" + s2
      ChatSend2 (s2)
  End Select
  End If
  If Trim(Config.WLog) <> "" Then LogFileWrite (StripHTML(s2))
End Sub

Sub Send (ByVal S As String)
' Will delay .75 seconds after sending.
  ScrollSend (S)
  DoEvents
  'If (Not DebugWindow) Then Delay (.75)
End Sub

Sub Send2 (ByVal S As String, ByVal T As Single)
' Will delay T seconds after sending.
  ScrollSend (S)
  'If (Not DebugWindow) Then Delay (T)
End Sub

Sub Send5 (ByVal S As String)
' Will delay .7 seconds after sending.
  ScrollSend (S)
  'If (Not DebugWindow) Then Delay (.7)
End Sub

Sub TestingKUB ()
Dim Ds As Integer
  Ds = 0
  Stop
  Do
    ScrollSend "Ignore me, I'm just testing KUB."
    Delay (.5)
  Loop Until (Ds = 1)
End Sub

