Option Explicit

Declare Function FindWindow Lib "User" (ByVal p1$, ByVal p2&) As Integer

Function AOLCheck () As Integer
Dim X As Integer
Dim NullStr As String
Dim AOL As Integer
Dim MDI As Integer
Dim Child As Integer
Dim X2 As Integer
Dim Y As Integer
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
  AOLChild = FindChatroom1()
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

Function FindChatroom1 () As Integer
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

Ver = AOLCheck()
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
FindChatroom1 = B
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
    Parent = FindChatroom1()
    View = FindChildByClass(Parent, "_AOL_VIEW")        ' Goes to the AOL Chat Window
    X1 = SendMessageL(View, 14, 0, 0)                   ' Prepares to get a line of chat
    S = String(X1 + 200, 0)                             ' X1 is the length of the string
    X2 = SendMessageS(View, 13, X1, S)                  ' S now contains the line of chat
    Case 444:
    'Frame25 = FindWindow("AOL Frame25", 0&)
    'MDIClient = FindChildByClass(Frame25, "MDIClient")
    'Parent = FindChildByClass(MDIClient, "AOL Child")
    Parent = FindChatroom1()
    View = FindChildByClass(Parent, "RICHCNTL")
    X1 = SendMessageL(View, 14, 0, 0)                   ' Prepares to get a line of chat
    S = Space$(X1)                             ' X1 is the length of the string
    X2 = SendMessageS(View, 13, X1 + 1, S)                ' S now contains the line of chat
  End Select
  GetLineOfChat = S
End Function

Sub ScrollSend1 (ByVal S As String)
' Sends a line of text to the AOL chat window without a care of scrolling.
Dim X, X2 As Integer
Dim Z As Long
Dim T As Long
Dim S2 As String
Dim Ns As String
Dim LCV As Integer
  DoEvents
  S2 = StripHiAscii(S)
  If (S2 = "") Or (S2 = "0") Then Exit Sub
  If (FindChatroom1() = 0) And (AOLCheck() <> 32) Then
    'S2 = StripHTML(S2)
    'If LineCount(fDebug!tDebug) >= 28 Then fDebug!tDebug = ""
    'fDebug!tDebug = fDebug!tDebug + S2 + Chr$(13) + Chr$(10)
  ElseIf AOLCheck() = 32 Then
    'S2 = StripHTML(S2)
      Ns = ""
      LCV = 0
      Do
        Do
          LCV = LCV + 1
          Ns = Ns + Mid$(S2, LCV, 1)
        Loop Until Mid$(S2, LCV, 1) = "|" Or LCV >= Len(S2)
        If Mid$(S2, LCV, 1) = "|" Then Ns = Mid$(Ns, 1, Len(Ns) - 1)
        X = FindWindow("mIRC32", 0&)       ' Find the AOL Frame25
        X = FindChildByClass(X, "MDIClient")    ' Find the MDI Client Chat Window
        X = FindChildByClass(X, "channel")
        'X = FindChatroom1()
        X = FindChildByClass(X, "Edit")    ' Finds the Chat Line
        Z = SendMessageS(X, 12, Len(Ns), Ns)       ' Writes the stuff to be sent to the chat line
        Z = SendMessage(X, wm_char, 13, 13)
        Ns = ""
      Loop Until LCV >= Len(S2)
  Else Select Case AOLCheck()
    Case 333:
      'S2 = StripHTML(S2)
      Ns = ""
      LCV = 0
      Do
        Do
          LCV = LCV + 1
          Ns = Ns + Mid$(S2, LCV, 1)
        Loop Until Mid$(S2, LCV, 1) = "|" Or LCV >= Len(S2)
        If Mid$(S2, LCV, 1) = "|" Then Ns = Mid$(Ns, 1, Len(Ns) - 1)
        'X = FindWindow("AOL FRAME25", 0&)       ' Find the AOL Frame25
        'X = FindChildByClass(X, "MDIClient")    ' Find the MDI Client Chat Window
        'X = FindChildByClass(X, "AOL Child")
        X = FindChatroom1()
        X = FindChildByClass(X, "_AOL_EDIT")    ' Finds the Chat Line
        Z = SendMessageS(X, 12, Len(Ns), Ns)       ' Writes the stuff to be sent to the chat line
        X2 = getnextwindow(X, 2)                ' Tab over to the Send button
        Delay (0)
        Click (X2)                              ' Click the Send button
        Ns = ""
      Loop Until LCV >= Len(S2)
    Case 444:
      'S2 = "<font face=""" + Trim(StripHiAscii(Config.FontName)) + """ color=""#" + Trim(StripHiAscii(Config.FontColor)) + """>" + S2
      ChatSend2 (S2)
  End Select
  End If
  'If Trim(Config.WLog) <> "" Then LogFileWrite (StripHTML(S2))
End Sub

Function StripHiAscii (ByVal S1 As String) As String
Dim X As Integer
Dim S2 As String
  S2 = ""
  For X = 1 To Len(S1)
    If Asc(Mid$(S1, X, 1)) >= 32 And Asc(Mid$(S1, X, 1)) <= 126 Then S2 = S2 + Mid$(S1, X, 1)
  Next X
  StripHiAscii = S2
End Function

