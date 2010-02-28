Option Explicit

Function GetStr (ByVal FileNum%) As String
Dim x%, C$, Ln%, O$
  O$ = ""
  C$ = ""
  Get #FileNum, , Ln%
  O$ = String$(Ln%, 0)
  'For x = 1 To Ln%
  '  Get #FileNum, , C$
  '  O$ = O$ + C$
  'Next x
  Get #FileNum, , O$
  GetStr = O$
End Function

Sub PutStr (ByVal FileNum%, ByVal O$)
Dim x%, Ln%, C As String * 1
  C$ = ""
  Ln = Len(O$)
  Put #FileNum, , Ln%
  'For x = 1 To Ln%
  '  C$ = Mid$(O$, x, 1)
  '  Put #FileNum, , C$
  'Next x
  Put #FileNum, , O$
End Sub

Sub Writeln (ByVal FilNum%, ByVal S As Variant)
Dim x%
Dim C$, Out$
  If ErrorTrap = True Then On Error GoTo sWL_Error
  ' Make compatible with other Variants!!!
  Out = Format$(S)
  For x% = 1 To Len(Out$)
    C$ = Mid$(Out$, x, 1)
    Put FilNum, , C$
  Next x%
  C$ = Chr$(13)
  Put FilNum, , C$
  C$ = Chr$(10)
  Put FilNum, , C$
  Exit Sub
sWL_Error:
  kDlgBox Error$, Erl, "Writeln Error"
  Exit Sub
End Sub

Function Readln (ByVal FilNum%) As Variant
Dim x%
Dim S$, C As String * 1
  If ErrorTrap = True Then On Error GoTo sRL_Error Else On Error GoTo 0
  ' Make compatible with other Variants!!!
  S$ = ""
  Do
    DoEvents
    Get FilNum, , C$
    If (C$ <> Chr$(13)) Then S$ = S$ + C$
  Loop Until (C$ = Chr$(13)) Or (EOF(FilNum))
  'Get FilNum, , C$
  'If Left$(S$, 1) = """" And Right$(S$, 1) = """" Then S$ = Mid$(S$, 2, Len(S$) - 2):
  Readln = S$
  Exit Function
sRL_Error:
  kDlgBox Error$, Erl, "Readln Error"
  Exit Function
End Function

