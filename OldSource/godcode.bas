Option Explicit

Function CommandNum (ByVal Keycode#, ByVal Cmd#) As Double
  CommandNum = Keycode * Cmd
End Function

Sub Decompile (ByVal Orig$, PIN%, Keycode%, Cmd#, Host$, Target$)
Dim X%, P%
  On Error GoTo DecomErr
  If Left$(Orig$, 1) = "~" Then
    X = 1
    P = X
    Do
      X = X + 1
    Loop Until (Mid$(Orig$, X, 1) = "!") Or (X >= Len(Orig$))
    If Mid$(Orig$, X, 1) = "!" Then
      PIN = Val(Mid$(Orig$, P + 1, X - P - 1))
      X = X + 1
      P = X
      Do
        X = X + 1
      Loop Until (Mid$(Orig$, X, 1) = ":") Or (X >= Len(Orig$))
      If Mid$(Orig$, X, 1) = ":" Then
        Keycode = Val(Mid$(Orig$, P, X - P))
        X = X + 1
        P = X
        Do
          X = X + 1
        Loop Until (Mid$(Orig$, X, 1) = "|") Or (X >= Len(Orig))
        If Mid$(Orig$, X, 1) = "|" Then
          Cmd = Val(Mid$(Orig$, P, X - P))
          'X = X + 1
          P = X + 1
          Do
            X = X + 1
          Loop Until (Mid$(Orig$, X, 1) = "`") Or (X >= Len(Orig))
          If Mid$(Orig$, X, 1) = "`" Then
            If P < X Then Host = (Mid$(Orig$, P, X - P))
            P = X + 1
            X = Len(Orig)
            If P < X Then Target = (Mid$(Orig$, P, X - P + 1))
          End If
          Exit Sub
        End If
      End If
    End If
  End If
DecomErr:
  PIN = -666
  Keycode = -666
  Cmd = -666
  Host$ = ""
  Target$ = ""
  Exit Sub
End Sub

Function SNKeyCode (ByVal SN As String, ByVal PIN As Integer) As Integer
Dim X%, K$, T1 As Double, T2 As Double, K1$
  K$ = ""
  For X = 1 To Len(SN)
    T2 = Asc(Mid$(SN, X, 1))
    T1 = Int(PIN * T2 * 2.05)
    'T1 = PIN * T2
    K$ = K$ + Trim(Str$(T1))
    'Stop
  Next X
  While Len(K$) > 4
    If Len(K$) Mod 2 = 0 Then
      X = 0
    Else
      X = -1
    End If
    K1$ = ""
    Do
      X = X + 2
      K1$ = K1$ + Mid$(K$, X, 1)
    Loop Until X >= Len(K$)
    K$ = K1$
  Wend
  K$ = Left$(K$, 4)
  SNKeyCode = Val(K$)
End Function

