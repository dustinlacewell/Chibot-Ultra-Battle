'Attribute VB_Name = "Encryption"
Option Explicit

Function Rot13 (ByVal Npt$) As String
Dim Out$, X%, a%, H%, T%, O%, N%
  Out = ""
  For X = 1 To Len(Npt)
    a = Asc(Mid$(Npt, X, 1))
    H = a \ 100
    T = a \ 10
    While (T >= 10)
      T = T \ 10
    Wend
    O = a Mod 10
    N = O * 10 + H * 10 + T
    While (N < 32)
      N = N + 32
    Wend
    While (N > 127)
      N = N - 32
    Wend
    Out = Out + Chr$(N)
  Next X
  Rot13 = Out
End Function

