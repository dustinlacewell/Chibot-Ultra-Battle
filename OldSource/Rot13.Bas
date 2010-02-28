Option Explicit

Function Rot13 (ByVal In$) As String
Dim Out$, x%, A%, H%, T%, O%, N%
  Out = ""
  For x = 1 To Len(In)
    A = Asc(Mid$(In, x, 1))
    H = A \ 100
    T = A \ 10
    While (T >= 10)
      T = T \ 10
    Wend
    O = A Mod 10
    N = O * 10 + H * 10 + T
    While (N < 32)
      N = N + 32
    Wend
    While (N > 127)
      N = N - 32
    Wend
    Out = Out + Chr$(N)
  Next x
  Rot13 = Out
End Function

