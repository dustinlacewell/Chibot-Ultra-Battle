Option Explicit

Sub QSort (l As Integer, r As Integer)
   '****************
   '**** quick sort
   '****************
   Dim i As Integer, j As Integer, m, h

   i = l
   j = r
   m = Senshi((l + r) / 2).FullName

   Do While i <= j

      For i = i To r
         If Senshi(i).FullName >= m Then Exit For
      Next i
      For j = j To l Step -1
         If Senshi(j).FullName <= m Then Exit For
      Next j
      If i <= j Then
         h = Senshi(i).FullName
         Senshi(i).FullName = Senshi(j).FullName
         Senshi(j).FullName = h
         i = i + 1
         j = j - 1
      End If
   Loop
   If i < r Then Call QSort(i, r)
   If l < j Then Call QSort(l, j)

End Sub

