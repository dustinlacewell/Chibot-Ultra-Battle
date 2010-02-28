Attribute VB_Name = "Sorter"
Option Explicit

Function SortType1(ByVal Ar As Variant) As Variant
Dim Closet1, Closet2 As Variant
Dim NewAr As Variant
Dim LCV1%, LCV2%
Dim Mn, Mx
  Mn = LBound(Ar, 1)
  Mx = UBound(Ar, 1)
  NewAr = Ar
  For LCV1% = Mn To Mx
    For LCV2% = LCV1% + 1 To Mx
      If Val(NewAr(LCV1, 1)) < Val(NewAr(LCV2, 1)) Then
        Closet1 = NewAr(LCV1, 0)
        Closet2 = NewAr(LCV1, 1)
        NewAr(LCV1, 0) = NewAr(LCV2, 0)
        NewAr(LCV1, 1) = NewAr(LCV2, 1)
        NewAr(LCV2, 0) = Closet1
        NewAr(LCV2, 1) = Closet2
      End If
    Next LCV2%
  Next LCV1%
  SortType1 = NewAr
End Function
