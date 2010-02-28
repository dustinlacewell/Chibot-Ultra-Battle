VERSION 2.00
Begin Form fSortStatus 
   BackColor       =   &H00404040&
   BorderStyle     =   0  'None
   Caption         =   "Sorting..."
   ClientHeight    =   750
   ClientLeft      =   4365
   ClientTop       =   855
   ClientWidth     =   2175
   FillColor       =   &H00404040&
   Height          =   1155
   Left            =   4305
   LinkTopic       =   "Form1"
   ScaleHeight     =   750
   ScaleWidth      =   2175
   Top             =   510
   Width           =   2295
   Begin Label Label1 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Sorting Characters..."
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   12
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   735
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   2175
   End
End
Option Explicit

Sub Form_Load ()
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3             ' Always on top
  SortEm
  Me.Hide
End Sub

Sub QSort (l As Integer, r As Integer)
   '****************
   '**** quick sort
   '****************
   Dim i As Integer, j As Integer, m
   Dim H As CharType

   i = l
   j = r
   m = Senshi((l + r) / 2).FullName
    
   Do While i <= j
      DoEvents
      For i = i To r
         If Senshi(i).FullName >= m Then Exit For
      Next i
      For j = j To l Step -1
         If Senshi(j).FullName <= m Then Exit For
      Next j
      If i <= j Then
         H = Senshi(i)
         Senshi(i) = Senshi(j)
         Senshi(j) = H
         i = i + 1
         j = j - 1
      End If
   Loop
   If i < r Then Call QSort(i, r)
   If l < j Then Call QSort(l, j)

End Sub

Sub SortEm ()
Dim S As CharType
Dim x1, X2, X3 As Integer
Dim Pct!
  Me.Show
  QSort 1, NumSenshi
End Sub

