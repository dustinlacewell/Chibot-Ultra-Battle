VERSION 5.00
Begin VB.Form fSortStatus 
   Appearance      =   0  'Flat
   BackColor       =   &H00404040&
   BorderStyle     =   0  'None
   Caption         =   "Sorting..."
   ClientHeight    =   750
   ClientLeft      =   4365
   ClientTop       =   855
   ClientWidth     =   2175
   FillColor       =   &H00404040&
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H80000008&
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   750
   ScaleWidth      =   2175
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Sorting Characters..."
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   735
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   2175
   End
End
Attribute VB_Name = "fSortStatus"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()
  SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3             ' Always on top
  SortEm
  ScrollSend1 ("Sort complete!")
  Me.Hide
End Sub

Private Sub QSort(L As Integer, R As Integer)
   '****************
   '**** quick sort
   '****************
   Dim I As Integer, j As Integer, M
   Dim H As CharType

   I = L
   j = R
   M = Senshi((L + R) / 2).FullName
    
   Do While I <= j
      DoEvents
      For I = I To R
         If Senshi(I).FullName >= M Then Exit For
      Next I
      For j = j To L Step -1
         If Senshi(j).FullName <= M Then Exit For
      Next j
      If I <= j Then
         H = Senshi(I)
         Senshi(I) = Senshi(j)
         Senshi(j) = H
         I = I + 1
         j = j - 1
      End If
   Loop
   If I < R Then Call QSort(I, R)
   If L < j Then Call QSort(L, j)

End Sub

Private Sub SortEm()
Dim S As CharType
Dim x1, X2, X3 As Integer
Dim Pct!
  Me.Show
  QSort 1, HighSenshi
End Sub

