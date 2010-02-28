VERSION 2.00
Begin Form Form1 
   BackColor       =   &H00000000&
   BorderStyle     =   3  'Fixed Double
   Caption         =   "Rot13"
   ClientHeight    =   840
   ClientLeft      =   1095
   ClientTop       =   1485
   ClientWidth     =   4665
   Height          =   1245
   Left            =   1035
   LinkTopic       =   "Form1"
   ScaleHeight     =   840
   ScaleWidth      =   4665
   Top             =   1140
   Width           =   4785
   Begin TextBox Label1 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   0
      TabIndex        =   1
      Top             =   360
      Width           =   4695
   End
   Begin TextBox Text1 
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   285
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   4695
   End
End
Option Explicit

Sub Form_Unload (Cancel As Integer)
  End
End Sub

Sub Text1_Change ()
  Label1.Text = Rot13(Text1.Text)
End Sub

