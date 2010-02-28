VERSION 5.00
Object = "{248DD890-BB45-11CF-9ABC-0080C7E7B78D}#1.0#0"; "MSWINSCK.OCX"
Begin VB.Form Form1 
   ClientHeight    =   3195
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   ScaleHeight     =   3195
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox Text4 
      BackColor       =   &H00C0C0C0&
      Height          =   285
      Left            =   120
      Locked          =   -1  'True
      TabIndex        =   3
      Text            =   "Text4"
      Top             =   2730
      Width           =   1215
   End
   Begin VB.TextBox Text3 
      Height          =   285
      Left            =   3240
      TabIndex        =   2
      Text            =   "Text3"
      Top             =   2520
      Width           =   1215
   End
   Begin VB.TextBox Text2 
      Height          =   1575
      Left            =   720
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   1
      Text            =   "Test.frx":0000
      Top             =   720
      Width           =   3735
   End
   Begin VB.TextBox Text1 
      Height          =   495
      Left            =   720
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      Text            =   "Test.frx":0006
      Top             =   120
      Width           =   3735
   End
   Begin MSWinsockLib.Winsock Winsock1 
      Left            =   120
      Top             =   120
      _ExtentX        =   741
      _ExtentY        =   741
      _Version        =   393216
      Protocol        =   1
      RemotePort      =   5567
      LocalPort       =   5567
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()
' To start the winsock control, you need to begin
' by sending data. We will use the ">" character
' since that is a popular character for chats.

Text1.Text = ">"

' When you open the program, you do not have a
' connection with another person, so you need
' to set your IP as the host IP.

Winsock1.RemoteHost = Winsock1.LocalIP

' Send the data.

Winsock1.SendData ">"

' Set the localIP as Text4

Text4.Text = Winsock1.LocalIP
End Sub

Private Sub Text1_KeyPress(KeyAscii As Integer)
' Set the box equivalent to itself.

Text1 = Text1

' Send the data that is keyed into this textbox
' to the other user.

Winsock1.SendData (KeyAscii)
End Sub

Private Sub Text2_KeyPress(KeyAscii As Integer)
' Set the textbox equivalent to itself.

Text2 = Text2

' Get the data from the other user as he/she is typing.

Winsock1.GetData (KeyAscii)
End Sub

Private Sub Text3_Change()
' Set the remote host equivalent to the
' IP address in Text3

Winsock1.RemoteHost = Text3.Text
End Sub

Private Sub Winsock1_DataArrival(ByVal bytesTotal As Long)
' Make a string that will represent the data
' that is being sent from one computer to'
' yours.

Dim strData As String

' Then tell winsock to get that data and import it.

Winsock1.GetData strData, vbString

' Changes the remote IP to where the incoming data
' came from.

Text3.Text = Winsock1.RemoteHostIP
Winsock1.RemoteHost = Winsock1.RemoteHostIP

' If the users press the backspace key, then it
' will register on both ends and send the data.

If Asc(strData) = 8 And Len(Text2) > 0 Then
   Text2.Text = Mid(Text2, 1, (Len(Text2) - 1))
Else
   Text2 = Text2 & strData
End If

' If the user presses the enter key, then it
' will register on both ends and send the data.

If Asc(strData) = 13 Then
   Text2 = Text2 & vbNewLine
End If

' This will make the textbox the other person is
' talking through automatically move down when it
' gets to the bottom of the textbox.

Text2.SelStart = Len(Text2)
End Sub

