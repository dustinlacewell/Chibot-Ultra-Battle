VERSION 2.00
Begin Form fCredits 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "???"
   ClientHeight    =   3570
   ClientLeft      =   4095
   ClientTop       =   1500
   ClientWidth     =   3495
   ForeColor       =   &H0000FFFF&
   Height          =   3975
   Left            =   4035
   LinkTopic       =   "Form1"
   ScaleHeight     =   3570
   ScaleWidth      =   3495
   Top             =   1155
   Width           =   3615
   Begin CheckBox cK 
      BackColor       =   &H00000000&
      Caption         =   "Kamek"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   3
      Top             =   3240
      Value           =   1  'Checked
      Width           =   855
   End
   Begin Timer Timer2 
      Interval        =   1
      Left            =   2520
      Top             =   3120
   End
   Begin Timer Timer1 
      Interval        =   1
      Left            =   3000
      Top             =   3120
   End
   Begin CommandButton Exi 
      Caption         =   "Impatient?"
      Height          =   255
      Left            =   1080
      TabIndex        =   2
      Top             =   3240
      Width           =   1215
   End
   Begin Image KamekR 
      Height          =   270
      Left            =   2880
      Picture         =   FCREDZ.FRX:0000
      Top             =   0
      Visible         =   0   'False
      Width           =   480
   End
   Begin Image KamekL 
      Height          =   270
      Left            =   0
      Picture         =   FCREDZ.FRX:067A
      Top             =   0
      Visible         =   0   'False
      Width           =   480
   End
   Begin Label Small 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   13.5
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00000000&
      Height          =   1575
      Left            =   120
      TabIndex        =   1
      Top             =   1560
      Width           =   3255
   End
   Begin Label Big 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "Comic Sans MS"
      FontSize        =   18
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00000000&
      Height          =   1215
      Left            =   120
      TabIndex        =   0
      Top             =   360
      Width           =   3255
   End
End
Option Explicit

Const M = 50
Const Inc = 50
Dim D(M) As PType

Sub Exi_Click ()
  If Exi.Caption = "Impatient?" Then
    MsgBox "Yeah, whatever."
  End If
  Unload fMidi
  fPreBattl.Show
  Me.Hide
End Sub

Sub Fadein (L As Label)
Dim X%, Y%
Dim E#
Static A%, Z%
Const I = 3
  If (A = 1) Then A = 2 Else A = 1
  If (cK = 0) Then A = 0
  Select Case A
    Case 1:
      KamekL.Left = L.Left - KamekL.Width
      KamekL.Top = L.Top - KamekL.Height
      KamekL.Visible = True
      For X = KamekL.Left To L.Left + L.Width + KamekL.Width Step I
        KamekL.Left = X
        If (X Mod 50 = 0) Then
          Z% = Z% + 1
          If (Z > M) Then Z = 0
          D(Z).X = X
          D(Z).Y = KamekL.Top + KamekL.Height
        End If
        DoEvents
      Next X
    Case 2:
      KamekR.Left = L.Left + L.Width + KamekR.Width
      KamekR.Top = L.Top - KamekR.Height
      KamekR.Visible = True
      For X = KamekR.Left To L.Left - KamekR.Width - L.Width Step -I
        KamekR.Left = X
        If (X Mod 50 = 0) Then
          Z% = Z% + 1
          If (Z > M) Then Z = 0
          D(Z).X = X + KamekR.Width
          D(Z).Y = KamekR.Top + KamekR.Height
        End If
        DoEvents
      Next X
  End Select
  For X = 0 To 255 Step 10
    'If (X Mod 10) = 0 Then
    L.ForeColor = RGB(X, X, X)
    Delay (0)
    'End If
  Next X
End Sub

Sub Fadeout (L As Label)
Dim X%
  For X = 255 To 0 Step -10
    L.ForeColor = RGB(X, X, X)
    Delay (0)
  Next X
End Sub

Sub Form_Load ()
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3             ' Always on top
  Me.Show
End Sub

Sub Timer1_Timer ()
Const D = 1
Dim Gi%
  Timer1.Interval = 0
  'GoTo Whatsit
  'PlayMid ("ending")
  Small = "ChUB 2000"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "Credits"
  Fadein Small
  Delay (D)
  Fadeout Small
  Big = "Main Program"
  Small = "Kamek"
  Fadein Big
  Delay (D)
  Fadein Small
  Delay (D)
  Fadeout Small
  Fadeout Big
  Big = "Testers"
  Small = "ALL you guys who played ChUB 2000 while I was hosting it!"
  Fadein Big
  Delay (D)
  Fadein Small
  Delay (D)
  Fadeout Small
  Fadeout Big
  Big = "Characters"
  Small = "SlrSenshiV"
  Fadein Big
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "MalkComedy"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "Qwikimart1"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "Xx Sn0 xX"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "Omegaquila"
  Fadein Small
  Delay (D)
  Fadeout Small
  Fadeout Big
  Big = "To get God Mode"
  Small = "Stand on your head and cluck like a chicken"
  Fadein Big
  Fadein Small
  Delay (D)
  Fadeout Small
  Fadeout Big
  Big = "Think Tank (New Ideas)"
  Small = "CetraPrez"
  Fadein Big
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "IAmLink100"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "SlrSenshiV"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "SpiderM64"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "Noah K 17"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "Heavy D150"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "PsYch0313"
  Fadein Small
  Delay D
  Fadeout Small
  Fadeout Big
  Big = "Speshul Thanks"
  Fadein Big
  Small = "Ai Senshi"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "SnowstarX"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "IM1BIGTard"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "MakoReno"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "Noah K 17"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "KalvnOwnzU"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "Tony Nitzke"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "My Lord and Savior, Jesus Christ"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = "The WB for showing the new Pokémon Episodes!!"
  Fadein Small
  Delay (D)
  Fadeout Small
  Small = YourSN + " for hosting ChUB 2000!"
  Fadein Small
  Delay (D)
  Fadeout Small
  Fadeout Big
  Big = "Good Idea"
  Gi = Rand(1, 6)
  Select Case Gi
    Case 1: Small = "Throwing a surprise party for your father."
    Case 2: Small = "Stopping to smell the roses."
    Case 3: Small = "Doing your own yard work."
    Case 4: Small = "Playing the piccolo in marching band."
    Case 5: Small = "Drinking a glass of milk every day."
    Case 6: Small = "Eating raw vegetables."
  End Select
  Fadein Big
  Fadein Small
  Delay (D)
  Fadeout Small
  Fadeout Big
  Big = "Bad Idea"
  Select Case Gi
    Case 1: Small = "Throwing a surprise party for your grandfather."
    Case 2: Small = "Stopping to FEEL the roses."
    Case 3: Small = "Doing your own dental work."
    Case 4: Small = "Playing the piano in marching band."
    Case 5: Small = "Drinking a glass of Drano every day."
    Case 6: Small = "Eating raw sewage."
  End Select
  Fadein Big
  Fadein Small
  Delay (D)
  Fadeout Small
  Fadeout Big
  Big = "Greetz"
  Small = "Xx Sn0 xX"
  Fadein Big
  Fadein Small
  Delay D
  Fadeout Small
  Small = "SSLRRanma"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "SlrPluto29"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "AndrKnight"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "Omegaquila"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "SlrSenshiV"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "XOhkiX"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "THEMASTR14"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "AgtSkater"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "MalkComedy"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "NikkiOwnzU"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "Buffy7000"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "Mr. Jones (ALIEN8 0 2)"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "MrBean1040"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "NeoKEndy"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "Jenova ICE"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "LitaJup657"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "Noah K 17/18"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "Tux77"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "KaiouMichi"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "HarukaPowa"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "WendyT98"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "Timeshifter"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "RummyJoker"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "Everyone on the Pokemon Board"
  Fadein Small
  Delay D
  Fadeout Small
  Small = "And anyone who knows that Pokégods are FAKE!!!"
  Fadein Small
  Delay D
  Fadeout Small
  Big = "Confucius Say"
  Small = "Man who stand on toilet is high on pot."
  Fadein Big
  Fadein Small
  Delay (D)
  Fadeout Small
  Fadeout Big
  Big = "The End"
  Small = "Now go away!"
  Fadein Big
  Playwav ("end")
  Fadein Small
  Exi.Caption = "Exit"
  Delay (D * 3)
  Fadeout Small
  Fadeout Big
  Big = ""
  Delay (D * 3)
  Small = "Leave already!"
  Fadein Small
  Delay (D)
  Fadeout Small
  Delay (D * 3)
  Small = "Leave or I'll erase your hard drive!"
  Fadein Small
  Delay (D)
  Fadeout Small
  Delay (D * 5)
  Big = "Since you stuck around..."
  Small = "Access the Secret Wave Player by right-clicking on the Midi Button on the Battle Screen."
  Fadein Big
  Fadein Small
  Delay (10)
  Fadeout Small
  Fadeout Big
End Sub

Sub Timer2_Timer ()
Dim Y%
  For Y = 1 To 50
    DoEvents
    If D(Y).X <> 0 Then
      D(Y).Y = D(Y).Y + Inc
      PSet (D(Y).X, D(Y).Y)
      PSet (D(Y).X, D(Y).Y - Inc), Me.BackColor
      If D(Y).Y > Me.Height Then D(Y).X = 0
    End If
  Next Y
End Sub

