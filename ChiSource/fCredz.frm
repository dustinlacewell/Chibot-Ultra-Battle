VERSION 5.00
Begin VB.Form fCredits 
   Appearance      =   0  'Flat
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "???"
   ClientHeight    =   3570
   ClientLeft      =   4095
   ClientTop       =   1500
   ClientWidth     =   3495
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H0000FFFF&
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3570
   ScaleWidth      =   3495
   Begin VB.CheckBox cK 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      Caption         =   "Kamek"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   3
      Top             =   3240
      Value           =   1  'Checked
      Width           =   855
   End
   Begin VB.Timer Timer2 
      Interval        =   1
      Left            =   2520
      Top             =   3120
   End
   Begin VB.Timer Timer1 
      Interval        =   1
      Left            =   3000
      Top             =   3120
   End
   Begin VB.CommandButton Exi 
      Appearance      =   0  'Flat
      Caption         =   "Impatient?"
      Height          =   255
      Left            =   1080
      TabIndex        =   2
      Top             =   3240
      Width           =   1215
   End
   Begin VB.Image KamekR 
      Appearance      =   0  'Flat
      Height          =   270
      Left            =   2880
      Picture         =   "FCREDZ.frx":0000
      Top             =   0
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image KamekL 
      Appearance      =   0  'Flat
      Height          =   270
      Left            =   0
      Picture         =   "FCREDZ.frx":0682
      Top             =   0
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Label Small 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   13.5
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   1575
      Left            =   120
      TabIndex        =   1
      Top             =   1560
      Width           =   3255
   End
   Begin VB.Label Big 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   18
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   1215
      Left            =   120
      TabIndex        =   0
      Top             =   360
      Width           =   3255
   End
End
Attribute VB_Name = "fCredits"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Const M = 50
Const Inc = 50
Dim D(M) As PType

Private Sub Exi_Click()
  If Exi.Caption = "Impatient?" Then
    MsgBox "Yeah, whatever."
  End If
  Unload fMidi
  'fPreBattl.Show
  Me.Hide
End Sub

Private Sub Fadein(L As Label)
Dim X%, Y%
Dim E#
Static a%, Z%
Const I = 3
  If (a = 1) Then a = 2 Else a = 1
  If (cK = 0) Then a = 0
  Select Case a
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

Private Sub Fadeout(L As Label)
Dim X%
  For X = 255 To 0 Step -10
    L.ForeColor = RGB(X, X, X)
    Delay (0)
  Next X
End Sub

Private Sub Form_Load()
  SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3             ' Always on top
  Me.Show
End Sub

Private Sub Timer1_Timer()
Const D = 1
Dim Gi%
  Timer1.interval = 0
  'GoTo Whatsit
  'PlayMid ("ending")
  Small = "ChUB Resurrection"
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
  Small = "ALL you guys who played ChUB Resurrection while I was hosting it!"
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
  Small = "KatLioness, for always being there to tell me what's wrong with my personality"
  Fadein Small
  Delay D
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
  Small = YourSN + " for hosting ChUB Resurrection!"
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

Private Sub Timer2_Timer()
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

