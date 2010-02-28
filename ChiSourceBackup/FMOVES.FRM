VERSION 5.00
Begin VB.Form fMoves 
   Appearance      =   0  'Flat
   BackColor       =   &H00404040&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Millenium Moves"
   ClientHeight    =   1020
   ClientLeft      =   2970
   ClientTop       =   2400
   ClientWidth     =   3615
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00000000&
   Icon            =   "FMOVES.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   1020
   ScaleWidth      =   3615
   Begin VB.CommandButton cWeapons 
      Appearance      =   0  'Flat
      Caption         =   "&Weapons"
      Height          =   255
      Left            =   1680
      TabIndex        =   7
      Top             =   720
      Width           =   1215
   End
   Begin VB.CommandButton cMeter 
      Appearance      =   0  'Flat
      Caption         =   "Cheese &Meter"
      Height          =   255
      Left            =   120
      TabIndex        =   8
      Top             =   720
      Width           =   1575
   End
   Begin VB.CommandButton bBasic 
      Appearance      =   0  'Flat
      Caption         =   "&Basic"
      Height          =   255
      Left            =   2880
      TabIndex        =   1
      Top             =   480
      Width           =   735
   End
   Begin VB.CommandButton cShow 
      Appearance      =   0  'Flat
      Caption         =   "&Show"
      Default         =   -1  'True
      Height          =   255
      Left            =   2880
      TabIndex        =   0
      Top             =   240
      Width           =   735
   End
   Begin VB.CommandButton cSuper 
      Appearance      =   0  'Flat
      Caption         =   "&Super Help"
      Height          =   255
      Left            =   1680
      TabIndex        =   5
      Top             =   480
      Width           =   1215
   End
   Begin VB.CommandButton cCounter 
      Appearance      =   0  'Flat
      Caption         =   "&Counterattacks"
      Height          =   255
      Left            =   120
      TabIndex        =   6
      Top             =   480
      Width           =   1575
   End
   Begin VB.TextBox tName 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   120
      TabIndex        =   3
      Top             =   240
      Width           =   2775
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Guess who? -->"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   840
      TabIndex        =   4
      Top             =   2160
      Width           =   1215
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Type character name to show moves"
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
      TabIndex        =   2
      Top             =   0
      Width           =   2655
   End
End
Attribute VB_Name = "fMoves"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

' FMOVES.FRM
' Allows the Host to display the moves individually

Const MyName = "fMoves"

Option Explicit

Private Sub bBasic_Click()
  Send ("--- Basic: (.rest) (.block-move) (.chictr-move) (.halt) (.get) (.taunt) (.flee) (.divert-#HP) (.#-move)")
  Send ("--- Control: (.status-person) (.cpu) (.i want a draw) (.attacking-person) (.rune-person) (.weapon-person) (.winning)")
  Send ("--- Weapons: (.wget-wpnname) (.wdrop) (.wpnlist)")
  Send ("--- Teams: (.defect-person)D (.nodefect)D (.newteam) (.teamname)C (.kick-person)C (.captain-person)C")
  Send ("D = only when Defect enabled. C = only Team Captain may do this.")
  Send ("--- .unmorph to return to normal form after using a morph")
End Sub

Private Sub bMoves_Click(index As Integer)
  DispMoves index, ""
End Sub

Private Sub cCounter_Click()
  Send ("To counterattack, type .block-<move> where <move> is the move you want to counterattack with. You")
  Send ("will need 100 Super Points in order to counter. You may also do a Chibot Counter, which is")
  Send (".chictr-<move>. This costs 100 Super Points up front, and 100 Super Points to complete the")
  Send ("counterattack. The difference is that Chibot Counterattacks will hit the enemy before they")
  Send ("actually attack you, so this is best used with a freeze or stun move!")
End Sub

Private Sub cMeter_Click()
  Send ("The Cheese Meter is a new meter in ChUB Resurrection. It keeps track of how much damage")
  Send ("you are dealing. Every time you do a move, it adds the total amount of HP done")
  Send ("by that move to your Cheese Meter. Your Cheese Meter will decrease gradually")
  Send ("over time. Most people shouldn't have a problem with it. However, if you are")
  Send ("cheesy, then it will stop you about around 1000 CP (Cheese Points).")
  Send ("You can still get around these limitations by counterattacks, but it")
  Send ("is not advisable to drive your meter up too high...")
  Send ("To make your meter go down quicker, do .rest or have someone use a Sleep attack on you.")
  Send ("If your CP exceeds a certain amount, you will collapse from exhaustion.")
End Sub

Private Sub cShow_Click()
Dim S As String
  S = tName.Text
  tName.Text = ""
  ShowMoves S, ""
End Sub

Private Sub cSuper_Click()
  Send ("=== EXPLANATION OF SUPER MOVES ===")
  Send ("Super moves are those moves with a * by them on your moves list. That means the move can be used as a Super move.")
  Send ("Increase your SP (Super Points) by doing normal moves that do damage. When you hit Level 1 you can do a Level 1 Super like this:")
  Send (".1-move-target")
  Send ("where move is the Super move and target is the person you want to target.")
  Send ("Remember only moves with a * by them can be Supered. You can get SP either by attacking or being attacked.")
End Sub

Private Sub cWeapons_Click()
  Send ("Weapons are a new addition to ChUB Resurrection. Weapons give you extra moves. Before battle,")
  Send ("you may pick a weapon using its selection command. To get moves for it, type /info-weaponname.")
  Send ("During battle, you use its moves as if they were part of your character's moves. Some weapons have charges,")
  Send ("meaning that some moves decrease the number of times you can use that weapon. Once you have expended it totally,")
  Send ("the weapon is destroyed. Some moves cause the weapon to be destroyed instantly, or dropped. To see what weapons")
  Send ("have been dropped, type /wpnlist. To drop your weapon, type /wdrop. To get a weapon, type /wget-weaponname.")
  Send ("ONCE THE BATTLE HAS STARTED, YOU MAY NOT PICK JUST ANY WEAPON! If you miss out on weapon selection,")
  Send ("then you must wait for someone to drop their weapon or for one to appear.")
End Sub

Private Sub Form_Load()
Dim X As Integer
  LoadPosition Me, MyName
  SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3             ' Always on top
  Me.Show
  tName.SetFocus
End Sub

Private Sub Form_Unload(Cancel As Integer)
  SavePosition Me, MyName

End Sub

Private Sub Label1_Click()
  kDlgBox "No, it's not Kamek, just a plain 'ol Magikoopa.", 48, ""
End Sub

Private Sub lShow_Click(index As Integer)
  DispMoves index + 1, ""
End Sub

