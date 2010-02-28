VERSION 2.00
Begin Form fMoves 
   BackColor       =   &H008080FF&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Millenium Moves"
   ClientHeight    =   1245
   ClientLeft      =   2970
   ClientTop       =   2400
   ClientWidth     =   3345
   ForeColor       =   &H00000000&
   Height          =   1650
   Icon            =   FMOVES.FRX:0000
   Left            =   2910
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   1245
   ScaleWidth      =   3345
   Top             =   2055
   Width           =   3465
   Begin CommandButton cWeapons 
      Caption         =   "&Weapons"
      Height          =   255
      Left            =   1560
      TabIndex        =   7
      Top             =   960
      Width           =   1095
   End
   Begin CommandButton cMeter 
      Caption         =   "Cheese &Meter"
      Height          =   255
      Left            =   120
      TabIndex        =   8
      Top             =   960
      Width           =   1455
   End
   Begin CommandButton bBasic 
      Caption         =   "&Basic"
      Height          =   255
      Left            =   2640
      TabIndex        =   1
      Top             =   720
      Width           =   615
   End
   Begin CommandButton cShow 
      Caption         =   "&Show"
      Default         =   -1  'True
      Height          =   255
      Left            =   2640
      TabIndex        =   0
      Top             =   480
      Width           =   615
   End
   Begin CommandButton cSuper 
      Caption         =   "&Super Help"
      Height          =   255
      Left            =   1560
      TabIndex        =   5
      Top             =   720
      Width           =   1095
   End
   Begin CommandButton cCounter 
      Caption         =   "&Counterattacks"
      Height          =   255
      Left            =   120
      TabIndex        =   6
      Top             =   720
      Width           =   1455
   End
   Begin TextBox tName 
      Height          =   285
      Left            =   120
      TabIndex        =   3
      Top             =   480
      Width           =   2535
   End
   Begin Label Label1 
      BackStyle       =   0  'Transparent
      Caption         =   "Guess who? -->"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      Height          =   255
      Left            =   840
      TabIndex        =   4
      Top             =   2160
      Width           =   1215
   End
   Begin Label Label2 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Type character name to show moves"
      FontBold        =   0   'False
      FontItalic      =   0   'False
      FontName        =   "MS Sans Serif"
      FontSize        =   8.25
      FontStrikethru  =   0   'False
      FontUnderline   =   0   'False
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   120
      TabIndex        =   2
      Top             =   240
      Width           =   2655
   End
End

' FMOVES.FRM
' Allows the Host to display the moves individually

Const MyName = "fMoves"

Option Explicit

Sub bBasic_Click ()
  Send ("--- Basic: (/rest) (/block-move) (/chictr-move) (/halt) (/get) (/taunt) (/flee) (/divert-#HP) (/#-move)")
  Send ("--- Control: (/status-person) (/cpu) (/i want a draw) (/attacking-person) (/rune)")
  Send ("--- Weapons: (/wget-wpnname) (/wdrop) (/wpnlist)")
  Send ("--- Teams: (/defect-person)D (/nodefect)D (/newteam) (/teamname)C (/kick-person)C")
  Send ("D = only when Defect enabled. C = only Team Captain may do this.")
  Send ("--- /unmorph to return to normal form after using a morph")
End Sub

Sub bMoves_Click (Index As Integer)
  DispMoves (Index)
End Sub

Sub cCounter_Click ()
  Send ("To counterattack, type /block-<move> where <move> is the move you want to counterattack with. You")
  Send ("will need 100 Super Points in order to counter. You may also do a Chibot Counter, which is")
  Send ("/chictr-<move>. This costs 100 Super Points up front, and 100 Super Points to complete the")
  Send ("counterattack. The difference is that Chibot Counterattacks will hit the enemy before they")
  Send ("actually attack you, so this is best used with a freeze or stun move!")
End Sub

Sub cMeter_Click ()
  Send ("The Cheese Meter is a new meter in ChUB 2000. It keeps track of how much damage")
  Send ("you are dealing. Every time you do a move, it adds the total amount of HP done")
  Send ("by that move to your Cheese Meter. Your Cheese Meter will decrease gradually")
  Send ("over time. Most people shouldn't have a problem with it. However, if you are")
  Send ("cheesy, then it will stop you about around 1000 CP (Cheese Points).")
  Send ("You can still get around these limitations by counterattacks, but it")
  Send ("is not advisable to drive your meter up too high...")
  Send ("To make your meter go down quicker, do /rest or have someone use a Sleep attack on you.")
  Send ("If your CP exceeds a certain amount, you will collapse from exhaustion.")
End Sub

Sub cShow_Click ()
Dim S As String
  S = tName.Text
  tName.Text = ""
  ShowMoves (S)
End Sub

Sub cSuper_Click ()
  Send ("=== EXPLANATION OF SUPER MOVES ===")
  Send ("Super moves are those moves with a * by them on your moves list. That means the move can be used as a Super move.")
  Send ("Increase your SP (Super Points) by doing normal moves that do damage. When you hit Level 1 you can do a Level 1 Super like this:")
  Send ("/1-move-target")
  Send ("where move is the Super move and target is the person you want to target.")
  Send ("Remember only moves with a * by them can be Supered. You can get SP either by attacking or being attacked.")
End Sub

Sub cWeapons_Click ()
  Send ("Weapons are a new addition to ChUB 2000. Weapons give you extra moves. Before battle,")
  Send ("you may pick a weapon using its selection command. To get moves for it, type /info-weaponname.")
  Send ("During battle, you use its moves as if they were part of your character's moves. Some weapons have charges,")
  Send ("meaning that some moves decrease the number of times you can use that weapon. Once you have expended it totally,")
  Send ("the weapon is destroyed. Some moves cause the weapon to be destroyed instantly, or dropped. To see what weapons")
  Send ("have been dropped, type /wpnlist. To drop your weapon, type /wdrop. To get a weapon, type /wget-weaponname.")
  Send ("ONCE THE BATTLE HAS STARTED, YOU MAY NOT PICK JUST ANY WEAPON! If you miss out on weapon selection,")
  Send ("then you must wait for someone to drop their weapon or for one to appear.")
End Sub

Sub Form_Load ()
Dim X As Integer
  LoadPosition Me, MyName
  SetWindowPos Me.hWnd, -1, 0, 0, 0, 0, 3             ' Always on top
  Me.Show
  tName.SetFocus
End Sub

Sub Form_Unload (Cancel As Integer)
  SavePosition Me, MyName

End Sub

Sub Label1_Click ()
  kDlgBox "No, it's not Kamek, just a plain 'ol Magikoopa.", 48, ""
End Sub

Sub lShow_Click (Index As Integer)
  DispMoves (Index + 1)
End Sub

