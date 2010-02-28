Option Explicit

Global Const MaxEle = 30

Type MStatusType
  Mute As Integer       ' Can't do special moves
  Chaos As Integer      ' Uncontrolled attacks, may attack self or friends
  Freeze As Integer     ' Frozen, can't do anything until ice melts or is burned away
  Sleep As Integer      ' Asleep, can be awakened with a hit
  Poison As Integer     ' Steadily lose HP until Healed
  Blind As Integer      ' Blind, lowers dexterity
  Defup As Integer      ' Increases Defense
  Haste As Integer      ' Increases Speed
  Attup As Integer      ' Increases Attack
  Weak As Integer       ' Lowers defense and attack
  Slow As Integer       ' Lowers speed
  Stun As Integer       ' Frozen and defense lowered
End Type

Type RanHappening
' Random happening in an Arena
  Name As String
  Frequency As Integer                 ' Happens x in every 400 Timer Cycles
  Element As Integer                   ' Element Type
  ElementStr As Integer                ' Strength -- Set this to zero and the RanHappening will just display the Hit message
  Hit As String                        ' When it happens and hits
  Miss As String                       ' When it happens and misses
  HitsAll As Integer
End Type

Type ArenaType
' The Arena Field Type
  Name As String                       ' Name
  Desc(10) As String                   ' Description
  AllAttacks As Single                 ' Multiplier on all attacks
  Effect(MaxEle) As Single             ' Multiplier on certain elements
  RestLowHP As Integer                 ' Default = 15
  RestHighHP As Integer                ' Default = 25
  RestLowMP As Integer                 ' Default = 15
  RestHighMP As Integer                ' Default = 25
  GradualMP As Integer                 ' Default = 1
  GradualHP As Integer                 ' Default = 0
  Event(10) As RanHappening            ' up to 10 random happenings
End Type

Global Arena As ArenaType

Global Const NoDmg = 0              ' No Damage -- Effect Only
Global Const Phys = 1               ' Physical Hit
Global Const Heal = 2               ' Healing Power
Global Const Morph = 3              ' Secret Morph
Global Const Cover = 4              ' Protection Move
Global Const StopMove = 5           ' Stop an enemy from attacking you
Global Const Critical = 6           ' Critical Hit / Mutual Death
Global Const Sacrifice = 7          ' Sacrifice Move
Global Const MindMeld = 9           ' Luna, Artemis, Diana Mind Melds
Global Const Invin = 10             ' Temporary Invincibility
Global Const HPTheft = 11           ' HP Theft
Global Const MPTheft = 12           ' MP Theft
Global Const Life = 17              ' Restore Life
Global Const Demi = 19              ' Cuts enemy's HP in half
Global Const MoonE = 20             ' Moon Power
Global Const Shadow = 21            ' Shadow
Global Const Water = 22             ' Water
Global Const Reveal = 23            ' Scan
Global Const Fire = 24              ' Fire nullifies Freeze.
Global Const Lit = 25               ' Lightning
Global Const Heart = 26             ' Heart
Global Const Earth = 27             ' Earth
Global Const RandE = 28             ' Random Elemental
Global Const Mult = 29              ' Multiple Elementals
Global Const Lum = 30               ' Luminous Energy

Sub ClearArena ()
Dim X2 As Integer
  Arena.Name = ""
  For X2 = 1 To 10
    Arena.Desc(X2) = ""
  Next X2
  Arena.AllAttacks = 1
  For X2 = 1 To MaxEle
    Arena.Effect(X2) = 1
  Next X2
  Arena.RestLowHP = 30
  Arena.RestHighHP = 45
  Arena.RestLowMP = 30
  Arena.RestHighMP = 45
  Arena.GradualHP = 0
  Arena.GradualMP = 1
  For X2 = 1 To 10
    EraseEvent (X2)
  Next X2
End Sub

Sub EraseEvent (ByVal X2%)
    Arena.Event(X2).Name = ""
    Arena.Event(X2).Frequency = 0
    Arena.Event(X2).ElementStr = 0
    Arena.Event(X2).Hit = ""
    Arena.Event(X2).Miss = ""
End Sub

Sub LoadArena (ByVal Filename As String)
Dim CD As Integer
Dim X2 As Integer
  If Len(Filename) > 4 Then
    If UCase(Right$(Filename, 4) = ".AN2") Then
      LoadNewArena (Filename)
      Exit Sub
    End If
  End If
  On Error GoTo erLoad
  Open Filename For Input As #2
    Input #2, Arena.Name
    For CD = 1 To 10
      Input #2, Arena.Desc(CD)
    Next CD
    On Error GoTo erTooManyDescLines
    Input #2, Arena.AllAttacks
    On Error GoTo erOther
    For X2 = 1 To MaxEle
      Input #2, Arena.Effect(X2)
    Next X2
    Input #2, Arena.RestLowHP
    Input #2, Arena.RestHighHP
    Input #2, Arena.RestLowMP
    Input #2, Arena.RestHighMP
    Input #2, Arena.GradualHP
    Input #2, Arena.GradualMP
    On Error GoTo erEvents
    For CD = 1 To 10
      Input #2, Arena.Event(CD).Name
      Input #2, Arena.Event(CD).Frequency
      Input #2, Arena.Event(CD).ElementStr
      Input #2, Arena.Event(CD).Hit
      Input #2, Arena.Event(CD).Miss
    Next CD
    If Not EOF(2) Then MsgBox "Warning: Arena " + Filename + " has too many Events. Only the first 10 were loaded.", 16, "ChUB 2000 Arena Warning"
  Close #2
  Exit Sub
erLoad:
  MsgBox "Error loading arena " + Filename, 16, "ChUB 2000 Arena Error"
  Stop
erTooManyDescLines:
  MsgBox "Error: Arena " + Filename + " has too many Description lines. Only 10 are allowed.", 16, "ChUB 2000 Arena Error"
  Stop
erOther:
  MsgBox "Error in arena " + Filename, 16, "ChUB 2000 Arena Error"
  Stop
erEvents:
  MsgBox "Error loading events for arena " + Filename, 16, "ChUB 2000 Arena Error"
  Stop
End Sub

Sub LoadNewArena (ByVal Filename As String)
Dim CD As Integer
Dim X2 As Integer
  On Error GoTo erLoad2
  Open Filename For Input As #2
    Input #2, Arena.Name
    For CD = 1 To 10
      Input #2, Arena.Desc(CD)
    Next CD
    On Error GoTo erTooManyDescLines2
    Input #2, Arena.AllAttacks
    On Error GoTo erOther2
    For X2 = 1 To MaxEle
      Input #2, Arena.Effect(X2)
    Next X2
    Input #2, Arena.RestLowHP
    Input #2, Arena.RestHighHP
    Input #2, Arena.RestLowMP
    Input #2, Arena.RestHighMP
    Input #2, Arena.GradualHP
    Input #2, Arena.GradualMP
    On Error GoTo erEvents2
    For CD = 1 To 10
      Input #2, Arena.Event(CD).Name
      Input #2, Arena.Event(CD).Frequency
      Input #2, Arena.Event(CD).ElementStr
      Input #2, Arena.Event(CD).Hit
      Input #2, Arena.Event(CD).Miss
      Input #2, Arena.Event(CD).HitsAll
    Next CD
    On Error Resume Next
    If Not EOF(2) Then MsgBox "Warning: Arena " + Filename + " has too many Events. Only the first 10 were loaded.", 16, "ChUB 2000 Arena Warning"
  Close #2
  Exit Sub
erLoad2:
  MsgBox "Error loading arena " + Filename, 16, "ChUB 2000 Arena Error"
  Stop
erTooManyDescLines2:
  MsgBox "Error: Arena " + Filename + " has too many Description lines. Only 10 are allowed.", 16, "ChUB 2000 Arena Error"
  Stop
erOther2:
  MsgBox "Error in arena " + Filename, 16, "ChUB 2000 Arena Error"
  Stop
erEvents2:
  MsgBox "Error loading events for arena " + Filename, 16, "ChUB 2000 Arena Error"
  Stop
End Sub

Sub SaveArena (ByVal Filename As String)
Dim CD As Integer
Dim X2 As Integer
  If Len(Filename) > 4 Then
    If UCase(Right$(Filename, 4) = ".AN2") Then
      SaveNewArena (Filename)
      Exit Sub
    End If
  End If
  On Error GoTo erSave
  Open Filename For Output As #2
    Write #2, Arena.Name
    For CD = 1 To 10
      Write #2, Arena.Desc(CD)
    Next CD
    Write #2, Arena.AllAttacks
    For X2 = 1 To MaxEle
      Write #2, Arena.Effect(X2)
    Next X2
    Write #2, Arena.RestLowHP
    Write #2, Arena.RestHighHP
    Write #2, Arena.RestLowMP
    Write #2, Arena.RestHighMP
    Write #2, Arena.GradualHP
    Write #2, Arena.GradualMP
    For CD = 1 To 10
      Write #2, Arena.Event(CD).Name
      Write #2, Arena.Event(CD).Frequency
      Write #2, Arena.Event(CD).ElementStr
      Write #2, Arena.Event(CD).Hit
      Write #2, Arena.Event(CD).Miss
    Next CD
  Close #2
  Exit Sub
erSave:
  MsgBox "Error saving file " + Filename, 16, "ChUB 2000 ArenaEd"
End Sub

Sub SaveNewArena (ByVal Filename As String)
Dim CD As Integer
Dim X2 As Integer
  On Error GoTo erSave2
  Open Filename For Output As #2
    Write #2, Arena.Name
    For CD = 1 To 10
      Write #2, Arena.Desc(CD)
    Next CD
    Write #2, Arena.AllAttacks
    For X2 = 1 To MaxEle
      Write #2, Arena.Effect(X2)
    Next X2
    Write #2, Arena.RestLowHP
    Write #2, Arena.RestHighHP
    Write #2, Arena.RestLowMP
    Write #2, Arena.RestHighMP
    Write #2, Arena.GradualHP
    Write #2, Arena.GradualMP
    For CD = 1 To 10
      Write #2, Arena.Event(CD).Name
      Write #2, Arena.Event(CD).Frequency
      Write #2, Arena.Event(CD).ElementStr
      Write #2, Arena.Event(CD).Hit
      Write #2, Arena.Event(CD).Miss
      Write #2, Arena.Event(CD).HitsAll
    Next CD
  Close #2
  Exit Sub
erSave2:
  MsgBox "Error saving file " + Filename, 16, "ChUB 2000 ArenaEd"
End Sub

Function TrimStr (ByVal S As Variant) As String
' A fast way of doing Trim(Str$("Whatever"))
  TrimStr = Trim(Str$(S))
End Function

