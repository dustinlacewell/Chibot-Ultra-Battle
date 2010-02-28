Option Explicit

Type Mstatustype
  Mute As Integer       ' Can't do moves other than Phys
  Chaos As Integer      ' Confusion
  Freeze As Integer     ' Frozen -- Wait for Ice to Thaw or use Fire to melt
  Sleep As Integer      ' Fast Asleep. Awake with a Phys Hit
  Poison As Integer     ' Lose HP
  Blind As Integer      ' Attack Value drops to Zero
  Bless As Integer
  'Fly As Integer
  Haste As Integer      ' Increases Speed
  HalfMP As Integer
  Curse As Integer
  Barrier As Integer
  MBarrier As Integer
  SCrow As Integer
  Slow As Integer       ' Lowers speed
  Stun As Integer       ' Knocked Out
  Life3 As Integer      ' Return to life once after being killed
  Regen As Integer      ' Get HP
  Stop As Integer       ' Frozen in Time
  Mushroom As Integer   ' Can't Act, Regain HP
  MIA As Integer        ' Missing from Battle -- Can't be Targeted
  Quick As Integer      ' Next move hits instantly
  Berserk As Integer    ' Releases Destructive Spirit
End Type

Type ItemType
  Name As String
  Spawn As String
  Telefrag As String
  PlayerGet As String
  YoumaGet As String
  PlayerHP As Integer
  PlayerMP As Integer
  YoumaHP As Integer
  YoumaMP As Integer
  PlayerStat As Mstatustype
  YoumaStat As Mstatustype
End Type

Global Item As ItemType

Sub ClearItem ()
  Item.Name = ""
  Item.Spawn = ""
  Item.Telefrag = ""
  Item.PlayerGet = ""
  Item.YoumaGet = ""
  Item.PlayerHP = 0
  Item.PlayerMP = 0
  Item.PlayerStat.Mute = 0
  Item.PlayerStat.Chaos = 0
  Item.PlayerStat.Freeze = 0
  Item.PlayerStat.Sleep = 0
  Item.PlayerStat.Poison = 0
  Item.PlayerStat.Blind = 0
  'Item.PlayerStat.DefUp = 0
  Item.PlayerStat.Haste = 0
  'Item.PlayerStat.AttUp = 0
  'Item.PlayerStat.Weak = 0
  Item.PlayerStat.Stop = 0
  Item.PlayerStat.Barrier = 0
  Item.PlayerStat.MBarrier = 0
  Item.PlayerStat.Quick = 0
  Item.PlayerStat.Berserk = 0
  Item.PlayerStat.Life3 = 0
  Item.PlayerStat.Regen = 0
  Item.PlayerStat.MIA = 0
  Item.PlayerStat.Bless = 0
  Item.PlayerStat.Curse = 0
  Item.PlayerStat.Mushroom = 0
  Item.PlayerStat.Slow = 0
  Item.PlayerStat.Stun = 0
  Item.YoumaHP = 0
  Item.YoumaMP = 0
End Sub

Sub LoadItem (ByVal FileName As String)
  On Error GoTo eLoadItem
  Open FileName For Input As #5
    Input #5, Item.Name
    Input #5, Item.Spawn
    Input #5, Item.Telefrag
    Input #5, Item.PlayerGet
    Input #5, Item.YoumaGet
    Input #5, Item.PlayerHP
    Input #5, Item.PlayerMP
    Input #5, Item.PlayerStat.Mute
    Input #5, Item.PlayerStat.Chaos
    Input #5, Item.PlayerStat.Freeze
    Input #5, Item.PlayerStat.Sleep
    Input #5, Item.PlayerStat.Poison
    Input #5, Item.PlayerStat.Blind
    Input #5, Item.PlayerStat.Curse
    Input #5, Item.PlayerStat.Haste
    Input #5, Item.PlayerStat.Barrier
    Input #5, Item.PlayerStat.Bless
    Input #5, Item.PlayerStat.Slow
    Input #5, Item.PlayerStat.Stun
    Input #5, Item.YoumaHP
    Input #5, Item.YoumaMP
    Input #5, Item.PlayerStat.Stop
    Input #5, Item.PlayerStat.MBarrier
    Input #5, Item.PlayerStat.Life3
    Input #5, Item.PlayerStat.Quick
    Input #5, Item.PlayerStat.Regen
    Input #5, Item.PlayerStat.Mushroom
    Input #5, Item.PlayerStat.MIA
    Input #5, Item.PlayerStat.Berserk
  Close #5
  Exit Sub
eLoadItem:
  MsgBox "Fatal error in item " + FileName, 16, "ChUB 2000 Item Error"
  Exit Sub
End Sub

Sub SaveItem (ByVal FileName As String)
  On Error GoTo eSaveItem
  Open FileName For Output As #5
    Write #5, Item.Name
    Write #5, Item.Spawn
    Write #5, Item.Telefrag
    Write #5, Item.PlayerGet
    Write #5, Item.YoumaGet
    Write #5, Item.PlayerHP
    Write #5, Item.PlayerMP
    Write #5, Item.PlayerStat.Mute
    Write #5, Item.PlayerStat.Chaos
    Write #5, Item.PlayerStat.Freeze
    Write #5, Item.PlayerStat.Sleep
    Write #5, Item.PlayerStat.Poison
    Write #5, Item.PlayerStat.Blind
    Write #5, Item.PlayerStat.Curse
    Write #5, Item.PlayerStat.Haste
    Write #5, Item.PlayerStat.Barrier
    Write #5, Item.PlayerStat.Bless
    Write #5, Item.PlayerStat.Slow
    Write #5, Item.PlayerStat.Stun
    Write #5, Item.YoumaHP
    Write #5, Item.YoumaMP
    Write #5, Item.PlayerStat.Stop
    Write #5, Item.PlayerStat.MBarrier
    Write #5, Item.PlayerStat.Life3
    Write #5, Item.PlayerStat.Quick
    Write #5, Item.PlayerStat.Regen
    Write #5, Item.PlayerStat.Mushroom
    Write #5, Item.PlayerStat.MIA
    Write #5, Item.PlayerStat.Berserk
    Write #5, 0
    Write #5, 0
    Write #5, 0
    Write #5, 0
  Close #5
  Exit Sub
eSaveItem:
  MsgBox "Error saving item " + FileName, 16, "ChUB 2000 ItemEdit"
  Exit Sub
End Sub

Function TrimStr (ByVal S As Variant) As String
' A fast way of doing Trim(Str$("Whatever"))
  TrimStr = Trim(Str$(S))
End Function

