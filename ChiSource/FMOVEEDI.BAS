' FMOVEEDI.FRM
Option Explicit
Const mc0024 = 1 ' &H1%
Const mc005C = 13 ' &HD%
Const mc007E = 6 ' &H6%
Const mc0088 = 5 ' &H5%
Const mc0092 = 1 ' &H1%
Const mc009C = 4 ' &H4%
Const m00AE = 0 ' &H0%
Const mc00B0 = 3 ' &H3%
Const mc00B2 = 4 ' &H4%
Const mc00B4 = 5 ' &H5%
Const mc00B6 = 6 ' &H6%
Const mc00B8 = 7 ' &H7%
Const mc00BA = 10 ' &HA%
Const mc00BC = 17 ' &H11%
Const mc00BE = 19 ' &H13%
Const mc00C0 = 23 ' &H17%
Const mc00C8 = 1 ' &H1%
Const mc00CA = 9 ' &H9%
Const mc00CC = 11 ' &HB%
Const mc00CE = 12 ' &HC%
Const mc00D0 = 20 ' &H14%
Const mc00D2 = 21 ' &H15%
Const mc00D4 = 22 ' &H16%
Const mc00D6 = 24 ' &H18%
Const mc00D8 = 25 ' &H19%
Const mc00DA = 26 ' &H1A%
Const mc00DC = 27 ' &H1B%
Const mc00DE = 28 ' &H1C%
Const mc00E0 = 29 ' &H1D%
Const mc00E2 = 30 ' &H1E%
Const mc00E4 = 2 ' &H2%
Const mc0104 = 2 ' &H2%
Const mc010E = 7 ' &H7%
Const mc0118 = 3 ' &H3%

Sub cCanSuper_Click ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0EF9 = cCanSuper
End Sub

Sub cCanSuper_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move can be supered. Not all types of attacks may be supered."
End Sub

Sub sub05DE ()
Dim l003C As Integer
  l003C = fn006F(mc0024, gv030A)
  Label2.Caption = "Estimated MP Cost: " + Trim(Str$(l003C))
End Sub

Sub Form_Load ()
  sub05E6
  hsMove.Value = gv030A
  Me.Show
End Sub

Sub Form_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move Editor"
End Sub

Sub Form_Unload (Cancel As Integer)
Dim l0056
' Const mc005C = 13 ' &HD%
  fDSEdit!cbMoves.Clear
  For l0056 = 1 To mc005C
    fDSEdit!cbMoves.AddItem gv0314(mc0024).M10B0(l0056).M0EE7
  Next l0056
  fDSEdit!cbMoves.ListIndex = gv030A - 1
End Sub

Sub hsMove_Change ()
  gv030A = hsMove.Value
  sub05E6
End Sub

Sub Label1_MouseDown (Index%, Button As Integer, Shift As Integer, X As Single, Y As Single)
Dim l0068 As String
' Const mc007E = 6 ' &H6%
' Const mc0088 = 5 ' &H5%
' Const mc0092 = 1 ' &H1%
' Const mc009C = 4 ' &H4%
  If  Button = 2 Then
    Label1(26).Visible = True
    tEleNum.Visible = True
  End If
  Select Case Index
    Case 0: l0068 = "Name of the attack."
    Case 1: l0068 = "Command to execute the move."
    Case 2: l0068 = "String shown when the move begins."
    Case 3: l0068 = "Shown when a Super attack based on this move begins."
    Case 4: l0068 = "For heals, the string shown when the player begins to heal himself. For multiple-target moves, the string shown when the move is completed."
    Case 5: l0068 = "String shown when the attack hits the target."
    Case 6: l0068 = "String shown for a critical hit."
  End Select
End Sub

Sub Label3_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "I'm tellin' ya, you'll screw up your character!"
End Sub

Sub oAllButSelf_Click ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0FC3 = mc007E
  gv0314(mc0024).M10B0(gv030A).M0EF9 = False
  cCanSuper.Value = 0
  cCanSuper.Enabled = False
  Label1(4).Caption = "Post-Attack String"
  sub05DE
End Sub

Sub oAllButSelf_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects everyone BUT the player using it."
End Sub

Sub oAllFoe_Click ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0FC3 = mc0088
  gv0314(mc0024).M10B0(gv030A).M0EF9 = False
  cCanSuper.Value = 0
  cCanSuper.Enabled = False
  Label1(4).Caption = "Post-Attack String"
  sub05DE
End Sub

Sub oAllFoe_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects all enemies in the battle."
End Sub

Sub oAllFriend_Click ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0FC3 = mc0092
  gv0314(mc0024).M10B0(gv030A).M0EF9 = False
  cCanSuper.Value = 0
  cCanSuper.Enabled = False
  Label1(4).Caption = "Pre-Heal Self/Team"
  sub05DE
End Sub

Sub oAllFriend_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects player's entire team."
End Sub

Sub oAllTeam_Click ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0FC3 = mc009C
  gv0314(mc0024).M10B0(gv030A).M0EF9 = False
  cCanSuper.Value = 0
  cCanSuper.Enabled = False
  Label1(4).Caption = "Post-Attack String"
  sub05DE
End Sub

Sub oAllTeam_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects one enemy team."
End Sub

Sub oElement_Click (Index%)
' Const m00AE = 0 ' &H0%
' Const mc00B0 = 3 ' &H3%
' Const mc00B2 = 4 ' &H4%
' Const mc00B4 = 5 ' &H5%
' Const mc00B6 = 6 ' &H6%
' Const mc00B8 = 7 ' &H7%
' Const mc00BA = 10 ' &HA%
' Const mc00BC = 17 ' &H11%
' Const mc00BE = 19 ' &H13%
' Const mc00C0 = 23 ' &H17%
' Const mc00C8 = 1 ' &H1%
' Const mc00CA = 9 ' &H9%
' Const mc00CC = 11 ' &HB%
' Const mc00CE = 12 ' &HC%
' Const mc00D0 = 20 ' &H14%
' Const mc00D2 = 21 ' &H15%
' Const mc00D4 = 22 ' &H16%
' Const mc00D6 = 24 ' &H18%
' Const mc00D8 = 25 ' &H19%
' Const mc00DA = 26 ' &H1A%
' Const mc00DC = 27 ' &H1B%
' Const mc00DE = 28 ' &H1C%
' Const mc00E0 = 29 ' &H1D%
' Const mc00E2 = 30 ' &H1E%
' Const mc00E4 = 2 ' &H2%
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0FAA = Index
  tElementStr.Enabled = True
  Select Case Index
    Case m00AE, mc00B0, mc00B2, mc00B4, mc00B6, mc00B8, mc00BA, mc00BC, mc00BE, mc00C0: tElementStr.Enabled = False
  End Select
  If  oEnemy.Value = True Then cCanSuper.Enabled = True
  Select Case Index
    Case m00AE, mc00C8, mc00B4, mc00B6, mc00CA, mc00CC, mc00CE, mc00BE, mc00D0, mc00D2, mc00D4, mc00D6, mc00D8, mc00DA, mc00DC, mc00DE, mc00E0, mc00E2, mc00C0:
      cCanSuper.Enabled = True
    Case mc00E4, mc00B8, mc00BC:
      If  oAllFriend <> True Then
        oFriend.Value = True
        oFriend_Click
      End If
  End Select
  Select Case Index
    Case m00AE, mc00E4, mc00B0, mc00B2, mc00B4, mc00B6, mc00B8, mc00CA, mc00BA, mc00BC, mc00BE, mc00C0, mc00CC, mc00CE: cCanSuper.Enabled = False
  End Select
  tEleNum.Text = fn00EF(gv0314(mc0024).M10B0(gv030A).M0FAA)
  sub05DE
  oElement(Index).Value = True
End Sub

Sub oElement_MouseMove (Index%, Button As Integer, Shift As Integer, X As Single, Y As Single)
' Const mc0104 = 2 ' &H2%
' Const mc010E = 7 ' &H7%
' Const mc0118 = 3 ' &H3%
  Select Case Index
    Case mc00C8: StatLine = "Physical attack."
    Case mc00E4: StatLine = "Restores HP"
    Case mc00B0: StatLine = "Allows player to change into another player in the current dataset."
    Case mc00B8: StatLine = "Player loses their life to restore their team to full health."
    Case mc00BA: StatLine = "Shielded from attacks!"
    Case mc00CC: StatLine = "Steals HP from target."
    Case mc00CE: StatLine = "Steals MP from target."
    Case mc00BC: StatLine = "Restores life to dead people."
    Case mc00C0: StatLine = "Shows cool stuff about target."
    Case m00AE: StatLine = "Causes no HP damage."
    Case mc00BE: StatLine = "Halves HP."
    Case mc00D0, mc00D2, mc00D4, mc00D6, mc00D8, mc00DA, mc00DC, mc00DE, mc00E2: StatLine = "Elemental (magic) damage"
    Case mc00E0: StatLine = "The unknown element."
  End Select
End Sub

Sub oEnemy_Click ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0FC3 = mc0104
  Select Case gv0314(mc0024).M10B0(gv030A).M0FAA
    Case m00AE, mc00C8, mc00B4, mc00B6, mc00CA, mc00CC, mc00CE, mc00BE, mc00D0, mc00D2, mc00D4, mc00D6, mc00D8, mc00DA, mc00DC, mc00DE, mc00E0, mc00E2, mc00C0: cCanSuper.Enabled = True
  End Select
  Label1(4).Caption = "Pre-Heal Self"
  sub05DE
End Sub

Sub oEnemy_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move is by default carried out on an enemy."
End Sub

Sub oEverybody_Click ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0FC3 = mc010E
  gv0314(mc0024).M10B0(gv030A).M0EF9 = False
  cCanSuper.Value = 0
  cCanSuper.Enabled = False
  Label1(4).Caption = "Post-Attack String"
  sub05DE
End Sub

Sub oEverybody_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move affects everyone in the battle."
End Sub

Sub oFriend_Click ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0FC3 = mc0118
  gv0314(mc0024).M10B0(gv030A).M0EF9 = False
  cCanSuper.Value = 0
  cCanSuper.Enabled = False
  Label1(4).Caption = "Pre-Heal Self"
  sub05DE
End Sub

Sub oFriend_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move is by default carried out on yourself or a friend."
End Sub

Sub oOnlySelf_Click ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0FC3 = 8
  gv0314(mc0024).M10B0(gv030A).M0EF9 = False
  cCanSuper.Value = 0
  cCanSuper.Enabled = False
  Label1(4).Caption = "Pre-Heal Self"
  sub05DE
End Sub

Sub oOnlySelf_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move will affect only the player using it regardless of who s/he targets."
End Sub

Sub StatLine_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Hi, I'm StatLine. I'll tell you what each thing does."
End Sub

Sub tAttDn_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target loses strength and cannot do as much damage."
End Sub

Sub tAttUp_Change ()
  gv0310 = True
  If  (Val(tAttUp) >= -1) And (Val(tAttUp) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0E6F = Val(tAttUp)
  sub05DE
End Sub

Sub tAttUp_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target has increased strength and causes more damage."
End Sub

Sub tBegin2Attack_Change ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0F05 = tBegin2Attack
End Sub

Sub tBegin2Attack_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "String shown when player starts doing the attack."
End Sub

Sub tBegin2HealSelf_Change ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0F2A = tBegin2HealSelf
End Sub

Sub tBegin2HealSelf_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If  oOnlySelf Or oEnemy Or oFriend Or oAllFriend Then
    StatLine = "String shown when player beings to use a Heal move on themself."
  Else
    StatLine = "String shown when an attack that hits everyone is carried out."
  End If
End Sub

Sub tBegin2SuperAttack_Change ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0F15 = tBegin2SuperAttack
End Sub

Sub tBegin2SuperAttack_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "String shown when beginning a Super Move."
End Sub

Sub tBerserk_Change ()
  gv0310 = True
  If  (Val(tBerserk) >= -1) And (Val(tBerserk) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0ED0 = Val(tBerserk)
  sub05DE
End Sub

Sub tBerserk_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target has increased attack power but uncontrolled attacks on random enemies."
End Sub

Sub tBlind_Change ()
  gv0310 = True
  If  (Val(tBlind) >= -1) And (Val(tBlind) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0E54 = Val(tBlind)
  sub05DE
End Sub

Sub tBlind_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target cannot see and moves do minimum damage."
End Sub

Sub tChaos_Change ()
  gv0310 = True
  If  (Val(tChaos) >= -1) And (Val(tChaos) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0E2E = Val(tChaos)
  sub05DE
End Sub

Sub tChaos_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target's mind is placed in chaos. Random moves on random people."
End Sub

Sub tCmdKey_Change ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0EEF = tCmdKey.Text
End Sub

Sub tCmdKey_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Move Command Key"
End Sub

Sub tCritHit_Change ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0F74 = tCritHit
End Sub

Sub tCritHit_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "String shown instead of Move Hit Normal if the attack is a Critical Hit"
End Sub

Sub tDefUp_Change ()
  gv0310 = True
  If  (Val(tDefUp) >= -1) And (Val(tDefUp) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0E5D = Val(tDefUp)
  sub05DE
End Sub

Sub tDefUp_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target has increased awareness and lessens damage."
End Sub

Sub tElementStr_Change ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0FB5 = Val(tElementStr)
  sub05DE
End Sub

Sub tElementStr_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "How much damage it would do if your Strength and their Defense were the same."
End Sub

Sub tEleNum_Change ()
On Error GoTo L69E4
  oElement(gv0314(mc0024).M10B0(gv030A).M0FAA).Value = False
L69E4: Resume Next
  oElement_Click (Val(tEleNum))
  On Error GoTo L6A24
  oElement(Val(tEleNum)).Enabled = True
L6A24: Exit Sub
End Sub

Sub tEleNum_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Hmmm......"
End Sub

Sub tFreeze_Change ()
  gv0310 = True
  If  (Val(tFreeze) >= -1) And (Val(tFreeze) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0E37 = Val(tFreeze)
  sub05DE
End Sub

Sub tFreeze_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is frozen solid and cannot move."
End Sub

Sub tHaste_Change ()
  gv0310 = True
  If  (Val(tHaste) >= -1) And (Val(tHaste) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0E66 = Val(tHaste)
  sub05DE
End Sub

Sub tHaste_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target moves faster."
End Sub

Sub tHealMeld_Change ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0F7F = tHealMeld
End Sub

Sub tHealMeld_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Huh?"
End Sub

Sub tHealSelf_Change ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0F68 = tHealSelf
End Sub

Sub tHealSelf_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "String shown when a heal on self or team is carried out"
End Sub

Sub tHit_Change ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0F45 = tHit
End Sub

Sub tHit_MouseDown (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If  oOnlySelf Or oEnemy Or oFriend Or oAllFriend Then
    StatLine = "String shown when player's attack hits another player (%T)"
  Else
    StatLine = "String shown for each individual person attacked (%T)"
  End If
End Sub

Sub tLife3_Change ()
  gv0310 = True
  If  (Val(tLife3) >= -1) And (Val(tLife3) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0E9A = Val(tLife3)
  sub05DE
End Sub

Sub tLife3_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target will come back to life after being killed."
End Sub

Sub tMIA_Change ()
  gv0310 = True
  If  (Val(tMIA) >= -1) And (Val(tMIA) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0EC0 = Val(tMIA)
  sub05DE
End Sub

Sub tMIA_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is not present in battle."
End Sub

Sub tMiss_Change ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0F8B = tMiss
End Sub

Sub tMiss_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  If  oOnlySelf Or oEnemy Or oFriend Or oAllFriend Then
    StatLine = "Oops! Your move missed or was blocked."
  Else
    StatLine = "Oops! Your move missed or was blocked by %T."
  End If
End Sub

Sub tMushroom_Change ()
  gv0310 = True
  If  (Val(tMushroom) >= -1) And (Val(tMushroom) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0EB4 = Val(tMushroom)
  sub05DE
End Sub

Sub tMushroom_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is turned into a fungus and restores HP at every turn interval."
End Sub

Sub tMute_Change ()
  gv0310 = True
  If  (Val(tMute) >= -1) And (Val(tMute) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0E26 = Val(tMute)
  sub05DE
End Sub

Sub tMute_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is unable to use moves other than physical moves and Basic moves."
End Sub

Sub tName_Change ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0EE7 = tName
End Sub

Sub tName_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Name of move. Used in status window and Super Attack messages."
End Sub

Sub tPoison_Change ()
  gv0310 = True
  If  (Val(tPoison) >= -1) And (Val(tPoison) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0E4A = Val(tPoison)
  sub05DE
End Sub

Sub tPoison_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target gradually loses HP."
End Sub

Sub tQuick_Change ()
  gv0310 = True
  If  (Val(tQuick) >= -1) And (Val(tQuick) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0EC7 = Val(tQuick)
  sub05DE
End Sub

Sub tQuick_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target's next non-morph non-life move is carried out immediately."
End Sub

Sub tRegen_Change ()
  gv0310 = True
  If  (Val(tRegen) >= -1) And (Val(tRegen) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0EA3 = Val(tRegen)
  sub05DE
End Sub

Sub tRegen_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target gradually restores HP."
End Sub

Sub tSleep_Change ()
  gv0310 = True
  If  (Val(tSleep) >= -1) And (Val(tSleep) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0E41 = Val(tSleep)
  sub05DE
End Sub

Sub tSleep_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is put to sleep and cannot move."
End Sub

Sub tSlow_Change ()
  gv0310 = True
  If  (Val(tSlow) >= -1) And (Val(tSlow) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0E8A = Val(tSlow)
  sub05DE
End Sub

Sub tSlow_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target moves slower."
End Sub

Sub tStop_Change ()
  gv0310 = True
  If  (Val(tStop) >= -1) And (Val(tStop) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0EAC = Val(tStop)
  sub05DE
End Sub

Sub tStop_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is placed in stasis and cannot move or act."
End Sub

Sub tStun_Change ()
  gv0310 = True
  If  (Val(tStun) >= -1) And (Val(tStun) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0E92 = Val(tStun)
  sub05DE
End Sub

Sub tStun_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target is stunned and cannot move."
End Sub

Sub tSuperHit_Change ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0F4C = tSuperHit
End Sub

Sub tSuperHit_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Shown when the Supered move is carried out"
End Sub

Sub tSuperMiss_Change ()
  gv0310 = True
  gv0314(mc0024).M10B0(gv030A).M0F93 = tSuperMiss
End Sub

Sub tSuperMiss_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Oops! You missed. D'OH!"
End Sub

Sub tWeak_Change ()
  gv0310 = True
  If  (Val(tWeak) >= -1) And (Val(tWeak) <= 100) Then gv0314(mc0024).M10B0(gv030A).M0FA0.M0E78 = Val(tWeak)
  sub05DE
End Sub

Sub tWeak_MouseMove (Button As Integer, Shift As Integer, X As Single, Y As Single)
  StatLine = "Target loses awareness and takes more damage."
End Sub

Sub sub05E6 ()
Dim l0304 As T0EDB
Dim l0308 As T0E17
Dim l030A
  gv0314(mc0024).M10B0(gv030A).M0FA0.M0E78 = 0
  gv0314(mc0024).M10B0(gv030A).M0FA0.M0E81 = 0
  gv0314(mc0024).M10B0(gv030A).M0FA0.M0E5D = 0
  gv0314(mc0024).M10B0(gv030A).M0FA0.M0E6F = 0
  l0304 = gv0314(mc0024).M10B0(gv030A)
  tName = l0304.M0EE7
  tBegin2Attack = l0304.M0F05
  tBegin2SuperAttack = l0304.M0F15
  tBegin2HealSelf = l0304.M0F2A
  tHit = l0304.M0F45
  tCritHit = l0304.M0F74
  tSuperHit = l0304.M0F4C
  tHealSelf = l0304.M0F68
  tHealMeld = l0304.M0F7F
  tMiss = l0304.M0F8B
  tSuperMiss = l0304.M0F93
  tCmdKey = l0304.M0EEF
  Select Case l0304.M0FC3
    Case mc0104: oEnemy_Click
                oEnemy = 1
    Case mc0118: oFriend_Click
                 oFriend = 1
    Case mc0092: oAllFriend_Click
                    oAllFriend = 1
    Case mc009C: oAllTeam_Click
                  oAllTeam = 1
    Case mc0088: oAllFoe_Click
                 oAllFoe = 1
    Case mc007E: oAllButSelf_Click
                     oAllButSelf = 1
    Case mc010E: oEverybody_Click
                    oEverybody = 1
    Case 8: oOnlySelf_Click
            oOnlySelf = 1
    Case Else: oEnemy_Click
               oEnemy = 1
  End Select
  On Error Resume Next
  For l030A = 0 To 30
    oElement(l030A).Value = 0
  Next l030A
  On Error GoTo L7B66
  oElement_Click (l0304.M0FAA)
  oElement(l0304.M0FAA) = 1
  Label1(26).Visible = False
  tEleNum.Visible = False
  tEleNum.Text = fn00EF(l0304.M0FAA)
  GoTo L7B96
L7B66:
  Label1(26).Visible = True
  tEleNum.Visible = False
  Resume L7B96
L7B96:
  tElementStr = fn00EF(l0304.M0FB5)
  l0308 = l0304.M0FA0
  tMute = fn00EF(l0308.M0E26)
  tChaos = fn00EF(l0308.M0E2E)
  tFreeze = fn00EF(l0308.M0E37)
  tSleep = fn00EF(l0308.M0E41)
  tPoison = fn00EF(l0308.M0E4A)
  tBlind = fn00EF(l0308.M0E54)
  tWeak = fn00EF(l0308.M0E78)
  tSlow = fn00EF(l0308.M0E8A)
  tStun = fn00EF(l0308.M0E92)
  tDefUp = fn00EF(l0308.M0E5D)
  tAttUp = fn00EF(l0308.M0E6F)
  tHaste = fn00EF(l0308.M0E66)
  tRegen = fn00EF(l0308.M0EA3)
  tStop = fn00EF(l0308.M0EAC)
  tLife3 = fn00EF(l0308.M0E9A)
  tMIA = fn00EF(l0308.M0EC0)
  tQuick = fn00EF(l0308.M0EC7)
  tBerserk = fn00EF(l0308.M0ED0)
  tMushroom = fn00EF(l0308.M0EB4)
  tAttDn = fn00EF(l0308.M0E81)
  cCanSuper.Value = l0304.M0EF9
  sub05DE
End Sub
