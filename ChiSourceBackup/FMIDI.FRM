VERSION 5.00
Object = "{22D6F304-B0F6-11D0-94AB-0080C74C7E95}#1.0#0"; "MSDXM.OCX"
Begin VB.Form fMidi 
   Appearance      =   0  'Flat
   BackColor       =   &H00812577&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Juke Box"
   ClientHeight    =   2550
   ClientLeft      =   6630
   ClientTop       =   675
   ClientWidth     =   3720
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
   Icon            =   "FMIDI.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2550
   ScaleWidth      =   3720
   Begin VB.CommandButton cbApplyConfig 
      Caption         =   "Apply"
      Height          =   255
      Left            =   5040
      TabIndex        =   21
      Top             =   2280
      Width           =   1215
   End
   Begin VB.ComboBox cbSPCInterpolation 
      Height          =   315
      ItemData        =   "FMIDI.frx":0442
      Left            =   6120
      List            =   "FMIDI.frx":0452
      Style           =   2  'Dropdown List
      TabIndex        =   15
      Top             =   1800
      Width           =   1215
   End
   Begin VB.ComboBox cbSPCMix 
      Height          =   315
      ItemData        =   "FMIDI.frx":0475
      Left            =   6120
      List            =   "FMIDI.frx":0485
      Style           =   2  'Dropdown List
      TabIndex        =   14
      Top             =   1440
      Width           =   1215
   End
   Begin VB.ComboBox cbSPCStereo 
      Enabled         =   0   'False
      Height          =   315
      ItemData        =   "FMIDI.frx":04B0
      Left            =   6120
      List            =   "FMIDI.frx":04BA
      Style           =   2  'Dropdown List
      TabIndex        =   13
      ToolTipText     =   "Temporarily Out of Commission"
      Top             =   1080
      Width           =   1215
   End
   Begin VB.ComboBox cbSPCBits 
      Height          =   315
      ItemData        =   "FMIDI.frx":04CC
      Left            =   6120
      List            =   "FMIDI.frx":04D9
      Style           =   2  'Dropdown List
      TabIndex        =   12
      Top             =   720
      Width           =   1215
   End
   Begin VB.ComboBox cbSPCRate 
      Height          =   315
      ItemData        =   "FMIDI.frx":04E8
      Left            =   6120
      List            =   "FMIDI.frx":0501
      Style           =   2  'Dropdown List
      TabIndex        =   10
      Top             =   360
      Width           =   1215
   End
   Begin VB.CommandButton cbMore 
      Caption         =   "More"
      Height          =   255
      Left            =   3120
      TabIndex        =   9
      Top             =   2280
      Width           =   615
   End
   Begin VB.CommandButton cHide 
      Appearance      =   0  'Flat
      Caption         =   "Hide"
      Height          =   255
      Left            =   2520
      TabIndex        =   8
      Top             =   2280
      Width           =   615
   End
   Begin VB.CommandButton cbPause 
      Appearance      =   0  'Flat
      Caption         =   "Pause"
      Height          =   255
      Left            =   1800
      TabIndex        =   3
      Top             =   2280
      Width           =   735
   End
   Begin VB.CommandButton cbPlay 
      Appearance      =   0  'Flat
      Caption         =   "Play"
      Height          =   255
      Left            =   1200
      Picture         =   "FMIDI.frx":0535
      TabIndex        =   2
      Top             =   2280
      Width           =   615
   End
   Begin VB.TextBox What 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   600
      TabIndex        =   7
      Text            =   "what"
      Top             =   3240
      Visible         =   0   'False
      Width           =   1215
   End
   Begin VB.CheckBox Lup 
      Appearance      =   0  'Flat
      BackColor       =   &H00812577&
      Caption         =   "Loop."
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   240
      TabIndex        =   6
      Top             =   2280
      Value           =   1  'Checked
      Width           =   855
   End
   Begin VB.DriveListBox DriveBox 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   315
      Left            =   0
      TabIndex        =   5
      Top             =   0
      Width           =   1935
   End
   Begin VB.Timer tiMidi 
      Interval        =   500
      Left            =   2400
      Top             =   2880
   End
   Begin VB.FileListBox File 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1980
      Left            =   1920
      Pattern         =   "*.mid;*.sp?;*.mp3"
      TabIndex        =   0
      Top             =   0
      Width           =   1815
   End
   Begin VB.DirListBox DirBox 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1440
      Left            =   0
      TabIndex        =   1
      Top             =   360
      Width           =   1935
   End
   Begin MediaPlayerCtl.MediaPlayer MediaPlayer1 
      Height          =   495
      Left            =   3600
      TabIndex        =   22
      Top             =   3960
      Width           =   615
      AudioStream     =   -1
      AutoSize        =   0   'False
      AutoStart       =   0   'False
      AnimationAtStart=   -1  'True
      AllowScan       =   -1  'True
      AllowChangeDisplaySize=   -1  'True
      AutoRewind      =   0   'False
      Balance         =   0
      BaseURL         =   ""
      BufferingTime   =   5
      CaptioningID    =   ""
      ClickToPlay     =   -1  'True
      CursorType      =   0
      CurrentPosition =   -1
      CurrentMarker   =   0
      DefaultFrame    =   ""
      DisplayBackColor=   0
      DisplayForeColor=   16777215
      DisplayMode     =   0
      DisplaySize     =   4
      Enabled         =   -1  'True
      EnableContextMenu=   -1  'True
      EnablePositionControls=   -1  'True
      EnableFullScreenControls=   0   'False
      EnableTracker   =   -1  'True
      Filename        =   ""
      InvokeURLs      =   -1  'True
      Language        =   -1
      Mute            =   0   'False
      PlayCount       =   1
      PreviewMode     =   0   'False
      Rate            =   1
      SAMILang        =   ""
      SAMIStyle       =   ""
      SAMIFileName    =   ""
      SelectionStart  =   -1
      SelectionEnd    =   -1
      SendOpenStateChangeEvents=   -1  'True
      SendWarningEvents=   -1  'True
      SendErrorEvents =   -1  'True
      SendKeyboardEvents=   0   'False
      SendMouseClickEvents=   0   'False
      SendMouseMoveEvents=   0   'False
      SendPlayStateChangeEvents=   -1  'True
      ShowCaptioning  =   0   'False
      ShowControls    =   0   'False
      ShowAudioControls=   0   'False
      ShowDisplay     =   0   'False
      ShowGotoBar     =   0   'False
      ShowPositionControls=   0   'False
      ShowStatusBar   =   0   'False
      ShowTracker     =   -1  'True
      TransparentAtStart=   0   'False
      VideoBorderWidth=   0
      VideoBorderColor=   0
      VideoBorder3D   =   0   'False
      Volume          =   -50
      WindowlessVideo =   0   'False
   End
   Begin VB.Label Label6 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Interpolation"
      BeginProperty Font 
         Name            =   "Tempus Sans ITC"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3840
      TabIndex        =   20
      Top             =   1800
      Width           =   2175
   End
   Begin VB.Label Label5 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Mixing Engine"
      BeginProperty Font 
         Name            =   "Tempus Sans ITC"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   375
      Left            =   3840
      TabIndex        =   19
      Top             =   1440
      Width           =   2175
   End
   Begin VB.Label Label4 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Mono/Stereo"
      BeginProperty Font 
         Name            =   "Tempus Sans ITC"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3840
      TabIndex        =   18
      Top             =   1080
      Width           =   2175
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Sample Quality (bits)"
      BeginProperty Font 
         Name            =   "Tempus Sans ITC"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3840
      TabIndex        =   17
      Top             =   720
      Width           =   2175
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Sample Rate"
      BeginProperty Font 
         Name            =   "Tempus Sans ITC"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3840
      TabIndex        =   16
      Top             =   360
      Width           =   2175
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "LEET0 SPC OPTIONZ"
      BeginProperty Font 
         Name            =   "Imperial"
         Size            =   12
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   3720
      TabIndex        =   11
      Top             =   0
      Width           =   3855
   End
   Begin VB.Label LTitle 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Song Name"
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
      Left            =   0
      TabIndex        =   4
      Top             =   2040
      Width           =   3735
   End
End
Attribute VB_Name = "fMidi"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim MidState As Integer
Dim Filn$
Dim Errored%

Const MyName = "fMidi"

Private Sub LoadSPCConfig()
  On Error GoTo ETrap1
  cbSPCRate.ListIndex = Val(GetSetting("ChUB Resurrection", "SPC", "Rate", "2"))
  cbSPCBits.ListIndex = Val(GetSetting("ChUB Resurrection", "SPC", "Bits", "0"))
  cbSPCStereo.ListIndex = Val(GetSetting("ChUB Resurrection", "SPC", "Mono", "0"))
  cbSPCMix.ListIndex = Val(GetSetting("ChUB Resurrection", "SPC", "Mix", "0"))
  cbSPCInterpolation.ListIndex = Val(GetSetting("ChUB Resurrection", "SPC", "Interp", "0"))
  cbApplyConfig_Click
  Exit Sub
ETrap1:
  MsgBox "Got the following error: " + Err.Description + " The error was in LoadSPCConfig."
  Resume Next
End Sub

Private Sub SaveSPCConfig()
On Error GoTo ETrap2
  SaveSetting "ChUB Resurrection", "SPC", "Rate", TrimStr(cbSPCRate.ListIndex)
  SaveSetting "ChUB Resurrection", "SPC", "Bits", TrimStr(cbSPCBits.ListIndex)
  SaveSetting "ChUB Resurrection", "SPC", "Mono", "0"
  SaveSetting "ChUB Resurrection", "SPC", "Mix", TrimStr(cbSPCMix.ListIndex)
  SaveSetting "ChUB Resurrection", "SPC", "Interp", TrimStr(cbSPCInterpolation.ListIndex)
  Exit Sub
ETrap2:
  MsgBox "Got the following error: " + Err.Description + " The error was in SaveSPCConfig."
  Resume Next
End Sub
Private Sub cbApplyConfig_Click()
Dim SPCCfg As Uematsu_CONFIG
Dim MiscCfg As Uematsu_Seperate_config
  On Error GoTo ETrap3
  SPCCfg.SmpRate = Val(cbSPCRate.List(cbSPCRate.ListIndex))
  SPCCfg.BPS = Val(cbSPCBits.List(cbSPCBits.ListIndex))
  SPCCfg.NChn = cbSPCStereo.ListIndex + 1
  SPCCfg.MixingEngine = cbSPCMix.ListIndex + 1
  SPCCfg.Interpolation = cbSPCInterpolation.ListIndex + 1
  SPCCfg.APR = 0
  SPCCfg.BufferLength = 500
  SPCCfg.VisRate = 60
  SPCCfg.MiscOpt = 1
  MiscCfg.Amp = 30
  MiscCfg.Stereo_Seperation = 32768
  Set_Config SPCCfg, MiscCfg
  SaveSPCConfig
  Exit Sub
ETrap3:
  MsgBox "Got the following error: " + Err.Description + " The error was in cbApplyConfig_Click."
  Resume Next
End Sub

Private Sub cbMore_Click()
  On Error GoTo ETrap4
  If cbMore.Caption = "More" Then
    fMidi.Width = 7635
    cbMore.Caption = "Less"
  Else
    fMidi.Width = 3825
    cbMore.Caption = "More"
  End If
  Exit Sub
ETrap4:
  MsgBox "Got the following error: " + Err.Description + " The error was in cbMore_Click."
  Resume Next
End Sub

Private Sub cbPause_Click()
Dim Ret%
On Error GoTo ETrap5
  If cbPause.Caption = "Pause" Then
    'mpSongs.Pause
    Select Case LCase(Right$(LTitle.Caption, 3))
      Case "mid":
        Ret = mciSendString("pause ChUBMidi", 0&, 0, 0)
      Case "spc":
        DoPause
      Case "mp3":
        MediaPlayer1.Pause
    End Select
    MidState = 2
    cbPause.Caption = "Resume"
  Else
    Select Case LCase(Right$(LTitle.Caption, 3))
      Case "mid":
        Ret = mciSendString("play ChUBMidi", 0&, 0, 0)
      Case "spc":
        DoPause
      Case "mp3":
        MediaPlayer1.Pause
    End Select
    'mpSongs.Play
    MidState = 1
    cbPause.Caption = "Pause"
  End If
  Exit Sub
ETrap5:
  MsgBox "Got the following error: " + Err.Description + " The error was in cbPause_Click."
  Resume Next
End Sub

Private Sub cbPause_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhMidi_Pause)
End Sub

Private Sub cbPlay_Click()
Dim Ret As Integer
Dim Fil As String
Dim Whoops$
  On Error GoTo ETrap6
  If cbPlay.Caption = "Stop" Then
    cbPlay.Caption = "Play"
    MidState = 0    ' Stopped
    Select Case LCase(Right$(LTitle.Caption, 3))
      Case "mid":
        Ret = mciSendString("stop ChUBMidi", 0&, 0, 0)
        Ret = mciSendString("close ChUBMidi", 0&, 0, 0)
      Case "spc":
        DoStop
      Case "mp3":
        MediaPlayer1.Stop
    End Select
    cbPause.Caption = "Pause"
  Else
    cbPlay.Caption = "Stop"
    If What.Text <> "" Then
      Fil = What.Text
      What.Text = ""
    Else
      Fil = UCase(File.Path + "\" + File.List(File.ListIndex))
    End If
    Filn$ = """" + Fil + """"
    'mpSongs.Open Filn
    Select Case LCase(Right$(Fil, 3))
      Case "mid":
        Ret = mciSendString("open " + Filn + " type sequencer alias ChUBMidi", 0&, 0, 0)
        If Ret <> 0 And Errored = 0 Then GoTo FuckedUpMidi
        Ret = mciSendString("play ChUBMidi", 0&, 0, 0)
        'mpSongs.Play
        If Ret = 0 Then
          MidState = 1    ' playing
        Else
          cbPlay.Caption = "Play?"
          If Errored = 0 Then
FuckedUpMidi:
            Whoops$ = Space$(255)
            Ret = mciGetErrorString(Ret, Whoops$, 255)
            MsgBox Trim(Whoops$), 48, "Unable to play " + LCase(Filn)
            Errored = 1
          End If
        End If
      Case "spc":
        DoPlay (Fil)
      Case "mp3":
        MediaPlayer1.FileName = Fil
        MediaPlayer1.Play
    End Select
    LTitle = File.List(File.ListIndex)
  End If
  Exit Sub
ETrap6:
  MsgBox "Got the following error: " + Err.Description + " The error was in cbPlay_Click."
  Resume Next
End Sub

Private Sub cbPlay_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then
    If cbPlay.Caption = "Play" Then
      Yoshi (yhMidi_Play)
    Else
      Yoshi (yhMidi_Stop)
    End If
  End If
End Sub

Private Sub cHide_Click()
  Me.Hide
End Sub

Private Sub cHide_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhMidi_Hide)
End Sub

Private Sub DirBox_Change()
  File.Path = DirBox.Path
End Sub

Private Sub DirBox_Click()
  DirBox.ToolTipText = DirBox.List(DirBox.ListIndex)
End Sub

Private Sub DirBox_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhMidi_DirBox)
End Sub

Private Sub DriveBox_Change()
  DirBox.Path = DriveBox.List(DriveBox.ListIndex)
  File.Path = DirBox.List(DirBox.Path)
End Sub

Private Sub File_Click()
Dim Msg$
  'LTitle = File.List(File.ListIndex)
  File.ToolTipText = File.List(File.ListIndex)
End Sub

Private Sub File_DblClick()
Dim Ret%
  If File.List(File.ListIndex) <> Filn$ Then
    What.Text = ""
    Ret = mciSendString("stop ChUBMidi", 0&, 0, 0)
    Ret = mciSendString("close ChUBMidi", 0&, 0, 0)
    'mpSongs.Stop
    DoStop
    MediaPlayer1.Stop
    MidState = 0
    cbPlay.Caption = "Play"
  End If
  'cbPlay.Caption = "Stop"
  What.Text = UCase(File.Path + "\" + File.List(File.ListIndex))
  cbPlay_Click
End Sub

Private Sub File_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhMidi_File)
End Sub

Private Sub Form_Load()
Dim Ret%, P1$, P2$, P3$, P4$, PO%
  LoadPosition Me, MyName
  SetWindowPos Me.hwnd, -1, 0, 0, 0, 0, 3
  Ret = mciSendString("stop ChUBMidi", 0&, 0, 0)
  Ret = mciSendString("close ChUBMidi", 0&, 0, 0)
  DoStop
  LoadSPCConfig
  Me.Show
  If Config.NewUser Then Yoshi (yhMidi)
  P1$ = "Millenium"
  P2$ = "MidPath"
  P3$ = App.Path
  P4$ = Space$(255)
  'PO% = GetPrivateProfileString(P1$, P2$, P3$, P4$, 255, "ChUB2000.ini")
  P4$ = GetSetting("ChUB Resurrection", "Settings", P2$, P3$)
  On Error GoTo Nope
  P4$ = Trim(P4$)
  DriveBox.Drive = Left$(P4$, 1)
  DirBox.Path = P4$
Nope:
  Exit Sub
End Sub

Private Sub Form_Unload(Cancel As Integer)
  SavePosition Me, MyName
Dim Ret%, P1$, P2$, P4$, PO%
  Ret = mciSendString("stop ChUBMidi", 0&, 0, 0)
  Ret = mciSendString("close ChUBMidi", 0&, 0, 0)
  DoStop
  'mpSongs.Stop
  P1$ = "Millenium"
  P2$ = "MidPath"
  P4$ = DirBox.Path
  'PO% = WritePrivateProfileString(P1$, P2$, P4$, "ChUB2000.ini")
  SaveSetting "ChUB Resurrection", "Settings", P2$, P4$
End Sub

Private Sub Lup_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
  If Button = 2 And Shift Then Yoshi (yhMidi_Lup)
End Sub

Private Sub MediaPlayer1_PlayStateChange(ByVal OldState As Long, ByVal NewState As Long)
  Select Case MediaPlayer1.PlayState
    Case mpClosed
        'Set imgStop.Picture = ImageList1.ListImages(17).Picture
        'Set imgPlay.Picture = ImageList1.ListImages(10).Picture
        'Set imgPause.Picture = ImageList1.ListImages(7).Picture
        cbPlay.Caption = "Play"
        cbPause.Caption = "Pause"
    Case mpPaused
        'Set imgStop.Picture = ImageList1.ListImages(16).Picture
        'Set imgPlay.Picture = ImageList1.ListImages(11).Picture
        'Set imgPause.Picture = ImageList1.ListImages(8).Picture
        cbPlay.Caption = "Stop"
        cbPause.Caption = "Unpause"
    Case mpPlaying
        'Set imgStop.Picture = ImageList1.ListImages(16).Picture
        'Set imgPlay.Picture = ImageList1.ListImages(11).Picture
        'Set imgPause.Picture = ImageList1.ListImages(7).Picture
        cbPlay.Caption = "Stop"
        cbPause.Caption = "Pause"
    Case mpStopped
        'Set imgStop.Picture = ImageList1.ListImages(17).Picture
        'Set imgPlay.Picture = ImageList1.ListImages(10).Picture
        'Set imgPause.Picture = ImageList1.ListImages(7).Picture
        'cbPlay.Caption = "Play"
        'cbPause.Caption = "Pause"
  End Select
End Sub

Private Sub tiMidi_Timer()
Dim Stat$, Ret As Integer
Dim mLen%, Msg As String * 255
    Select Case LCase(Right$(LTitle.Caption, 3))
      Case "mid":
        If MidState > 0 Then
          Stat$ = "status ChUBMidi mode"
          Msg$ = String$(255, 0)
          Ret = mciSendString(Stat$, Msg$, 255, 0)
          If InStr(Msg$, "stopped") Or (InStr(Msg$, "paused") And MidState = 1) Then
            Select Case Lup.Value
              Case 1:
                Ret = mciSendString("seek ChUBMidi to start", 0&, 0, 0)
                Ret = mciSendString("play ChUBMidi", 0&, 0, 0)
                cbPlay.Caption = "Stop"
              Case Else:
                Ret = mciSendString("stop ChUBMidi", 0&, 0, 0)
                Ret = mciSendString("close ChUBMidi", 0&, 0, 0)
                cbPlay.Caption = "Play"
                cbPause.Caption = "Pause"
                If Lup.Value = 0 Then
                  File.ListIndex = Rand(0, File.ListCount - 1)
                  cbPlay_Click
                End If
            End Select
          End If
        End If
      Case "mp3":
        If MediaPlayer1.PlayState = mpStopped And cbPlay.Caption = "Stop" Then
          Select Case Lup.Value
            Case 1:
              If cbPlay.Caption = "Stop" Then MediaPlayer1.Play
            Case Else:
              If Lup.Value = 0 Then
                cbPlay.Caption = "Play"
                File.ListIndex = Rand(0, File.ListCount - 1)
                cbPlay_Click
              End If
          End Select
        End If
    End Select
End Sub

Private Sub What_Change()
  If What.Text <> UCase(File.Path + "\" + File.List(File.ListIndex)) And What.Text <> "" Then
    cbPlay_Click
    cbPlay.Caption = "Stop"
  End If
End Sub

