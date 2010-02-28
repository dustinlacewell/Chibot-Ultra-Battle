Attribute VB_Name = "mdlJCOM_SPC"
'JCOM-SPC main module for VB: Playing only version by John Moore
'Website: jcom.shorturl.com     email:butter_pat_head@hotmail.com
'Date created:Who cares?/Who cares?/2001
'Last updated:26/02/2001
'Platform: Win9x / WinME
'conpatibility:VB4 (untested),VB5 ,VB6 (untested)
'
'INFO:
'This modlue should be able to be used in any vb 4,5,6 app
'you may choose to use it in, just if possible, give me a
'little credit in you finised app!
'This file contains most of the Uematsu.dll api calls
'which are described in 'Uematsu.html' which can be found
'with the current release of 'Super Jukebox'.
'I have written some subs to make using Uematsu.dll easyer:
'
' DoPlay (Fname as String)
'   Plays the SPC specified in Fname, eg.
'   spctoplay = "c:\spcs\finfan3.spc"
'   DoPlay (spctoplay) Or DoPlay("c:\spcs\finfan3.spc")
'   Notes:
'   If called while a spc is playing it will restart the SPC,
'   If called while a spc is SPCPaused it will resume playback.
'
' DoPause
'   Pauses / Unpauses currenly playing spc
'
' DoStop
'   Stops the currently playing spc
'   Note: Allways call this when you shut you app down
'           (if possible, avoid using ENDs).
'
' Set_Config (cfg1 as Uematsu_CONFIG, cfg2 as Uematsu_Seperate_config)
'   Sets SNESAPU/Uematsu.dll's configuration.
'   Not 100% needed but is usefull if you want optimum
'   performance on a range of different specced PCs.
    
Public Const THREAD_PRIORITY_IDLE As Long = -15
Public Const THREAD_PRIORITY_LOWEST As Long = -2
Public Const THREAD_PRIORITY_BELOW_NORMAL As Long = -1
Public Const THREAD_PRIORITY_NORMAL As Long = 0
Public Const THREAD_PRIORITY_ABOVE_NORMAL As Long = 1
Public Const THREAD_PRIORITY_HIGHEST As Long = 2
Public Const THREAD_PRIORITY_TIME_CRITICAL As Long = 15
Public temp As Long
Public Playing As Boolean
Public TimePlaying As Long
Public SPCPaused As Boolean
Public Current As String
Public FileType As Byte
Type Uematsu_Seperate_config
    Mixing_Priority As Long         'Note: use const's defined up top of file but is usually best if left at 0
    Amp As Long                     'Preamp level: 30 is usually the best
    Stereo_Seperation As Long       'Set Stereo seperation:
                                    'Settings range from:
                                    '0 - No seperation (mono)
                                    '32768 - Normal (snes)
                                    '65536 - Full (completly left / right, no center)
End Type
Type Uematsu_CONFIG
    SmpRate As Long                 'Sample rate in Hertz (8000 > 96000)
    BPS     As Long                 'Number of bits per sample (8 / 16 / 32)
    NChn As Long                    'Number of channels, 1=mono, 2=stereo
    MixingEngine As Long            'Mixing engine to use (1 = 386, 2 = Intel MMX, 3 = AMD 3DNow!, 4 = Intel SSE
    Interpolation As Long           'Sound interpolation, 0=none, 1=linear, 2=cubic, 3=gaussian
    BufferLength As Long            'Sound output buffer length (in milliseconds) 100-10000
    VisRate As Long                 'Rate of visualization in Hertz, 10-120 (leave alone for now)
    APR As Long                     'Automatic preamp (0 off/1 on)
    APRThreshhold As Long           'automatic preamp threshold
    MiscOpt As Long                 'Misc options (bit 1 = low pass filter, bit 2 = old sample decompression routine, bit 3 = "Surround" sound)
End Type
Declare Function Uematsu_SetPreAmp Lib "uematsu.dll" (ByVal Amp As Long) As Long
Declare Function Uematsu_SetWindowHandle Lib "uematsu.dll" (ByVal frmHwnd As Long) As Long
Declare Function Uematsu_SetMainSS Lib "uematsu.dll" (ByVal Seperation As Long) As Long
Declare Function Uematsu_GetCoreInfo Lib "uematsu.dll" (ByRef Version As Long, ByRef MinimumVersion As Long, ByRef MiscOptions As Long) As Long
Declare Function Uematsu_Play Lib "uematsu.dll" (ByVal lpszFilename As String) As Long
Declare Function Uematsu_Pause Lib "uematsu.dll" () As Long
Declare Function Uematsu_UnPause Lib "uematsu.dll" () As Long
Declare Function Uematsu_Stop Lib "uematsu.dll" () As Long
Declare Function Uematsu_SetConfiguration Lib "uematsu.dll" (lpCfg As Uematsu_CONFIG) As Long
'Declare Function Uematsu_GetID666Tag Lib "Uematsu.dll" (ByRef ipszFileName As String, lpTag As ID666_tag) As Long
Declare Function Uematsu_GetPlayTime Lib "uematsu.dll" () As Long
Declare Function Uematsu_MuteChannels Lib "uematsu.dll" (nChannels As Long) As Long
Declare Function Uematsu_SetMixingThreadPriority Lib "uematsu.dll" (nPriority As Long) As Long

Sub DoPlay(Fname As String)
Dim TempFname As String
TempFname = Fname
If SPCPaused = True And Playing = True Then
    temp = Uematsu_UnPause()
    If temp <> 0 Then SPCPaused = False
End If
If Playing = True Then
    temp = Uematsu_Stop()
    If temp <> 0 Then Playing = False
End If
temp = Uematsu_Play(TempFname)
If temp <> 0 Then Playing = True
End Sub
Sub DoPause()
If SPCPaused = False And Playing = True Then
    temp = Uematsu_Pause()
    If temp <> 0 Then SPCPaused = True
    Exit Sub
End If
If SPCPaused = True And Playing = True Then
    temp = Uematsu_UnPause()
    If temp <> 0 Then SPCPaused = False
    Exit Sub
End If
End Sub
Sub DoStop()
If Playing = True Then
    temp = Uematsu_Stop()
    If temp <> 0 Then Playing = False: SPCPaused = False
End If
End Sub
Sub Set_Config(cfg1 As Uematsu_CONFIG, cfg2 As Uematsu_Seperate_config)
temp = Uematsu_SetConfiguration(cfg1)
temp = Uematsu_SetPreAmp(cfg2.Amp)
temp = Uematsu_SetMixingThreadPriority(cfg2.Mixing_Priority)
temp = Uematsu_SetMainSS(cfg2.Stereo_Seperation)
End Sub


