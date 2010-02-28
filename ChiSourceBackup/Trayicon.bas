Attribute VB_Name = "TrayIcon"
Type NOTIFYICONDATA
    cbSize              As Long
    hWnd                As Long
    uID                 As Long
    uFlags              As Long
    uCallbackMessage    As Long
    hIcon               As Long
    szTip               As String * 64
End Type
Const NIM_ADD = 0
Const NIM_MODIFY = 1
Const NIM_DELETE = 2
Const NIF_MESSAGE = 1
Const NIF_ICON = 2
Const NIF_TIP = 4
Const NIF_DOUBLECLICKLEFT = 7725
Const NIF_MOUSEDOWNRIGHT = 7740
Private Declare Function Shell_NotifyIconA Lib "shell32" (ByVal dwMessage As Long, lpData As NOTIFYICONDATA) As Integer
'Sub TrayNotify(hWnd As stdole.OLE_HANDLE, Icon As Form, TipText As String)
Sub TrayNotify(hWnd As Long, Icon As Form, TipText As String)
Dim nd      As NOTIFYICONDATA
                Dim nRet    As Integer
                    nd.hWnd = Icon.hWnd
                    nd.uID = vbNull
                    nd.uCallbackMessage = &H200
                    nd.hIcon = Icon.Icon
                    nd.szTip = TipText & Chr$(0)
                    nd.uFlags = NIF_MESSAGE Or NIF_ICON Or NIF_TIP
                    nd.cbSize = Len(nd)
                    nRet = Shell_NotifyIconA(NIM_ADD, nd)
                    If nRet = 0 Then
                        Exit Sub
                    End If
End Sub
' note: In order for this area to work put code in the
' form_mousemove event like
' Call TrayClick(Button,Shift,X,Y)
Sub TrayClick(Fm As Form, hWnd As Long, Button As Integer, Shift As Integer, X As Single, Y As Single)
If (Button + Shift + Y) = 0 Then
    Select Case X
        Case NIF_DOUBLECLICKLEFT
            'AIEnable_Click
            'Fm.PopupMenu Fm.mnuTray
            'TrayRemove hWnd
            Fm.Visible = True
            Fm.WindowState = 0
            'Fm.Tag = ""
        Case NIF_MOUSEDOWNRIGHT
            'PopupMenu OpMnu
            Fm.PopupMenu Fm.mnuTray
    End Select
End If
End Sub
Sub TrayModify(hWnd As Long, Icon As Form, TipText As String)
Dim nd      As NOTIFYICONDATA
                Dim nRet    As Integer
                    nd.hWnd = hWnd
                    nd.uID = vbNull
                    nd.uCallbackMessage = &H200
                    nd.hIcon = Icon.Icon
                    nd.szTip = TipText & Chr$(0)
                    nd.uFlags = NIF_MESSAGE Or NIF_ICON Or NIF_TIP
                    nd.cbSize = Len(nd)
                    nRet = Shell_NotifyIconA(NIM_MODIFY, nd)
                    If nRet = 0 Then
                        Exit Sub
                    End If
End Sub
Sub TrayRemove(hWnd As Long)
Dim nd As NOTIFYICONDATA
                Dim nRet As Integer
                    nd.hWnd = hWnd
                    nd.uID = vbNull
                    nd.uCallbackMessage = &H200
                    nd.uFlags = NIF_MESSAGE Or NIF_ICON Or NIF_TIP
                    nd.cbSize = Len(nd)
                    nRet = Shell_NotifyIconA(NIM_DELETE, nd)
End Sub

