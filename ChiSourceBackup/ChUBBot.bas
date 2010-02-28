Attribute VB_Name = "ChUBBot"
Option Explicit

Sub ToChUBBot(ByVal S$)
  mIRCChatSend ("/.ctcp ChUBBot1 " + S$)
  mIRCChatSend ("/.ctcp ChUBBot2 " + S$)
End Sub

