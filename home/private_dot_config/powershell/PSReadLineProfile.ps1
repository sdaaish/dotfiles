# Profile settings for PSReadlineKeyHandler
# See  https://github.com/PowerShell/PSReadLine/blob/master/PSReadLine/SamplePSReadLineProfile.ps1

Set-PSReadLineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadLineKeyHandler -Key DownArrow -Function HistorySearchForward
Set-PSReadLineKeyHandler -Key Ctrl+q -Function TabCompleteNext
Set-PSReadLineKeyHandler -Key Ctrl+Q -Function TabCompletePrevious
