# Python pip completion

# From: python -m pip completion --powershell | Out-File -Encoding default -Append $PROFILE

# pip powershell completion start
if ((Test-Path Function:\TabExpansion) -and -not `
    (Test-Path Function:\_pip_completeBackup)) {
    Rename-Item Function:\TabExpansion _pip_completeBackup
}

function TabExpansion($line, $lastWord) {
    $exe = (get-command python.exe).source
    $lastBlock = [regex]::Split($line, '[|;]')[-1].TrimStart()
    if ($lastBlock.StartsWith("$exe -m pip ")) {
        $Env:COMP_WORDS=$lastBlock
        $Env:COMP_CWORD=$lastBlock.Split().Length - 1
        $Env:PIP_AUTO_COMPLETE=1
        (& $exe -m pip).Split()
        Remove-Item Env:COMP_WORDS
        Remove-Item Env:COMP_CWORD
        Remove-Item Env:PIP_AUTO_COMPLETE
    }
    elseif (Test-Path Function:\_pip_completeBackup) {
        # Fall back on existing tab expansion
        _pip_completeBackup $line $lastWord
    }
}
# pip powershell completion end

