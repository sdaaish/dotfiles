<#
.SYNOPSIS
A bunch of functions to use when presenting

.DESCRIPTION
Various functions and aliases that help with presentation of tools, git among others.
#>


function Show-GitCommand {
    param(
        [string[]]$options
    )
    $cmd = "git"
    ">>> Command: {0} {1}`n" -f $cmd,($options -join " ")
    & $cmd @options
}
function s {
    $options = @("status","-sb")
    Show-GitCommand $options
}

function lt {
    $options = @("log", "--graph", "--oneline", "--decorate", "--all")
    Show-GitCommand $options
}
function st {
    $options = @("ls-files", "--stage")
    Show-GitCommand $options
}
