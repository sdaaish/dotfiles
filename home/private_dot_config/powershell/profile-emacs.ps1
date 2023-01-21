# Separate prompt for Powershell in Emacs as inferior shell.

Import-Module $PSScriptRoot\MyFunctions\MyFunctions\MyFunctions.psd1

# Use a local path for modules
$env:PSModulePath = Set-LocalModulePath

# See https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_prompts?view=powershell-7.2
function prompt {

    # Catch the error code from the global scope
    $exit = $global:?

    # The at sign creates an array in case only one history item exists.
    $history = @(Get-History)
    if($history.Count -gt 0)
    {
	      $lastItem = $history[$history.Count - 1]
	      $lastId = $lastItem.Id
    }

    $nextCommand = $lastId + 1
    $currentDirectory = Get-Location
    $time = Get-Date -Format "yyyy-MM-dd HH:mm"

    $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = [Security.Principal.WindowsPrincipal] $identity
    $adminRole = [Security.Principal.WindowsBuiltInRole]::Administrator

    $context = $(if (Test-Path variable:/PSDebugContext) { "[DBG]" }
                 elseif($principal.IsInRole($adminRole)) { "[ADMIN]" }
                 else { '' }
                )

    $prompt1 = @(
        "$($PSStyle.Foreground.Cyan)${time}$($PSStyle.Reset)"
        "$($PSStyle.Bold)$($PSStyle.Foreground.Magenta) - [PS:${PSEdition}] - $($PSStyle.Reset)"
        "$($PSStyle.Foreground.Blue)${nextCommand} - $($PSStyle.Reset)"
        "$($PSStyle.ForeGround.BrightCyan)${env:USERNAME}@${env:COMPUTERNAME} ${currentDirectory}$($PSStyle.Reset)"
    )

    $prompt2 = $(
        if ($exit) {
            "$($PSStyle.Bold)$($PSStyle.ForeGround.BrightGreen)OK$($PSStyle.Reset)"
            "$($PSStyle.Bold)$($PSStyle.ForeGround.BrightBlue)${context} >$($PSStyle.Reset)"
        }
        else {
            "$($PSStyle.Bold)$($PSStyle.ForeGround.BrightRed)!!$($PSStyle.Reset)"
            "$($PSStyle.Bold)$($PSStyle.ForeGround.BrightBlue)${context} >$($PSStyle.Reset)"
        }
    )

    Write-Host $($prompt1 -join "")
    Write-Host $($prompt2) -NoNewLine
    return " "
}

# Use the same aliases here as in the Terminal version.
if (Test-Path $PSScriptRoot\aliases.ps1){
    . $PSScriptRoot\aliases.ps1
}

# Change dircolors in Emacs
$dir = @(
    $($psstyle.background.black)
    $($psstyle.foreground.Blue)
    $($psstyle.bold)
)

$Psstyle.fileinfo.directory = $($dir -join "")
