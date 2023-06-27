# Local version of Powershell profile
Import-Module $PSScriptRoot\MyFunctions\MyFunctions\MyFunctions.psd1 -Force
"{0,-20}: {1}ms" -f "Entering profile",(Get-RunningTime $starttime)

if (Test-Admin) {
    $env:SUPERUSER = $true
}

# Use a local path for modules
$env:PSModulePath = Set-LocalModulePath

# Settings for PSReadLine
if ($env:TERM_PROGRAM) {
    $view = "InlineView" # Check for VSCode
}
else {
    $view = "ListView"
}

$PSReadLineOptions = @{
    EditMode                      = "Emacs"
    BellStyle                     = "None"
    PredictionSource              = "History"
    PredictionViewStyle           = $view
    HistorySearchCursorMovesToEnd = $true
    Colors                        = @{
        Selection = "#fd625d"
    }
}
Set-PSReadlineOption @PSReadLineOptions
"{0,-20}: {1}ms" -f "After PSReadLine",(Get-RunningTime $starttime)

if (Test-Path $Psscriptroot\PsreadlineProfile.ps1) {
    . $psscriptroot\PsreadlineProfile.ps1
}
# Add output of all commands to $__, set as default value
$PSDefaultParameterValues["Out-Default:OutVariable"] = "__"

# Oh-my-PoSH https://ohmyposh.dev/docs
$Theme = Join-Path ${env:USERPROFILE} ".config\oh-my-posh\my-posh-theme.omp.json"
$env:POSH_GIT_ENABLED = $true
Import-Module Posh-Git
"{0,-20}: {1}ms" -f "After git",(Get-RunningTime $starttime)

#oh-my-posh.exe --init --shell pwsh --config jandedobbeleer | Invoke-Expression
#Enable-PoshTransientPrompt

function Invoke-Starship-PreCommand {
    $host.ui.RawUi.WindowTitle = "PS> $($psversiontable.PSEdition)@$env:USERNAME"
}

$env:ROOT = $true

$ENV:STARSHIP_CONFIG = Join-Path ${env:UserProfile} ".config\starship\starship.toml"
Invoke-Expression (&starship init powershell)
"{0,-20}: {1}ms" -f "After starship",(Get-RunningTime $starttime)

# Colorthemes for files
# Local fix for a bug in 5.1, see https://github.com/devblackops/Terminal-Icons/issues/5#issuecomment-1057072605
try { Get-Command 'Import-PowerShellDataFile' -ErrorAction Stop | Out-Null }
catch {
    function Import-PowerShellDataFile {
        [CmdletBinding()]
        Param (
            [Parameter(Mandatory = $true)]
            [Microsoft.PowerShell.DesiredStateConfiguration.ArgumentToConfigurationDataTransformation()]
            [hashtable] $Path
        )
        return $Path
    }
}

$ColorTheme = Join-Path ${env:UserProfile} ".config\ColorThemes\MyColorTheme.psd1"
Add-TerminalIconsColorTheme -Path $ColorTheme -Force
Set-TerminalIconsTheme -ColorTheme MyColorTheme

"{0,-20}: {1}ms" -f "Before alias",(Get-RunningTime $starttime)

if (Test-Path $PSScriptRoot\aliases.ps1) {
    . $PSScriptRoot\aliases.ps1
}

if (Test-Path $PSScriptRoot\python-pip.ps1){
    . $PSScriptRoot\python-pip.ps1
}

"{0,-20}: {1}ms" -f "Start time",(Get-RunningTime $starttime)
