#
## Local version of Powershell profile
#

Function Print-Debug {
    param($message)
    "{0,-20}: {1}ms" -f $Message, (Get-RunningTime $starttime)
}

# Workaroud for Linux, loads this file directly
if (-not($starttime)) {
    $starttime = Get-Date
}

$module = "MyFunctions{0}MyFunctions{0}MyFunctions.psd1" -f [io.path]::DirectorySeparatorChar
Import-Module $(Join-Path $PSScriptRoot $module) -Force
Print-Debug "Entering profile"

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

Set-PSReadLineOption @PSReadLineOptions
Print-Debug "After PSReadLine"

if (Test-Path $Psscriptroot\PSReadLineProfile.ps1) {
    . $(Join-Path $psscriptroot PSReadLineProfile.ps1)
}
# Add output of all commands to $__, set as default value
$PSDefaultParameterValues["Out-Default:OutVariable"] = "__"

# Set default encoding format
$PSDefaultParameterValues['*:Encoding'] = 'utf8'

# Oh-my-PoSH https://ohmyposh.dev/docs
# $Theme = Join-Path ${env:USERPROFILE} ".config\oh-my-posh\my-posh-theme.omp.json"
# $env:POSH_GIT_ENABLED = $true
# Import-Module Posh-Git
# "{0,-20}: {1}ms" -f "After git",(Get-RunningTime $starttime)

#oh-my-posh.exe --init --shell pwsh --config jandedobbeleer | Invoke-Expression
#Enable-PoshTransientPrompt

# Starship config
function Invoke-Starship-PreCommand {
    $host.ui.RawUi.WindowTitle = "PS> $($psversiontable.PSEdition)@$env:USERNAME"
}

# Replace previous prompts with a leaner symbol, transient.
# Might show artifacts if PSReadLine history is long.
function Invoke-Starship-TransientFunction {
    &starship module character
}

# Enable showing symbols for the root user in starship.
$env:ROOT = $true

# Show the powershell version number in starship
$version = $PSVersionTable.PSVersion
if ($version -gt [version]"6.0"){
    $env:PSVERSION = $version
}
else {
    $value = "{0}.{1}" -f $version.major,$version.minor
    $env:PSVERSION = $value
}

Print-Debug "Before starship"
$starshipConfig = ".config{0}starship{0}starship.toml" -f [io.path]::DirectorySeparatorChar
$ENV:STARSHIP_CONFIG = Join-Path ${HOME} $starshipConfig
Invoke-Expression (&starship init powershell)
Enable-TransientPrompt

Print-Debug "Before aliases"
# Source local aliases and functions
$alias = Join-Path $PSScriptRoot aliases.ps1
if (Test-Path $alias) {
    . $alias
}

# FZF
$env:FZF_DEFAULT_OPTS = "--height=40% --layout=reverse --info=inline --border --margin=1 --padding=1"

# Ripgrep
$env:RIPGREP_CONFIG_PATH = $(Resolve-path "$HOME/.config/ripgrep/config")

## Exclude this for now
# $pip = Join-Path $PSScriptRoot python-pip.ps1
# if (Test-Path $pip) {
#     . $pip
# }

# Detect Emacs and set the editor environment to use emacsclient with the server
$emacsDir = Join-Path ${HOME} .config\emacs.new\server
if (Get-Process |? Name -Match emacs| ? Path -match emacs.exe) {
    # Edge case: Test if emacs has written to the server directory
    if (Test-Path $emacsDir){
        $serverPath = Resolve-Path $emacsDir
        $server = Resolve-Path (Get-Childitem -Path $serverPath -File -Filter server* |
            Sort -Property LastWriteTime |
            Select -Property FullName -Last 1).Fullname
        $env:EDITOR="emacsclientw.exe -f $($server.path) -c"
    }
    elseif (($env:EDITOR).length -gt 0){
        $env:EDITOR = [System.Environment]::GetEnvironmentVariable("EDITOR","USER")
    }
    else {
        $env:EDITOR = "runemacs.exe"
    }
}
elseif (($env:EDITOR).length -gt 0){
    $env:EDITOR = [System.Environment]::GetEnvironmentVariable("EDITOR","USER")
}
else {
    $env:EDITOR = "runemacs.exe"
}

Print-Debug "After EDITOR"

# Use zoxide for navigation
$env:_ZO_ECHO = 1
try {
    Invoke-Expression (& { (zoxide init powershell | Out-String) }) -ErrorAction Stop
}
catch {
    "Ignoring zoxide"
}

# Colorthemes for files
# Local fix for a bug in 5.1, see https://github.com/devblackops/Terminal-Icons/issues/5#issuecomment-1057072605
# try { Get-Command 'Import-PowerShellDataFile' -ErrorAction Stop | Out-Null }
# catch {
#     function Import-PowerShellDataFile {
#         [CmdletBinding()]
#         Param (
#             [Parameter(Mandatory = $true)]
#             [Microsoft.PowerShell.DesiredStateConfiguration.ArgumentToConfigurationDataTransformation()]
#             [hashtable] $Path
#         )
#         return $Path
#     }
# }

# Print-Debug "Before ColorThemes"
# $themeFile = ".config{0}ColorThemes{0}MyColorTheme.psd1" -f [io.path]::DirectorySeparatorChar
# $ColorTheme = Join-Path ${HOME} $themeFile
# Import-module Terminal-Icons
# Add-TerminalIconsColorTheme -Path $ColorTheme -Force
# Set-TerminalIconsTheme -ColorTheme MyColorTheme

Print-Debug "Start time"
