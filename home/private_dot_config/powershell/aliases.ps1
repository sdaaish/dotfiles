# My various aliases

# Set own aliases
Set-Alias -Name ci -Value code-insiders.cmd
Set-Alias -Name cm -Value chezmoi
Set-Alias -Name cmdiff -Value Chezmoi-Diff
Set-Alias -Name cms -Value Chezmoi-Status
Set-Alias -Name cmg -Value Chezmoi-GitStatus
Set-Alias -Name cmu -Value Chezmoi-UnManaged
Set-Alias -Name cmr -Value Chezmoi-RecursiveDiff

Set-Alias -Name dirr -Value Get-ChildItemRecursive

Set-Alias -Name src -Value Reload-PowershellProfile
Set-Alias -Name alias -Value Search-Alias

Set-Alias -Name em -Value emacs-client
Set-Alias -Name emx -Value emacs-client
Set-Alias -Name emc -Value Select-EmacsVersion
Set-Alias -Name emq -Value EmacsQ

Set-Alias -Name gs -Value Get-CommandSyntax

# Listing files
Set-Alias -Name lll -Value Find-Links

# Git
Set-Alias -Name gts -Value Get-MyGitStatus
Set-Alias -Name gtl -Value Get-MyGitLog
Set-Alias -Name dotgit -Value Invoke-DotGit
Set-Alias -Name dgit -Value Invoke-DotGit

# Docker
Set-Alias -Name dk -Value 'docker'
Set-Alias -Name dco -Value 'docker compose'

# Rancher desktop/Kubernetes
Set-Alias -Name k -Value 'kubectl'
Set-Alias -Name nk -Value 'nerdctl'

# Ubuntu multipass virtual servers
Set-Alias -Name mps -Value multipass

# Defender
Set-Alias -Name mdatp -Value 'C:\Program Files\Windows Defender\MpCmdRun.exe'

# Firefox
Set-Alias -Name ff -Value Start-Firefox

Set-Alias -Name myexplorer -Value Set-MyExplorer

# Ubuntu multipass virtual servers
Set-Alias -Name mps -Value multipass

Set-Alias -Name top -Value Get-TopProcess

Set-Alias -Name ra -Value Resolve-Address
Set-Alias -Name lo -Value Resolve-LocalDnsName

Set-Alias -Name sus -Value Update-Scoop
Set-Alias -Name wug -Value Update-WinGet

Set-Alias -Name ytp -Value yt-dlp

if (Test-Path Alias:man) {
    Remove-Item Alias:man
}
if (-not($isLinux)) {
    Set-Alias -Name man -Value Search-LinuxMan
}

# Utilities
Set-Alias -Name fdq -Value Format-DoubleQuote
Set-Alias -Name cl -Value New-List
Set-Alias -Name gopass -Value $env:localappdata\gopass\gopass.exe

# Some functions
function .. {
    Set-Location ..
}
function ... {
    Set-Location $(Join-Path .. ..)
}
function cdh {
    Set-Location ~
}
function cdm {
    Set-Location $(Join-Path $HOME Videos)
}
function cdr {
    Set-Location $(Join-Path $HOME repos)
}
function cdrw {
    Set-Location $(Join-Path $HOME Work)
}
function cdw {
    Set-Location $(Join-Path $HOME Downloads)
}
function cdv {
    Set-Location $(Join-Path $HOME Vagrantdir)
}
function ls {
    Get-ChildItem $args -Attributes H, !H, A, !A, S, !S
}
function ll {
    [cmdletbinding()]
    Param (
        $Path
    )

    Get-ChildItem $Path -Attributes H, !H, A, !A, S, !S
}

function lla {
    [cmdletbinding()]
    Param (
        $Path
    )
    Get-ChildItem $Path -Attributes H, !H, A, !A, S, !S, C, !C, E, !E
}

function lle {
    [cmdletbinding()]
    Param (
        $Path
    )
    Get-ChildItem $Path -File -Attributes H, !H, A, !A, S, !S |
        Group-Object -Property Extension |
        Sort-Object -Property count -Descending
}

function lls {
    [cmdletbinding()]
    Param (
        $Path
    )
    Get-ChildItem $Path -Attributes H, !H, A, !A, S, !S | Sort-Object Length
}

function llt {
    [cmdletbinding()]
    Param (
        $Path
    )
    Get-ChildItem $Path -Attributes H, !H, A, !A, S, !S | Sort-Object lastwritetime
}
# Alias for help-command
function gh([string]$help) {
    $ErrorActionPreference = "Ignore"
    Get-Help -Name $help -Online
}
# Debug emacs
Function emdi {
    emacs.exe --debug-init
}

# Start emacsclient with custom emacs server
function emacs-client() {
    [cmdletbinding()]
    param(
        [Parameter(Position = 0)]
        [string]$InitDir = $(Join-Path $HOME .config/emacs),

        [Parameter(Position = 1, ValueFromRemainingArguments)]
        [string[]]$Path

        #        [string]$ServerName = "server"
    )

    # Default servername is "server" but you can run Emacs with multiple different servernames.
    #    $ServerName = "server/${ServerName}"
    $ServerName = "server/server"
    $date = Get-Date -Format 'yyyyMMdd-HH.mm.ss'
    $logfile = Join-Path $(Resolve-Path ~/tmp) "emacs-client-${date}.log"
    $serverfile = Join-Path $(Resolve-Path $InitDir) $ServerName -ErrorAction ignore

    $cmd = Get-Command emacsclientw.exe
    $options = @(
        "--quiet"
        "--alternate-editor=runemacs.exe"
        "--server-file=${serverfile}"
        "--create-frame"
    )

    $cmdline = $options -join " "
    "$cmd {0} {1}" -f $($options -join " "), ($path -join " ")
    Start-Process $cmd -ArgumentList $cmdline
}

Function Select-EmacsVersion {
    # Choose and launch a Emacs from installed versions
    try { $exe = (Get-Command runemacs.exe -ErrorAction Stop).path }
    catch { throw "No such file, 'runemacs.exe'" }

    $versions = Get-ChildItem ~/.config -Filter *emacs* -Directory |
        Where-Object { $_.basename -notmatch "chemacs" } |
        Select-Object resolvedtarget, basename

    $i = 0
    $versions.ForEach(
        {
            "[{1}] {0,-20}" -f $_.basename, $i++
        }
    )

    $answer = Read-Host -Prompt "Select version"

    if ([system.string]::IsNullOrWhiteSpace($answer)) {
        break
    }

    $OldPreference = $ErrorActionPreference
    $ErrorActionPreference = "SilentlyContinue"

    $valid = 0..$($versions.count - 1)
    if ([int]$answer -in $valid) {

        $selected = $versions[$answer].resolvedtarget
        $options = @(
            "--init-directory=$selected"
            "--geometry=150x50+10+10"
        )

        "$exe {0}" -f $($options -join " ")
        Start-Process $exe -ArgumentList $options
    }

    $ErrorActionPreference = $OldPreference
}

# Wrapper to start Emacs in the terminal
Function EmacsQ {
    emacs.exe -Q -nw --eval "(progn (setq visible-bell t)(setq ring-bell-function 'ignore))" $args
}

# Alias for git status
Function Get-MyGitStatus {
    git status -sb
}

# Kill explorer and restart it
function pse {
    Get-Process -Name explorer | Stop-Process -Force
    Write-Host "Explorer restarted"
}

# Find links in the filesystem
function Find-Links {
    [cmdletbinding()]
    Param (
        $Path
    )
    Get-ChildItem $Path -ErrorAction SilentlyContinue |
        Where-Object { $_.Linktype } |
        Select-Object FullName, Target, LastWriteTime, LinkType
}
Function Get-CommandSyntax {
    [cmdletbinding()]
    Param (
        $command
    )
    Get-Command $command -Syntax
}

Function Reload-PowershellProfile {

    . $profile.CurrentUserAllHosts
}

# Make update of path easier
Function refreshenv {
    $paths = @(
				([System.Environment]::GetEnvironmentVariable("Path", "Machine") -split ([io.path]::PathSeparator))
				([System.Environment]::GetEnvironmentVariable("Path", "User") -split ([io.path]::PathSeparator))
    )
    $env:path = ($paths | Select-Object -Unique) -join ([io.path]::PathSeparator)
}

Function mysudo {
    param(
        [Parameter(Mandatory)]
        [string]$FilePath,

        [Parameter(ValueFromRemainingArguments)]
        [string[]]$ArgumentList
    )

    Start-Process @PSBoundParameters -Verb Runas
}

function Update-WinGet {
    [cmdletbinding()]

    param(
        [string[]]$Pkg
    )

    if ($Pkg.count -gt 0) {
        Write-Host "Upgrading packages $($Pkg -join " ")" -ForegroundColor Green
        $Pkg.foreach(
            {
                Write-Host "Upgrading $_" -ForegroundColor Green
                winget upgrade --source=winget $_
            }
        )
    }
    else {
        Write-Host "Checking status..." -ForegroundColor Green
        winget upgrade --source=winget
    }


}

function Update-Scoop {
    param(
        [string]$package
    )

    if ([system.string]::isNullOrWhiteSpace($package)) {
        & scoop update
        & scoop status
    }
    else {
        & scoop update $package
    }
}

Function Install-WslTools {
    $uri = "https://github.com/rupor-github/wsl-ssh-agent/releases/latest/download/wsl-ssh-agent.json"
    scoop install $uri
}

# Chezmoi
function Chezmoi-Diff {
    chezmoi git pull -- --autostash --rebase
    chezmoi diff
}
Function Chezmoi-Status {
    param(
        $Path = $(Resolve-Path ".")
    )
    chezmoi status $Path
}
Function Chezmoi-GitStatus {
    chezmoi git status -- -sb
}
Function Chezmoi-UnManaged {
    param(
        $Path = "."
    )
    chezmoi unmanaged $(Resolve-Path $Path)
}
Function Chezmoi-RecursiveDiff {
    param(
        $Path = $(Resolve-Path ".")
    )
    $Path = $Path -replace "\\$", ""
    chezmoi diff --recursive $Path
}

# Show current week
function Get-CurrentWeek {
    $culture = [System.Globalization.CultureInfo]::CurrentCulture
    $Calendar = $culture.DateTimeFormat.Calendar
    $date = Get-Date
    [int]$week = $Calendar.GetWeekOfYear($date, [System.Globalization.CalendarWeekRule]::FirstFourDayWeek, [DayOfWeek]::Monday)
    $week
}

# Take a list of strings and add qoutes.
function Format-DoubleQuote {
    param (
        [string[]]$List
    )

    $tmp = ($list -split ",") -join ""","""
    $result = """$tmp"""
    $result
}

# Return a list of objects.
Function New-List {
    $args
}

# Load python aliases
if (Test-Path $PSScriptRoot/PythonAlias.ps1) {
    . $(Join-Path $PSScriptRoot PythonAlias.ps1)
}

# Load local aliases
if (Test-Path $PSScriptRoot/local.ps1) {
    . $(Join-Path $PSScriptRoot local.ps1)
}

# Find files recursive with pattern
Function Get-ChildItemRecursive {
    param(
        [Parameter(
            Position = 0,
            ValueFromPipeLine,
            HelpMessage = "Enter one or more paths to search in."
        )]
        [string[]]$Path = ".",

        [Parameter(
            Position = 1,
            ValueFromPipeLine,
            HelpMessage = "Enter the filter to search for, default='*'."
        )]
        [string]$Filter = "*"
    )

    $dirOptions = @{
        Recurse = $true
        Path    = $Path
        Filter  = $Filter
        File    = $true
    }

    $selectOptions = @{
        Property = @("FullName", "Length", @{Label = "LastWriteTime"; Expression = { Get-Date $_.LastWriteTime -UFormat "%F %T" } })
    }

    Get-ChildItem @dirOptions | Select-Object @selectOptions
}

# Lookup a name with DNSClient-PS
Function Resolve-LocalDnsName {
    [cmdletbinding()]
    param(
        [Parameter(Mandatory, Position = 0)]
        [string]$Name,

        [Parameter(Position = 1)]
        [string]$NameServer
    )

    switch -regex ($name) {
        "([0-9]{1,3}\.){3}[0-9]{1,3}" {
            $response = (Resolve-Dns -Query $Name -QueryType PTR -NameServer $Nameserver).answers
        }
        "/([0-9a-fA-F]{1,4}::?){1,7}([0-9a-fA-F]{1,4})" {
            $response = (Resolve-Dns -Query $Name -QueryType PTR -NameServer $Nameserver).answers
        }
        default {
            $response = (Resolve-Dns -Query $Name -QueryType A -NameServer $Nameserver).answers
            $response += (Resolve-Dns -Query $Name -QueryType AAAA -NameServer $Nameserver).answers
        }
    }
    $response
}
