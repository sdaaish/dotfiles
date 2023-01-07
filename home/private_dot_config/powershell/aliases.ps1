# My various aliases

# Set own aliases
Set-Alias -Name src -Value Reload-PowershellProfile
Set-Alias -Name alias -Value Search-Alias

Set-Alias -Name em -Value emacs-client
Set-Alias -Name emx -Value emacs-client

Set-Alias -Name gs -Value Get-CommandSyntax

# Listing files
Set-Alias -Name lll -Value Find-Links

# Git
Set-alias -Name gts -Value Get-MyGitStatus
Set-Alias -Name gtl -Value Get-MyGitLog
Set-Alias -Name dotgit -Value Invoke-DotGit
Set-Alias -Name dgit -Value Invoke-DotGit

# Docker
Set-Alias -Name dk -Value 'docker'
Set-Alias -Name dco -Value 'docker-compose'

# Ubuntu multipass virtual servers
Set-Alias -Name mps -Value multipass

# Defender
Set-Alias -Name mdatp -Value  'C:\Program Files\Windows Defender\MpCmdRun.exe'

# Firefox
Set-Alias -Name ff -Value Start-Firefox

Set-Alias -Name myexplorer -Value Set-MyExplorer

# Ubuntu multipass virtual servers
Set-Alias -Name mps -Value multipass

Set-Alias -Name top -Value Get-TopProcess

Set-Alias -Name ra -Value Resolve-Address

Set-Alias -Name sus -Value Update-Scoop
Set-Alias -Name wug -Value Update-WinGet

Set-Alias -Name ytp -Value yt-dlp

# Some functions
function .. {
    cd ..
}
function ... {
    cd ..\..
}
function cdh {
    Set-Location ~
}
function cdm {
    Set-Location ~\Videos
}
function cdr {
    Set-Location ~\repos
}
function cdrw {
    Set-Location ~\Work
}
function cdw {
    Set-Location ~\Downloads
}
function cdv {
    Set-Location ~\Vagrantdir
}
function ls {
    Get-ChildItem $args -Attributes H,!H,A,!A,S,!S
}
function ll {
    [cmdletbinding()]
    Param (
        $Path
    )

    Get-ChildItem $Path -Attributes H,!H,A,!A,S,!S
}

function lla {
    [cmdletbinding()]
    Param (
        $Path
    )
    Get-ChildItem $Path -Attributes H,!H,A,!A,S,!S,C,!C,E,!E
}

function lls {
    [cmdletbinding()]
    Param (
        $Path
    )
    Get-ChildItem $Path -Attributes H,!H,A,!A,S,!S|Sort-Object Length
}

function llt {
    [cmdletbinding()]
    Param (
        $Path
    )
    Get-ChildItem $Path -Attributes H,!H,A,!A,S,!S| Sort-Object lastwritetime
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

function emacs-client() {
    $date =  Get-Date -Format 'yyyyMMdd-HH.mm.ss'
    $logfile = Join-Path $(Resolve-path ~/tmp) "emacs-client-${date}.log"
    # Workaround for using chemacs2 with server in Windows10
    $serverfile = $(Resolve-Path ~/.config/emacs.default/server/server -ErrorAction ignore).Path

    $cmd = Get-Command emacsclientw.exe
    $options = @(
        "--quiet"
        "--alternate-editor=runemacs.exe"
        "--server-file=${serverfile}"
        "--create-frame"
    )

    # Starts emacsclient and daemon if not started
    if ($args.count -eq 0 ) {
        # Create a new frame if no files as argument
        & $cmd @options *> $logfile
    }
    else {
        # Dont create a new frame if files exists as argument
        & $cmd @options  $args *> $logfile
    }
}

# Alias for git status
Function Get-MyGitStatus {
    git status -sb
}

# Kill explorer and restart it
function pse {
    Get-Process -Name explorer|Stop-Process -force
    Write-Host "Explorer restarted"
}

# Find links in the filesystem
function Find-Links {
    [cmdletbinding()]
    Param (
        $Path
    )
    Get-ChildItem $Path -ErrorAction SilentlyContinue|
      Where-Object {$_.Linktype}|
      Select-Object FullName, Target,LastWriteTime,LinkType
}
Function Get-CommandSyntax {
    [cmdletbinding()]
    Param (
        $command
    )
    Get-Command $command -Syntax
}

Function Reload-PowershellProfile {
    . $(Join-Path ~ .config/powershell/profile.ps1)
}

# Make update of path easier
Function refreshenv {
		$paths = @(
				([System.Environment]::GetEnvironmentVariable("Path","Machine") -split ";")
				([System.Environment]::GetEnvironmentVariable("Path","User") -split ";")
		)
		$env:path = ($paths|Select-Object -Unique) -join ";"
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
    param()
    winget upgrade --source=winget $args
}

function Update-Scoop {
    param()
    scoop update ; scoop status
}

Function Install-WslTools {
    $uri = "https://github.com/rupor-github/wsl-ssh-agent/releases/latest/download/wsl-ssh-agent.json"
    scoop install $uri
}
