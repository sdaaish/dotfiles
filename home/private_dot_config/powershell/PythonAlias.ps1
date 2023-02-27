# File to keep python alias in

# Aliases
Set-Alias -Name pipu -Value Upgrade-PipPackage
Set-Alias -Name pipc -Value Clear-PipCache
Set-Alias -Name pv -Value Enable-Env
Set-Alias -Name penv -Value New-PythonEnv
Set-Alias -Name de -Value deactivate
Set-Alias -Name pycheck -Value Check-PythonVersion

# Functions
Function Upgrade-PipPackage {
    param(
        [string]$Package
    )
    if ($Package){
        python --no-cache-dir -m $Package install --upgrade
    }
    else {
        python -m pip install --upgrade pip
    }
}

Function Clear-PipCache {
    pip cache purge
}

Function New-PythonEnv {
    param(
        [Parameter(Mandatory)]
        $EnvPath
    )
    python -m venv $EnvPath
}

Function Enable-Env {
    param(
        [Parameter(Mandatory)]
        $EnvPath
    )

    $ErrorActionPreference = "Stop"

    try {
        & (Join-Path $EnvPath Scripts/Activate.ps1)
    }
    catch {
        Write-Error "No such environment, $EnvPath"
    }
}
Function Check-PythonVersion {
    py -c "import sys;print(sys.version)"
    py -c "import sys;print(sys.executable)"
}
