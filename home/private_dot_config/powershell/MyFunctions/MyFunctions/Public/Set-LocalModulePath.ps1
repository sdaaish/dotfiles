# Create a new local modulepath and add it to PSModulePath
Function Set-LocalModulePath {

    [cmdletbinding()]
    param()

    begin {
        # Current Documents folder
        $Documents = [System.Environment]::GetFolderPath("MyDocuments")
        Write-Verbose "Documents-path: $Documents"
    }

    process {
        if ($isLinux){
            # Dont do anything at the moment
            Write-Verbose "$env:PSModulePath"
        }
        else {

            # Check wich version of Powershell
            switch ($PSVersionTable.PSEdition){
                "Core" {$version = "PowerShell/Modules"}
                "Desktop" { $version = "WindowsPowerShell/Modules"}
            }

            # Resolve the path to modules depending on version of Powershell
            $LocalDirectory = [System.IO.Path]::GetFullPath((Join-Path -Path $env:USERPROFILE -ChildPath ".local"))
            $NewModuleDirectory = Join-Path -Path $LocalDirectory -ChildPath $version
            Write-Verbose "New module directory: $NewModuleDirectory"

            try {
                Test-Path $NewModuleDirectory -ErrorAction Stop-Process
            }
            catch {
                New-Item -Path $NewModuleDirectory -ItemType Directory -Force|Out-Null
            }

            $OldModulePath = $env:PSModulePath -split(";")
            [string[]]$NewModulePath = $NewModuleDirectory
            $NewModulePath += $OldModulePath
        }
    }
    end {
        $NewModulePath -join(";")
        Write-Verbose "Old module path: $env:PSModulePath"
        Write-Verbose "New module path: $NewModulePath"
    }
}
