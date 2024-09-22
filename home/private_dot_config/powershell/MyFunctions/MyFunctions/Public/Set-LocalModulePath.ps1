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
        if ($isLinux) {
            # Dont do anything at the moment
            Write-Verbose "$env:PSModulePath"
        }
        else {

            # Resolve the path
            $NewModuleDirectory = [System.IO.Path]::GetFullPath((Join-Path -Path $env:USERPROFILE -ChildPath ".local/share/PowerShell/Modules"))
            Write-Verbose "New module directory: $NewModuleDirectory"

            try {
                Test-Path $NewModuleDirectory -ErrorAction Stop-Process
            }
            catch {
                New-Item -Path $NewModuleDirectory -ItemType Directory -Force | Out-Null
            }

            $OldModulePath = $env:PSModulePath -split ([io.path]::PathSeparator)
            [string[]]$NewModulePath = $NewModuleDirectory
            $NewModulePath += $OldModulePath
        }
    }
    end {
        $ModulePath = ($NewModulePath | Select-Object -Unique) -join ([io.path]::PathSeparator)
        Write-Verbose "Old module path: $env:PSModulePath"
        Write-Verbose "New module path: $ModulePath"
        $ModulePath
    }
}
