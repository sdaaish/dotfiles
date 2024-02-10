<#
.SYNOPSIS
Removes the Path from the environment variable

.DESCRIPTION
Takes an input PATH and removes it from the current environment path.
#>

function Remove-FromPath {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
		    $Path,

		    [ValidateSet("User", "Machine")]
		    [string]
		    $Scope = "User"

    )

    process {

        $msg = "Original Path: {0}" -f $path
        Write-Verbose "$msg"

        # I the path is entered with trailing slash or have a executable file
        if ($Path -match "(\\$)?(\\\w{1,}\.exe$)?"){
            $rpath = $Path -replace "(\\$)?(\\\w{1,}\.exe)?",""
        }

        # Create a new path that will work for regex
        $rpath = $rpath -replace "\\","\\"

        $msg = "Replacement path: {0}" -f $rpath
        Write-Verbose "$msg"

        $oldenv = [system.environment]::GetEnvironmentVariable("Path", $Scope)
        $msg = "Old environment: {0}" -f $oldenv
        Write-Verbose "$msg"

        # Search for matching part, and replace up to and including \; if it exists
        if ($oldenv -match $rpath){
            $rpath = "({0})(\\;)?" -f $rpath
            $newenv = $oldenv -replace $rpath,""
            $newenv = ($newenv) -replace ';;+', ';' -replace '::+', ':' -replace '\;', ''
            $msg = "{0}New environment: {1}" -f [Environment]::NewLine,$newenv
            Write-Verbose "$msg"

       	    [System.Environment]::SetEnvironmentVariable("Path", $newenv, $Scope)
            "New path set`n{0}`nfor Scope={1}" -f [system.environment]::GetEnvironmentVariable("Path", $Scope), $Scope
        }
        else {
            $msg = "No such path in evironment, {0}" -f $Path
            Write-Error $msg
        }
    }
}
