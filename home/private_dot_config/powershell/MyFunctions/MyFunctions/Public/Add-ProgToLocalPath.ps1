<#
.SYNOPSIS
Adds a Path to the environment variable.

.DESCRIPTION
Takes an input PATH and adds it to the current environment path.
Will strip the file part if givenincluded in the argument.
#>

Function Add-ProgToLocalPath {
	  [cmdletbinding()]
	  param(
		    [Parameter(Mandatory)]
		    $Path,

		    [ValidateSet("User", "Machine")]
		    [string]
		    $Scope = "User"
	  )

	  if (Test-Path $Path) {
		    if (Test-Path -Path $Path -PathType Leaf) {
			      $Path = Split-Path $Path -Parent -Resolve
		    }
		    else {
			      $Path = (Resolve-Path $Path).Path
		    }
	  }
	  else {
		    throw "No such Path, $Path"
	  }

	  $oldenv = ([system.environment]::GetEnvironmentVariable("Path", $Scope)) -split ([io.path]::PathSeparator)
	  $oldenv += $Path

	  $newenv = ((($oldenv | Select-Object -Unique) -join ([io.path]::PathSeparator)) + ([io.path]::PathSeparator)) -replace ';;+', ';' -replace '::', ':'
	  [System.Environment]::SetEnvironmentVariable("Path", $newenv, $Scope)
	  "New path set`n{0}`nfor Scope={1}" -f [system.environment]::GetEnvironmentVariable("Path", $Scope), $Scope
}
