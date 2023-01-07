Function Add-ProgToLocalPath {
		[cmdletbinding()]
		param(
				[Parameter(Mandatory)]
				$Path,

				[ValidateSet("User","Machine")]
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

		$oldenv = ([system.environment]::GetEnvironmentVariable("Path",$Scope)) -split ";"
		$oldenv += $Path

		$newpath = ((($oldenv | Select-Object -Unique) -join ";") + ";") -replace ';;+',';'
		[System.Environment]::SetEnvironmentVariable("Path",$newpath,$Scope)
		"New path set`n{0}`nfor Scope={1}" -f [system.environment]::GetEnvironmentVariable("Path",$Scope),$Scope
}
