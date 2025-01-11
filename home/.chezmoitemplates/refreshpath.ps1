# Reload environment for PATH

# A template for the Chezmoi scripts
Function refreshpath {
    $paths = @(
				([System.Environment]::GetEnvironmentVariable("Path", "Machine") -split ([io.path]::PathSeparator))
				([System.Environment]::GetEnvironmentVariable("Path", "User") -split ([io.path]::PathSeparator))
    )
    $env:path = ($paths | Select-Object -Unique) -join ([io.path]::PathSeparator)
}
refreshpath
