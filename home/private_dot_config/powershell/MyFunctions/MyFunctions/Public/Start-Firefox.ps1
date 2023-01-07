# Find the correct executable for firefox
Function Start-Firefox {
    [cmdletbinding()]
    Param (
        [string]$InitProfile = "HOME",
        [string]$Url
    )

    # Default install
    if (Test-Path 'C:\Program Files\Mozilla FireFox\firefox.exe') {
        $firefox =  'C:\Program Files\Mozilla FireFox\firefox.exe'
    }
    # Scoop install
    elseif (Test-Path ${env:USERPROFILE}/scoop/apps/firefox/current/firefox.exe) {
        $firefox = Resolve-Path ${env:USERPROFILE}/scoop/apps/firefox/current/firefox.exe
    }
    # Modern MSStore/Winget install
    else {
        $package = Get-AppxPackage Mozilla.MozillaFirefox
        [xml]$AppManifest = Get-Content ([System.IO.Path]::Combine($package.InstallLocation,"AppxManifest.xml"))
        $firefox = Join-Path $package.InstallLocation $AppManifest.Package.Applications.Application.Executable
    }
    $options = @(
        "-P", $InitProfile
        "-new-tab", $Url
    )
    Write-Verbose "InitProfile: ${InitProfile}, Url: ${Url}"
    Write-Verbose "Firefox: ${firefox}"
    & "$firefox" @options
}
