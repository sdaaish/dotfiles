# Function to install Nerd Fonts

# Inspired from https://github.com/ChrisTitusTech/powershell-profile/

function Install-Fonts {
    param (
        [string]$FontName,
        [string]$Url
    )

    try {
        [void] [System.Reflection.Assembly]::LoadWithPartialName("System.Drawing")
        $fontFamilies = (New-Object System.Drawing.Text.InstalledFontCollection).Families.Name
        if ($fontFamilies -notcontains "${FontName}") {

            $TmpFile = ([system.IO.Path]::GetTempFileName()) -replace "\.*",""
            $zipFilePath = "${TmpFile}.zip"
            $extractPath = "${TmpFile}"

            $webClient = [System.Net.WebClient]::new()
            $webClient.DownloadFileAsync(([System.Uri]::new($Url)), $zipFilePath)

            while ($webClient.IsBusy) {
                Start-Sleep -Seconds 2
            }

            Expand-Archive -Path $zipFilePath -DestinationPath $extractPath -Force
            $destination = (New-Object -ComObject Shell.Application).Namespace(0x14)

            Get-ChildItem -Path $extractPath -Recurse -Filter "*.?tf" | ForEach-Object {
                If (-not(Test-Path "C:\Windows\Fonts\$($_.Name)")) {
                    $destination.CopyHere($_.FullName, 0x10)
                }
            }

        }
        else {
            Write-Host "Font ${FontName} already installed"
        }
    }
    catch {
        Write-Error "Failed to download or install ${FontName} font. Error: $_"
    }
    finally {
        if (Test-Path $extractPath){
            Remove-Item -Path $extractPath -Recurse -Force
        }
        if (Test-Path $zipFilePath){
            Remove-Item -Path $zipFilePath -Force
        }
    }
}
