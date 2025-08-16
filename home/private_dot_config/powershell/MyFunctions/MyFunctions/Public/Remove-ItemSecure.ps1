<#
.SYNOPSIS
Removes a file by overwriting it with random data.

.DESCRIPTION
From an Anti-Forensics talk, https://www.youtube.com/watch?v=J_4shMbM8DM
Removes a file by overwriting it with random data, and then deleting it.

#>

function Remove-ItemSecure {

    [cmdletbinding()]
    param(
        [parameter(Mandatory)]
        [string]$Path,
        [int]$Passes = 3
    )
    process {

        try {
            $File = Resolve-Path $Path
            $Directory = Split-Path $File -Parent
            $msg = "File: {0}, Directory: {1}" -f $File,$Directory
            Write-Verbose $msg
        }
        catch {
            Write-Error "No such file: ${Path}"
        }

        try {
            $length = (Get-Item $File).Length

            for ($i = 1;$i -le $Passes; $i++) {
                $rand = New-Object byte[] $length
                (New-Object System.Random).NextBytes($rand)
                [IO.File]::WriteAllBytes($File,$rand)
            }

            $randomName = [System.IO.Path]::GetrandomFileName()
            $newPath = Join-Path $Directory $randomName
            Rename-Item -Path $File -NewName $randomName -Force

            Remove-Item -Path $newPath -Force
            $msg = "Securely deleted: {0}" -f $File
            Write-Verbose $msg
        }
        catch {
            Write-Error "Failed to delete ${Path}"
        }
    }
}
