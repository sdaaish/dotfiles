# Sets environment variables

# Settings for Emacs as editor.
[System.Environment]::SetEnvironmentVariable("ALTERNATE_EDITOR", "runemacs.exe","User")
[System.Environment]::SetEnvironmentVariable("EDITOR", "runemacs.exe","User")

if (Test-Path "C:\Windows\System32\OpenSSH\ssh.exe"){
    [System.Environment]::SetEnvironmentVariable("GIT_SSH", $(Resolve-Path "C:\Windows\System32\OpenSSH\ssh.exe"),"User")
}

[System.Environment]::SetEnvironmentVariable("HOME", $(Resolve-Path ${env:UserProfile}),"User")
