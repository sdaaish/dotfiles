# Makes a backup of the old Windows Terminal settings file.
Function Save-WTSettingsFile {
    param(
        $Path
    )

    $DestinationFolder = Split-Path $Path

    $date = Get-Date -Format "yyyyMMdd-HHmmss"
    $Destination = Join-Path $DestinationFolder "settings.$date.json"

    Copy-Item -Path $Path -Destination $Destination

    # Remove old backups
    Get-ChildItem -Path $DestinationFolder -File|
      Where-Object Name -Match 'settings\.(\d){8}-(\d){6}\.json'|
      Where-Object LastWriteTime -lt $((Get-Date).AddDays(-3))|
      Remove-Item -Force
}

# Change the settings
Function Set-TerminalSettings {

    param(
        $Path,
        $NewSetting
    )
    $ErrorActionPreference = "Stop"
    # Read the original
    $settings = Get-Content -Path $Path |ConvertFrom-Json

    foreach ($property in $NewSetting.GetEnumerator()){
        $key =  $property.name
        $value =  $property.value
        try {$settings.$key = $value}
        catch {$settings|add-Member -MemberType NoteProperty -Name $key -Value $Value}
    }

    $settings|ConvertTo-Json -Depth 5| Out-File $Path

}

# Take a backup
$WTSettingsFile = $(Join-Path $HOME "AppData/Local/Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/settings.json")

Save-WTSettingsFile -Path $WTSettingsFile

# Configure Actions, ie key bindings.
$MyDefaults = @{
    "actions" = @(
        @{ command = @{ action = "closeTab" }},
        @{ command = @{ action = "newTab" }
           keys = "ctrl+t" },
        @{ command = @{ action = "prevTab" }
           keys = "ctrl+pgup" },
        @{ command = "closePane"
           keys = "ctrl+shift+w" },
        @{ command = "unbound"
           keys = "alt+shift+minus" },
        @{ command = "unbound"
           keys = "alt+minus" },
        @{ command = "unbound"
           keys = "ctrl+shift+plus" },
        @{ command = @{ action = "nextTab" }
           keys = "ctrl+pgdn" },
        @{ command = @{ action = "commandPalette" }
           keys = "alt+x" }
    )
}
Set-TerminalSettings -Path $WTSettingsFile -NewSetting $MyDefaults

# Configure generic  settings
$MyDefaults = @{
    alwaysShowTabs =  $true
    copyFormatting =  "none"
    copyOnSelect =  $true
    launchMode = "maximized"
    defaultProfile =  "{574e775e-4f2a-5b96-ac1e-a2962a402336}"
    showTabsInTitlebar = $true
    tabSwitcherMode = "disabled"
}
Set-TerminalSettings -Path $WTSettingsFile -NewSetting $MyDefaults

# Configure defaults and font etc.
$MyDefaults = @{
    profiles = @{
        defaults = @{
            bellStyle = "none"
            closeOnExit = "never"
            colorScheme = "Campbell"
            cursorShape = "underscore"
            font = @{
                face =  "CaskaydiaCove Nerd Font"
                size =  11
            }
            useAcrylic = $false
        }
    }
}
Set-TerminalSettings -Path $WTSettingsFile -NewSetting $MyDefaults.profiles

# No blinking cursor. Terminal needs restart
Set-ItemProperty 'HKCU:\Control Panel\Desktop\' -Name CursorBlinkRate -Value -1
