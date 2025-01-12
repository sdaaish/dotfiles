{{ if eq .chezmoi.os "windows" }}

# Load function and update PATHS
{{- template "refreshpath.ps1" -}}

# Test for prvililege
{{- template "test-for-admin.ps1" -}}

$MicrosoftFonts = @(
    @{
        Name = "CascadiaCode"
        Url = "https://github.com/microsoft/cascadia-code/releases/download/v2407.24/CascadiaCode-2407.24.zip"
    }
)

$version = "v3.3.0"
$NerdFonts = @(
    @{
        Name = "ComicShannsMono Nerd Font"
        Url = "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/ComicShannsMono.zip"
    },
    @{
        Name = "AnonyMicePro Nerd Font"
        Url = "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/AnonymousPro.zip"
    },
    @{
        Name = "CodeNewRoman"
        Url = "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/CodeNewRoman.zip"
    },
    @{
        Name = "DejaVuSans Nerd Font"
        Url = "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/DejaVuSansMono.zip"
    },
    @{
        Name = "FiraCode Nerd Font"
        Url = "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/FiraCode.zip"
    },
    @{
        Name = "FiraMono Nerd Font"
        Url = "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/FiraMono.zip"
    },
    @{
        Name = "JetBrainsMono Nerd Font"
        Url = "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/JetBrainsMono.zip"
    },
    @{
        Name = "Noto Nerd Font"
        Url = "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/Noto.zip"
    },
    @{
        Name = "RobotoMono Nerd Font"
        Url = "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/RobotoMono.zip"
    },
    @{
        Name = "SauceCodePro Nerd Font"
        Url = "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/SourceCodePro.zip"
    },
    @{
        Name = "Symbols Nerd Font"
        Url = "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/NerdFontsSymbolsOnly.zip"
    },
    @{
        Name = "Terminess Nerd Font"
        Url = "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/Terminus.zip"
    },
    @{
        Name = "Ubuntu Nerd Font"
        Url = "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/Ubuntu.zip"
    }
)


{{- template "install-fonts.ps1" -}}

$MicrosoftFonts.Foreach({
    Install-Fonts -FontName $_.Name -Url $_.Url
})

$NerdFonts.Foreach({
    Install-Fonts -FontName $_.Name -Url $_.Url
})

{{ end }}
