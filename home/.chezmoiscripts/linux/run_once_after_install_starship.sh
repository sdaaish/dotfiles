{{- if eq .chezmoi.os "linux" -}}

#!/usr/bin/env bash

set -eufo pipefail

[ $(command -v starship) ] && exit 0

if [[ $(command -v curl) ]]
then
    curl -sSLf https://starship.rs/install.sh | sh
elif [[ $(command -v wget) ]]
then
    wget https://starship.rs/install.sh -O - | sh
else
    echo "Curl or WGET not installed"
    exit 1
fi

{{ end }}
