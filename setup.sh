#!/usr/bin/env bash


set -euf

SRC="/src"
DST="/dst"

export FORCE=1

export PATH=${DST}/bin:$PATH
sh -c "$(curl -fsLS get.chezmoi.io)" -- -b "${DST}/bin"

mkdir -p ${DST}/.config/chezmoi
cat << EOF > ${DST}/.config/chezmoi/chezmoi.toml
[data]
    email = "ubuntu@home.arpa"
    name = "Ubuntu"
    githubname = "${GITHUB_USERNAME:-ubuntu}"
    personal = true
    proxy = false

EOF

chezmoi -S ${SRC} -D ${DST} -c ${DST}/.config/chezmoi/chezmoi.toml init
chezmoi -S ${SRC} -D ${DST} -c ${DST}/.config/chezmoi/chezmoi.toml apply

cat ${DST}/.config/chezmoi/chezmoi.toml
