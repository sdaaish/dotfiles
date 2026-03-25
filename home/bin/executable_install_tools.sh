#!/usr/bin/env bash

# Install tools for the current distro
grep "suse" /etc/os-release > /dev/null 2>&1 && _OS="OPENSUSE" || _OS=

if [[ $EUID != 0 ]]
then
    echo "Usage: Run this command, $0, with sudo"
    exit 1
fi

if [[ "${_OS@U}" == "OPENSUSE" ]]
then
    zypper refresh
    zypper install fzf zoxide ripgrep direnv eza fd age minisign bat jq yq uv pyenv
fi
