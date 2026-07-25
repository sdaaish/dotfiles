#!/usr/bin/env bash

# Create aliases for Incus

_add_alias() {
    incus alias remove --quiet "$1" 2>/dev/null
    incus alias add --quiet "$1" "$2" 2>/dev/null
}

# Set incus aliases
if [ $(command -v incus) ]
then

    current=$(incus alias list -f compact,noheader|awk '{print $1}')
    _add_alias ps "list status=running -c ns4beumDS"
    _add_alias rl "remote list"
    _add_alias rs "remote switch local"
    _add_alias c "remote get-default"
    _add_alias a "alias list"
    _add_alias fl "list --fast"
    incus alias list
fi
