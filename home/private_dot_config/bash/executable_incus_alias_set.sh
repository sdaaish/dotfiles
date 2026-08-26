#!/usr/bin/env bash

# Create aliases for Incus
# Usage: $0 [all]

# Add the alias after removing duplicate
_add_alias() {
    incus alias remove --quiet "$1" 2>/dev/null
    incus alias add --quiet "$1" "$2" 2>/dev/null
}

# Used to remove all existing
_remove_alias() {
incus alias remove --quiet "$1" 2>/dev/null
}

# Remove all existing aliases if the argument "all" is on the commandline
if [[ "${1@U}" == "ALL" ]]
then
    current=$(incus alias list -f compact,noheader|awk '{print $1}')
    for i in $current
    do
        _remove_alias "$i"
    done
fi

# Add incus aliases
if [ "$(command -v incus)" ]
then
    _add_alias ps "list -c ns4beumDS @ARGS@ status=running"
    _add_alias fl "list -c ns4tPc @ARGS@ status=running"
    _add_alias ll "list -c ns4tPc @ARGS@"
    _add_alias rl "remote list"
    _add_alias rsl "remote switch local"
    _add_alias rsr "remote switch @ARGS@"
    _add_alias c "remote get-default"
    _add_alias a "alias list"
    incus alias list
fi
