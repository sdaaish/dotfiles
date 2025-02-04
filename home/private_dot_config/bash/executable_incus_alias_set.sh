#!/usr/bin/env bash

set -euo pipefail

# Set incus aliases
if [ $(command -v incus) ]
then

    current=$(incus alias list -f compact,noheader|awk '{print $1}')
    incus alias add ps "list status=running -c ns4beumDS" 2>/dev/null
    incus alias list
fi
