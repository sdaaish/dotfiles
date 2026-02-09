#!/usr/bin/env bash

# Info pages through FZF
_result=$(info -k "${1:-}" \
              | fzf --header="Press Ctrl-t to toggle sort." --header-lines=0 \
                    --bind="ctrl-t:toggle-sort" \
              | awk -F' --' '{print $1}' \
              | xargs echo -n)
info "${_result}"
