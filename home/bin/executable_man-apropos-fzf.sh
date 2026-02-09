#!/usr/bin/env bash

# Man pages through FZF
unset FZF_DEFAULT_OPTS
if command -v "bat"
then
    PAGER="bat --color=always --decorations=always"
fi

man -k "${1:-}" \
    | fzf --header="Press Ctrl-t to toggle sort." \
          --header-lines=0 \
          --bind="ctrl-t:toggle-sort" \
          --preview="man {1}{2}" \
          --preview-window="down,50%" \
          --height="100%" \
          --layout=reverse \
    | awk '{print $1$2}' \
    | xargs man
