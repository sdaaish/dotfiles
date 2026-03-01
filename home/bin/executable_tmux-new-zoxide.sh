#!/usr/bin/env bash

# Create a new tmux session with zoxide
zoxide query --list |\
    fzf --preview="ls -1 --color=always {}" \
        --preview-window="right,30%,40%" \
        --reverse \
        --bind 'enter:become(tmux new-session -d -c {1} -n $(basename {1}) -s $(basename {1})\; switch-client -t $(basename {1})  || true)' \
        --bind "q:abort" \
        --header "ESC, q : Exit / ENTER: New session"

exit 0
