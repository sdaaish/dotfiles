#!/usr/bin/env bash

# A tmux session switcher using FZF
tmux list-sessions -F "#{p20:session_name} - #{p-3:session_id}" |\
    grep -vF "$(tmux display-message -p '#S')" |\
    fzf --preview="tmux list-windows -t {-1} -F '#{p20:window_name} - windows:#{p-2:session_windows} - panes:#{p-2:window_panes} #{p-20:window_id}'" \
        --preview-window="up,10%,10%" \
        --reverse \
        --bind "enter:become(tmux switch-client -t {-1} || true)" \
        --bind "q:abort" \
        --header "ESC, q : Exit / ENTER: Switch session"

exit 0
