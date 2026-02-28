#!/usr/bin/env bash

# A tmux window switcher using FZF
tmux list-panes -aF "#{p20:session_name} #{p20:window_name} - #{p-3:window_id} #{p-3:pane_id}"|\
    #grep -vF "$(tmux display-message -p '#{windows_id}')" |\
    fzf --preview="tmux list-panes -t {-2} -F '#{p20:window_name} #{=/20/...:pane_title} - windows:#{p-10:session_windows} - panes:#{p-2:window_panes}'" \
        --preview-window="up,10%,20%" \
        --reverse \
        --bind "enter:become(tmux switch-client -t {-1} || true)" \
        --bind "q:abort" \
        --header "ESC, q : Exit / ENTER: Switch window"

exit 0
