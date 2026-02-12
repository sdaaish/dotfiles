#!/usr/bin/env bash

# This query searches for files containing the first pattern, and then for files matching the second pattern.
# I have excluded CSV files from being searched.

# From https://github.com/junegunn/fzf/blob/master/ADVANCED.md#controlling-ripgrep-search-and-fzf-search-simultaneously

export TEMP=$(mktemp -u)
trap 'rm -f "$TEMP"' EXIT

INITIAL_QUERY="${*:-}"
TRANSFORMER='
  rg_pat={q:1}      # The first word is passed to ripgrep
  fzf_pat={q:2..}   # The rest are passed to fzf

  if ! [[ -r "$TEMP" ]] || [[ $rg_pat != $(cat "$TEMP") ]]; then
    echo "$rg_pat" > "$TEMP"
    printf "reload:sleep 0.1; rg --type-not=csv --column --line-number --no-heading --color=always --smart-case %q || true" "$rg_pat"
  fi
  echo "+search:$fzf_pat"
'
fzf --ansi --disabled --query "$INITIAL_QUERY" \
    --with-shell 'bash -c' \
    --header 'CTRL-/: Toggle preview | CTRL-SPC: Toggle wrap' \
    --bind "start,change:transform:$TRANSFORMER" \
    --bind 'ctrl-/:change-preview-window(right|down|hidden)' \
    --bind 'ctrl-space:toggle-wrap' \
    --bind 'enter:become($EDITOR {1} +{2})' \
    --color "hl:-1:underline,hl+:-1:underline:reverse" \
    --delimiter : \
    --height '60%' \
    --preview 'bat --color=always {1} --highlight-line {2}' \
    --preview-window 'right,60%,border-line,+{2}+3/3,~3'
