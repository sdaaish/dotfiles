#!/usr/bin/env bash

#
# Alias file

# Remove old aliases
unalias emx &>/dev/null

# List alias
alias dmesg='dmesg -T --color=always| less -R'
alias l='less'
alias la='ls -la'
alias ll='ls -lAhG'
alias llt='ls -ltrah'
alias llh='ls -lah'
alias lls='ls -lAhSrG'
alias lll='find-links'

alias rehash='hash -r'

# Stupid stuff
alias sl='sl -ea'

# Change dir
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../..'
alias cdh='cd /mnt/c/Users/Stig'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    #test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Git stuff
alias gta='git alias'
alias gtd='git diff'
alias gtr='git log --graph'
alias gts='git status -sb'
alias gtp='git push && git pull'
alias glog="\git log --color --all --date-order --decorate --dirstat=lines,cumulative --stat | sed 's/\([0-9] file[s]\? .*)$\)/\1\n_______\n-------/g' | \less -R"

# emacs support
alias em='emacsclient -t -a ""'
alias ems='sudo emacsclient -t -a ""'
alias emdi='emacs --debug-init'
alias emd='emacs --daemon &>~/tmp/emacs-daemon.log &'
alias emk='emacsclient --eval "(save-buffers-kill-emacs)" --no-wait'
alias emfg='emacs --fg-daemon --debug-init'
alias emq='emacs -Q \
--border-color "White smoke" \
--foreground-color "dark red" \
--cursor-color "gray" \
--no-blinking-cursor'
alias doom='~/.config/emacs/bin/doom'

# binary support
alias bbk='bbk_cli'

# system stuff
alias myver='lsb_release -a'

# Other stuff
alias kb='keybase'

# Certificate stuff
alias ov="openssl verify"
alias o5="openssl x509 -text -noout"

# Fonts
alias install-fonts="sudo apt-get install fonts-powerline"

# Tools
alias psa="ps ax -O ppid"
alias e="env|sort"

# Docker
alias dk=docker
alias dco=docker-compose

# Python
alias python=python3
alias pip=pip3

# Code
alias ci='code-insiders'

# rclone GUI
alias rcgui='rclone rcd --rc-web-gui'

# systemd
alias sysu="systemctl --user"
