#!/usr/bin/env bash

#
# Alias file

# List alias
alias dmesg='dmesg -T --color=always| less -R'
alias l='less'
alias la='ls -la'
alias ll='ls -lAhG'
alias llt='ls -ltrah'
alias llh='ls -lah'
alias lls='ls -lAhSrG'

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
alias ci='git commit'
alias co='git checkout'
alias gtd='git diff'
alias gtr='git log --graph'
alias gts='git status -sb'
alias gtp='git push && git pull'

# emacs support
alias em='emacsclient -c -a ""'
alias ems='sudo emacsclient -t -a ""'
alias emdi='emacs --debug-init'
alias emx='emacsclient -t -a ""'
alias emd='emacs --daemon'
alias emk='emacsclient --eval "(save-buffers-kill-emacs)" --no-wait'

# binary support
alias bbk='bbk_cli'

# system stuff
alias myver='lsb_release -a'

# Other stuff
alias kb='keybase'

# Certificate stuff
alias ov="openssl verify"
alias o5="openssl x509 -text -noout"
