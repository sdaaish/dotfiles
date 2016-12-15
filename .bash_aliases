#
# Alias file

# List alias
alias l='less'
alias ll='ls -lAhG'
alias llt='ls -ltrah'
alias llh='ls -lah'

# Change dir
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../..'
alias cdh='cd /mnt/c/Users/Stig'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    #test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

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
alias ci='git commit'
alias co='git checkout'
