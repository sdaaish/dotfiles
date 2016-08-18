#Alias file

alias l='less'
alias ll='ls -la'
alias llt='ls -ltra'
alias llh='ls -lah'

alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../..'

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
alias gts='git status -sb'
alias ci='git commit'
alias co='git checkout'

