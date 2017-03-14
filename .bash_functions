#!/usr/bin/env bash

#
## A place for all the functions
#
apc() {
      if [ ${#*} != 0 ]
      then
            apt-cache search ${*}
      else
	printf "Searches for available packages.\n"
	printf "Usage: apc <1 or more args>\n"
      fi
}
# Upgrade system
apd() {
    sudo bash -c  "apt-get update ; apt-get upgrade -y"
}
#Do dist-upgrade
apdd() {
    sudo bash -c  "apt-get update ; apt-get dist-upgrade -y"
}
# Check for outdated packages
apo() {
    sudo bash -c "sudo apt-get update; \
    	 apt-get dist-upgrade --dry-run| grep ^Inst|cut -d\" \" -f2"
}

cdr() {
    cd ~/repos
    ls -1
}

cx() {
     chmod a+x ${*}
     }

cvsloc() {
    export CVSROOT=:fork:$HOME/cvsroot/CVSROOT
}

check-etckeeper() {
    sudo bash -c "etckeeper unclean || etckeeper commit"
}

#
## From http://www.cyberciti.biz/faq/linux-unix-colored-man-pages-with-less-command/
man() {
      env \
          LESS_TERMCAP_mb=$(printf "\e[1;35m") \
          LESS_TERMCAP_md=$(printf "\e[1;33m") \
          LESS_TERMCAP_me=$(printf "\e[0m") \
          LESS_TERMCAP_se=$(printf "\e[0m") \
          LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
          LESS_TERMCAP_ue=$(printf "\e[0m") \
          LESS_TERMCAP_us=$(printf "\e[1;31m") \
          man "$@"
}

# Get BBK
get-bbk() {
	  #Set download URL's
	  AMD=http://beta1.bredbandskollen.se/download/bbk-cli_0.3.8_amd64.deb
	  ARM=http://beta1.bredbandskollen.se/download/bbk-cli_0.3.8_armhf.deb
	  TMP=$(mktemp bbk.deb.XXXXX)
	  
	  #Check arch
	  if [[ $(uname -a|grep x86_64) ]]
	  then
		wget -O ${TMP} ${AMD}
	  elif [[ $(uname -a|grep armv7l) ]]
	  then
		wget -O ${TMP} ${ARM}
	  else
	  	printf "No supported arch\n"
		exit 1
	  fi

	  #Install the file
	  if [[ -f ${TMP} ]]
	  then
	      sudo dpkg -i ${TMP}
	  fi
	  rm -f ${TMP}

	  # remove old file if it exists
	  if [[ -f ~/bin/bbk ]]
	  then
	  	  rm -f ~/bin/bbk
	  fi
}

# Base16 script to change lxss colors
# from https://github.com/chriskempson/base16-shell
get-base16() {
	 git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
	 }
	 
src() {
      . ~/.bashrc
      }
srca() {
    cd ~/repos/dotfiles
    git pull
    make
    . ~/.bashrc
}

ssa() {
      eval $(ssh-agent -s)
}

# Grep - green
sc-services() {
  export GREP_COLOR='1;32'
  systemctl list-units --type=service | grep --color -E "active running|$"
}

# Update repos
update-repos(){
    SRCDIR="${HOME}/repos"

    # git repositories
    for D in $(find $SRCDIR -type d -name '\.git'); do
	git -C $(dirname $D) config --get remote.origin.url
	git -C $(dirname $D) pull
	echo
    done
}

#Install emacs.d, new version
install-emacsd(){
    #If there is a link to old emacs-repo-file, remove it. If there is a file, move it to old.
    if [[ -L ~/.emacs ]]
    then
	rm -f ~/.emacs 2>/dev/null
    fi
    
    if [[ -f ~/.emacs ]]
    then
	mv -f ~/.emacs ~/.emacs.old 2>/dev/null
    fi
    
    #If there is an old emacs.d, copy stuff and delete it
    if [[ -d ~/.emacs.d ]]
    then
	mkdir ~/.emacs.d 2>/dev/null
	cp -a ~/.emacs.d ~/.emacs.d.old 2>/dev/null
	rm -rf ~/.emacs.d  2>/dev/null
    fi
    #Clone emacs.d repo to local repo-dir
    #Method to clone
    if [[ $(grep git@github.com ~/.ssh/config) ]]
    then
	clone="git clone git@github.com:sdaaish/emacs.d.git emacs.d"
    else
	clone="git clone https://github.com/sdaaish/emacs.d.git"
    fi
    
    if [[ -d ~/repos ]]
    then
	cd ~/repos
    else
	cd ~/repos
    fi
    ${clone}
    ln -s ~/repos/emacs.d ~/.emacs.d
}
# Install basic stuff that are useful on virtual linux-machines
lxss-install-basic(){
    lista="git make bin-utils build-essential python3 python-pip \
               emacs24-nox bind9-utils whois html2text dos2unix gnupg gnutls-bin \
               sshguard cowsay lolcat locate"

    sudo apt-get -y -q update
    for prg in ${lista}
    do
        printf "Installing ${prg}\n"
        sudo apt-get -y -q install ${prg}
    done
    sudo apt-get -y autoremove
}
