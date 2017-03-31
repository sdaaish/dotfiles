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
    URL="https://support.bredbandskollen.se/support/solutions/articles/1000245679-bredbandskollen-f%C3%B6r-linux"
    for file in $(curl -s $URL |
	html2text|
	sed '/http:.*\.deb/!d'|
	sed 's/.*http:/http:/')
    do
	ARCH=$(uname -m)
	if [[ $ARCH == x86_64 ]]
	then
	    echo $file|grep amd64
	else
	    echo $file|grep arm 
	fi

    done

    #Set download URL's
    AMD=http://beta1.bredbandskollen.se/download/bbk-cli_0.3.8_amd64.deb
    ARM=http://beta1.bredbandskollen.se/download/bbk-cli_0.3.8_armhf.deb
    TMP=$(mktemp bbk.deb.XXXXX)
    
    #Check arch
    if [[ $(uname -a|grep x86_64) ]]
    then
	wget -qO ${TMP} ${AMD}
    elif [[ $(uname -a|grep armv7l) ]]
    then
	wget -qO ${TMP} ${ARM}
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
    lista="git make binutils build-essential python3 python-pip \
               emacs24-nox bind9utils whois html2text dos2unix gnupg gnutls-bin \
               sshguard cowsay lolcat locate tree etckeeper most screen ssmtp mpack ufw"

    sudo apt-get -y -q update
    for prg in ${lista}
    do
        printf "Installing ${prg}\n"
        sudo apt-get -y -q install ${prg}
    done
    sudo apt-get -y autoremove
}
# Install useful net-tools
net-install-stuff(){
    lista="bind9utils whois sshguard screen tmux ssmtp mpack ufw nmap \
        iptraf ncat ngrep htop nload curl wget"

    sudo apt-get -y -q update
    for prg in ${lista}
    do
        printf "Installing ${prg}\n"
        sudo apt-get -y -q install ${prg}
    done
    sudo apt-get -y autoremove
}
# Transfer files with https://transfer.sh
transfer-vt(){
    # write to output to tmpfile because of progress bar
    tmpfile=$( mktemp -t transferXXX )
    curl --progress-bar --upload-file $1 https://transfer.sh/$(basename $1) >> $tmpfile
    cat $tmpfile
    rm -f $tmpfile
}
#
# Defines transfer alias and provides easy command line file and folder sharing.
# From https://gist.github.com/nl5887/a511f172d3fb3cd0e42d

transfer() { 
    # Check for curl
    curl --version 2>&1 > /dev/null
    if [ $? -ne 0 ]; then
        echo "Could not find curl."
        return 1
    fi

    # check arguments
    if [ $# -eq 0 ]; 
    then 
        printf "No arguments specified. Usage:\necho transfer /tmp/test.md\ncat /tmp/test.md | transfer test.md\n"
        return 1
    fi

    # get temporarily filename, output is written to this file show progress can be showed
    tmpfile=$( mktemp -t transferXXXXX )
    
    # upload stdin or file
    file=$1

    if tty -s; 
    then 
        basefile=$(basename "$file" | sed -e 's/[^a-zA-Z0-9._-]/-/g') 

        if [ ! -e $file ];
        then
            echo "File $file doesn't exists."
            return 1
        fi
        
        if [ -d $file ];
        then
            # zip directory and transfer
            zipfile=$( mktemp -t transferXXX.zip )
            cd $(dirname $file) && zip -r -q - $(basename $file) >> $zipfile
            curl --progress-bar --upload-file "$zipfile" "https://transfer.sh/$basefile.zip" >> $tmpfile
            rm -f $zipfile
        else
            # transfer file
            curl --progress-bar --upload-file "$file" "https://transfer.sh/$basefile" >> $tmpfile
        fi
    else 
        # transfer pipe
        curl --progress-bar --upload-file "-" "https://transfer.sh/$file" >> $tmpfile
    fi
   
    # cat output link
    cat $tmpfile
    printf "\n"
    
    # cleanup
    rm -f $tmpfile
}

