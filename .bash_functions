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
    	 apt-get dist-upgrade --dry-run| grep ^Inst|cut -d\" \" -f2| grep ."
}
# Reove unwanted stuff
apr() {
    sudo bash -c  "apt-get --yes autoremove"
}

cdr() {
    cd ~/repos
    ls -1
}

cdrw() {
    cd ~/work
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
check-speed() {
    # Checks speed for IPv4 and IPv6
    printf "Latency Download Upload Server\n"
    bbk --quiet
    bbk --quiet --v6
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
	return 1
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

# Commit all org-files
oc() {
    if [[ -d ~/Dropbox/emacs/org ]]
    then
        DATE=$(date '+%Y%m%d-%H:%M:%S')
        pushd ~/Dropbox/emacs/org
        git add *.org *.org_archive
        git commit -m "Comitting changes $DATE"
        git push
    fi
}

src() {
      . ~/.bashrc
      }
srca() {
    printf "Udates ~/emacs.d and ~/dotfiles\n"
    printf "Pulling dotfiles: "; git -C ~/repos/dotfiles/ pull
    printf "Pulling emacs.d: "; git -C ~/repos/emacs.d/ pull
    (cd ~/repos/dotfiles/ && make)
    . ~/.bashrc
}
# Starts ssh-agent
ssa() {
      eval $(ssh-agent -s)
}
# Add all local keys
ssk() {
    for key in $(ls ~/.ssh/*.pub)
    do
        ssh-add ${key%.pub}
    done
}

# Grep - green
sc-services() {
  export GREP_COLOR='1;32'
  systemctl list-units --type=service | grep --color -E "active running|$"
}

# Update repos
update-repos(){

    if [ $# -eq 1 ]
    then
        dir=$1
    else
        dir=.
    fi

    # git repositories
    for D in $(find $dir -type d -name '\.git'); do
	      git -C $(dirname $D) config --get remote.origin.url
	      git -C $(dirname $D) pull
	      echo
    done
}

# Check git remote
function check-remotes() {
    if [ $# -eq 1 ]
    then
        dir=$1
    else
        printf "You must specify an input directory.\n"
        return 1
    fi

    if [[ -d $dir ]]
    then
        for D in $(find $dir -type d -name '\.git')
        do
            printf "Checking directory: $(dirname $D)\n"
            git -C $(dirname $D) remote -v
        done
    else
        printf "Not a directory: $dir\n"
        return 2
    fi
}

#Install emacs.d, new version
install-emacs-d(){
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
	      cp -a ~/.emacs.d ~/.emacs.d.old 2>/dev/null
	      rm -rf ~/.emacs.d  2>/dev/null
    fi

    #Clone emacs.d repo to local repo-dir
    #Method to clone
    if [[ $(grep git@github.com ~/.ssh/config 2>/dev/null) ]]
    then
	      clone="git clone git@github.com:sdaaish/emacs.d.git"
    else
	      clone="git clone https://github.com/sdaaish/emacs.d.git"
    fi

    if [[ -d ~/repos ]]
    then
	      cd ~/repos
    else
        mkdir ~/repos
	      cd ~/repos
    fi
    ${clone}
    ln -s ~/repos/emacs.d ~/.emacs.d
}

# Install latest emacs version
install-emacs-dev() {
    sudo apt-add-repository ppa:ubuntu-elisp/ppa
    sudo apt-get update
    sudo apt-get install emacs-snapshot
}

# Install latest stable git-version
install-git-latest() {
    sudo add-apt-repository --yes ppa:git-core/ppa
    sudo apt-get update
    sudo apt-get --yes install git
}

# Install basic stuff that are useful on virtual linux-machines
install-lxss-basic(){
    lista="git make binutils build-essential python3 python-pip \
               emacs25-nox html2text dos2unix gnupg gnutls-bin \
               sshguard locate tree etckeeper most zsh apt-file"

    sudo apt-get -y -q update
    for prg in ${lista}
    do
        printf "Installing ${prg}\n"
        sudo apt-get -y -q install ${prg}
    done
    sudo apt-get -y autoremove
}

# Install domain-tools
install-domain-tool(){
    lista="dnsutils bind9-host bind9utils whois"
    sudo apt-get -y -q update
    for prg in ${lista}
    do
        printf "Installing ${prg}\n"
        sudo apt-get -y -q install ${prg}
    done
    sudo apt-get -y autoremove
}

# Install useful net-tools
install-net-stuff(){
    lista="sshguard screen tmux ssmtp mpack ufw nmap \
        iptraf ngrep htop nload curl wget telnet mtr-tiny"

    sudo apt-get -y -q update
    for prg in ${lista}
    do
        printf "Installing ${prg}\n"
        sudo apt-get -y -q install ${prg}
    done
    sudo apt-get -y autoremove
}
# Install unnecessary stuff
install-fun-stuff(){
    lista="cowsay lolcat fortune fortunes-ubuntu-server fortunes-bofh-excuses sl"
    for prg in $lista
    do
        printf "Installing ${prg}\n"
        sudo apt-get install ${prg}
    done
    sudo apt-get -y autoremove
}

# Install Powershell
install-powershell() {
    REL=$(lsb_release -r| awk '{print $2}')

    # Import the public repository GPG keys
    curl https://packages.microsoft.com/keys/microsoft.asc | sudo apt-key add -

    # Add the correct repository from Microsoft based on Linux version
    case ${REL} in
        "14.04")
            printf "Installing for 14.04\n"
            curl https://packages.microsoft.com/config/ubuntu/14.04/prod.list | \
                sudo tee /etc/apt/sources.list.d/microsoft.list;;
        "16.04"|"18")
            printf "Installing for 16.04\n"
            curl https://packages.microsoft.com/config/ubuntu/16.04/prod.list | \
                sudo tee /etc/apt/sources.list.d/microsoft.list;;
        "17.04")
            printf "Installing for 17.04\n"
            curl https://packages.microsoft.com/config/ubuntu/17.04/prod.list | \
                sudo tee /etc/apt/sources.list.d/microsoft.list;;
        *)      printf "No version to get, version=${REL}\n!"
                return 1;;
    esac

    # Update apt-get
    sudo apt-get update

    # Install PowerShell
    sudo apt-get install -y powershell
}

# Install jekyll on Windows bash. Use updated version.
# From https://jekyllrb.com/docs/windows/
install-jekyll () {
        sudo apt-get update -y
        sudo apt-get upgrade -y
        sudo apt-get install -y gcc make
        sudo apt-add-repository -y ppa:brightbox/ruby-ng
        sudo apt-get update -y
        sudo apt-get install -y ruby2.3 ruby2.3-dev build-essential
        sudo gem update
        sudo gem install jekyll bundler
        jekyll -v
    }

# Install keybase
function install-keybase {
    sudo apt-get update -y
    sudo apt-get upgrade -y
    curl -O https://prerelease.keybase.io/keybase_amd64.deb
    sudo dpkg -i keybase_amd64.deb
    sudo apt-get install -f
    run_keybase    
}

# Install keybase-cli
function install-keybase-cli {
    go get github.com/keybase/client/go/keybase
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

# Start emacs client frame and emacs server if not already started.
# Without options a new frame is created, with options an existing frame are used.
emx() {
    if [[ $# -eq 0 ]]
    then
        emacsclient --alternate-editor "" --create-frame &>~/tmp/emacs-client.log &
    else
        emacsclient --alternate-editor "" "$*" &>~/tmp/emacs-clients.log &
    fi
}

# Fix SSH daemon on WSL
fix-wsl-ssh() {
    sshd_config=/etc/ssh/sshd_config
    sed 's/^Port 22$/Port 22222/' $sshd_config
    sed 's/^PermitRootLogin prohibit-password$/PermitRootLogin no/' $sshd_config
    sed 's/^PasswordAuthentication yes$/PasswordAuthentication no/' $sshd_config
}

# This sets up a branch for this host and pushes the etckeeper repo to remote server.
# See https://serverfault.com/questions/28973/is-it-possible-to-use-etckeeper-with-a-single-shared-git-repository
fix-etckeeper-repo() {
    # The remote repo
    myorigin=ssh://gitlocal/~/code/etckeeper

    # Rename the master branch to the hostname of the system
    sudo git -C /etc branch -m master $HOSTNAME
    # Add the remote repo
    sudo git -C /etc remote add origin ${myorigin}
    # make a commit to be sure
    sudo git -C /etc commit -m "Initial commit to specific branch"
    # And push the master branch to the remote
    sudo git -C /etc push -u origin master:$HOSTNAME
}
