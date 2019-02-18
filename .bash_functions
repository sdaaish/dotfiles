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
    printf "Checking for upgrades...\n------------------------\n"
    sudo bash -c "sudo apt-get update >/dev/null; \
    	 apt-get dist-upgrade --dry-run| grep ^Inst|cut -d\" \" -f2| grep .|sort"
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
    ~/bin/bbk_cli --quiet
    ~/bin/bbk_cli --quiet --v6
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

# Get BBK from bredbandskollen
get-bbk() {
    # Name for the file
    binary=~/bin/bbk_cli

    # Remove old file if it exists
    if [[ -f $binary ]]
    then
	      rm -f $binary
    fi

    # Get architecture
	  ARCH=$(uname -m)
	  if [[ $ARCH == x86_64 ]]
	  then
        AMD=https://frontend.bredbandskollen.se/download/bbk_cli_linux_amd64-1.0
	      wget -qO ${binary} ${AMD}
        chmod a+x ${binary}
    elif [[ $ARCH == armv7l ]]
    then
        ARM=https://frontend.bredbandskollen.se/download/bbk_cli_linux_armhf-1.0
	      wget -qO ${binary} ${ARM}
        chmod a+x ${binary}
    else
	      printf "No supported arch\n"
	      return 1
	  fi
}

# Base16 script to change lxss colors
# from https://github.com/chriskempson/base16-shell
get-base16() {
	  git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
}

# Get powerline fonts
get-powerline-fonts(){
    if [[ -d ~/tmp ]]
    then
        pushd ~/tmp
        git clone https://github.com/powerline/fonts.git --depth=1 
        cd fonts
        ./install.sh
        cd ..
        rm -rf fonts
        popd
    fi
}

# find links
find-links(){
    find ${1:-.} -type l -ls|awk '{printf "%-50s\t%-50s\n",$13,$11}'
}

# Commit all org-files
oc() {
    if [[ -d ~/Dropbox/emacs/org ]]
    then
        DATE=$(date '+%Y%m%d-%H:%M:%S')
        pushd ~/Dropbox/emacs
        git add bookmarks
        cd org
        git add *.org *.org_archive archive/*.org*
        git commit -m "Comitting changes $DATE"
        git push
        popd
    fi
}

src() {
    . ~/.bashrc
}
srca() {
    printf "Udates ~/emacs.d and ~/dotfiles\n"
    printf "Pulling dotfiles: "; git -C ~/repos/dotfiles/ pull
    printf "Pulling emacs.d: "; git -C ~/repos/.emacs.d/ pull
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

    REPODIR=~/repos
    DST=${REPODIR}/.emacs.d

    #If there is a link to old emacs-repo-file, remove it. If there is a file, move it to old.
    if [[ -L ~/.emacs ]]
    then
	      rm -f ~/.emacs 2>/dev/null
    fi

    if [[ -f ~/.emacs ]]
    then
	      mv -f ~/.emacs ~/.emacs.old 2>/dev/null
    fi

    #If there is an old emacs.d, move it to a backup copy
    if [[ -d ~/.emacs.d ]]
    then
	      mv -f ~/.emacs.d ~/.emacs.d.old 2>/dev/null
    fi

    #Clone emacs.d repo to local repo-dir
    if [[ $(grep git@github.com ~/.ssh/config 2>/dev/null) ]]
    then
	      SRC="git@github.com:sdaaish/emacs.d.git"
    else
	      SRC="https://github.com/sdaaish/emacs.d.git"
    fi

    git clone ${SRC} ${DST}
    cd ${DST}
    make emacs
}

# Install latest emacs version
install-emacs-snapshot() {
    sudo apt-add-repository --yes ppa:ubuntu-elisp/ppa
    sudo apt-get --yes update
    sudo apt-get --yes install emacs-snapshot
}

# Install latest stable git-version
install-git-latest() {
    sudo add-apt-repository --yes ppa:git-core/ppa
    sudo apt-get update
    sudo apt-get --yes install git
}

# Install basic stuff that are useful on virtual linux-machines
install-lxss-basic(){
    lista="git make binutils build-essential \
               html2text dos2unix gnupg gnutls-bin \
               locate tree etckeeper most zsh apt-file"

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
    ID=$(lsb_release -i -s|tr [:upper:] [:lower:])
    REL=$(lsb_release -r -s)

    # Import the public repository GPG keys
    printf "Adding Microsoft PPA\n"
    curl https://packages.microsoft.com/keys/microsoft.asc | sudo apt-key add -

    printf "Downloading powershell for ${ID} ${REL}\n"
    curl https://packages.microsoft.com/config/${ID}/${REL}/prod.list | \
        sudo tee /etc/apt/sources.list.d/microsoft.list

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
function install-keybase-full {
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

# Install mail-tools for emacs
install-mailtools() {
    # Refresh
    sudo apt update -y
    sudo apt-get upgrade -y

    # Install mail-programs and dependencies to build notmuch
    sudo apt install -y ca-certificates msmtp msmtp-mta isync gnutls-bin \
         gcc clang make libgnutls28-dev \
         libxapian-dev libgmime-2.6-dev libtalloc-dev zlib1g-dev

    if [ -d ~/repos ]
    then
        mkdir -p ~/repos/github/notmuch
        git clone https://git.notmuchmail.org/git/notmuch ~/repos/github/notmuch
        cd ~/repos/github/notmuch
        make
        sudo make install
    fi
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

# Install docker
# From https://nickjanetakis.com/blog/setting-up-docker-for-windows-and-wsl-to-work-flawlessly
install-docker-for-wsl() {
    # Update the apt package list.
    sudo apt-get update --yes

    # Install Docker's package dependencies.
    sudo apt-get install --yes \
         apt-transport-https \
         ca-certificates \
         curl \
         software-properties-common

    # Download and add Docker's official public PGP key.
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

    # Verify the fingerprint.
    sudo apt-key fingerprint 0EBFCD88

    # Add the `stable` channel's Docker upstream repository.
    sudo add-apt-repository \
         "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"

    # Update the apt package list (for the new apt repo).
    sudo apt-get update --yes

    # Install the latest version of Docker CE.
    sudo apt-get install --yes docker-ce

    # Allow your user to access the Docker CLI without needing root access.
    sudo usermod -aG docker $USER

    # Install Python and PIP.
    sudo apt-get install -y python python-pip

    # Install Docker Compose into your user's home directory.
    pip install --user docker-compose
}
