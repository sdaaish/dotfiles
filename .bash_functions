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

apd() {
    sudo bash -c  "apt-get update ; apt-get upgrade -y"
}

cdr() {
      cd ~/repos
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
}

# Base16 script to change lxss colors
# from https://github.com/chriskempson/base16-shell
get-base16() {
	 git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
	 }
	 
src() {
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
