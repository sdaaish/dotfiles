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

# Get Beta1 of BBK
get-bbk() {
	  #Set download URL's
	  AMD='http://beta1.bredbandskollen.se/download/bbk_cli_linux_amd64'
	  ARM='http://beta1.bredbandskollen.se/download/bbk_cli_linux_arm'

	  if [[ $(uname -a|grep x86_64) ]]
	  then
		wget -O ~/bin/bbk ${AMD}
	  elif [[ $(uname -a|grep armv7l) ]]
	  then
		wget -O ~/bin/bbk ${ARM}
	  else
	  	printf "No supported arch\n"
	  fi

	chmod a+x ~/bin/bbk
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
