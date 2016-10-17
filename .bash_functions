#
## A place for all the functions
#
apc() {
      apt-get search ${*}
}

apd() {
    sudo bash -c  "apt-get update ; apt-get upgrade"
}
      
cx() {
     chmod a+x ${*}
     }

cvsloc() {
         export CVSROOT=:fork:$HOME/cvsroot/CVSROOT
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
