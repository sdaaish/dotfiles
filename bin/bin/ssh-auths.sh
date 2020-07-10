#!/bin/bash

# Checks failed logins via ssh
# 2016-05-14

LOG=/var/log/auth.log

#grep -E '[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0.9]{1,3}'
#grep -E '([0-9]{1,3}\.){3}[0.9]{1,3}'
#awk '{match($0,/[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+/); ip = substr($0,RSTART,RLENGTH); print ip;}' auth.log| awk 'NF'

if [ -f $LOG ];then
    awk '/sshd\[.* disconnect/{print $9}' $LOG| sed 's/:$//'| sort -u
else
    echo "No such file, $LOG"
fi
