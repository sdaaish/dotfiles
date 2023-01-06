#!/bin/bash

# Script that runs through the entire addressspace for RFC1918 172.16/12 network
# Requires iplookup
# 20161003/SDAA

	for ((j=16;j<32;j++))
	do
	    for ((i=0;i<256;i++))
	    do
		if [ $(which iplookup) ]
		then
		    iplookup 172.$j.$i.1
		fi
	    done
        done
	

