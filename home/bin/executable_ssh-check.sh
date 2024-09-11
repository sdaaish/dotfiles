#!/usr/bin/env bash
# -*- mode: bash; -*-

# Example to generate SHA256+Base64 fingerprints from public key
## From https://www.lastbreach.com/en/blog/ssh-public-key-verification-with-fingerprinthash
#

for file in /etc/ssh/ssh_*.pub;do
    check=$(awk '{print $2}' ${file} |base64 -d|sha256sum -b|sed 's/ .*$//'|xxd -r -p|base64)
    printf "${file} has SHA256 checksum ${check}\n"
    printf "And has visual fingerprint:\n"
    ssh-keygen -lvf ${file}
done

