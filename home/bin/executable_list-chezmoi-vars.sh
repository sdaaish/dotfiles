#!/usr/bin/env bash

# List built-in variables in chezmoi

VARS='
.chezmoi.arch
.chezmoi.args
.chezmoi.cacheDir
.chezmoi.config
.chezmoi.configFile
.chezmoi.destDir
.chezmoi.executable
.chezmoi.fqdnHostname
.chezmoi.gid
.chezmoi.group
.chezmoi.homeDir
.chezmoi.hostname
.chezmoi.kernel
.chezmoi.os
.chezmoi.osRelease
.chezmoi.pathListSeparator
.chezmoi.pathSeparator
.chezmoi.sourceDir
.chezmoi.sourceFile
.chezmoi.targetFile
.chezmoi.uid
.chezmoi.username
.chezmoi.version.builtBy
.chezmoi.version.commit
.chezmoi.version.date
.chezmoi.version.version
.chezmoi.windowsVersion
.chezmoi.workingTree'

for var in $VARS
do
    printf "###  $var = "
    printf  "{{ $var }}" | chezmoi execute-template
    printf "\n"
done


echo " List config values"
var=$(echo "{{ .chezmoi.config }}"|chezmoi execute-template)
for i in $var
do
    echo $i
done
