#!/usr/bin/env bash

# Example taken from https://writingco.de/blog/how-i-manage-my-dotfiles-using-gnu-stow/

mkdir ~/{.cache/custom,bin,tmp,code,repos} 2>/dev/null

# Update git submodules
git submodule update --init

# Stow the following directories
base=(
    bash
    bin
    git
    gpg
    emacs
)

# Stow apps to target, restow deletes the old and updates them again
stowit() {
    target="$1"
    app="$2"
    stow -v -R -t "${target}" "${app}" 2>&1| grep -v BUG
}

echo ""
echo "Stowing apps for user: $(whoami)"

# Install the apps
for app in "${base[@]}"
do
    stowit "${HOME}" "$app"
done

echo ""
echo "##### ALL DONE"
