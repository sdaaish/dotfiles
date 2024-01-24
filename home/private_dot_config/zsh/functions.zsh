# -*- mode: shell-script; -*-

# Add things that differs from Bash file syntax to ZSH.

src() {
    source "$HOME/.config/zsh/.zshrc"
}

# Function to install plugins for Zsh
# WIP
add-plugin() {

    if [[ $# -ne 1 ]]
    then
        echo "Usage: Enter the plugin directory."
        return 1
    else

        PLUGINDIR="$XDG_CACHE_HOME/zsh/plugins"
        PLUGIN="$PLUGINDIR/$1"

        if [[ ! -d $PLUGINDIR ]]
        then
            mkdir -p $PLUGINDIR
        fi

        if [[ -d $PLUGIN ]]
        then
            for i in $(ls -1 $PLUGIN/*.plugin.zsh)
            do
                source $i
            done
        else
            git clone "https://github.com/$1.git" $PLUGIN
        fi
    fi
}
