if status is-interactive
    # Commands to run in interactive sessions can go here

    # Abbreviations
    abbr -a -U -- gco 'git checkout'
    abbr -a -U -- gts 'git status'
    abbr -a -U -- cm 'chezmoi'
    # abbr -a -U -- tldr 'tealdeer'

    # Use zoxide
    zoxide init fish | source

    # Use starship
    starship init fish | source

end
