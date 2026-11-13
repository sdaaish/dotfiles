#!/usr/bin/env bash

# Import alias for the Gitea CLI, gt.
gt alias set a 'alias list' --clobber
gt alias set co 'pr checkout' --clobber
gt alias set create-draft-pr 'pr create --draft --assignee @me' --clobber
gt alias set create-draft-release '!gt release create $(uv version --short) --generate-notes --verify-tag --prerelease --target $(git branch --show-current) --title "$1"' --clobber
gt alias set create-release 'release create "$1" --generate-notes --verify-tag --fail-on-no-commits' --clobber

# Repository management
gt alias set pyrepo 'repo create "$1" --public --add-readme --clone --gitignore=Python --license=bsd-3-clause-clear'
#gt alias set gt-create 'repo create --private --source=. --remote=origin; git push -u --all; gt browse'  --clobber
