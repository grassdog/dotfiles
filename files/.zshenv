# Config for all zsh shells

##########
# Editors
##########

export EDITOR='nvim'
export GUI_EDITOR='zed'
export PAGER='less'
export VISUAL=$EDITOR
export SVN_EDITOR=$EDITOR
export BUNDLER_EDITOR=$EDITOR

# Setup some search commands
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'

export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

# Source secrets into environment
[[ -r "$HOME/.secrets" ]] && source "$HOME/.secrets"
