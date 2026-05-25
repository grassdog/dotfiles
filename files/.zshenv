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

# Get homebrew going
[ -f /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"

# Add local scripts, Obsidian, and VSCode to my path
export PATH="${HOME}/.bin:${HOME}/dev/scripts:/usr/local/bin:/usr/local/sbin:/Applications/Obsidian.app/Contents/MacOS:/Applications/Visual Studio Code.app/Contents/Resources/app/bin:${PATH}"

# Add local bin, bun, and Matter CLI to path
export PATH="$HOME/.local/bin:$HOME/.bun/bin:$HOME/.matter/bin:$PATH"

# Enable direnv for all sessions
if type direnv >/dev/null 2>&1; then
  eval "$(direnv hook zsh)"
fi

# Add asdf
export PATH="${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"

# Adding Homebrew man path
export MANPATH="/usr/local/share/man:${MANPATH}"

# Setup some search commands
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'

export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

# Source secrets into environment
[[ -r "$HOME/.secrets" ]] && source "$HOME/.secrets"
