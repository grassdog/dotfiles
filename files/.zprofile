# Config for login shells, run once before .zshrc.
# PATH/MANPATH set here are exported and inherited by every child shell,
# so this is where one-time, potentially expensive setup belongs.

# Get homebrew going
[ -f /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"

# Add local scripts, Obsidian, and VSCode to my path
export PATH="${HOME}/.bin:${HOME}/dev/scripts:/usr/local/bin:/usr/local/sbin:/Applications/Obsidian.app/Contents/MacOS:/Applications/Visual Studio Code.app/Contents/Resources/app/bin:${PATH}"

# Add local bin, bun, cargo, and Matter CLI to path
export PATH="$HOME/.local/bin:$HOME/.bun/bin:$HOME/.matter/bin:$HOME/.cargo/bin:$PATH"

# Adding Homebrew man path
export MANPATH="/usr/local/share/man:${MANPATH}"

# Added by OrbStack: command-line tools and integration
# This won't be added again if you remove it.
source ~/.orbstack/shell/init.zsh 2>/dev/null || :
