##########
# Editors
##########

export EDITOR='vim'
export PAGER='less'
export VISUAL=$EDITOR
export SVN_EDITOR=$EDITOR
export BUNDLER_EDITOR=$EDITOR

if [[ $(uname) == Darwin ]]; then
  export BUNDLER_EDITOR="mvim"
fi

export PATH="${HOME}/.bin:/usr/local/bin:/usr/local/sbin:/usr/local/share/npm/bin:/usr/local/heroku/bin:${PATH}"

export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)

# Adding Homebrew man path
export MANPATH="/usr/local/share/man:${MANPATH}"

export NODEPATH="/usr/local/lib/node_modules"

HOSTRC="$HOME/.dotfiles/zsh/$(hostname).zsh"

[[ -r $HOSTRC ]] && source $HOSTRC
