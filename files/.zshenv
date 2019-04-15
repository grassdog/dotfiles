##########
# Editors
##########

export EDITOR='vim'
export PAGER='less'
export VISUAL=$EDITOR
export SVN_EDITOR=$EDITOR
export BUNDLER_EDITOR=$EDITOR

export PATH="${HOME}/.bin:${HOME}/.cargo/bin:/usr/local/bin:/usr/local/sbin:/usr/local/share/npm/bin:/usr/local/heroku/bin:/Applications/Visual Studio Code.app/Contents/Resources/app/bin:${PATH}"

# export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)

# Adding Homebrew man path
export MANPATH="/usr/local/share/man:${MANPATH}"

export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'
