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

##########
# Paths
##########

export PATH="${HOME}/.bin:/usr/local/bin:/usr/local/sbin:/usr/local/share/npm/bin:/usr/local/heroku/bin:${PATH}"

export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.7.0_13.jdk/Contents/Home"

# Adding Homebrew man path
export MANPATH="/usr/local/share/man:${MANPATH}"

export NODEPATH="/usr/local/lib/node_modules"

export SUBLHOME="${HOME}/Library/Application Support/Sublime Text 2/Packages/User"

export ANDROID_HOME="/usr/local/opt/android-sdk"

export NLTK_DATA='/Users/rgrasso/.nltk_data'

# password-containing environment variables
[[ -r "$HOME/.secrets" ]] && source "$HOME/.secrets"

# Chruby
if [[ -e /usr/local/opt/chruby/share/chruby/chruby.sh ]]; then
  source /usr/local/opt/chruby/share/chruby/chruby.sh
  source /usr/local/opt/chruby/share/chruby/auto.sh
  chruby $(cat ~/.ruby-version)
fi

