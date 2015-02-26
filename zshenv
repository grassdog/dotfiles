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

export PATH="/usr/local/bin:/Applications/Postgres.app/Contents/Versions/9.3/bin:/usr/local/sbin:/usr/local/share/npm/bin:/usr/local/heroku/bin:${PATH}"

GHC_DOT_APP=(/Applications/ghc*.app)
export GHC_DOT_APP
if [ -d "$GHC_DOT_APP" ]; then
  export PATH="${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi

export PATH="${HOME}/.bin:${PATH}"

export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)

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
  [[ -r ~/.ruby-version ]] && chruby $(cat ~/.ruby-version)
  [[ -r ./.ruby-version ]] && chruby $(cat ./.ruby-version)
fi

# Remove the need for bundle exec ... or ./bin/...
# by adding ./bin to path if the current project is trusted

function set_local_bin_path() {
  # Replace any existing local bin paths with our new one
  export PATH="${1:-""}`echo "$PATH"|sed -e 's,[^:]*\.git/[^:]*bin:,,g'`"
}

function add_trusted_local_bin_to_path() {
  if [[ -d "$PWD/.git/safe" ]]; then
    # We're in a trusted project directory so update our local bin path
    set_local_bin_path "$PWD/.git/safe/../../bin:"
  fi
}

# Make sure add_trusted_local_bin_to_path runs after chruby so we
# prepend the default chruby gem paths
if [[ -n "$ZSH_VERSION" ]]; then
  if [[ ! "$preexec_functions" == *add_trusted_local_bin_to_path* ]]; then
    preexec_functions+=("add_trusted_local_bin_to_path")
  fi
fi
