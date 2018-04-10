# Go
export GOPATH=$HOME/.golang
export GOROOT=/usr/local/opt/go/libexec
export PATH=$PATH:$GOPATH/bin

# Add Haskell stack to the path
export PATH=~/.local/bin/:$PATH

alias mux="mux-personal"

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
