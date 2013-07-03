##################
# Common commands
##################

alias h='history -40'
alias l.='ls -d .[^.]*'
alias l='ls -ohGF'  # -l long listing, -G color
alias lt='ls -lt'   # sort with recently modified first
alias la="ls -AlG"

# no spelling corrections (man zshbuiltins)
alias mv='nocorrect mv'
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
alias rm='nocorrect rm'

function take() {
  mkdir -p "$1"
  cd "$1"
}

alias vi='vim'
alias le='less -SR'
alias be="bundle exec" # Bundler

# Give me context
alias ag='ag -C'

# Emacs

alias ed="emacs --daemon"
alias e="emacsclient -n -t"
alias ec="emacsclient -n -c"

###############
# OSX Specific
###############

if [[ $(uname) == Darwin ]]; then
  alias emacs="/usr/local/Cellar/emacs/24.2/bin/emacs"
  alias emacsclient="/usr/local/Cellar/emacs/24.2/bin/emacsclient"

  alias subl="/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin/subl"
  alias s="/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin/subl"

  alias v='mvim'
  alias o='open . &'
  safari() {  open -a Safari "$@"  }
  alias fixopenwith='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'
fi

alias tmux="TERM=screen-256color-bce tmux"

############
# Processes
############

alias k9="killall -9"
function killnamed () {
  ps ax | grep $1 | cut -d ' ' -f 2 | xargs kill
}
function psg() {
  ps aux | grep $1
}
alias psu='ps auxw'         # Wide ps sorted by CPU usage
alias tu='top -o cpu'       # cpu
alias tm='top -o vsize'     # memory

function any() {
  emulate -L zsh
  unsetopt KSH_ARRAYS
  if [[ -z "$1" ]] ; then
    echo "any - grep for process(es) by keyword" >&2
    echo "Usage: any " >&2 ; return 1
  else
    ps xauwww | grep -i --color=auto "[${1[1]}]${1[2,-1]}"
  fi
}

#########
# Utils
#########

# Find files
function f() {
  find . -name "*$1*" -print
}

# Remove a bunch of files
function clean() {
  if [ $# -eq 1 ]
  then
    find . -name $1 -exec rm {} \;
  else
    echo "No args passed so doing nothing"
  fi
}

# Build ruby
function build_ruby() {
  ruby-build $1 ~/.rubies/$1
}

# Serve up the current directory with webrick
function rserve() {
  ruby -rwebrick -e"s = WEBrick::HTTPServer.new(:Port => 3000,  :DocumentRoot => Dir.pwd); trap('INT') { s.shutdown }; s.start"
}

function trash() {
  mv "$@" ~/.Trash
}
