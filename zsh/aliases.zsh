##################
# Common commands
##################

alias h='history -40'
alias l.='ls -d .[^.]*'
alias l='ls -ohGF'  # -l long listing, -G color
alias lt='ls -lt'   # sort with recently modified first
alias la="ls -AlG"

function take() {
  mkdir -p "$1"
  cd "$1"
}

alias vi='vim'
alias v='vim'
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

  alias vm='mvim'
  alias o='open . &'
  alias fixopenwith='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'
fi

alias tmux="TERM=screen-256color-bce tmux"

#######
# Git
#######

alias ga='git add'
alias gb='git branch'
alias gc='git commit -v'
alias gca='git commit -v -a'
alias gco='git checkout'
alias gdt='git difftool'
alias gd='git diff --word-diff'
alias gpo='git push origin master'
alias gpl='git pull origin master'
alias gnp="git-notpushed"
alias gst='git status'
alias gs='git status -s'
alias gl='git log --graph --pretty="format:%C(yellow)%h%Cblue%d%Creset %s %C(green) %an, %ar%Creset"'

alias git-update-modules='git submodule foreach git pull'

# Grep through commit history for a string
function git-grep-commits() {
  git grep "$1" $(git rev-list --all)
}

function git-current-branch() {
  git symbolic-ref HEAD 2> /dev/null | sed -e 's/refs\/heads\///'
}

# Commit staged changes and quote all args as message
function gcm() {
    git commit -v -m "$*"
}

# Run any specs that have been modified
function git-spec() {
  git diff --name-only ${1:-git-svn} | grep _spec.rb | xargs spec
}

alias gtags='~/.git_template/hooks/ctags'

############
# Processes
############

alias psu='ps auxw'         # Wide ps sorted by CPU usage
alias tu='top -o cpu'       # By cpu
alias tm='top -o vsize'     # By memory

function psg() {
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

# Find biggest files
function biggest() {
  find . -type f -print0 | xargs -0 du -s | sort -n | tail -150 | cut -f2 | xargs -I{} du -sh {}
}

# Remove a bunch of files
function clean() {
  if [ $# -eq 1 ]
  then
    find . -name $1 -exec trash {} \;
  else
    echo "No args passed so doing nothing"
  fi
}

function g() {
  grep -ri $1 $2
}

# Build ruby
function build-ruby() {
  ruby-build $1 ~/.rubies/$1
}

# Serve up the current directory with webrick
function serve-dir() {
  ruby -rwebrick -e"s = WEBrick::HTTPServer.new(:Port => 3000,  :DocumentRoot => Dir.pwd); trap('INT') { s.shutdown }; s.start"
}

function trash() {
  mv "$@" ~/.Trash
}
