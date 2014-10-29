##################
# Common commands
##################

LSC='--color=auto'

if [[ $(uname) == Darwin ]]; then
  LSC='-G'
fi

alias l.="ls $LSC -d .[^.]*"
alias l="ls $LSC -ohF"
alias lt="ls $LSC -lt"   # sort with recently modified first
alias la="ls $LSC -Al"

alias h="history -40"

alias c="cd ~/code"
alias p="cd ~/code/projects"

function take() {
  mkdir -p "$1"
  cd "$1"
}

# Set iterm title
function title() {
  echo -ne "\e]1;$1\a"
}

alias vi='vim'
alias v='vim'
alias le='less -SR'
alias be="bundle exec"
alias bi="bundle install"

# Give me context
alias ag='ag -C'
# Only search over git files
function gag() {
  ag -C "$*" $(git ls-files)
}

# Emacs
alias ed="emacs --daemon"
alias e="emacsclient -n -t"
alias ec="emacsclient -n -c"

# Tasks
export TASKS="${HOME}/Dropbox/Notes/Tasks"
alias t="taskmeister -t ${TASKS}"
alias tg="taskmeister -t ${TASKS} -l general.md"

###############
# OSX Specific
###############

if [[ $(uname) == Darwin ]]; then
  alias subl="/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin/subl"
  alias s="/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin/subl"

  alias gv='mvim'
  alias fixopenwith='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'

  alias play="osascript -e 'tell app \"iTunes\" to playpause'"
  alias next="osascript -e 'tell app \"iTunes\" to next track'"
  alias prev="osascript -e 'tell app \"iTunes\" to previous track'"
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
alias gdc='git diff --word-diff --cached'
alias gpu='git push'
alias gpl='git update'
alias gnp='git-notpushed'
alias gst='git status'
alias gs='git status -s'
alias gl='git ls'

alias git-update-modules='git submodule foreach git pull'

alias git-most-changed='git log --pretty=format: --name-only | sort | uniq -c | sort -rg | head -10'

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

#########
# Java
#########

function j7() {
  export JAVA_HOME=$(/usr/libexec/java_home -v 1.7)
}

function j8() {
  export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)
}

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

alias synctapas="rsync -rtvu ~/Movies/Learning/ruby-tapas/ /Volumes/Bitshack/Movies/Learning/ruby-tapas/"
alias wifi="networksetup -setairportpower en0"
alias netlisteners='lsof -i -P | grep LISTEN'

alias profileme="history 1 | awk '{print \$2}' | awk 'BEGIN{FS=\"|\"}{print \$1}' | sort | uniq -c | sort -nr | head -n 20"

# Find files
function f() {
  find "${2-.}" -name "*$1*" -print
}

# Find biggest files
function biggest() {
  find . -type f -print0 | xargs -0 du -s | sort -n | tail -150 | cut -f2 | xargs -I{} du -sh {}
}

# Remove a bunch of files
function clean() {
  if [ $# -ne 1 ]; then
    echo "Usage: clean file-name-to-clean"
    return 1
  fi

  find . -name $1 -exec mv \{\} ~/.Trash \;
}

function g() {
  grep -ri $1 ${2-.}
}

# Syntax highlight a file and copy onto clipboard
function hl() {
  highlight -O rtf -t 2 -K 40 -k 'Source Code Pro' --style twilight $1 | pbcopy
}

# Readline wrapped scheme
function scheme-rl() {
  rlwrap -r -c -f "$HOME/.tools/mit_scheme_bindings.txt" scheme
}

# Build ruby
function build-ruby() {
  ruby-build $1 ~/.rubies/$1
}

# Serve up the current directory with webrick
function serve-dir() {
  ruby -rwebrick -e"s = WEBrick::HTTPServer.new(:Port => 8888,  :DocumentRoot => Dir.pwd); trap('INT') { s.shutdown }; s.start"
}

function trash() {
  mv "$@" ~/.Trash
}
