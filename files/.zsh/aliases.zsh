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

alias h="history -50"

function take() {
  mkdir -p "$1"
  cd "$1"
}

alias ea='nvim ~/.zsh/aliases.zsh'

# Set iterm title
function title() {
  if [ "$TMUX" == "" ]; then
    echo -ne "\e]0;$1\a"
  else
    echo -ne "\033k$1\033\\"
  fi
}

alias vi='nvim'
alias v='nvim'
alias le='less -SR'
alias be="bundle exec"

alias showerthought="fortune ~/Dropbox/Backups/showerthoughts"

# Give me context
alias ag='ag -C'

# Case insensitive by default
alias rg='rg -i'

# Open emacsclient in terminal
function et() {
  emacsclient --alternate-editor="" -nw "$@"
}

# Open emacsclient in a GUI frame
function eg() {
  emacsclient --alternate-editor="" -n "$@"
}

# tmux
alias tmux="TERM=screen-256color-bce tmux"
# New window
function nw() {
  tmux new-window -c ${2:-~/dev} -n $1
}

###############
# OSX Specific
###############

if [[ $(uname) == Darwin ]]; then
  alias gv='mvim'
  alias fixopenwith='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'

  # Open via Emacs.app
  alias e="~/.bin/emacs-start"
fi

#######
# Git
#######

alias ga='git add'
alias gap='git add -p'
alias gb='git branch'
alias gbr='git branch --remote'
alias gc='git commit -v'
alias gca='git commit -v -a'
alias gco='git checkout'
alias gma='git checkout master'
alias gdt='git difftool'
alias gd='git diff'
alias gds='git dsf'
alias gdc='git diff --cached'
alias gdw='git diff --word-diff'
alias gpo='git push origin'
alias gpu='git push'
alias gpl='git update'
alias grom='git rebase origin/master'
alias gnp='git-notpushed'
alias gs='git status -s'
alias gl='git ls'
alias gg='git ls-files | grep'

alias git-update-modules='git submodule foreach git pull'

alias git-most-changed='git log --pretty=format: --name-only | sort | uniq -c | sort -rg | head -10'

# Show the revision at a certain date on current branch
function git-rev() {
  git rev-list -n1 --before="${1:-1 day ago}" $(git rev-parse --abbrev-ref HEAD)
}

# Show diff since a date
function git-changed() {
  git diff $(git-rev "${1:-1 day ago}")
}

# Commit staged changes and quote all args as message
function gcm() {
  git commit -v -m "$*"
}

function git-current-branch() {
  git symbolic-ref HEAD 2> /dev/null | sed -e 's/refs\/heads\///'
}

function gpuu() {
  git push -u origin $(git-current-branch)
}

#######
# Ruby
#######

alias rd="bundle exec rspec -f d"
alias wip="bundle exec cucumber -p wip"
alias rca="bundle exec rubocop --auto-correct"

############
# Processes
############

alias psu='ps auxw'         # Wide ps sorted by CPU usage

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

#######
# AWS #
#######

function aws-db-versions {
  aws rds describe-db-instances --region us-east-1 --query 'DBInstances[].[DBInstanceIdentifier,AutoMinorVersionUpgrade,EngineVersion]' --output text
}

##########
# Elixir #
##########

alias exc="iex -S mix"

#########
# Utils
#########

function portgrep {
  lsof -n -i4TCP:${1-8000} | grep LISTEN
}

alias wifi="networksetup -setairportpower en0"
alias netlisteners='lsof -i -P | grep LISTEN'

# Print some stats on my shell commands
alias profileme="history 1 | awk '{print \$2}' | awk 'BEGIN{FS=\"|\"}{print \$1}' | sort | uniq -c | sort -nr | head -n 20"

# Find files
function f() {
  find "${2-.}" -name "*$1*"
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

# Some selecta-based helpers
function ff() {
  "${EDITOR:-vim}" "$(find . -not -path '*/\.*' -type f | selecta)"
}

function posts() {
  "${EDITOR:-vim}" "$(find ~/dev/dance.computer/source/posts -maxdepth 1 -type f | selecta)"
}

function notes() {
  "${EDITOR:-vim}" "$(find ~/Dropbox/Notes -type f  -not -path '*/\.*' | selecta)"
}

# Look up SSL cert details
function ssl-details() {
  echo | openssl s_client -connect $1:443 2>/dev/null | openssl x509 -noout -issuer -subject -dates
}
