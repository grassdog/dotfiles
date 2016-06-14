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

function take() {
  mkdir -p "$1"
  cd "$1"
}

alias ea='vim ~/.zsh/aliases.zsh'

# Set iterm title
function title() {
  if [ "$TMUX" == "" ]; then
    echo -ne "\e]0;$1\a"
  else
    echo -ne "\033k$1\033\\"
  fi
}

alias vi='vim'
alias v='vim'
alias le='less -SR'
alias be="bundle exec"
alias bi="bundle install"

# Give me context
alias ag='ag -C'

function et() {
  emacsclient --alternate-editor="" -t "$@"
}

function eg() {
  emacsclient --alternate-editor="" -c -n "$@"
}

# Tasks
export TASKS="${HOME}/Dropbox/Notes/Tasks"
alias t="taskmeister -t ${TASKS}"
alias tg="taskmeister -t ${TASKS} -l general.md"

# tmux
alias tmux="TERM=screen-256color-bce tmux"

###############
# OSX Specific
###############

if [[ $(uname) == Darwin ]]; then
  alias subl="/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin/subl"
  alias s="/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin/subl"

  alias gv='mvim'
  alias fixopenwith='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'

  alias e="~/.bin/emacs-start"

  alias play="osascript -e 'tell app \"iTunes\" to playpause'"
  alias next="osascript -e 'tell app \"iTunes\" to next track'"
  alias prev="osascript -e 'tell app \"iTunes\" to previous track'"
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
alias gd='git diff --word-diff'
alias gdc='git diff --word-diff --cached'
alias gpo='git push origin'
alias gpu='git push'
alias gpl='git update'
alias grom='git rebase origin/master'
alias gnp='git-notpushed'
alias gs='git status -s'
alias gl='git ls'

alias git-update-modules='git submodule foreach git pull'

alias git-most-changed='git log --pretty=format: --name-only | sort | uniq -c | sort -rg | head -10'

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

function portgrep {
  lsof -n -i4TCP:${1-8000} | grep LISTEN
}

alias synctapas="rsync -rtvu ~/Movies/Learning/ruby-tapas/ /Volumes/Bitshack/Movies/Learning/ruby-tapas/"
alias wifi="networksetup -setairportpower en0"
alias netlisteners='lsof -i -P | grep LISTEN'

alias profileme="history 1 | awk '{print \$2}' | awk 'BEGIN{FS=\"|\"}{print \$1}' | sort | uniq -c | sort -nr | head -n 20"

# Find files
function f() {
  find "${2-.}" -name "*$1*"
}

alias gg='git ls-files | grep'

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

# Dotfiles
function dotpl() {
  pushd ~/.dotfiles
  git update
  popd
}

alias dot="pushd ~/.dotfiles"

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

function pj() {
  cd $(find ~/code/projects -maxdepth 1 -type d | selecta)
}

function posts() {
  e $(find ~/code/projects/dance.computer/source/posts -maxdepth 1 -type f | selecta)
}

function notes() {
  e "$(find ~/Dropbox/Notes -maxdepth 3 -type f | grep -v '/\.' | selecta)"
}

alias findpid="ps axww -o pid,user,%cpu,%mem,start,time,command | selecta | sed 's/^ *//' | cut -f1 -d' '"

# By default, ^S freezes terminal output and ^Q resumes it. Disable that so
# that those keys can be used for other things.
unsetopt flowcontrol
# Run Selecta in the current working directory, appending the selected path, if
# any, to the current command, followed by a space.
function insert-selecta-path-in-command-line() {
    local selected_path
    # Print a newline or we'll clobber the old prompt.
    echo
    # Find the path; abort if the user doesn't select anything.
    selected_path=$(find * -type f | selecta) || return
    # Append the selection to the current command buffer.
    eval 'LBUFFER="$LBUFFER$selected_path "'
    # Redraw the prompt since Selecta has drawn several new lines of text.
    zle reset-prompt
}
# Create the zle widget
zle -N insert-selecta-path-in-command-line
# Bind the key to the newly created widget
bindkey "^S" "insert-selecta-path-in-command-line"
