##################
# Common commands
##################

alias cls='clear; l'
alias h='history -40'
alias l.='ls -d .[^.]*'
alias l='ls -ohGF'  # -l long listing, -G color
alias lt='ls -lt'   # sort with recently modified first
alias la="ls -AlG"

# no spelling corrections  (man zshbuiltins)
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
alias e='emacs -nw'

###############
# OSX Specific
###############

if [[ $(uname) == Darwin ]]; then
  alias subl="/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin/subl"
  alias s="/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin/subl"
  alias v='mvim'
  alias o='open . &'
  safari() {  open -a Safari "$@"  }
  alias apps='open /Applications'
  alias csub='cd ~/Library/Application\ Support/Sublime\ Text\ 2/Packages'
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

# Serve up the current directory with webrick
function rserve() {
  ruby -rwebrick -e"s = WEBrick::HTTPServer.new(:Port => 3000,  :DocumentRoot => Dir.pwd); trap('INT') { s.shutdown }; s.start"
}

######################################################
# Safe rm (from http://github.com/suztomo/dotfiles)
######################################################

function rmf(){
    for file in $*
    do
        __rm_single_file $file
    done
}

function __rm_single_file(){
    if ! [ -d ~/.Trash/ ]
    then
        command /bin/mkdir ~/.Trash
    fi

    if ! [ $# -eq 1 ]
    then
        echo "__rm_single_file: 1 argument required but $# passed."
        exit
    fi

    if [ -e $1 ]
    then
        BASENAME=`basename $1`
        NAME=$BASENAME
        COUNT=0
        while [ -e ~/.Trash/$NAME ]
        do
            COUNT=$(($COUNT+1))
            NAME="$BASENAME.$COUNT"
        done

        command /bin/mv $1 ~/.Trash/$NAME
    else
        echo "No such file or directory: $file"
    fi
}


