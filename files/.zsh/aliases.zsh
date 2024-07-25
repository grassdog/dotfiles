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

alias v='nvim'
alias le='less -SR'
alias be="bundle exec"

# Update asdf and its plugins
alias update-asdf="brew upgrade asdf && asdf plugin update --all"

# Update the version of bundler in Gemfile
alias update-bundler="bundle update --bundler"

# tmux
alias tmux="TERM=screen-256color-bce tmux"

# Find conflicted file copies in Dropbox folders (useful for .git directories)
alias find-dropbox-conflicts="find . -name '*conflicted*'"

###############
# OSX Specific
###############

if [[ $(uname) == Darwin ]]; then
  alias fixopenwith='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'
fi

#######
# Git
#######

alias gap='git add -p'
alias gb='git branch'
alias gbr='git branch --remote'
alias gc='git commit -v'
alias gca='git commit -v -a'
alias gco='git checkout'
alias gcm='git checkout master'
alias gdt='git difftool'
# Git diff syntax
alias gds='git difftool --tool=difftastic'
alias gd='git diff'
alias gdd='git dd'
alias gdc='git diff --cached'
alias gdw='git diff --word-diff'
alias gpu='git push'
alias gpl='git update'
alias gro='git rebase @{u}'
alias gnp='git-notpushed'
alias gs='git status -s'
alias gl='git ls'
alias gll='git ll'
alias gls='git lls'
alias glp='git lp'
alias gg='git ls-files | grep'
alias gac='git add . && git commit --amend'
# Add, amend, and force push
alias gaf='git add . && git commit --amend --no-edit && gpu -f'

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

# Push a branch to origin and set up tracking
function gpuu() {
  git push -u origin $(git-current-branch)
}

# Reset to the commit at the base of feature branch and master/main so I can craft some nice commits for pushing
function git-rewind() {
  local trunk="${1:-master}"
  git reset --mixed $(git merge-base $(git rev-parse --abbrev-ref HEAD) $trunk)
}


#######
# Ruby
#######

alias rsp="bundle exec rspec -f d"
alias cop-correct="bundle exec rubocop --auto-correct"

# Run rubocop on modified files
function rubo-changed() {
  # Locally modified
  git ls-files -m | xargs ls -1 2>/dev/null | grep '\.rb$' | xargs rubocop
  # Committed but differing from origin/master
  git diff-tree -r --no-commit-id --name-only head origin/master | xargs rubocop
}

function install-gems() {
  gem install syntax_tree rubocop solargraph rspotify
}

# Format a ruby file with syntax_tree
function format-ruby() {
  stree write --print-width=120 --plugins=plugin/trailing_comma "$1"
}

#######
# Node
#######

function install-node-modules() {
  npm install -g prettier @prettier/plugin-ruby tern jsonlint spaceship-prompt
}

############
# Processes
############

# Wide ps sorted by CPU usage
alias psu='ps auxw'

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

# Syntax highlight a file and copy onto clipboard
function hl() {
  highlight -O rtf -t 2 -K 40 -k 'Source Code Pro' --style twilight $1 | pbcopy
}

# Readline wrapped scheme
function scheme-rl() {
  rlwrap -r -c -f "$HOME/.tools/mit_scheme_bindings.txt" scheme
}

# Serve up the current directory with webrick
function serve-dir() {
  ruby -rwebrick -e"s = WEBrick::HTTPServer.new(:Port => 8888,  :DocumentRoot => Dir.pwd); trap('INT') { s.shutdown }; s.start"
}

# Look up SSL cert details
function ssl-details() {
  echo | openssl s_client -connect $1:443 2>/dev/null | openssl x509 -noout -issuer -subject -dates
}

function extract() {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2) tar xvjf $1   ;;
      *.tar.gz)  tar xvzf $1   ;;
      *.bz2)     bunzip2 $1    ;;
      *.rar)     unrar x $1    ;;
      *.gz)      gunzip $1     ;;
      *.tar)     tar xvf $1    ;;
      *.tbz2)    tar xvjf $1   ;;
      *.tgz)     tar xvzf $1   ;;
      *.zip)     unzip $1      ;;
      *.Z)       uncompress $1 ;;
      *.7z)      7z x $1       ;;
      *)         echo "'$1' cannot be extracted via >extract<" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

alias mux="tmuxinator"
# Start a new tmux session using the project template
muxp() {
  tmuxinator start project -n $1 $1
}

# ftpane - switch tmux pane with fzf
ftpane() {
  local panes current_window current_pane target target_window target_pane
  panes=$(tmux list-panes -s -F '#I:#P - #{pane_current_path} #{pane_current_command}')
  current_pane=$(tmux display-message -p '#I:#P')
  current_window=$(tmux display-message -p '#I')

  target=$(echo "$panes" | grep -v "$current_pane" | fzf +m --reverse) || return

  target_window=$(echo $target | awk 'BEGIN{FS=":|-"} {print$1}')
  target_pane=$(echo $target | awk 'BEGIN{FS=":|-"} {print$2}' | cut -c 1)

  if [[ $current_window -eq $target_window ]]; then
    tmux select-pane -t ${target_window}.${target_pane}
  else
    tmux select-pane -t ${target_window}.${target_pane} &&
    tmux select-window -t $target_window
  fi
}

# fh - repeat history with fzf search
fh() {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
}

