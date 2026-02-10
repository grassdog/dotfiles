##################
# Common commands
##################

LSC='--color=auto'

if [[ $(uname) == Darwin ]]; then
  LSC='-G'
fi

alias l="ls $LSC -ohF"
alias lt="ls $LSC -lt"   # sort with recently modified first
alias la="ls $LSC -Al"

alias h="history -50"

function take() {
  mkdir -p "$1"
  cd "$1"
}

alias ea='nvim ~/.zsh/aliases.zsh'

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

# Use Dracula theme so it's properly visible in terminal
alias bat='bat --theme Dracula --pager "less -RFi"'

alias obsidian-vaults='cd ~/Library/Mobile\ Documents/iCloud~md~obsidian/Documents'


#######
# Git
#######

alias gap='git add -p'
alias gb='git branch'
alias gc='git commit -v'
alias gca='git commit -v -a'
alias gco='git checkout'
alias gcm='git checkout master'
alias gdt='git difftool'
# Git diff syntax
alias gds='git difftool --tool=difftastic'
alias gd='git diff'
alias gdc='git diff --cached'
alias gpu='git push'
alias gpl='git update'
alias gro='git reset --hard origin/main'
alias gnp='git-notpushed'
alias gs='git status -s'
alias gl='git ls'
alias glp='git lp'
alias gg='git grep-log'
alias gac='git add . && git commit --amend'

alias git-update-modules='git submodule foreach git pull'

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

alias spec="bundle exec rspec -f d"
alias rubo-correct="bundle exec rubocop --auto-correct"

# Run rubocop on modified files
function rubo-changed() {
  # Locally modified
  git ls-files -m | xargs ls -1 2>/dev/null | grep '\.rb$' | xargs rubocop
  # Committed but differing from origin/master
  git diff-tree -r --no-commit-id --name-only head origin/master | xargs rubocop
}

# Format a ruby file with syntax_tree
function format-ruby() {
  stree write --print-width=120 --plugins=plugin/trailing_comma "$1"
}


############
# Processes
############

# Wide ps sorted by CPU usage
alias psu='ps auxw'


#########
# Utils
#########

# Create me a temporary directory to play in
tempd () {
  cd "$(mktemp -d)"
  chmod -R 0700 .
  if [[ $# -eq 1 ]]; then
    mkdir -p "$1"
    cd "$1"
    chmod -R 0700 .
  fi
}

# Serve up the current directory with webrick
function serve-dir() {
  ruby -rwebrick -e"s = WEBrick::HTTPServer.new(:Port => 8888,  :DocumentRoot => Dir.pwd); trap('INT') { s.shutdown }; s.start"
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

# Switch tmux pane with fzf
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

