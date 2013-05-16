#
# Git aliases and commands
#

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

# Grep through commit history for a string
function git-grep-commits() {
  git grep "$1" $(git rev-list --all)
}

function git_current_branch() {
  git symbolic-ref HEAD 2> /dev/null | sed -e 's/refs\/heads\///'
}

# Commit staged changes and quote all args as message
function gcm() {
    git commit -v -m "$*"
}

# Quickly clobber a file and checkout
function grf() {
  rm $1
  git checkout $1
}

# Run any specs that have been modified
function git-spec() {
  git diff --name-only ${1:-git-svn} | grep _spec.rb | xargs spec
}
