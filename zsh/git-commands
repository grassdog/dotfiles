# Git aliases and commands
alias ga='git add'
alias gb='git branch'
alias gc='git commit -v'
alias gca='git commit -v -a'
alias gco='git checkout'
alias gdt='git difftool'
alias gdm='git diff master'
alias gnp="git-notpushed"
alias gst='git status'
alias gs='git status -s'
alias grb='git rebase -p'
alias gm='git merge --no-ff'
alias gl='git log --graph --pretty="format:%C(yellow)%h%Cblue%d%Creset %s %C(green) %an, %ar%Creset"'

function git_current_branch() {
  git symbolic-ref HEAD 2> /dev/null | sed -e 's/refs\/heads\///'
}

# Usage: new-github grassdog project_name
function new-github() {
  git remote add origin git@github.com:$1/$2.git
  git push origin master
  git config branch.master.remote origin
  git config branch.master.merge refs/heads/master
  git config push.default current
}

# Commit pending changes and quote all args as message
function gg() {
    git commit -v -a -m "$*"
}

# Commit staged changes and quote all args as message
function gcm() {
    git commit -v -m "$*"
}

# Setup a tracking branch from [remote] [branch_name]
function gbt() {
  git branch --track $2 $1/$2 && git checkout $2
}

# Quickly clobber a file and checkout
function grf() {
  rm $1
  git checkout $1
}

# Run any specs that have been modified
function gitspec() {
  git diff --name-only ${1:-git-svn} | grep _spec.rb | xargs spec
}

