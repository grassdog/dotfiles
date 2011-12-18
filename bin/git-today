#!/usr/bin/env zsh
# git-today
# Written by: Gianni Chiappetta <gianni@runlevel6.org>
# Requires: git 1.7.5+, zsh 4.3.11+

function get_user { git config user.email }

function git_branch() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo ${ref#refs/heads/}
}

# Command normalisation
branch_name="git-today-tmp"
cmd="go"
user=$(get_user)
since="1 day ago"

while getopts "hu:s:" option
do
  case $option in
    h)
      cmd="help"
      ;;
    u)
      user=$OPTARG
      ;;
    s)
      since=$OPTARG
      ;;
    ?)
      cmd="help"
      ;;
  esac
done

# Help
read -d '' help_text <<"EOS"
Git Today - Show one big diff of the work you have done in the past day.
Written by: Gianni Chiappetta <gianni@runlevel6.org>

Usage:
  git today [arguments...]

  Arguments:
    -h
      Show this help.

    -u
      User to find diff for. Default is current user. *Currently broken*

    -s
      Date to search since. Default is the last day.

  Examples:
    git today

    git today -s "14 days ago"

    git today -u some@email.tld

    git today -s "7 days ago" -u person@email.tld
EOS

# Meat... mmm
if [[ $cmd = 'help' ]]; then
  echo $help_text
  return
elif [[ $cmd = 'go' ]]; then
  eval $'commits=( ${(s.\n.)"$(git log --pretty="%H %ae" --since="$since" | grep $user | grep -oE \'^[a-f0-9]+\')"} )'

  if [ ${#commits} -eq 1 ]; then
    if [ -z $commits ]; then
      echo "No commits!"
    else
      git show $commits
    fi
  else
    orig=$(git_branch)
    git branch -d $branch_name &> /dev/null
    git branch $branch_name $orig &> /dev/null
    git checkout $branch_name &> /dev/null
    git reset --soft $commits[-1] &> /dev/null
    git diff --staged
    git reset --hard $orig &> /dev/null
    git checkout $orig &> /dev/null
    git branch -d $branch_name &> /dev/null
  fi
fi

# vim: set filetype=zsh :
