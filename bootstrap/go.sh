#!/usr/bin/env bash
#/ Usage: bootstrap.sh
#/ Setup Mac OS X development box.
set -e

log() { echo "--> $*"; }

bootstrap() {
  ensure-directories-exist

  log "Installing babushka"
  sh -c "`curl https://babushka.me/up`"

  git clone https://github.com/grassdog/dotfiles ~/.dotfiles

  log "Linking babushka deps"
  mkdir -p ~/.babushka
  ln -s ~/.dotfiles/babushka-deps ~/.babushka/deps

  # TODO This could be a babushka dep
  log "Installing dotfiles"
  cd ~/.dotfiles
  rake install
}

ensure-directories-exist() {
  log "Checking to make sure you have a /usr/local directory"
  if [ -d /usr/local ]
  then
    log "/usr/local exists. Awesome."
  else
    log "Creating /usr/local."
    sudo mkdir -p /usr/local
    sudo chmod g+w /usr/local
    sudo chgrp staff /usr/local
    sudo chown -R `whoami`:admin /usr/local/bin
  fi

  log "Checking permissions in /usr/local/..."
  # Make sure the current user can write to /usr/local/babushka
  if [ -w /usr/local ]
  then
    log "/usr/local is writable. Awesome."
  else
    sudo chmod g+w /usr/local
    sudo chgrp staff /usr/local
  fi

  if [ -w /usr/local/bin ]
  then
    log "/usr/local/bin is writable. Also Awesome."
  else
    sudo chmod g+w /usr/local
    sudo chgrp staff /usr/local
  fi

  if [ -w /usr/local/babushka ]
  then
    log "/usr/local/babushka is writable."
  elif [ -e /usr/local/babushka ]
  then
    log "/usr/local/babushka exists, but is not writable by you. Fixing that."
    sudo chgrp staff /usr/local/babushka
    sudo chmod g+w /usr/local/babushka
  else
    log "/usr/local/babushka does not exist. That's OK. We'll create it later."
  fi


  if [ -w /usr/local/share/man ]
  then
    log "/usr/local/share/man is writable."
  else
    log "/usr/local/share/man is not writable by you. Fixing that."
    sudo mkdir -p /usr/local/share/man
    sudo chgrp staff /usr/local/share/man
    sudo chmod g+w /usr/local/share/man
  fi

  log "OK. Permissions are good to go."
}

main() {
  log "Open (s)trap or continue (b)ootstrapping? (s/b/q)"
  read string
  if [ "s" == "$string" ]; then
    log "Follow the instructions on the strap site then run this script again"
    open https://osx-strap.herokuapp.com
    exit
  elif [ "b" == "$string" ]; then
    bootstrap
  else
    echo "Exiting"
    exit 1
  fi
}

main
