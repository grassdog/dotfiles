#!/usr/bin/env bash
#/ Usage: bootstrap.sh [OPTIONS]
#/ Setup macOS development box.
#/
#/ -d, --debug  run in debug mode
#/ -h, --help   show help

# With liberal pilfering from https://github.com/MikeMcQuaid/strap

function show_usage {
  grep '^#/' "$0" | cut -c4- 1>&2
  exit 1
}

while true; do
  case "$1" in
    -d | --debug) DEBUG="1"; shift ;;
    -h | --help)  show_usage; shift ;;
    -- ) shift; break ;;
    * ) break ;;
  esac
done

if [ -n "$DEBUG" ]; then
  set -x
else
  Q="-q"
fi

#
# Logging
#
RESET='\033[0m'
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'

abort() { echo -e "${RED}!!! $*${RESET}" >&2; exit 1; }
step()  { echo -e "\n${GREEN}---> $*${RESET}"; }
log()   { echo -e "$*"; }
warn()  { echo -e "${YELLOW}*** $*${RESET}"; }
ok()    { echo -e "${GREEN}<--- OK${RESET}"; }

#
# Checks
#
sw_vers -productVersion | grep $Q -E "^10.(9|10|11|12|13)" || {
  abort "Run bootstrap.sh on macOS 10.9/10/11/12/13."
}

[ "$USER" = "root" ] && abort "Run bootstrap as yourself, not root."
groups | grep $Q admin || abort "Add $USER to the admin group."

DOTFILES_FULL_PATH="$(cd "$(dirname "$0")" && pwd)"
HOSTNAME=$(hostname)

step "Ensure Homebrew is installed"
if ! type -t "brew" > /dev/null; then
  log "Installing Homebrew"
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" </dev/null
fi
ok

step "Configure enough git to get started"
if ! git config user.name >/dev/null; then
  git config --global user.name "Ray Grasso"
fi

if ! git config user.email >/dev/null; then
  git config --global user.email "ray.grasso@gmail.com"
fi

if ! git config github.user >/dev/null; then
  git config --global github.user grassdog
fi

# Setup GitHub HTTPS credentials.
if [ "$(git config --global credential.helper)" != "osxkeychain" ]
then
  git config --global credential.helper osxkeychain
fi

# No git 2.x warning message when pushing
if ! git config push.default >/dev/null; then
  git config --global push.default simple
fi
ok

step "Install common brews"
brew bundle --file=$DOTFILES_FULL_PATH/Brewfile
ok

step "Ensure common directories exist"
mkdir -p ~/dev
mkdir -p ~/private
ok


step "Ensuring dotfiles repo is downloaded"
if [ ! -d $HOME/.dotfiles ]; then
  git clone https://github.com/grassdog/dotfiles.git $HOME/.dotfiles
fi
ok

function link_files {
  local SOURCE_DIR=$1

  log "Linking files in $SOURCE_DIR"

  for f in $(find $SOURCE_DIR -maxdepth 1 -mindepth 1); do
    log "Linking $f"
    # ln -sf $f $HOME
  done
}

step "Link dotfiles"
[ -d $DOTFILES_FULL_PATH/files ] && link_files $DOTFILES_FULL_PATH/files
[ -d $DOTFILES_FULL_PATH/hosts/$HOSTNAME ] && link_files $DOTFILES_FULL_PATH/hosts/$HOSTNAME
[ -d $HOME/Dropbox/Backups/$HOSTNAME ] && link_files $HOME/Dropbox/Backups/$HOSTNAME
ok

step "Check FileVault is enabled"
if fdesetup status | grep $Q "FileVault is Off"; then
  warn "Please run 'sudo fdesetup enable -user \"$USER\"' to enable full-disk encryption."
else
  ok
fi

