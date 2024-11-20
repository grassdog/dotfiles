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
fi

#
# Logging
#
RESET='\033[0m'
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'

abort() { echo -e "${RED}==❗️ $*${RESET}" >&2; exit 1; }
step()  { echo -e "\n${GREEN}==🚀  $*${RESET}"; }
log()   { echo -e "$*"; }
warn()  { echo -e "${YELLOW}==⚠️  $*${RESET}"; }
ok()    { echo -e "${GREEN}==✅${RESET}"; }

[ "$USER" = "root" ] && abort "Run bootstrap as yourself, not root."
groups | grep $Q admin || abort "Add $USER to the admin group."

DOTFILES_FULL_PATH="$(cd "$(dirname "$0")" && pwd)"
HOSTNAME=$(hostname -f)

step "Ensure Homebrew is installed"
if [[ ! -x /opt/homebrew/bin/brew ]]; then
  log "Installing Homebrew"
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
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

# No git 2.x warning message when pushing
if ! git config push.default >/dev/null; then
  git config --global push.default simple
fi
ok

step "Ensure common directories and file exist"
mkdir -p ~/dev
mkdir -p ~/.config
mkdir -p ~/.ssh
[ ! -r ~/dev/scripts ] && [ -d $HOME/Library/CloudStorage/Dropbox/Code/ ] && ln -s $HOME/Library/CloudStorage/Dropbox/Code/scripts ~/dev
touch ~/.z
ok

step "Ensuring dotfiles repo is downloaded and up to date"
if [ ! -d $HOME/.dotfiles ]; then
  git clone https://github.com/grassdog/dotfiles.git $HOME/.dotfiles
else
  cd $HOME/.dotfiles && git pull origin main --autostash
fi
ok

function link_files {
  local SOURCE_DIR=$1
  local TARGET_DIR=${2:-$HOME}

  log "Linking files in $SOURCE_DIR"

  for f in $(find $SOURCE_DIR -maxdepth 1 -mindepth 1 \( ! -name .DS_Store ! -name README.md ! -name _dontlink ! -name .config ! -name bootstrap.sh \)); do
    log "Linking $f to $TARGET_DIR"
    ln -sf "$f" "$TARGET_DIR"
  done
}

step "Link dotfiles"
[ -d $DOTFILES_FULL_PATH/files ] && link_files $DOTFILES_FULL_PATH/files
[ -d $DOTFILES_FULL_PATH/hosts/$HOSTNAME ] && link_files $DOTFILES_FULL_PATH/hosts/$HOSTNAME
ok

step "Link dotfiles in Dropbox"
[ -d $HOME/Library/CloudStorage/Dropbox/Backups/hosts/$HOSTNAME ] && link_files $HOME/Library/CloudStorage/Dropbox/Backups/hosts/$HOSTNAME
ok

step "Link .config files"
link_files $DOTFILES_FULL_PATH/files/.config $HOME/.config
ok

if [ -r ~/.Brewfile ]; then
step "Install linked Brewfiles"
brew bundle -v --no-upgrade --file=~/.Brewfile
ok
fi

step "Install apps from the App Store"
mas install 1365531024 # 1Blocker - Ad Blocker
mas install 1569813296 # 1Password for Safari
mas install 1091189122 # Bear
mas install 1451400394 # bookmarker for pinboard
mas install 1465439395 # Dark Noise
mas install 1055511498 # Day one
mas install 288545208  # Instapaper
# mas install 2145332318 # Ivory for Mastodon
mas install 1622835804 # Kagi Search for Safari
mas install 1614730313 # MusicBox: Save Music for Later
mas install 1289197285 # MindNode 6
mas install 1006739057 # NepTunes
mas install 1505432629 # NotePlan
mas install 1596506190 # Play: Save Videos Watch Later
mas install 1851660095 # Raycast Companion
mas install 1640236961 # Save to Reader
mas install 1376402589 # StopTheMadness
# mas install 2112214276 # Supercopy for Safari (1.0)
mas install 1475387142 # Tailscale
mas install 904280696  # Things
mas install 1225570693 # Uylsses
mas install 1621370168 # WorldWideWeb – Desktop
ok


if [ -r $DOTFILES_FULL_PATH/hosts/$HOSTNAME/bootstrap.sh ]; then
step "Run host specific bootstrap script"
  "$DOTFILES_FULL_PATH/hosts/$HOSTNAME/bootstrap.sh"
ok
fi

step "Set shell to zsh"
[[ $(echo $SHELL) != $(which zsh) ]] && sudo dscl . -create /Users/${whoami} UserShell $(which zsh)
ok

step "Install vim config"
mkdir -p ~/.cache/vim/tmp/undo
mkdir -p ~/.cache/vim/tmp/backups
ln -sf $DOTFILES_FULL_PATH/files/.vim $HOME
ln -sf $DOTFILES_FULL_PATH/files/.vim/vimrc $HOME/.vimrc
mkdir -p $HOME/.vim/autoload
[ ! -f $HOME/.vim/autoload/plug.vim ] && curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
ok

step "Install neovim config"
~/.config/nvim/setup.sh
ok

# Have to copy this as a symlink doesn't seem to work
step "Copy Karabiner config"
mkdir -p ~/.config/karabiner
[ ! -f $HOME/.config/karabiner/karabiner.json ] && [ -d $HOME/Library/CloudStorage/Dropbox/Backups/ ] && cp "$HOME/Library/CloudStorage/Dropbox/Backups/karabiner/karabiner.json" "$HOME/.config/karabiner"
ok

step "Install tmux config"
mkdir -p ~/.tmux/plugins
[ ! -d $HOME/.tmux/plugins/tpm ] && git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
ok

step "Install asdf plugins"
asdf plugin add ruby https://github.com/asdf-vm/asdf-ruby.git
asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
asdf plugin update --all
ok

step "Install default asdf versions"
while read plugin; do
  asdf install $plugin
done < ~/.tool-versions
ok

step "Install service menu items"
mkdir -p ~/Library/Services
SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
for f in $(find $DOTFILES_FULL_PATH/services -maxdepth 1 -mindepth 1 ! -name .DS_Store); do
  log "Copying $f to ~/Library/Services"
  cp -R "$f" ~/Library/Services
done
IFS=$SAVEIFS
ok

step "Install fonts"
mkdir -p ~/Library/Fonts
[ -d $HOME/Library/CloudStorage/Dropbox/Backups/ ] && find ~/Library/CloudStorage/Dropbox/Backups/Fonts/ToInstall -name '*.ttf' -o -name '*.otf' -exec cp {} ~/Library/Fonts \;
chmod -x ~/Library/Fonts/*.ttf
chmod -x ~/Library/Fonts/*.otf
ok

step "Install spelling files"
[ -d $HOME/Library/CloudStorage/Dropbox/Backups/ ] && ln -sf $HOME/Dropbox/Backups/Spell/aspell.en.pws "$HOME/.aspell.en.pws"
[ -d $HOME/Library/CloudStorage/Dropbox/Backups/ ] && ln -sf $HOME/Dropbox/Backups/Spell/aspell.en.prepl "$HOME/.aspell.en.prepl"
ok


step "Write macOS defaults"

# Autohide the dock
defaults write com.apple.dock autohide -bool true

# Full keyboard access to controls
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

# expanded save panel
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true

# expanded print panel
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true

# no launch warnings
defaults write com.apple.LaunchServices LSQuarantine -bool false

# no press and hold
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# no auto-correct
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

# copy from Quicklook windows
defaults write com.apple.finder QLEnableTextSelection -bool true

# full path in window titles
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# increase window resize speed
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001

# save screenshots in PNG format
defaults write com.apple.screencapture type -string png

# avoid creating DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# disable the warning before emptying the Trash
defaults write com.apple.finder WarnOnEmptyTrash -bool false

# empty Trash securely
defaults write com.apple.finder EmptyTrashSecurely -bool true

# make Dock icons of hidden applications translucent
defaults write com.apple.dock showhidden -bool true

# add a context menu item for showing the Web Inspector in web views
defaults write NSGlobalDomain WebKitDeveloperExtras -bool true

# dock icon size is 38 pixels
defaults write com.apple.dock tilesize -int 38

# menu bar clock
defaults write com.apple.menuextra.clock DateFormat -string "d MMM h:mm a"

# time machine off
defaults write com.apple.TimeMachine AutoBackup -bool false

# disable smart quotes
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

# disable smart dashes
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

# finder show all filename extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# finder show status bar
defaults write com.apple.finder ShowStatusBar -bool true

# finder show path bar
defaults write com.apple.finder ShowPathbar -bool true

# use column view in all Finder windows by default
defaults write com.apple.finder FXPreferredViewStyle -string "clmv"

# no feedback sound when changing volume.defaults
defaults write NSGlobalDomain com.apple.sound.beep.feedback -bool false
ok

step "Check FileVault is enabled"
if fdesetup status | grep $Q "FileVault is Off"; then
  warn "Please run 'sudo fdesetup enable -user \"$USER\"' to enable full-disk encryption."
else
  ok
fi
