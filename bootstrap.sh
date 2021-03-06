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

abort() { echo -e "${RED}!!! $*${RESET}" >&2; exit 1; }
step()  { echo -e "\n${GREEN}---> $*${RESET}"; }
log()   { echo -e "$*"; }
warn()  { echo -e "${YELLOW}*** $*${RESET}"; }
ok()    { echo -e "${GREEN}<--- OK${RESET}"; }

#
# Checks
#
sw_vers -productVersion | grep $Q -E "^10.(9|10|11|12|13|14)" || {
  abort "Run bootstrap.sh on macOS 10.9/10/11/12/13/14."
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

step "Ensure common directories exist"
mkdir -p ~/dev
mkdir -p ~/private
mkdir -p ~/btsync
mkdir -p ~/.config
mkdir -p ~/.ssh
mkdir -p ~/.cache/emacs/saves
[ ! -r ~/dev/scripts ] && ln -s $HOME/Dropbox/Code/scripts ~/dev
ok

step "Ensuring dotfiles repo is downloaded and up to date"
if [ ! -d $HOME/.dotfiles ]; then
  git clone https://github.com/grassdog/dotfiles.git $HOME/.dotfiles
else
  cd $HOME/.dotfiles && git pull origin master
fi
ok

function link_files {
  local SOURCE_DIR=$1
  local TARGET_DIR=${2:-$HOME}

  log "Linking files in $SOURCE_DIR"

  for f in $(find $SOURCE_DIR -maxdepth 1 -mindepth 1 \( ! -name .DS_Store ! -name README.md ! -name _dontlink \)); do
    log "Linking $f to $TARGET_DIR"
    ln -sf "$f" "$TARGET_DIR"
  done
}

step "Link dotfiles"
[ -d $DOTFILES_FULL_PATH/files ] && link_files $DOTFILES_FULL_PATH/files
[ -d $DOTFILES_FULL_PATH/hosts/$HOSTNAME ] && link_files $DOTFILES_FULL_PATH/hosts/$HOSTNAME
[ -d $HOME/Dropbox/Backups/$HOSTNAME ] && link_files $HOME/Dropbox/Backups/$HOSTNAME
ok

step "Link .config files"
link_files $DOTFILES_FULL_PATH/files/.config $HOME/.config
ok

step "Set shell to zsh"
[[ $(echo $SHELL) != $(which zsh) ]] && sudo dscl . -create /Users/${whoami} UserShell $(which zsh)
ok

step "Install Kakoune config"
git clone https://github.com/andreyorst/plug.kak.git $HOME/.config/kak/plugins/plug.kak
ok

step "Install vim config"
mkdir -p ~/.cache/vim/tmp/undo
mkdir -p ~/.cache/vim/tmp/backups
ln -sf $DOTFILES_FULL_PATH/files/.vim $HOME
ln -sf $DOTFILES_FULL_PATH/files/.vim/vimrc $HOME/.vimrc
mkdir -p $HOME/.vim/autoload
[ ! -f $HOME/.vim/autoload/plug.vim ] && curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
ok

step "Link Karabiner config"
mkdir -p ~/.config/karabiner
[ ! -f $HOME/.config/karabiner/karabiner.json ] && ln -sf "$HOME/Dropbox/Backups/karabiner/karabiner.json" "$HOME/.config/karabiner"
ok

step "Install tmux config"
mkdir -p ~/.tmux/plugins
[ ! -d $HOME/.tmux/plugins/tpm ] && git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
ok

step "Install asdf plugins"
asdf plugin-add ruby https://github.com/asdf-vm/asdf-ruby.git
asdf plugin-add nodejs https://github.com/asdf-vm/asdf-nodejs.git
bash -c '${ASDF_DATA_DIR:=$HOME/.asdf}/plugins/nodejs/bin/import-release-team-keyring'
asdf plugin-add elixir https://github.com/asdf-vm/asdf-elixir.git
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

step "Install Doom emacs"
if [ ! -d ~/.emacs.d ]; then
  git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
  ~/.emacs.d/bin/doom install
fi

step "Install dev colour picker"
mkdir -p ~/Library/ColorPickers
if [ ! -d ~/Library/ColorPickers/DeveloperColorPicker.colorPicker ]; then
curl -sS http://download.panic.com/picker/developercolorpicker.zip > dcp.zip && \
unzip dcp.zip                                  && \
mv "Developer Color Picker" ~/Library/ColorPickers/DeveloperColorPicker.colorPicker && \
rm dcp.zip
fi
ok

step "Install fonts"
mkdir -p ~/Library/Fonts
find ~/Dropbox/Backups/Fonts/ToInstall -name '*.ttf' -o -name '*.otf' -exec cp {} ~/Library/Fonts \;
chmod -x ~/Library/Fonts/*.ttf
chmod -x ~/Library/Fonts/*.otf
ok

step "Install spelling files"
ln -sf $HOME/Dropbox/Backups/Spell/aspell.en.pws "$HOME/.aspell.en.pws"
ln -sf $HOME/Dropbox/Backups/Spell/aspell.en.prepl "$HOME/.aspell.en.prepl"
ok

if [ -r ~/.Brewfile ]; then
step "Install linked Brewfiles"
brew bundle -v --file=~/.Brewfile
ok
fi

step "Write macOS defaults"

# Autohide the dock
defaults write com.apple.dock autohide -bool true

# Full keyboard access to controls
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

# short key repeat delay
defaults write NSGlobalDomain InitialKeyRepeat -int 12

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

# debug menu in safari enabled
defaults write com.apple.Safari IncludeDebugMenu -bool true

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
