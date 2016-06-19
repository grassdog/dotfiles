# OSX settings

dep "auto hide dock.defaults" do
  domain "com.apple.dock"
  key "autohide"
  value true
end

dep "full keyboard access to controls.defaults" do
  domain "NSGlobalDomain"
  key "AppleKeyboardUIMode"
  value 3
end

dep "fast key repeat rate.defaults" do
  domain "NSGlobalDomain"
  key "KeyRepeat"
  value 2
end

dep "short key repeat delay.defaults" do
  domain "NSGlobalDomain"
  key "InitialKeyRepeat"
  value 12
end

dep "expanded save panel.defaults" do
  domain "NSGlobalDomain"
  key "NSNavPanelExpandedStateForSaveMode"
  value true
end

dep "expanded print panel.defaults" do
  domain "NSGlobalDomain"
  key "PMPrintingExpandedStateForPrint"
  value true
end

dep "no launch warnings.defaults" do
  domain "com.apple.LaunchServices"
  key "LSQuarantine"
  value false
end

dep "no press and hold.defaults" do
  domain "NSGlobalDomain"
  key "ApplePressAndHoldEnabled"
  value false
end

dep "no auto-correct.defaults" do
  domain "NSGlobalDomain"
  key "NSAutomaticSpellingCorrectionEnabled"
  value false
end

dep "copy from Quicklook windows.defaults" do
  domain "com.apple.finder"
  key "QLEnableTextSelection"
  value true
end

dep "full path in window titles.defaults" do
  domain "com.apple.finder"
  key "_FXShowPosixPathInTitle"
  value true
end

dep "debug menu in safari enabled.defaults" do
  domain "com.apple.Safari"
  key "IncludeDebugMenu"
  value true
end

dep "increase window resize speed.defaults" do
  domain "NSGlobalDomain"
  key "NSWindowResizeTime"
  value 0.001
end

dep "require password immediately after sleep.defaults" do
  domain "com.apple.screensaver"
  key "askForPasswordDelay"
  value 0
end

dep "require password after screen saver begins.defaults" do
  domain "com.apple.screensaver"
  key "askForPassword"
  value 1
end

dep "save screenshots in PNG format.defaults" do
  domain "com.apple.screencapture"
  key "type"
  value "png"
end

dep "avoid creating DS_Store files on network volumes.defaults" do
  domain "com.apple.desktopservices"
  key "DSDontWriteNetworkStores"
  value true
end


dep "disable the warning when changing a file extension.defaults" do
  domain "com.apple.finder"
  key "FXEnableExtensionChangeWarning"
  value false
end

dep "disable the warning before emptying the Trash.defaults" do
  domain "com.apple.finder"
  key "WarnOnEmptyTrash"
  value false
end

dep "empty Trash securely.defaults" do
  domain "com.apple.finder"
  key "EmptyTrashSecurely"
  value true
end

dep "make Dock icons of hidden applications translucent.defaults" do
  domain "com.apple.dock"
  key "showhidden"
  value true
end

dep "add a context menu item for showing the Web Inspector in web views.defaults" do
  domain "NSGlobalDomain"
  key "WebKitDeveloperExtras"
  value true
end

dep "dock icon size is 38 pixels.defaults" do
  domain "com.apple.dock"
  key "tilesize"
  value 38
end

dep "menu bar clock.defaults" do
  domain "com.apple.menuextra.clock"
  key "DateFormat"
  value "d MMM h:mm a"
end

dep "time machine off.defaults" do
  domain "com.apple.TimeMachine"
  key "AutoBackup"
  value false
end

dep "disable smart quotes.defaults" do
  domain "NSGlobalDomain"
  key "NSAutomaticQuoteSubstitutionEnabled"
  value false
end

dep "disable smart dashes.defaults" do
  domain "NSGlobalDomain"
  key "NSAutomaticDashSubstitutionEnabled"
  value false
end

dep "finder show all filename extensions.defaults" do
  domain "NSGlobalDomain"
  key "AppleShowAllExtensions"
  value true
end

dep "finder show status bar.defaults" do
  domain "com.apple.finder"
  key "ShowStatusBar"
  value true
end

dep "finder show path bar.defaults" do
  domain "com.apple.finder"
  key "ShowPathbar"
  value true
end

# Four-letter codes for the other view modes: `icnv`, `clmv`, `Flwv`, `Nlsv`
dep "use column view in all Finder windows by default.defaults" do
  domain "com.apple.finder"
  key "FXPreferredViewStyle"
  value "clmv"
end


dep "no feedback sound when changing volume.defaults" do
  domain "NSGlobalDomain"
  key "com.apple.sound.beep.feedback"
  value false
end

#
# Other settings
#

dep "show users Library folder" do
  meet { shell "chflags nohidden ~/Library" }
end

dep "timezone is perth" do
  met? { `sudo systemsetup -gettimezone`.strip == "Time Zone: Australia/Perth" }
  meet { shell "sudo systemsetup -settimezone Australia/Perth" }
end

dep "osx computer name set", :computer_name, :local_hostname, :for => :osx do
  # Set computer name (as done via System Preferences -> Sharing)
  computer_name.ask("OS X Computer Name").default(shell('scutil --get ComputerName'))
  local_hostname.ask("OS X Local Hostname").default(computer_name.to_s.downcase.gsub(' ', '-'))

  met? {
    shell("scutil --get ComputerName") == computer_name &&
    shell("scutil --get HostName") == computer_name &&
    shell("scutil --get LocalHostName") == local_hostname &&
    shell("defaults read /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName") == local_hostname
  }
  meet {
    sudo "scutil --set ComputerName '#{computer_name}'"
    sudo "scutil --set HostName '#{computer_name}'"
    sudo "scutil --set LocalHostName '#{local_hostname}'"
    sudo "defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string '#{local_hostname}'"
  }
end


dep "power settings" do
  meet {
    shell "sudo /usr/bin/pmset -c sleep 0"
    shell "sudo /usr/bin/pmset -c displaysleep 10"
    shell "sudo /usr/bin/pmset -c disksleep 10"
    # wake on network access
    shell "sudo /usr/bin/pmset -c womp 1"
    shell "sudo /usr/bin/pmset -c lidwake 1"
  }
end
