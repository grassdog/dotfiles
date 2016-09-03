dep "loco" do
  requires "loco-osx-settings",
           "loco-base",
           "loco-dev-env",
           "brews",
           "brew-apps"
end

dep "loco-osx-settings" do
  requires "auto hide dock.defaults",
           "full keyboard access to controls.defaults",
           "fast key repeat rate.defaults",
           "short key repeat delay.defaults",
           "expanded save panel.defaults",
           "expanded print panel.defaults",
           "no launch warnings.defaults",
           "no press and hold.defaults",
           "no auto-correct.defaults",
           "copy from Quicklook windows.defaults",
           "full path in window titles.defaults",
           "debug menu in safari enabled.defaults",
           "increase window resize speed.defaults",
           "require password immediately after sleep.defaults",
           "require password after screen saver begins.defaults",
           "save screenshots in PNG format.defaults",
           "avoid creating DS_Store files on network volumes.defaults",
           "disable the warning when changing a file extension.defaults",
           "disable the warning before emptying the Trash.defaults",
           "make Dock icons of hidden applications translucent.defaults",
           "add a context menu item for showing the Web Inspector in web views.defaults",
           "dock icon size is 38 pixels.defaults",
           "menu bar clock.defaults",
           "time machine off.defaults",
           "disable smart quotes.defaults",
           "disable smart dashes.defaults",
           "finder show all filename extensions.defaults",
           "finder show status bar.defaults",
           "finder show path bar.defaults",
           "use column view in all Finder windows by default.defaults",
           "no feedback sound when changing volume.defaults",
           "show users Library folder",
           "timezone is perth",
           "power settings",
           "osx computer name set".with(computer_name: loco)

  after {
    shell "killall -HUP Dock"
    shell "killall -HUP Finder"
  }
end

dep "loco-base" do
  requires "dotfiles-installed",
           "moom-config.file",
           "proselint.pip",
           "aws-vault",
           "all-fonts"
end

dep "loco-dev-env" do
  requires "dev-env",
           "loco-global-ruby",
           "loco-global-node"
end

dep "loco-global-ruby" do
  requires "global-ruby".with(version: "2.3.1")
end

dep "loco-global-node" do
  requires "default-node".with(version: "6.4")
end

