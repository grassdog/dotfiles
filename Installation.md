# OS X Installation

This repo contains the bulk of what I use to stand up a OSX box how I like it. Follow the steps below.

## Bootstrap up the environment

Firstly it gets the host in shape using [strap](https://github.com/MikeMcQuaid/strap) and then fleshes things out with [Homebrew](http://brew.sh/) and [Babushka](http://babushka.me/).

```sh
bash <(curl -s https://raw.githubusercontent.com/grassdog/osx/master/bootstrap.sh)
```

## Configure and Install things via Babushka

Base commands to get things in shape:

```sh
$ babushka personal:loco-osx-settings
$ babushka personal:loco-base
$ babushka personal:loco-dev-env
```

If you've got time up your sleeve you can install all the apps in the world:

```sh
$ babushka personal:brews
$ babushka personal:brew-apps
```

To get your node and Ruby dev on:

```sh
$ babushka personal:loco-node
$ babushka personal:loco-ruby
```

For the bold you can kick it off all at once:

```sh
$ babushka personal:loco
```

Restart host to ensure all settings are in effect.

## Services

Check the status of services via `brew services list`.

## Manual Steps

- Open and set up Dropbox then pause syncing
    - Don't show desktop notifications
- Copy across Dropbox and Music files from backup and `chown -R` them to new user
- Resume Dropbox syncing
- Open and set up 1Password
- Log into Chrome and set as default browser
- Finder
    - Add ~/Dropbox/Notes and ~/dev into Finder sidebar
    - New window opens in ~/Dropbox
    - View options on home folder and "show Library folder"
- Command drag time machine icon off menu bar
- Settings
    - Users and groups > Change profile picture
    - Internet Accounts
        - Add Google for mail and messages only
        - Add Work gmail for calendar
        - Add Twitter account
    - Accessibility > Reduce transparency
    - Keyboard > Set caps lock as control
    - Keyboard > Shortcuts > Spotlight > Change spotlight to `ctrl-option-cmd-space`
    - Keyboard > Shortcuts > Spotlight > Switch off show finder search window
    - Keyboard > Shortcuts > Mission Control > Disable all Mission Control key shortcuts
    - Trackpad > Disable Two finger click
    - Trackpad > Disable Smart zoom and Rotate
    - Trackpad > Disable swipe between pages
    - Desktop and ScreenSaver > Add Dropbox/Pictures/Wallpapers folder and change every hour
    - Desktop and ScreenSaver > Arabesque screensaver
    - Security > Accessibility add apps
        - Dash
        - Fantastical 2
        - Moom
- Place Chrome, Omnifocus, Slack, Soulver, MacVim, ~/Dropbox/Backups/Hollywood.inetloc, and iTerm2 into the Dock
- iTerm
    - Preferences > Set as default terminal
    - Sync preferences ~/Dropbox/Backups/iterm
- Install SSH Keys and config
    - Run Babushka tasks and install in Github and Forge
    - Alternatively install Deus keys
- 1Password
    - Security > Don't lock when screensaver activated
    - Switch on third party integration under advanced settings
- Alfred
    - Install Powerpack from 1Password
    - Set sync folder to ~/Dropbox/Backups/Alfred
    - General > Set shortcut to `cmd-space`
    - Enable 1Password
    - Enable Clipboard history
    - Disable contacts in Alfred
    - Appearance > Yosemite Light
    - Appearance > Hide Menu Bar Icon
- Dash 3
    - Set up synching to `~/Dropbox/Backups/Dash`
- Calendar.app
    - Disable notifications
- Fantastical 2
    - Menu bar icon date and weekday
    - Defaults to last selected calendar and list
    - Change key shortcut to `shift-f12`
    - General > Start Month on current or selected week
- Flux
    - Set wake up time
- Airfoil
    - Only show in menu bar
    - Install extras for instant on
- DayOne
    - Setup sync
    - Baskerville 18pt, no auto bold, no twitter names
    - Disable reminders
    - Disable popover previews
    - Disable spelling corrections
- TextExpander
    - Setup Dropbox sync
    - Hide icon in Dock
    - Launch at login
    - Don't show main window at launch
- Calibre
    - Preferences > Install plugin from file > `~/Dropbox/Backups/Calibre DRM plugins/tools_v6.0.9/DeDRM_calibre_plugin/DeDRM_plugin.zip`
- Yojimbo
    - Link files `ln -s ~/Dropbox/Backups/Yojimbo ~/Library/Application\ Support/Yojimbo`
    - When handling links create web archives
- Copy across or clone projects into `~/dev`
- Copy across from USB backup
    - ~/dev/old
    - ~/dev/scratch
- Lightroom
    - Copy across Lightroom catalog and masters from USB backup
    - Copy across Lightroom presets from backup into new location `~/Library/Application Support/Adobe/Lightroom/{Develop Preset,Export Presets,Filename Templates}`
- OpenGPG
    - Copy across any keys from 1Password
- Photoshop
- MS Office
    - Customize install to not install cruft
- Copy across development VMs

