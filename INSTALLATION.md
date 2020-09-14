# Bootstrap a macOS box

Use the steps below to stand up a macOS box how I like it.

## Bootstrap up the environment

Sign in and set the hostname. A bunch of Dropbox files will need to be synced before running the script below.

```sh
bash <(curl -s https://raw.githubusercontent.com/grassdog/dotfiles/master/bootstrap.sh)
```

Restart host to ensure all settings are in effect.

Look in the [hosts](hosts) directory for host specific steps.

## Services

Check the status of services via `brew services list`.

## Manual Steps

- Open and set up 1Password
- Open and set up Dropbox then pause syncing
    - Don't show desktop notifications
    - Optional: Copy across Dropbox and Music files from backup and `chown -R` them to new user. Resume syncing.
- Log into Chrome and set as default browser
    - Set global shortcut for Meet Mute extension to `⌘⇧9`
- Finder
    - Add ~/Dropbox/Notes and ~/dev into Finder sidebar
    - New window opens in ~/Dropbox
    - View options on home folder and "show Library folder"
- Settings
    - Users and groups > Change profile picture
    - Internet Accounts
        - Add Google for mail and messages only
        - Add Work gmail for calendar
    - Accessibility > Reduce transparency
    - Keyboard > Set caps lock as control
    - Keyboard > Shortcuts > Spotlight > Change spotlight to `ctrl-option-cmd-space`
    - Keyboard > Shortcuts > Spotlight > Switch off show finder search window
    - Keyboard > Shortcuts > Mission Control > Disable all Mission Control key shortcuts
    - Keyboard > Shortcuts > Services > OpenPGP: Decrypt Selection to `ctrl-cmd--`
    - Keyboard > Shortcuts > Services > OpenPGP: Encrypt Selection to `ctrl-cmd-=`
    - Keyboard > Shortcuts > Services > OpenPGP: Verify Signature Selection to `ctrl-cmd-shift--`
    - Keyboard > Shortcuts > Services > OpenPGP: Sign Selection to `ctrl-cmd-shift-=`
    - Trackpad > Disable Two finger click
    - Trackpad > Disable Smart zoom and Rotate
    - Trackpad > Disable swipe between pages
    - Desktop and ScreenSaver > Add ~/Dropbox/Pictures/Wallpapers folder and change every hour
    - Desktop and ScreenSaver > Arabesque screensaver
    - Security > Accessibility add apps
        - Dash
        - Fantastical 2
        - Moom
- Place Chrome, Things.app, Slack, Soulver, MacVim, and iTerm2 into the Dock
- Install Mailplane and add accounts
- iTerm
    - Preferences > Set as default terminal
    - Sync preferences ~/Dropbox/Backups/iterm
- Install SSH Keys and config from 1Password
- 1Password
    - Security > Don't lock when screensaver activated
    - Switch on third party integration under advanced settings
- Moom
-   - Set up left half, right half, and maximise custom shortcuts
    - Run as faceless app
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
    - Enable notifications
- Fantastical 2
    - Menu bar icon date and weekday
    - Defaults to last selected calendar and list
    - Change key shortcut to `shift-f12`
    - General > Start Month on current or selected week
    - Disable notifications
- Things
    - Install the Things Helper application for sharing across apps.
    - Set shortcut for quick entry to `cmd-opt-space`.
- Flux
    - Set wake up time
- Toggl
    - Set global shortcut to `⌃⌥⇧⌘t` 
    - Set idle detection to 10 minutes
    - Show timer on menu bar
    - Don't show dock icon
- DayOne
    - Setup sync
    - Baskerville 18pt, no auto bold, no twitter names
    - Disable reminders
    - Disable popover previews
    - Disable spelling corrections
- Calibre
    - Preferences > Install plugin from file > `~/Dropbox/Backups/Calibre DRM plugins/KFX Input.zip`
    - Preferences > Install plugin from file > `~/Dropbox/Backups/Calibre DRM plugins/DeDRM_tools_6.6.1/DeDRM_calibre_plugin/DeDRM_plugin.zip`
- Copy across or clone projects into `~/dev`
- Copy across from USB backup
    - ~/dev/old
    - ~/dev/scratch
- Install Lightroom Classic from Adobe CC
    - Copy across Lightroom catalog and masters from USB backup
    - Copy across Lightroom presets from backup into new location `~/Library/Application Support/Adobe/Lightroom/{Develop Preset,Export Presets,Filename Templates}`
- OpenGPG
    - Copy across any keys from 1Password
- VSCode
    - Install command line tools
    - Run `~/dev/scripts/vscode-extensions install`
