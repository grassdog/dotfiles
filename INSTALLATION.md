# Bootstrap a macOS box

Use the steps below to stand up a macOS box how I like it.

## Bootstrap up the environment

- Sign in and set the hostname.
- Enable FileVault.
- Run a software update on the machine.
- Sign into iCloud.
- Sign into the App store

Run the script below.

```sh
bash <(curl -s https://raw.githubusercontent.com/grassdog/dotfiles/master/bootstrap.sh)
```

You'll likely need to run it multiple times as it symlinks on Dropbox files which need time to be synced.


- Open and sign into 1Password
    - Security > Don't lock when screensaver activated
- Open and sign into Dropbox
    - Notifications > Disable `New files synced` and `Edits to files`
- Restart host to ensure all settings are in effect.
- Generate [personal access token on Github](https://github.com/settings/tokens) and input it when pushing up changes from the CLI.

## Services

Check the status of services via `brew services list`.

## Manual Steps

- Settings
    - Users and groups > Drag a new profile picture across
    - Internet Accounts
        - Add Work Google for email and calendar
        - Enable everything except keychain for icloud
    - Accessibility > Display > Reduce transparency
    - Keyboard > Key repeat max and Delay to minimum
    - Keyboard > Modifier Keys > Set caps lock as control
    - Keyboard > Shortcuts > Spotlight > Change spotlight to `ctrl-option-cmd-space`
    - Keyboard > Shortcuts > Spotlight > Switch off show finder search window
    - Keyboard > Shortcuts > Mission Control > Disable all Mission Control key shortcuts
    - Keyboard > Shortcuts > Services > Paste Chrome link into NotePlan `⌃⌥⇧⌘b`
    - Keyboard > Shortcuts > Services > Copy current page as markdown link `⌃⌥⇧⌘c`
    - Trackpad > Disable Two finger click
    - Trackpad > Disable Smart zoom and Rotate
    - Trackpad > Disable swipe between pages
    - Desktop and ScreenSaver > Add ~/Dropbox/Pictures/Wallpapers folder and change every hour
    - Desktop and ScreenSaver > Drift
    - Security > Accessibility add apps
        - Fantastical 2
        - Moom
- iTerm2
    - iTerm2 > Set as default terminal
    - Sync preferences ~/Dropbox/Backups/iterm
    - Preferences > Profiles > Colours - Import Dracula theme from ~/Dropbox/Backups/iterm colours
    - Preferences > Profiles > Fonts - Fira code, Retina, 12pt 
- Alfred
    - Install Powerpack from 1Password
    - Set sync folder to ~/Dropbox/Backups/Alfred
    - General > Set shortcut to `cmd-space`
    - Enable 1Password
    - Enable Clipboard history
    - Disable contacts in Alfred
    - Appearance > Yosemite Light
    - Appearance > Hide Menu Bar Icon
- Choosy
    - Set default browser
    - Start at login
    - Don't show in menu bar
    - Set browsers Firefox -> Chrome -> Safari
    - Import settings from ~/Dropbox/Backups/Choosy/behaviours.plist to ~/Library/Application Support/Choosy
- Moom
    - Run as faceless app
    - Launch at login
    - Import settings: https://manytricks.com/osticket/kb/faq.php?id=53
    - `defaults export com.manytricks.Moom ~/Desktop/Moom.plist` on source machine
    - `defaults import com.manytricks.Moom ~/Desktop/Moom.plist` on target machine
- Firefox
    - Customise shortcut bar and remove pocket and Firefox account.
    - Set LeechBlock config.
- Chrome
    - Set global shortcut for Meet Mute extension to `⌘⇧9`
- Finder
    - Add ~/dev into Finder sidebar
    - New window opens in ~/Downloads
- Place Chrome, Things.app, Slack, Soulver, MacVim, and iTerm2 into the Dock
- Mailplane
    - Add work and personal accounts
    - Make App dock only
    - Disable notifications
- Fantastical 3
    - Sign in with Apple
    - Enter version 2 license key
    - Add Work account
    - Add icloud account (with app specific password)
    - Menu bar icon date and weekday
    - Defaults to last selected calendar and list
    - Change key shortcut to `shift-f12`
    - Enable notifications (no application badge though)
        - Show shared calendar notifications
        - Show notifications for all day tasks at 7am
        - Disable other notifications
        - Disable drive time notifications
- Mail.app
    - Disable notifications
- Calendar.app
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
- Bear
    - Set global shortcut to new note `⌃⌥⇧⌘a` 
- Install SSH Keys and config from 1Password
- NotePlan
    - Use CloudKit for syncing.
    - Set global shortcut to `⌃⌥⇧⌘j` 
    - Start week on Monday
    - Don't recognise `*` as todo
    - Use `-` as default
    - Automatically update note links
- Karabiner Elements
    - Don't show icon in menu bar
    - Set caps lock to control in karabiner virtual keyboard
- DayOne
    - Setup sync
    - No auto title
    - No selected formating menu
    - Disable reminders
- VSCode
    - Turn on setting sync (sign in via Github)
    - Install command line tools
- Google Meet Web app via Chrome
    - Browse to Google Meet and install Chrome app from address bar
- Copy across or clone projects into `~/dev`
- Install Lightroom Classic from Adobe CC
    - Copy across Lightroom catalog and masters from USB backup
    - Copy across Lightroom presets from backup into new location `~/Library/Application Support/Adobe/Lightroom/{Develop Preset,Export Presets,Filename Templates}`

# Optional stuff

- Dash 3
    - Set up syncing to `~/Dropbox/Backups/Dash`
- Calibre
    - Preferences > Install plugin from file > `~/Dropbox/Backups/Calibre DRM plugins/KFX Input.zip`
    - Preferences > Install plugin from file > `~/Dropbox/Backups/Calibre DRM plugins/DeDRM_tools_6.6.1/DeDRM_calibre_plugin/DeDRM_plugin.zip`

