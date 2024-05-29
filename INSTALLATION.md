# Bootstrap a macOS box

Use the steps below to stand up a macOS box how I like it.

## Bootstrap up the environment

- Sign in and set the hostname.
    - Set hostname in system preferences.
    - Run `sudo scutil --set HostName hostname` in Terminal.
- Enable FileVault.
- Run a software update on the machine.
- Sign into iCloud.
- Sign into the App store.
- Install 1Password and sign in.
    - Security > Don't lock when screensaver activated.
- Install Dropbox and sign in.
    - Disable external drive and photo backups.
    - Notifications > Disable `New files synced` and `Edits to files`.
    - Go to finder and make the following folders available offline
        - `Backups`
        - `Documents`
        - `Library`
        - `Media/Images`.
        - `Code`.

Run the script below.

```sh
bash <(curl -s https://raw.githubusercontent.com/grassdog/dotfiles/main/bootstrap.sh)
```

You'll likely need to run it multiple times as it symlinks on Dropbox files which need time to be synced.

- Restart host to ensure all settings are in effect.
- Auth via `git-credentials-manager`.

## Services

Check the status of services via `brew services list`.

## Manual Steps

- Settings
    - Users and groups > Drag a new profile picture across
    - Internet Accounts
        - Add Work Google for email and calendar
        - Enable everything except keychain for iCloud
    - Accessibility > Display > Reduce transparency
    - Keyboard > Key repeat max and Delay to minimum
    - Keyboard > Use F1 keys as standard function keys on external keyboards
    - Keyboard > Customise control strip - drag play pause down to far right to replace Siri button
    - Keyboard > Modifier Keys > Set caps lock as control
    - Keyboard > Shortcuts > Spotlight > Change spotlight to `ctrl-space`
    - Keyboard > Shortcuts > Spotlight > Switch off show finder search window
    - Keyboard > Shortcuts > Mission Control > Disable all Mission Control key shortcuts
    - Keyboard > Shortcuts > Services > Paste Chrome link into NotePlan `⌃⌥⇧⌘b`
    - Keyboard > Shortcuts > Services > Copy current page as markdown link in Brave `⌃⌥⇧⌘c`
    - Keyboard > Shortcuts > Services > Copy current page as markdown link in Chrome `⌃⌥⇧⌘x`
    - Keyboard > Shortcuts > Mission Control > Mission Control `⌃⌥⇧⌘<backtick>`
    - Keyboard > Shortcuts > Mission Control > Move left a space `⌃⌥⇧⌘←`
    - Keyboard > Shortcuts > Mission Control > Move right a space `⌃⌥⇧⌘→`
    - Keyboard > Shortcuts > Mission Control > Show desktop  `⌃⌥⇧⌘d`
    - Trackpad > Disable Two finger click
    - Trackpad > Disable Smart zoom and Rotate
    - Trackpad > Disable swipe between pages
    - Desktop and ScreenSaver > Add ~/Dropbox/Media/Images/Wallpapers folder and change every hour
    - Desktop and ScreenSaver > Drift
    - Dock & Menu Bar > Clock > Show the day of the week > Always
- iTerm2
    - iTerm2 > Set as default terminal
    - Sync preferences ~/Dropbox/Backups/iterm
    - Preferences > Profiles > Colours - Import Dracula theme from ~/Dropbox/Backups/iterm colours
    - Preferences > Profiles > Fonts - Fira code, Retina, 12pt
    - Preferences > General > Closing > Don't confirm quit for single or multiple sessions
- Alfred
    - Install Powerpack from 1Password
    - Set sync folder to ~/Dropbox/Backups/Alfred
    - General > Set shortcut to `cmd-space`
    - Enable Clipboard history
    - Disable contacts in Alfred
    - Snippets > Automatically expand snippets by keyword
    - Appearance > Yosemite Light
    - Appearance > Hide Menu Bar Icon
- Safari
    - Under AutoFill settings disable usernames, passwords, contacts, and credit cards.
    - View > Show status bar.
    - Preferences > Advanced > Show full website address.
    - Enable extensions
    - Configure Kagi search to replace Yahoo
    - Set Yahoo as the default search engine
- Moom
    - Run as faceless app
    - Launch at login
    - Import settings: https://manytricks.com/osticket/kb/faq.php?id=53
    - `defaults export com.manytricks.Moom ~/Desktop/Moom.plist` on source machine
    - `defaults import com.manytricks.Moom ~/Desktop/Moom.plist` on target machine
- Finder
    - Add ~/dev into Finder sidebar
    - New window opens in ~/Downloads
    - View > Customize toolbar > Add Airdrop icon to toolbar.
- Place Chrome, Things.app, Slack, Soulver, MacVim, and iTerm2 into the Dock
- Karabiner Elements
    - Don't show icon in menu bar
    - Set caps lock to control in `System Preferences > Keyboard` for karabiner virtual keyboard
- WebCatalog
    - Install Gmail and Google calendar apps
    - Add work accounts
- Mimestream
    - Add personal email accounts
    - Enable notifications in system settings and disable everything except app badge
- Fantastical 3
    - Sign in with Apple
    - Enter version 2 license key
    - Add Work account
    - Add iCloud account (with app specific password)
    - Menu bar icon date and weekday
    - Defaults to last selected calendar and list
    - Default event is 30 minutes
    - Start week on Sunday
    - Start week view on Today or selected day
    - Show 13 hours at a time
    - Days per week = 5
    - Day from 7am - 7pm
    - Show multi-day events in all-day section
    - Create three calendar sets: Everything, Work, and Personal
    - Appearance > App Icon Badge shows > Nothing
    - Change key shortcut to `shift-f12`
    - Enable notifications (no application badge though)
        - Show shared calendar notifications
        - Show notifications for all day tasks at 7am
        - Disable Show invitation messages in Notification Center
        - Disable drive time notifications
- Docker for Mac
    - Preferences > Experimental features > Enable VirtioFS accelerated directory sharing
- Sign out of iMessages and FaceTime
- Mail.app
    - Disable notifications
    - Preferences > New messages Sound > None
- Calendar.app
    - Disable notifications
    - Preferences > General > Default Calendar App - Set to Fantastical.
- Things
    - Install the Things Helper application for sharing across apps
    - Set shortcut for quick entry to `cmd-opt-space`
    - Set shortcut for quick entry with autofill to `⌃⌥⇧⌘space`.
    - Enable notifications but no badges
- Bear
    - Set global shortcut to new note `⌃⌥⇧⌘z`
- NotePlan
    - Use CloudKit for syncing.
    - Set global shortcut to `⌃⌥⇧⌘j`
    - Start week on Monday
    - Don't recognise `*` as todo
    - Use `-` as default
    - Automatically update note links
- QLStephen
    - Allow in security settings.
- DayOne
    - Setup sync
    - No auto title
    - No selected formating menu
    - Disable reminders
- VSCode
    - Turn on setting sync (sign in via Github)
    - Install command line tools
- Spotify
    - Disable friend feed and notifcation of next song
- Copy across or clone projects into `~/dev`
- Copy dotfiles
    - ~/.aws/config
    - ~/.config/exercism
- Nova.app
    - Install extensions.
    - Copy over customised key bindings.
    - Install command line tool.
- Cleanshot
    - Use default system key mappings.
- NepTunes
    - Set play/pause shortcut to `⌃⌥⇧⌘p`
    - Scrobble from Spotify is unchecked


# Optional stuff

- Install Okta Verify from the App Store for work computer
- Download Elgato Control Centre and Stream Deck from https://www.elgato.com/ww/en/s/downloads
    - Install plugins
        - Audio Switcher, Audio Mute, Zoom, Spotify, Slack status, Run OSA Script, Shortcuts, Apple Music, VSCode, XCode.
    - Import profile backup into Stream Deck from `~/Dropbox/Backups/StreamDeck`.
        - Instructions at https://help.elgato.com/hc/en-us/articles/360048424432-Elgato-Stream-Deck-How-to-Back-Up-and-Restore-Profiles-
- Chrome
    - Set global shortcut for Meet Mute extension to `⌘⇧9`
- Arc browser
    - Sign in.
    - Install extensions.
    - Switch Instapaper shortcut to `ctrl-s`.
    - Disable Arc shortcut for `new little Arc window`.
- Microsoft Teams
- Google Meet Web app via Chrome
    - Browse to Google Meet and install Chrome app from address bar (or copy across from ~/Applications)
- Download and install Webster's 1913 Dictionary
    - Instructions at https://github.com/cmod/websters-1913
- Create Lifelog, Last.fm, and ChatGPT web desktop apps.
- Choosy
    - Set default browser
    - Start at login
    - Don't show in menu bar
    - Set browsers Chrome -> Arc -> Safari -> Google Meet
    - Import settings from ~/Dropbox/Backups/Choosy/behaviours.plist to ~/Library/Application Support/Choosy
- Seconds Pro
    - Set up periodic reminders while I'm in meetings.
- Install Photoshop from Adobe CC
- Install Topaz Photo AI
- Install Lightroom Classic from Adobe CC
    - Copy across Lightroom catalog and masters from USB backup
    - Manually copy across settings from previous install
    - Copy across Lightroom presets from backup into new location `~/Library/Application Support/Adobe/Lightroom/{Develop Preset,Export Presets,Filename Templates,Metadata Presets}`
    - Copy across Lightroom presets from backup into new location `~/Library/Application Support/Adobe/CameraRaw/Settings/*`
    - Restart Lightroom
    - Map Module > Import My Locations from backup
    - Pick `~/Dropbox/Photos/Lightroom/Backups` path in backup dialog when exiting Lightroom.
- Install Fuji Raw Studio
- Install SSH Keys and config from 1Password
- Dash 3
    - Set up syncing to `~/Dropbox/Backups/Dash`
- Calibre
    - Preferences > Install plugin from file > `~/Dropbox/Backups/Calibre DRM plugins/KFX Input.zip`
    - Preferences > Install plugin from file > `~/Dropbox/Backups/Calibre DRM plugins/DeDRM_tools_6.6.1/DeDRM_calibre_plugin/DeDRM_plugin.zip`
- Set up aws-vault (key and secret in 1Password)
    - `aws-vault add envato-aws-users-ray.grasso`

