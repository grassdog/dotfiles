#!/usr/bin/env bash
#/ Usage: install-mas-apps.sh [OPTIONS]
#/ Install Mac App Store apps.
#/
#/ -d, --debug  run in debug mode
#/ -h, --help   show help

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

if command -v mas >/dev/null 2>&1; then
  echo "Install mas via 'brew install mas'"
  exit 1
fi

echo "Install apps from the App Store"
mas install 1569813296 # 1Password for Safari
mas install 937984704  # Amphetamine
mas install 1091189122 # Bear
mas install 1451400394 # bookmarker for pinboard
mas install 1465439395 # Dark Noise
mas install 1055511498 # Day one
mas install 6763755310 # Indigo for Bluesky & Mastodon
mas install 288545208  # Instapaper
mas install 1548677272 # Save to Matter
mas install 1614730313 # MusicBox: Save Music for Later
mas install 1289197285 # MindNode 6
mas install 1006739057 # NepTunes
mas install 1869226229 # Obsidian Web Clipper
mas install 1596506190 # Play: Save Videos Watch Later
mas install 1851660095 # Raycast Companion
mas install 1640236961 # Save to Reader
mas install 1376402589 # StopTheMadness
mas install 2112214276 # Supercopy for Safari
mas install 961632517  # Be Focused Pro: Pomodoro Timer
mas install 1475387142 # Tailscale
mas install 904280696  # Things
mas install 1662217862 # Wipr 2
mas install 1363637349 # Unread: An RSS Reader
