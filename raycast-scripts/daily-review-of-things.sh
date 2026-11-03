#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Daily review of Things
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 🚀

# Documentation: Trigger Keyboard Maestro macro to review Things tasks.
# @raycast.author grassdog
# @raycast.authorURL https://raycast.com/grassdog

osascript -e 'tell application "Keyboard Maestro Engine" to do script "45818D4B-8A90-41B0-A687-AD0F436DE299"'
