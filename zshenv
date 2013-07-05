##########
# Editors
##########

export EDITOR='vim'
export PAGER='less'
export SVN_EDITOR=$EDITOR
export BUNDLER_EDITOR=$EDITOR

if [[ $(uname) == Darwin ]]; then
  export BUNDLER_EDITOR="mvim"
fi

##########
# Paths
##########

export PATH="${HOME}/.bin:/usr/local/bin:/usr/local/sbin:/usr/local/share/npm/bin:/usr/local/heroku/bin:${PATH}"

# Add Java to my environment
export PATH="${PATH}:/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home/bin"
export JAVA_HOME="/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home"

# Adding Homebrew man path
export MANPATH="/usr/local/share/man:${MANPATH}"

export NODEPATH="/usr/local/lib/node_modules"

export SUBLHOME="${HOME}/Library/Application Support/Sublime Text 2/Packages/User"

export ANDROID_HOME="/usr/local/opt/android-sdk"

