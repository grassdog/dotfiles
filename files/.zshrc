##########
# History
##########

setopt HIST_IGNORE_DUPS     # Ignore adjacent duplication command history list
setopt HIST_IGNORE_SPACE    # Don't store commands with a leading space into history
setopt INC_APPEND_HISTORY   # write history on each command
setopt EXTENDED_HISTORY     # add more info
export HISTFILE=~/.zsh_history
export SAVEHIST=100000
export HISTSIZE=100000

# Get Z going
source `brew --prefix`/etc/profile.d/z.sh

#########################
# Prompt and completions
#########################

if [ -d /opt/homebrew/share/zsh/site-functions ]; then
  fpath=( /opt/homebrew/share/zsh/site-functions $fpath )
fi

autoload compinit
compinit

[[ -r /opt/homebrew/opt/spaceship/spaceship.zsh ]] && source "/opt/homebrew/opt/spaceship/spaceship.zsh"

autoload -U promptinit; promptinit

SPACESHIP_PROMPT_ORDER=(
  time          # Time stamps section
  user          # Username section
  dir           # Current directory section
  host          # Hostname section
  git           # Git section (git_branch + git_status)
  exec_time     # Execution time
  line_sep      # Line break
  battery       # Battery level and status
  jobs          # Background jobs indicator
  exit_code     # Exit code section
  char          # Prompt character
)


SPACESHIP_TIME_SHOW=true
SPACESHIP_EXIT_CODE_SHOW=true


##########
# Plugins
##########

[[ -r /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh ]] && source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh

###############
# Corrections
###############

setopt NOCORRECT    # I don't want corrections
setopt NOCORRECTALL # Only correct commands not args

# Ignore dups in the directory stack
setopt PUSHDIGNOREDUPS

# Correctly escape pasted URLs
autoload url-quote-magic
zle -N self-insert url-quote-magic

setopt CLOBBER             # Allow redirect to clobber files
export NULLCMD=:           # Allow for file truncation

export REPORTTIME=10       # Show elapsed time if command took more than X seconds
export LISTMAX=0           # Ask to complete if top of list would scroll off screen


# Make CTRL-W delete after other chars, not just spaces
export WORDCHARS='*?[]~=&;!#$%^(){}'

# Emacs key bindings
bindkey -e

bindkey "\e[1~" beginning-of-line
bindkey "\e[2~" quoted-insert
bindkey "\e[3~" delete-char
bindkey "\e[4~" end-of-line
bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history
bindkey "\e[7~" beginning-of-line
bindkey "\e[8~" end-of-line
bindkey "\e[H" beginning-of-line
bindkey "\e[F" end-of-line
bindkey "\eOH" beginning-of-line
bindkey "\eOF" end-of-line
bindkey "\eOd" backward-word
bindkey "\eOc" forward-word

# Get my aliases
source ~/.zsh/aliases.zsh

# Enable history in iex
export ERL_AFLAGS="-kernel shell_history enabled"

export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

if type direnv >/dev/null 2>&1; then
  eval "$(direnv hook zsh)"
fi

# tmuxinator is setting this for some reason so unset it
unset GEM_HOME

# Set up asdf
export ASDF_DATA_DIR="$HOME/.asdf"
export PATH="$ASDF_DATA_DIR/shims:$PATH"

# Add bun to path
export PATH="$HOME/.bun/bin:$PATH"

# Add local bin as well
export PATH="$HOME/.local/bin:$PATH"

# Integrate fzf into my shell
source <(fzf --zsh)

# Remove the need for bundle exec ... or ./bin/...
# by adding ./bin to path if the current project is trusted

function set_local_bin_path() {
  # Replace any existing local bin paths with our new one
  export PATH="${1:-""}`echo "$PATH"|sed -e 's,[^:]*\.git/[^:]*bin:,,g'`"
}

function add_trusted_local_bin_to_path() {
  if [[ -d "$PWD/.git/safe" ]]; then
    # We're in a trusted project directory so update our local bin path
    set_local_bin_path "$PWD/.git/safe/../../bin:"
  fi
}

# Make sure add_trusted_local_bin_to_path runs after chruby so we
# prepend the default chruby gem paths
if [[ -n "$ZSH_VERSION" ]]; then
  if [[ ! "$preexec_functions" == *add_trusted_local_bin_to_path* ]]; then
    preexec_functions+=("add_trusted_local_bin_to_path")
  fi
fi

[[ -r "$HOME/.secrets" ]] && source "$HOME/.secrets"

fpath=($fpath "/Users/ray/.zfunctions")
