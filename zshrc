##########
# History
##########

setopt HIST_IGNORE_DUPS     # Ignore adjacent duplication command history list
setopt HIST_IGNORE_SPACE    # Don't store commands with a leading space into history
setopt INC_APPEND_HISTORY   # write history on each command
# setopt SHARE_HISTORY        # share history across sessions
setopt EXTENDED_HISTORY     # add more info
export HISTFILE=~/.zsh_history
export SAVEHIST=100000
export HISTSIZE=100000

# Get Z working
source "${HOME}/.dotfiles/z/z.sh"

##########
# Prompt
##########

autoload -U colors
colors
setopt prompt_subst

function get_ruby_version() {
  ruby --version | cut -d' ' -f 1-2
}

PROMPT='%{$fg[green]%}$(date "+%a %H:%M:%S") %{$fg[cyan]%}%n@%m:%{$reset_color%}%{$fg[yellow]%}${PWD/#$HOME/~}%{$reset_color%} %{$fg[blue]%}$(vcprompt)%{$fg[green]%}$(get_ruby_version)
%{$fg[green]%}→ %{$reset_color%}'


#################
# Corrections
#################

setopt NOCORRECT    # I don't want corrections
setopt NOCORRECTALL # Only correct commands not args

# Ignore dups in the directory stack
setopt PUSHDIGNOREDUPS

# Correctly escape pasted URLs
autoload url-quote-magic
zle -N self-insert url-quote-magic

setopt CLOBBER      # Allow redirect to clobber files
export NULLCMD=:           # Allow for file truncation

export REPORTTIME=10       # Show elapsed time if command took more than X seconds
export LISTMAX=0           # Ask to complete if top of list would scroll off screen

###############
# Completions
###############

# Load completions for Ruby, Git, etc.
autoload compinit
compinit
fpath=($HOME/.zsh/Completion $fpath)

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
