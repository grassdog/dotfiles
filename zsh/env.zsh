##########
# Editors
##########

export EDITOR='vim'
export PAGER='less'
export SVN_EDITOR='vim'
export BUNDLER_EDITOR="vim"

if [[ $(uname) == Darwin ]]; then
  export BUNDLER_EDITOR="mvim"
fi

##########
# Paths
##########

export PATH="${HOME}/bin:${HOME}/.bin:/usr/local/bin:/usr/local/sbin:/usr/local/share/npm/bin:${PATH}"

# Add Java to my environment
export PATH="${PATH}:/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home/bin"
export JAVA_HOME="/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home"

# Adding Homebrew man path
export MANPATH="/usr/local/share/man:${MANPATH}"

##########
# History
##########

setopt HIST_IGNORE_DUPS     # Ignore adjacent duplication command history list
setopt HIST_IGNORE_SPACE    # Don't store commands with a leading space into history
setopt INC_APPEND_HISTORY   # write history on each command
setopt SHARE_HISTORY        # share history across sessions
setopt EXTENDED_HISTORY     # add more info
export HISTFILE=~/.zsh_history
export SAVEHIST=1000000
export HISTSIZE=1000000

##########
# Prompt
##########

autoload -U colors
colors
setopt prompt_subst

PROMPT='%{$fg[green]%}$(date "+%a %H:%M:%S") %{$fg[cyan]%}%n@%m:%{$reset_color%}%{$fg[yellow]%}${PWD/#$HOME/~}%{$reset_color%} %{$fg[blue]%}$(vcprompt)%{$fg[green]%}$(rvm-prompt i v g)
%{$fg[green]%}→ %{$reset_color%}'


#################
# Corrections
#################

setopt NOCORRECT    # I don't want corrections
setopt NOCORRECTALL # Only correct commands not args

setopt AUTOPUSHD PUSHDMINUS PUSHDTOHOME AUTOCD PUSHDIGNOREDUPS

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


# Add RVM to PATH for scripting
PATH=$PATH:$HOME/.rvm/bin

# Add rvm to my environment
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm

