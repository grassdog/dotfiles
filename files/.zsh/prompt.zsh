# With liberal larceny from https://github.com/caiogondim/bullet-train-oh-my-zsh-theme

autoload -U colors
colors
# Turn on command substitution in prompt
setopt prompt_subst

# GIT PROMPT
. ~/.zsh/git.zsh
ZSH_THEME_GIT_PROMPT_PREFIX="ᛦ "
ZSH_THEME_GIT_PROMPT_SUFFIX=""
ZSH_THEME_GIT_PROMPT_CLEAN=" ✔"
ZSH_THEME_GIT_PROMPT_DIRTY=" Δ"
ZSH_THEME_GIT_PROMPT_ADDED="+"
ZSH_THEME_GIT_PROMPT_MODIFIED="✎"
ZSH_THEME_GIT_PROMPT_DELETED="-"
ZSH_THEME_GIT_PROMPT_UNTRACKED="∿"
ZSH_THEME_GIT_PROMPT_RENAMED="➜"
ZSH_THEME_GIT_PROMPT_UNMERGED="═"
ZSH_THEME_GIT_PROMPT_AHEAD="⬆"
ZSH_THEME_GIT_PROMPT_BEHIND="⬇"
ZSH_THEME_GIT_PROMPT_DIVERGED="⬍"

#
# PROMPT COMPONENTS
#

prompt_git() {
  local ref dirty mode repo_path git_prompt
  repo_path=$(git rev-parse --git-dir 2>/dev/null)

  if $(git rev-parse --is-inside-work-tree >/dev/null 2>&1); then
    if [[ -n $(git status --porcelain --ignore-submodules) ]]; then
      git_prompt="%{$fg[blue]%}"
    else
      git_prompt="%{$fg[white]%}"
    fi

    # prompt_segment "${git_prompt}$(git_prompt_info)$(git_prompt_status)"
    prompt_segment "${git_prompt}[$(git_prompt_info)]"
  fi
}

prompt_nvm() {
  local nvm_prompt
  if type nvm >/dev/null 2>&1; then
    nvm_prompt=$(nvm current 2>/dev/null)
    [[ "${nvm_prompt}x" == "x" ]] && return
  else
    nvm_prompt="$(node --version)"
  fi
  nvm_prompt=${nvm_prompt}
  prompt_segment "%{$fg[green]%}⬡ $nvm_prompt"
}

# RUBY
# RVM: only shows RUBY info if on a gemset that is not the default one
# RBENV: shows current ruby version active in the shell; also with non-global gemsets if any is active
# CHRUBY: shows current ruby version active in the shell
prompt_ruby() {
  local ruby_prompt=''
  if command -v rvm-prompt > /dev/null 2>&1; then
    ruby_prompt="$(rvm-prompt i v g)"
  elif command -v chruby > /dev/null 2>&1; then
    ruby_prompt="$(chruby | sed -n -e 's/ \* //p')"
  elif command -v rbenv > /dev/null 2>&1; then
    current_gemset() {
      echo "$(rbenv gemset active 2&>/dev/null | sed -e 's/ global$//')"
    }

    if [[ -n $(current_gemset) ]]; then
      ruby_prompt="$(rbenv version | sed -e 's/ (set.*$//')"@"$(current_gemset)"
    else
      ruby_prompt="$(rbenv version | sed -e 's/ (set.*$//')"
    fi
  fi

  prompt_segment "%{$fg[magenta]%}♦ $ruby_prompt"
}


# Status:
# - was there an error
# - am I root
# - are there background jobs?
prompt_status() {
  local symbols
  symbols=()
  [[ $RETVAL -ne 0 ]] && symbols+="✘$RETVAL"
  [[ $UID -eq 0 ]] && symbols+="%{$fg{yellow}%}⚡%f"
  [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="⚙"

  if [[ -n "$symbols" && $RETVAL -ne 0 ]]; then
    prompt_segment "%{$fg[red]%}$symbols"
  elif [[ -n "$symbols" ]]; then
    prompt_segment "%{$fg[green]%}$symbols"
  fi
}

prompt_time() {
  prompt_segment "%D{%T}"
}

prompt_context() {
  local dir=''
  # Long directories
  dir="${dir}%0~"

  # Medium directories
  # dir="${dir}%4(c:...:)%3c"

  prompt_segment "%{$fg[cyan]%}%n@%m:%{$fg[yellow]%}$dir"
}

#
# Putting it all together
#

PROMPT_ORDER=(
  time
  status
  context
  git
  ruby
  nvm
)

prompt_segment() {
  [[ -n $1 ]] && echo -n " $1"
}

build_prompt() {
  RETVAL=$?

  for segment in $PROMPT_ORDER
  do
    prompt_$segment
  done
}

PROMPT='$(build_prompt)
%{$fg[green]%}$ %{$reset_color%}'
