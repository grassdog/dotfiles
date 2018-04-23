# With liberal larceny from https://github.com/caiogondim/bullet-train-oh-my-zsh-theme

autoload -U colors
colors
# Turn on command substitution in prompt
setopt prompt_subst

# GIT PROMPT
. ~/.zsh/git.zsh
ZSH_THEME_GIT_PROMPT_PREFIX=""
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

prompt_node() {
  local node_prompt="$(node --version | sed 's|v\([0-9\.]*\).*|\1|')"

  [[ -n "$node_prompt" ]] && prompt_segment "%{$fg[green]%}⬡ $node_prompt"
}

prompt_ruby() {
  local ruby_prompt="$(ruby --version | sed 's|ruby \([0-9\.]*\).*|\1|')"

  [[ -n "$ruby_prompt" ]] && prompt_segment "%{$fg[magenta]%}♦$ruby_prompt"
}

prompt_elixir() {
  local elixir_prompt="$(elixir --version | grep Elixir | sed 's|Elixir \([0-9\.]*\).*|\1|')"
  [[ -n "$elixir_prompt" ]] && prompt_segment "%{$fg[blue]%}☤$elixir_prompt"
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
  [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="⚙ "

  if [[ -n "$symbols" && $RETVAL -ne 0 ]]; then
    prompt_segment "%{$fg[red]%}$symbols"
  elif [[ -n "$symbols" ]]; then
    prompt_segment "%{$fg[green]%}$symbols"
  fi
}

prompt_time() {
  prompt_segment "%{$fg[green]%}%D{%T}"
}

prompt_aws_session() {
  if [[ -n "$AWS_SESSION_NAME" ]]; then
    prompt_segment "%{$fg[red]%}[$AWS_SESSION_NAME]"
  fi
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
  node
  elixir
  aws_session
)

prompt_segment() {
  [[ -n $1 ]] && echo -n "$1 "
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
