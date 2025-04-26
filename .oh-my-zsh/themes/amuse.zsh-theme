
# vim:ft=zsh ts=2 sw=2 sts=2

# Must use Powerline font, for \uE0A0 to render.
ZSH_THEME_GIT_PROMPT_PREFIX=" on %{$fg[magenta]%}\uE0A0 "
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[red]%}!"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[green]%}?"
ZSH_THEME_GIT_PROMPT_CLEAN=""

ZSH_THEME_RUBY_PROMPT_PREFIX="%{$fg[red]%}‹"
ZSH_THEME_RUBY_PROMPT_SUFFIX="›%{$reset_color%}"

VIRTUAL_ENV_DISABLE_PROMPT=1  # Disable default virtualenv prompt

# Function to display the virtual environment name
function virtualenv_prompt_info() {
  if [[ -n "$VIRTUAL_ENV" ]]; then
    echo "($(basename "$VIRTUAL_ENV"))"
  fi
}

PROMPT='
 %{$fg_bold[cyan]%}%~%{$reset_color%}$(git_prompt_info)$(virtualenv_prompt_info) %{$fg[magenta]%}
 ❯%{$reset_color%} '
# ⌚ %{$fg_bold[red]%}%*%
RPROMPT='$(ruby_prompt_info)'

# Disable default virtualenv prompt and define custom prompt
VIRTUAL_ENV_DISABLE_PROMPT=0
ZSH_THEME_VIRTUAL_ENV_PROMPT_PREFIX=" %{$fg[green]%}🐍 "
ZSH_THEME_VIRTUAL_ENV_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_VIRTUALENV_PREFIX=$ZSH_THEME_VIRTUAL_ENV_PROMPT_PREFIX
ZSH_THEME_VIRTUALENV_SUFFIX=$ZSH_THEME_VIRTUAL_ENV_PROMPT_SUFFIX
