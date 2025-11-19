ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
[ ! -d $ZINIT_HOME ] && mkdir -p "$(dirname $ZINIT_HOME)"
[ ! -d $ZINIT_HOME/.git ] && git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
source "${ZINIT_HOME}/zinit.zsh"

zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
# Set-up icons for files/folders in terminal
alias ls='eza -a --icons'
alias ll='eza -al --icons'
alias lt='eza -a --tree --level=1 --icons'
alias y='yazi'

# Set-up FZF key bindings (CTRL R for fuzzy history finder)
source <(fzf --zsh)

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

# doom
export PATH="$HOME/.emacs.d/bin:$PATH"

export VIMRUNTIME=/usr/share/nvim/runtime
eval "$(starship init zsh)"
if [[ $(tty) == *pts* ]]; then
    fastfetch --config examples/13
else
    echo ""
    if [[ -f /bin/hyprctl ]]; then
        echo "Start Hyprland with command Hyprland"
    fi
fi

# Zoxide
eval "$(zoxide init --cmd cd zsh)"
