function fish_prompt -d "Write out the prompt"
    # This shows up as USER@HOST /home/user/ >, with the directory colored
    # $USER and $hostname are set by fish, so you can just use them
    # instead of using `whoami` and `hostname`
    printf '%s@%s %s%s%s > ' $USER $hostname \
        (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
end

if status is-interactive # Commands to run in interactive sessions can go here

    # No greeting
    set fish_greeting

    # Use starship
    starship init fish | source
    if test -f ~/.local/state/quickshell/user/generated/terminal/sequences.txt
        cat ~/.local/state/quickshell/user/generated/terminal/sequences.txt
    end

    # Aliases
    alias pamcan pacman
    alias ls 'eza -la --icons'
    alias clear "printf '\033[2J\033[3J\033[1;1H'"
    alias q 'qs -c ii'
    alias fzf 'fzf --preview="bat --color=always {}"'
    alias nfzf 'nvim (fzf --preview="bat --color=always {}")'
    alias y yazi

    #source
    set -Ux PATH $PATH $HOME/.emacs.d/bin
end

if string match -q '*pts*' (tty)
    fastfetch --config examples/13
else
    echo ""
    if test -f /bin/hyprctl
        echo "Start Hyprland with command Hyprland"
    end
end
