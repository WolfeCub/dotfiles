if status --is-interactive
    set BASE16_SHELL "$HOME/.config/base16-shell/"
    source "$BASE16_SHELL/profile_helper.fish"
end

set -g fish_key_bindings fish_hybrid_key_bindings # Vim bindings with regular bindigs in normal mode
set fish_cursor_insert block # Block cursor for insert mode
set fish_greeting # No greeting
set fish_prompt_pwd_dir_length 0 # Show full path

# Syntax colors
set fish_color_command green
set fish_color_param normal

function fish_prompt
    echo
    echo (string join '' \
        (set_color red) $USER (set_color white) @ (set_color green) $hostname ' ' \
        (set_color blue) (prompt_pwd) (set_color normal))
    echo '$ '
end

function fish_right_prompt
    tput sc; tput cuu1; tput cuf 1
    jobs %% 2> /dev/null | cut -d " " -f6
    echo " [$(date '+%H:%M:%S')]"
    tput rc
end

function preexec_whitespace --on-event fish_preexec
   echo
end

# No mode prompt
function fish_mode_prompt
end

function in_path
    command -v "$argv[1]" &> /dev/null
end

# Aliases
alias groot='cd $(git rev-parse --show-toplevel)'
in_path "nvim" && alias vim=nvim
in_path "bat" && alias cat=bat
in_path "eza" && alias ls=eza

if in_path kubecolor
    alias k=kubecolor
else
    alias k=kubectl
end

# Sources
in_path fzf && fzf --fish | source -
in_path zoxide && zoxide init fish | source

