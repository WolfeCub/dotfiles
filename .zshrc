
##
## S E T T I N G S
##
# Vi mode
bindkey -v

# Enable colors
autoload -U colors && colors

# enable colored output from ls, etc. on FreeBSD-based systems
export CLICOLOR=1

# awesome cd movements from zshkit
setopt autocd autopushd pushdminus pushdsilent pushdtohome cdablevars
DIRSTACKSIZE=5

# Enable extended globbing
setopt extendedglob

# Allow [ or ] whereever you want
unsetopt nomatch

# Setup zsh path
fpath=(
    ~/.zsh/completion/
    ~/.zsh/prompt/
    $fpath)

autoload -Uz compinit
compinit

# Set emacs as default editor
export EDITOR='emacs -nw'
export VISUAL='emacs -nw'

## History
HISTFILE=$HOME/.zhistory       # enable history saving on shell exit
setopt APPEND_HISTORY          # append rather than overwrite history file.
HISTSIZE=1200                  # lines of history to maintain memory
SAVEHIST=1000                  # lines of history to maintain in history file.
setopt HIST_EXPIRE_DUPS_FIRST  # allow dups, but expire old ones when I hit HISTSIZE
setopt EXTENDED_HISTORY        # save timestamp and runtime information

# Adds case insensitivity
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
# Color completion folders
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==34=34}:${(s.:.)LS_COLORS}")';
# Kill colors
zstyle ':completion:*:*:kill:*' list-colors '=(#b) #([0-9]#)*( *[a-z])*=34=31=33'
# Option colors
zstyle ':completion:*:options' list-colors '=^(-- *)=34'
# Highlights current option
zstyle ':completion:*' menu select

# Colorify man
function man() {
    env \
	LESS_TERMCAP_mb=$(printf "\e[1;31m") \
	LESS_TERMCAP_md=$(printf "\e[1;31m") \
	LESS_TERMCAP_me=$(printf "\e[0m") \
	LESS_TERMCAP_se=$(printf "\e[0m") \
	LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
	LESS_TERMCAP_ue=$(printf "\e[0m") \
	LESS_TERMCAP_us=$(printf "\e[1;32m") \
	man "$@"
}

##
## T W E A K S
##
fancy-ctrl-z () {
    if [[ $#BUFFER -eq 0 ]]; then
        BUFFER="fg"
        zle accept-line
    else
        zle push-input
        zle clear-screen
    fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

##
## K E Y B I N D I N G S
##
bindkey '^P' up-history
bindkey '^N' down-history

# backspace and ^h working even after
# returning from command mode
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char

# ctrl-w removed word backwards
bindkey '^w' backward-kill-word
backward-kill-dir () {
    local WORDCHARS=${WORDCHARS/\/}
    zle backward-kill-word
}
zle -N backward-kill-dir
bindkey '^[^?' backward-kill-dir

# ctrl-r starts searching history backward
bindkey '^r' history-incremental-search-backward

# VI MODE KEYBINDINGS (ins mode)
bindkey -M viins '^a'    beginning-of-line
bindkey -M viins '^e'    end-of-line
bindkey -M viins '^k'    kill-line
bindkey -M viins '^w'    backward-kill-word
bindkey -M viins '^u'    backward-kill-line

##
## A L I A S E S
##
# Misc
alias :q='exit'
# List directory contents
alias lsa='ls -lah --color'
alias l='ls --color'
alias ll='ls -lh --color'
alias la='ls -A --color'
alias ls='ls --color'
# Push and pop directories on directory stack
alias md='mkdir -p'
alias rd=rmdir
alias d='dirs -v | head -10'
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'
# Emacs clients
alias ed='emacs -nw'
alias et='emacsclient -t'
alias ec='emacsclient -c'

##
## P R O M P T
##
# modify the prompt to contain git branch name if applicable
git_prompt_info() {
    current_branch=$(git current-branch 2> /dev/null)
    if [[ -n $current_branch ]]; then
        echo " %{$fg_bold[green]%}$current_branch%{$reset_color%}"
    fi
}
setopt promptsubst
PS1='${SSH_CONNECTION+"%{$fg_bold[green]%}%n@%m:"}%{$fg_bold[blue]%}%~%{$reset_color%}$(git_prompt_info) $ '

precmd() { RPROMPT="" }
function zle-line-init zle-keymap-select {
    VIM_PROMPT="%{$fg_bold[yellow]%} [% N]% %{$reset_color%}"
    RPROMPT='`jobs %% 2> /dev/null | cut -d " " -f6` [`date +%H:%M:%S`]${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/}$EPS1'
    zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

# Delay of 0.1 seconds
export KEYTIMEOUT=1

source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 
