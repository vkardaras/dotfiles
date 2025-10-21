# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=11000
SAVEHIST=10000
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/vasilis/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# enable starship prompt
eval "$(starship init zsh)"

export XDG_CONFIG_HOME="$HOME/.config/"
export PATH="$HOME/doom_emacs/bin:$PATH"
# load optionrc if it exists
[ -f "${XDG_CONFIG_HOME}/zsh/optionrc" ] && source "${XDG_CONFIG_HOME}/zsh/optionrc"

# plugins
source "${XDG_CONFIG_HOME}/zsh/plugins/f-sy-h/F-Sy-H.plugin.zsh" 
source "${XDG_CONFIG_HOME}/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh" 
source "${XDG_CONFIG_HOME}/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh" 
source "${XDG_CONFIG_HOME}/zsh/plugins/.zsh-vi-mode/zsh-vi-mode.plugin.zsh" 

zmodload zsh/terminfo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
bindkey '^[[A' history-substring-search-up
bindkey '^[OA' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey '^[OB' history-substring-search-down
bindkey -M vicmd '^[[A' history-substring-search-up 
bindkey -M vicmd '^[OA' history-substring-search-up 
bindkey -M vicmd '^[[B' history-substring-search-down
bindkey -M vicmd '^[OB' history-substring-search-down
bindkey -M viins '^[[A' history-substring-search-up 
bindkey -M viins '^[OA' history-substring-search-up 
bindkey -M viins '^[[B' history-substring-search-down 
bindkey -M viins '^[OB' history-substring-search-down

# colours
autoload -U colors && colors	      # colours
autoload -U compinit && compinit    # basic completion
autoload -U compinit colors zcalc   # theming

# tab completion
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' # Case insensitive tab completion
zstyle ':completion:*' list-colors "${(s.:.)--color=auto}"                        # Colored completion (different colors for dirs/files/etc)
zstyle ':completion:*' rehash true                                                # automatically find new executables in path
zstyle ':completion:*' menu select                                                # Highlight menu selection

# Color man pages
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'
export LESS=-R

# Set up fzf key bindings and fuzzy completion
source <(fzf --zsh)
