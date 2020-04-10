source ~/.config/zsh/colors.zsh
source ~/.config/zsh/setopt.zsh
source ~/.config/zsh/exports.zsh
source ~/.config/zsh/prompt.zsh
source ~/.config/zsh/completion.zsh
source ~/.config/zsh/aliases.zsh
source ~/.config/zsh/bindkeys.zsh
source ~/.config/zsh/functions.zsh
source ~/.config/zsh/history.zsh
source ~/.config/zsh/zsh_hooks.zsh
if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
        source /etc/profile.d/vte.sh
fi

# FIX TRAMP ERRORS
# [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '

source /home/moksha/.config/broot/launcher/bash/br
