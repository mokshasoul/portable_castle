if command -V dircolors >/dev/null 2>&1; then
    eval "$(dircolors -b)"
    # Only alias ls if dircolors is installed
    alias ls="ls -F --color=auto"
    alias dir="dir --color=auto"
    alias vdir="vidr --color=auto"
fi

alias grep="grep --color=auto"
alias fgrep="fgrep --color=auto"
alias egrep="egrep --color=auto"
alias less="less -R"

# aliases for refreshing shell environment
alias sourcez="source ~/.zshrc"
alias refreshenv="source ~/.zshrc"

# ls
alias l="ls -CF"
alias ll="ls -lh"
alias la="ls -A"
alias sl="ls"

# Make unified diff syntax the default
alias diff="diff -u"

# json prettify
alias json="python -m json.tool"

# octal-text permissions for files
alias perms="stat -c '%A %a %n'"

# expand sudo aliases ?? <-- investigate ;)
alias sudo="sudo "

# Make df and du always human readable ;)
alias df="df -h"
alias du="du -h"

# Isomount
alias mountiso="udisksctl loop-setup -r -f"
# Open-Configs
alias zshconfig="${VISUAL} ~/.zshrc"
alias zshconfigp="${VISUAL} ~/.zsh/prompt.zsh"
alias zshconfiga="${VISUAL} ~/.zsh/aliases.zsh"
alias zshconfigb="${VISUAL} ~/.zsh/bindkeys.zsh"
alias zshconfigc="${VISUAL} ~/.zsh/completion.zsh"
alias zshconfigcol="${VISUAL} ~/.zsh/colors.zsh"
alias zshconfigf="${VISUAL} ~/.zsh/functions.zsh"
alias zshconfigh="${VISUAL} ~/.zsh/history.zsh"
alias zshconfigs="${VISUAL} ~/.zsh/setopt.zsh"
alias zshconfige="${VISUAL} ~/.zsh/exports.zsh"
alias zshconfighooks="${VISUAL} ~/.zsh/zsh_hooks.zsh"
alias i3config="${VISUAL} ~/.config/i3/config"
alias i3status="${VISUAL} ~/.config/i3/pystatus.py"
alias masterdir="cd ~/Dropbox/University\ Stuff/TUM/Master_Thesis"
alias svn_precommit="svn status | grep \"^!\" | awk \"\{print \$2\}\" |xargs svn delete"

# alias msfconsole="msfconsole --quiet -x \"db_connect ${USER}@msf\""

#####
# openSuse Specifics - Zypper <>
#####
alias zse="noglob sudo zypper se" # Search 
alias zsei="noglob sudo zypper se -s i" # Search installed packages
alias zin="noglob sudo zypper in" # Install
alias zup="sudo zypper up" # Normal uprade
alias zdup="sudo zypper dup --no-allow-vendor-change" # Dist-Upgrade
alias zrmv="noglob sudo zypper rm" # Remove
alias zps="sudo zypper ps -s"
autoload -Uz age

# Directory shorts
# Does not work, throws bad assignment
# alias -g ... = '../..'
# alias -g .... = '../../..'
# alias -g ..... = '../../../..'
# .extensions opens with program
alias -s .tex='${VISUAL}'
alias -s .zsh='${VISUAL}'
alias -s .html='firefox'
alias -s .pdf='zathura'

if command -V dircolors >/dev/null 2>&1; then
    eval "$(dircolors -b)"
    # Only alias ls if dircolors is installed
    if [ -f /opt/coreutils/bin/ls ]; then
        alias ls="/opt/coreutils/bin/ls --color=auto";

    else
        alias ls="ls -F --color=auto"

    fi
    alias dir="dir --color=auto"
    alias vdir="vidr --color=auto"
fi

if [ -f /etc/os-release ] ; then
    . /etc/os-release
    OS=$NAME
fi
alias killemacs="emacsclient -e '(save-buffers-kill-emacs)'"
alias xclip="xclip -selection clip-board"
alias delpyc="find . -name \"*.pyc\" -delete"
case $OS in
    "Arch Linux")
	source ~/.config/zsh/arch.zsh
        ;;
    Fedora)
        alias dnfi="sudo dnf install"
        alias dnfu="sudo dnf update"
        ;;
    "openSUSE Tumbleweed")
        alias zdup="sudo zypper dup --allow-vendor-change"
        alias zin="sudo zypper install"
        alias zref="sudo zypper refresh"
        alias zse="noglob sudo zypper se" # Search
        alias zsei="noglob sudo zypper se -s i" # Search installed packages
        alias zin="noglob sudo zypper in" # Install
        alias zup="sudo zypper up" # Normal uprade
        alias zdup="sudo zypper dup --no-allow-vendor-change" # Dist-Upgrade
        alias zrmv="noglob sudo zypper rm" # Remove
        alias zps="sudo zypper ps -s"
        export SSH_ASKPASS="/usr/lib/ssh/ksshaskpass"
        ;;
    "Gentoo")
        # emerge Alias Gentoo
        alias eav="noglob sudo emerge -av"
        alias euf="sudo emerge -avDU @world"
        alias eufn="sudo emerge -aDN @world"
        alias eufvn="sudo emerge avDN @world"
        alias ufed="sudo ufed"
        alias checkconf="sudo find /etc -name '._cfg????_*'"
        ;;
esac
