# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:$HOME/.gem/ruby/2.6.0/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/$USER/.oh-my-zsh"

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder
 
# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(copyfile command-not-found extract git ubuntu gpg-agent colorize archlinux sudo encode64 emacs)

source $ZSH/oh-my-zsh.sh

# User configuration

export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='vim'
else
    export EDITOR="emacsclient -t"                  # $EDITOR opens in terminal
    export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI mode
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/private"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
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

if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
        source /etc/profile.d/vte.sh
fi

alias grep="grep --color=auto"
alias fgrep="fgrep --color=auto"
alias egrep="egrep --color=auto"
alias less="less -R"

# aliases for refreshing shell environment
alias sourcez="source ~/.zshrc"
alias refreshenv="source ~/.zshrc"

alias l="ls -CF"
alias ll="ls -lh"
alias la="ls -A"
alias sl="ls"
if [ -f /etc/os-release ] ; then
    . /etc/os-release
    OS=$NAME
fi
alias killemacs="emacsclient -e '(save-buffers-kill-emacs)'"
alias xclip="xclip -selection clip-board"
alias delpyc="find . -name \"*.pyc\" -delete"
case $OS in
    Arch)
        alias ls='ls --color=auto'
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

#Prevents wine from adding menu entries and desktop files
#export WINEDLLOVERRIDES='winemenubuilder.exe=d'
# GENTOO SETUP
# Isomount
alias mountiso="udisksctl loop-setup -r -f"
# Open-Configs
alias zshconfig="$EDITOR   ~/.zshrc"
alias zshconfigp="$EDITOR ~/.zsh/prompt.zsh"
alias zshconfiga="$EDITOR ~/.zsh/aliases.zsh"
alias zshconfigb="$EDITOR ~/.zsh/bindkeys.zsh"
alias zshconfigc="$EDITOR ~/.zsh/completion.zsh"
alias zshconfigcol="$EDITOR ~/.zsh/colors.zsh"
alias zshconfigf="$EDITOR ~/.zsh/functions.zsh"
alias zshconfigh="$EDITOR ~/.zsh/history.zsh"
alias zshconfigs="$EDITOR ~/.zsh/setopt.zsh"
alias zshconfige="$EDITOR ~/.zsh/exports.zsh"
alias zshconfighooks="$EDITOR ~/.zsh/zsh_hooks.zsh"
alias i3config="$EDITOR ~/.config/i3/config"
alias i3status="$EDITOR ~/.config/i3/pystatus.py"
alias svn_precommit="svn status | grep \"^!\" | awk \"\{print \$2\}\" |xargs svn delete"
# FIX TRAMP ERRORS
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '
