# !/bin/bash  -*- mode: sh; -*-
#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi
# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000
# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    if [ -f /opt/coreutils/bin/ls ]; then
	    alias ls='/opt/coreutils/bin/ls --color=auto'
    else
	    alias ls='ls --color=auto'
    fi
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# virtualenvwrapper
if [ -f /usr/bin/virtualenvwrapper ]; then
    source /usr/bin/virtualenvwrapper.sh
fi
export WORKON_HOME=~/python_envs/


# Set GPG TTY
export GPG_TTY=$(tty)

# Refresh gpg-agent tty in case user switches into an X session
gpg-connect-agent updatestartuptty /bye >/dev/null

# Path to the bash it configuration
# export BASH_IT="/home/$USER/.bash_it"

# Lock and Load a custom theme file
# location /.bash_it/themes/
# export BASH_IT_THEME='zork'

# SSH-AGENT
export SSH_ASKPASS="/usr/bin/ksshaskpass"

# (Advanced): Change this to the name of your remote repo if you
# cloned bash-it with a remote other than origin such as `bash-it`.
# export BASH_IT_REMOTE='bash-it'

# Your place for hosting Git repos. I use this for private repos.
# export GIT_HOSTING='git@git.domain.com'

# Don't check mail when opening terminal.
unset MAILCHECK

# Change this to your console based IRC client of choice.
export IRC_CLIENT='weechat'

# Set this to the command you use for todo.txt-cli
export TODO="t"

# Set this to false to turn off version control status checking within the prompt for all themes
export SCM_CHECK=false

# Set Xterm/screen/Tmux title with only a short hostname.
# Uncomment this (or set SHORT_HOSTNAME to something else),
# Will otherwise fall back on $HOSTNAME.
export SHORT_HOSTNAME=$(hostname -s)

# Set Xterm/screen/Tmux title with only a short username.
# Uncomment this (or set SHORT_USER to something else),
# Will otherwise fall back on $USER.
#export SHORT_USER=${USER:0:8}

# Set Xterm/screen/Tmux title with shortened command and directory.
# Uncomment this to set.
#export SHORT_TERM_LINE=true

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/djl/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

# (Advanced): Uncomment this to make Bash-it reload itself automatically
# after enabling or disabling aliases, plugins, and completions.
# export BASH_IT_AUTOMATIC_RELOAD_AFTER_CONFIG_CHANGE=1

# Load Bash It
# source "$BASH_IT"/bash_it.sh
# Load command not found emulation

export EDITOR="vim"

# LOCALE
if [ -f "/etc/locale.conf" ]; then
    if [ grep -q en_CY "/etc/locale.conf" ]; then
        export XCOMPOSEFILE=/usr/share/X11/locale/en_CY.UTF-8/Compose
        export LC_CTYPE="en_CY.UTF-8"
    fi
else
    export LC_CTYPE="en_US.UTF-8"
fi

if [ -f /etc/os-release ] ; then
    . /etc/os-release
    OS=$NAME
fi
alias killemacs="emacsclient -e '(save-buffers-kill-emacs)'"
alias xclip="xclip -selection clip-board"
alias delpyc="find . -name \"*.pyc\" -delete"
case $OS in
    Arch)
        alias paci="yaourt -S"
        alias pacs="yaourt -Ssa"
        alias pacr="yaourt -R"
        alias pacsq="yaourt -Ssaq"
        alias pacu="yaourt -Syyua"
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
        export SSH_ASKPASS="/usr/lib/ssh/ksshaskpass"
        ;;
esac

# AUTOCOMPLETE FOR ALIASES
# complete -F pacaur paci

# Makes emacs command hook on server
function emacs {
    if [[ $# -eq 0 ]]; then
        /usr/bin/emacs # "emacs" is function, will cause recursion
        return
    fi
    args=($*)
    for ((i=0; i <= ${#args}; i++)); do
        local a=${args[i]}
        # NOTE: -c for creating new frame
        if [[ ${a:0:1} == '-' && ${a} != '-c' ]]; then
            /usr/bin/emacsclient -nc ${args[*]}
            return
        fi
    done
    setsid emacsclient -n -a /usr/bin/emacs ${args[*]}
}

# Prompt
# PS1='[\u@\h \W]\$ '
