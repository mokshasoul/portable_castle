function precmd {
    # vcs_info
    # Put string "hostname::/full/dir/path" in the title-bar:
    echo -ne "\e]2;$PWD\a"

    # Put parentdir/curdir into tab
    echo -ne "\e]1;$PWD: h: t/$PWD: t\a"
}

function set_running_app {
    print -Pn "\e]0; $PWD: t:$(history -1 | cut -b7-) \a"
}
# Sets the tab and window titles with a given command.
function _terminal-set-titles-with-command {
  emulate -L zsh
  setopt EXTENDED_GLOB

  # Get the command name that is under job control.
  if [[ "${2[(w)1]}" == (fg|%*)(\;|) ]]; then
    # Get the job name, and, if missing, set it to the default %+.
    local job_name="${${2[(wr)%*(\;|)]}:-%+}"

    # Make a local copy for use in the subshell.
    local -A jobtexts_from_parent_shell
    jobtexts_from_parent_shell=(${(kv)jobtexts})

    jobs "$job_name" 2>/dev/null > >(
      read index discarded
      # The index is already surrounded by brackets: [1].
      _terminal-set-titles-with-command "${(e):-\$jobtexts_from_parent_shell$index}"
    )
  else
    # Set the command name, or in the case of sudo or ssh, the next command.
    local cmd="${${2[(wr)^(*=*|sudo|ssh|-*)]}:t}"
    local truncated_cmd="${cmd/(#m)?(#c15,)/${MATCH[1,12]}...}"
    unset MATCH

    set-window-title "$cmd"
    set-tab-title "$truncated_cmd"
  fi
}
# Sets the terminal or terminal multiplexer window title.
function set-window-title {
  local title_format{,ted}
  zstyle -s ':prezto:module:terminal:window-title' format 'title_format' || title_format="%s"
  zformat -f title_formatted "$title_format" "s:$argv"

  if [[ "$TERM" == screen* ]]; then
    title_format="\ek%s\e\\"
  else
    title_format="\e]2;%s\a"
  fi

  printf "$title_format" "${(V%)title_formatted}"
}

# Sets the terminal tab title.
function set-tab-title {
  local title_format{,ted}
  zstyle -s ':prezto:module:terminal:tab-title' format 'title_format' || title_format="%s"
  zformat -f title_formatted "$title_format" "s:$argv"

  printf "\e]1;%s\a" ${(V%)title_formatted}
}
autoload -Uz add-zsh-hook

__show_pwd() {
    print -Pn "\e]0; [%n@%m:%d]\a"
}
# Sets the tab and window titles with a given path.
function _terminal-set-titles-with-path {
  emulate -L zsh
  setopt EXTENDED_GLOB

  local absolute_path="${${1:a}:-$PWD}"
  local abbreviated_path="${absolute_path/#$HOME/~}"
  local truncated_path="${abbreviated_path/(#m)?(#c15,)/...${MATCH[-12,-1]}}"
  unset MATCH

  set-window-title "$abbreviated_path"
  set-tab-title "$truncated_path"
}

add-zsh-hook precmd _terminal-set-titles-with-path
add-zsh-hook preexec _terminal-set-titles-with-command
