# shellcheck disable=SC1090,SC1091
# Modified: Lungang Fang 2023-10-25T14:10:11+1100>

#* Do nothing if not running interactively
[[ "$-" != *i* ]] && return

#* Source in global settings
if [ -f /etc/bashrc ]; then
    source /etc/bashrc
fi

#* tty

#TERM=dtterm
stty -ixon        # Disable flow control, i.e. ^s "freeze screen" and ^q resume
stty erase ^?     # kill ^u intr ^c eof ^d stop ^s
set -o ignoreeof  # do not logout when "ctrl + D pressed"
set -o emacs
# For some old machines
# set -o vi                   # emacs style does not work well
# set -o viraw                # for auto-completion of dirs

export EDITOR=vi

#* Shell options
# command history related
shopt -s histappend
shopt -s histverify  # edit the resulting command instead of run it directly
# directory
shopt -s autocd
shopt -s cdspell
shopt -s dirspell
shopt -s checkwinsize
# shopt -q progcomp

#* PS1
# *NOTE* must use single quotes NOT double quotes so that escape backslashes are
# not escaped.
# TODO: replace color escape codes with 'tput setaf' etc.?
PS1='\n'                      # An extra line to separte previous output and PS1
PS1=$PS1'\[\e[32m\]╭ '        # 1st line, additional info
PS1=$PS1'\[\e[38;5;101m\]\! \t ' # time and command history number
PS1=$PS1'$(ps1_git)'
PS1=$PS1'$(ps1_kube)'
PS1=$PS1'\n\[\e[32m\]│ '         # 2nd line, common info in PS1
PS1=$PS1'$(ps1_warn_msg)'        # warning message if there is any
PS1=$PS1'\[\e[38;5;106m\]\u@\h:' # user@host
PS1=$PS1'\[\e[38;5;33m\]\w '     # working directory
PS1=$PS1'\n\[\e[32m\]╰ \$ '      # 3rd line, "$"/"#" sign on a new line
PS1=$PS1'\[\e[0m\]'              # restore to the default color

function ps1_warn_msg {
    [ -z "$MY_WARN" ] || printf '\e[7;35m%s\e[0m ' "$MY_WARN"
}

function ps1_git {
    command -v git >/dev/null || return
    # get git branch of pwd
    local branch="$(git branch 2>/dev/null | grep "\*" | colrm 1 2)"
    if [ -n "$branch" ]; then
        printf '\e[0;36m%s ' "git:$branch"
    fi
}

function ps1_kube {
    command -v kubectl >/dev/null || return
    local kube_context="$(kubectl config current-context 2>/dev/null)"
    local kube_namespace="$(kubectl config view --minify --output 'jsonpath={..namespace}' 2>/dev/null)"
    if [ -n "$kube_context" -o -n "$kube_namespace" ]; then
        printf '\e[0;36m%s ' "kube:$kube_context/$kube_namespace"
    fi
}

#* auto-completion
if [ -r "/usr/local/etc/profile.d/bash_completion.sh" ]; then
    source "/usr/local/etc/profile.d/bash_completion.sh"
fi

if [ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]; then
    source "/opt/homebrew/etc/profile.d/bash_completion.sh"
fi

if [ -d "$HOME/.bash_completion.d" ]; then
    for each in $HOME/.bash_completion.d/*; do
        source "$each"
    done
fi

#* aliases and functions (note: prefer functions than aliases)
alias hex='od -Ax -tx1z -v'
alias no_color='sed -e "s/\x1b\[[0-9;]*m//g"' # remove escape sequences for ANSI
                                              # color etc.
alias ls='ls --color=auto'
alias rm='rm -I'                  # IMHO, much better than 'rm -i'
alias lsmnt='mount | column -t'   # a better format

#** command history
function nh {
    echo 'Discard command history'
    export HISTFILE=/dev/null
    # Can actually restore it by setting HISTFILE before quit the session
}

#** directory bookmark
declare -A _lgfang_dir_bookmark
declare _lgfang_dir_file=~/.dir_mark

function dm {
    # directory bookmark
    local usage="
$FUNCNAME        Store current directory to the first available bookmark
$FUNCNAME x      Store current directory to bookmark 'x' (overwrite if needed)
$FUNCNAME -x     Remove bookmark 'x'

Where x is one of [0-9a-z]"

    # reload & save every time bookmark/jump to keep the file up to minute. CPU
    # consumption should not be a concern.
    [ -r "$_lgfang_dir_file" ] && source "$_lgfang_dir_file"

    local subscript=$1
    local each

    if [ -n "$subscript" ] && ! [[ "$subscript" =~ ^-?[0-9a-z]$ ]]; then
        echo "Invalid subscript '$subscript', usage: $usage" >&2
        return 1
    fi

    if [[ "$subscript" =~ ^-.*$ ]]; then
        subscript=${subscript#-}
        local dir=${_lgfang_dir_bookmark[$subscript]}
        unset _lgfang_dir_bookmark[$subscript]
        declare -p _lgfang_dir_bookmark > "$_lgfang_dir_file"
        echo "Bookmark removed: $subscript -> '$dir'"
        return 0
    fi

    local pwd=$(pwd)

    for each in {0..9} {a..z}; do # remember this many directories
        if [ "${_lgfang_dir_bookmark[$each]}" == "$pwd" ]; then
            echo "Already exits: $each -> $pwd"
            return
        fi
    done

    if [ -z "$subscript" ]; then # didn't specify a subscript, find one unused

        for each in {0..9} {a..z}; do
            if [ -z "${_lgfang_dir_bookmark[$each]}" ]; then
                subscript=$each
                break
            fi
        done

        if [ -z "$subscript" ]; then
            echo "Cannot find any unoccupied subscript," \
                 "please explictly specify one" >&2
            return 1
        fi
    fi

    _lgfang_dir_bookmark[$subscript]="$pwd"
    declare -p _lgfang_dir_bookmark > "$_lgfang_dir_file"
    echo "Bookmark added: $subscript -> '$pwd'"
}

function dj {
    local usage="$FUNCNAME x (where x is one of [0-9a-z])"
    local subscript=$1

    [ -r "$_lgfang_dir_file" ] && source "$_lgfang_dir_file"
    declare -p _lgfang_dir_bookmark &>/dev/null

    if [ $? -ne 0 ]; then
        echo "no bookmark available" >&2
        return 1
    fi

    if ! [[ "$subscript" =~ ^[0-9a-z]$ ]]; then
        echo "Invalid subscript '$subscript', usage: $usage" >&2
        return 1
    fi

    if [ -z "${_lgfang_dir_bookmark[$subscript]}" ]; then
        echo "no bookmark set for '$subscript'" >&2
        return 1
    fi

    cd "${_lgfang_dir_bookmark[$subscript]}"
}

function lsdm {
    local usage="
$FUNCNAME [PATTERN]

List directory bookmarks (if given, only those which match the PATTERN)."

    local pattern=$1

    [ -r "$_lgfang_dir_file" ] && source "$_lgfang_dir_file" || return 0

    for each in "${!_lgfang_dir_bookmark[@]}"; do
        local dir=${_lgfang_dir_bookmark[$each]}
        if [ -z "$pattern" ] || [[ "$dir" =~ $pattern ]]; then
            echo -e "$each - $dir"
        fi
    done
}

#** directory stack
function cd {
    # function instead alias to take effect even in scripts (say, my "ep")
    mycd "$@"
}

function mycd {
    # 1, pushd by default. 2, supports "cd from to".
    local dest

    if [ $# -eq 0 ]; then
        dest=~
    elif [ $# -eq 1 ]; then
        dest=$1
    elif [ $# -eq 2 ]; then
        dest=${PWD//$1/$2}
    else
        echo "error: two many arguments" >&2
        return 1
    fi

    mypushd "$dest"
}

function mypushd {
    ## 1. Don't bloat the history forever.
    #* 2. shopt pushdsilent not available in bash, redirect to /dev/null

    local dest=$1

    if [[ "$dest" =~ ^\.\.\.\.*$ ]]; then
        # expand "cd ...." to cd "../../.."
        dest=${dest#..}
        dest="..${dest//.//..}"
    fi

    builtin pushd "$dest" > /dev/null

    # Remove duplication
    local index stored new_one
    new_one=$(builtin dirs +0)

    for index in {1..10}; do
        stored=$(builtin dirs +${index} 2>/dev/null) || break
        if [ "$stored" == "$new_one" ]; then
            popd -n +$index >/dev/null 2>&1
            break
        fi
    done

    # Delete 11th dir if there is, hence keep the stack size <=10.
    builtin popd -n +11 >/dev/null 2>&1
}

alias dirs='dirs -v'
alias bd='pushd +1 >/dev/null'  # backward in history
alias fd='pushd -0 >/dev/null'  # forward

#** emacs
# start emacsclient (and emacs daemon if necessary)
alias et='emacsclient -a "" -t'
alias ew='emacs-w32&'           # start GUI emacs, for cygwin

function ep { # go to current directory of emacs(daemon)
    cd "$(emacsclient -e '(expand-file-name
        (with-current-buffer (window-buffer) default-directory))' | tr -d \")"
}

#** git

function gerrit {
    # submit current commit to gerrit for review
    local branch=$1
    [ -n "$branch" ] || branch=$(git name-rev --name-only HEAD)
    # NOTE: Do NOT use the following measure in git_4_ps1 since this
    # command cannot deal with detached checkout
    [ -n "$branch" ] || echo "ERROR: not in a valid branch!" >&2
    git push origin "HEAD:refs/for/$branch"
}

#** json/jq
# convert bson dump to valid json for jq
function bson2json {
    # usage: cat test.json | bson2json | jq '...'
    sed -e 's/BinData([0-9]*,\([^)]*\))/\1/g' \
        -e 's/Timestamp(\([0-9]*\)[^)]*)/\1/g' \
        -e 's/ISODate("\([^"]*\)"[^)]*)/"\1"/g' \
        -e 's/NumberLong("\([^"]*\)"[^)]*)/"\1"/g' \
        -e 's/NumberLong(\([^)]*\))/"\1"/g' \
        -e 's/ObjectId("\([^"]*\)"[^)]*)/"\1"/g' \
        -e 's/LUUID("\([^"]*\)"[^)]*)/"\1"/g'\
        -e 's/UUID("\([^"]*\)"[^)]*)/"\1"/g'
}

#** kubernetes

# kubectl autocomplete if this command is installed
command -v kubectl >/dev/null && source <(kubectl completion bash)

# alias 'k' and ensure autocomplete also works for it.
alias k=kubectl
complete -F __start_kubectl k

function kns {
    # a function to set namespace. It is not worthwhile to `brew install kubectx' for kubens

    if [[ -n "$2" ]]; then
        # Two or more parameters, error out
        echo "Usage: kns [namespace]" >&2
        return 1
    elif [[ -z "$1" ]]; then
        # No namespace specified, list existing ones
        kubectl get namespace
    elif ! kubectl get namespace "$1" >/dev/null 2>&1; then
        echo "Error: namespace '$1' does not exist" >&2
        return 1
    else
        kubectl config set-context $(kubectl config current-context) --namespace="$1"
    fi
}

#*** GKE
if [ -f "$HOME/.local/google-cloud-sdk/path.bash.inc" ]; then
    source "$HOME/.local/google-cloud-sdk/path.bash.inc"
fi
if [ -f "$HOME/.local/google-cloud-sdk/completion.bash.inc" ]; then
     source "$HOME/.local/google-cloud-sdk/completion.bash.inc"
fi

#** ssh

alias scp='scp -o LogLevel=error' # don't print motd etc.
alias ssh='ssh -o LogLevel=error'

function get_ssh_agent {        # print ssh agent info

    if [ -n "$SSH_AGENT_PID" -o -n "$SSH_AUTH_SOCK" ]; then
        echo "Current ssh agent is:"
        for each in SSH_AGENT_PID SSH_AUTH_SOCK; do
            eval "echo export $each=\${$each}"
        done
        return
    fi

    # Search in command history. This works because we set shopt to append
    # command history on the fly.
    hist=$(history | grep SSH | grep -v grep | awk '{$1="";print $0}')
    # Note: don't "sort -u", which will break match between agent pid and sock
    if [ -n "$hist" ]; then
        echo "Possible ssh agent(s):"
        echo "$hist"
    else
        echo "No clue about ssh agent"
    fi
}

#** terminal window
function mytitle { # set window title. Reset if no argument.
    # usage: title [text]
    if [ -z "$ORG_PROMPT_COMMAND" ]; then
        # for bash: store system default
        ORG_PROMPT_COMMAND=$PROMPT_COMMAND
    fi

    if [ $# -gt 0 ]; then
        PROMPT_COMMAND="" # for bash
        echo -ne "\033]0;$1\007"
    else
        # restore system default # for bash
        PROMPT_COMMAND=$ORG_PROMPT_COMMAND
    fi
}
export -f mytitle

#** tmux related

function tg { # Attach to specified tmux session
    #* 1. If the session does not exist, create it.
    ## 2. If no session name specified, prompt to choose from existing ones.

    # A simple/naive replacement of this "bloated" function:
    # tmux -2 attach -t "$session_name" || tmux -2 new -s "$session_name"

    if ! command -v tmux >/dev/null; then
        echo "Warn: tmux could not be found, not starting any tmux session"
        return
    fi

    local usage="tg [-d] [session_name]"
    local detach_others=""

    while getopts "d" opt; do
        case $opt in
            d) detach_others="-d";;
            ?) echo "$usage" >&2;;
        esac
    done
    shift $((OPTIND - 1))

    local session_name="$1"

    if [ -n "$session_name" ]; then
        tmux -2 attach $detach_others -t "$session_name" \
            || tmux -2 new -s "$session_name"
        return
    fi

    # No session name specified, act according to the number of sessions
    local sessions=$(tmux list-sessions -F "#{session_name}")

    if [ -z "$sessions" ]; then
        tmux -2 new -s 'misc'
        return
    fi

    if [ "$(echo "$sessions" | wc -l)" -eq 1 ]; then
        tmux -2 attach $detach_others -t "$sessions"
        return
    fi

    # Multiple sessions, prompt to choose one

    local IFS=$'\n' # In case session names contain whitespaces. Must
                    # 'local' to NOT pollute the global 'IFS'.
                    # $'LITERAL_STR' => ansi-c quoting
    local PS3="Select a session: "

    select session_name in $sessions; do

        if [ -n "$session_name" ]; then # A valid choice
            tmux -2 attach $detach_others -t "$session_name"
            return
        else
            echo "Invalid index '$REPLY', please retry"
        fi

    done
}

function tt {
    # List all tty used by tmux. If given a process name, find out all related
    # tmux panes, go to one of it.

    # usage: tt [process_name]

    # Note: once you find a pane, you may send keys to that process WITHOUT
    # going to that pane by running 'tmux send-keys -t s:w.p abcd'.

    local process_name="$1"
    local procs proc panes pane IFS PS3 choices choice

    if [ -n "$process_name" ]; then
        procs=$(ps -e | grep "\b$process_name" | grep -v '?')
    else
        procs=$(ps -e | grep -v '?')
    fi

    panes=$(tmux list-panes -a -F '#S:#I.#P #{pane_tty}')

    IFS=$'\n'
    for pane in $panes; do
        tty=$(echo "$pane" | awk -v FS='/' '{print $NF}')
        proc=$(echo "$procs" | grep "\b$tty\b")
        if [ -n "$proc" ]; then
            # got it, do a little format
            proc=$'\n'"$proc"
            choice=$(paste <(echo "$pane") <(echo "$proc"))
            choices=("${choices[@]}" "$choice")
        fi
    done

    PS3='Which pane to go? '
    select choice in "${choices[@]}"; do
        if [ -n "$choice" ]; then #
            tmux switch-client -t "$(echo "$choice" | awk '{print $1; exit}')"
            return
        else
            echo "Invalid '$REPLY', retry"
        fi
    done
}
export -f tt

function to_tmux_buffer {
    # usage: cat file | this_function
    while read line; do
        tmux set-buffer "$line"
    done
}

function tmux_clean_buffers {
    # Tmux paste buffer is mainly for copy/paste between CLI. If a large chunk
    # of documentation or source code is saved into a tmux paste buffer. Pasting
    # such content into CLI (or even editors) via tmux can cause issues. Run
    # this function manually to delete suspiciously large buffers (> 2048 bytes
    # by default) to avoid accidentally pasting such buffers.

    local threshold=${1:-256}
    tmux list-buffers | awk -v threshold=$threshold '{if($2 > threshold){print $1, $2, $3}}' | while read line; do
        echo "Deleting ${line%:*}"
        tmux delete-buffer -b ${line%%:*};
    done
    echo "all buffers bigger than $threshold bytes are deleted"
}

#* .inputrc stuff, BASH ONLY

# Usually, following settings are put into ~/.inputrc, with only stuff
# enclosed within single quotes kept. For me, I prefer to keep all stuff in
# one place to make it more explicit.

bind 'set show-all-if-ambiguous on'
bind 'set completion-ignore-case on'

# double <esc> to cycle through possible completions
bind '"\e\e":menu-complete'

# M-p,M-n works like those in eshell
bind '"\ep": history-search-backward'
bind '"\en": history-search-forward'

bind '"\C-w":kill-region'
