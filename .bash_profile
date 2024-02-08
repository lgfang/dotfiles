# Modified: Lungang Fang 2024-02-08T11:35:59+1100>

#* Env variables

# For OS X homebrew commands
PATH="/opt/homebrew/bin:$PATH"
PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"
MANPATH=/opt/homebrew/share/man:$MANPATH

# For OS X homebrew openssl
PATH="/opt/homebrew/opt/openssl@3/bin:$PATH"

export PATH=$HOME/.local/bin:$PATH
export MANPATH=$HOME/.local/share/man:$MANPATH
export INFOPATH=$HOME/.local/share/info:$INFOPATH
export HISTCONTROL="ignoredups"
export HISTFILESIZE=1000
export HISTSIZE=500

# For OS X java
# remove JAVA_HOME etc., brew install openjdk and "ln -s" properly.

# For golang
export GOPATH=$HOME/golang

#* source in .bashrc *after* the ENVs (PATH etc.) are set.
[ -f "$HOME/.bashrc" ] && source "$HOME/.bashrc"

#* load a variety of stuff

#* Attach to (or start) a tmux session when opening a new terminal
[ -n "$TMUX" ] || tg

