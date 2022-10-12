# Modified: Lungang Fang 10/10/2022 18:20>

#* source in .bashrc

[ -f "$HOME/.bashrc" ] && source "$HOME/.bashrc"

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

# Attach to (or start) a tmux session when opening a new terminal
[ -n "$TMUX" ] || tg

