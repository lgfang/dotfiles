# Modified: Lungang Fang 10/10/2022 18:20>

#* source in .bashrc

[ -f "$HOME/.bashrc" ] && source "$HOME/.bashrc"

#* Env variables

# For OS X homebrew commands
PATH="/opt/homebrew/bin:$PATH"
PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"
MANPATH=/opt/homebrew/share/man:$MANPATH

# For OS X homebrew openssl
PATH=/usr/local/opt/openssl@1.1/bin:$PATH

export PATH=$HOME/Library/Python/2.7/bin:$PATH # for AWS cli

export PATH=$HOME/.local/bin:$PATH
export MANPATH=$HOME/.local/share/man:$MANPATH
export INFOPATH=$HOME/.local/share/info:$INFOPATH
export HISTCONTROL="ignoredups"
export HISTFILESIZE=1000
export HISTSIZE=500

# For OS X java
# To get java home after installing JDK8, ' /usr/libexec/java_home -v 1.8'
export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_181.jdk/Contents/Home"
# export JAVA_HOME="/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home"

# Attach to (or start) a tmux session when opening a new terminal
[ -n "$TMUX" ] || tg

