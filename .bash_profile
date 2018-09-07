# Modified: Lungang Fang 09/07/2018 10:20>

#* source in .bashrc

[ -f "$HOME/.bashrc" ] && source "$HOME/.bashrc"

#* Env variables

# For OS X homebrew coreutils
PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
MANPATH=/usr/local/opt/coreutils/libexec/gnuman:$MANPATH

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

#* load a variety of stuff
if declare -f loaddm >/dev/null; then
    loaddm
fi
