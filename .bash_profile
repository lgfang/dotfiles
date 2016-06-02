# Modified: Fang Lungang 02/27/2016 21:14>

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

#* load a variety of stuff
if declare -f loaddm >/dev/null; then
    loaddm
fi

