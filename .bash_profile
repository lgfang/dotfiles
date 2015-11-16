# Modified: Fang Lungang 12/14/2015 11:01>

#* source in .bashrc

[ -f "$HOME/.bashrc" ] && source "$HOME/.bashrc"

#* Env variables

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

