#export XAPPLRESDIR=$HOME/.app-defaults

export PATH=${HOME}/bin:${HOME}/.cabal/bin:$PATH
# export SVN_EDITOR=yi

function hs-search () 
{ 
    find . -name '*.hs' -print0 -or -name '*.lhs' -print0 | xargs -0 egrep -n --color=auto "$*"
}

function pl-search () 
{ 
    find . -name '*.pm' -print0 -or -name '*.pl' -print0 | xargs -0 egrep -n --color=auto "$*"
}

function cpp-search () 
{ 
    find . -name '*.cpp' -print0 \
        -or -name '*.c' -print0 \
        -or -name '*.cc' -print0 \
        -or -name '*.h' -print0 \
        -or -name '*.hpp' -print0 \
        -or -name '*.cxx' -print0 \
        | xargs -0 egrep -n --color=auto "$*"
}

function header-search () 
{ 
    find . -name '*.h' -print0 \
        -or -name '*.hpp' -print0 \
        | xargs -0 egrep -n --color=auto "$*"
}

# Meh. regular gvim sucks under xmonad
# This doesn't work under CentOS due to a kernel bug?
# https://bugzilla.redhat.com/show_bug.cgi?id=180353
function gvim ()
{
    (xterm -e /bin/bash -l -c vim "$@") &
}

[ -z "$PS1" ] && return

alias ll='ls -l'
alias lla='ls -la'
alias ghci='ghci -XScopedTypeVariables'
alias gopen='gnome-open'
alias global-cabal-install='cabal install --global -p --enable-documentation --root-cmd=sudo'
alias user-cabal-install='cabal install --user -p --enable-documentation --root-cmd=sudo'
alias head-cabal-install='cabal install --with-compiler=/home/coconnor/Development/ghc/inplace/bin/ghc-stage2 --with-hc-pkg=/home/coconnor/Development/ghc/inplace/bin/ghc-pkg'

export GHC_HEAD=/home/coconnor/Development/ghc/inplace/bin/ghc-stage2

export EDITOR=vim

export P4CONFIG=.p4settings

