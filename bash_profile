source $HOME/.bashrc

if [[ -f ${RCS_PRIVATE_DIR}/profile_private ]] ; then
  source ${RCS_PRIVATE_DIR}/profile_private
fi

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

alias ll='ls -l'
alias lla='ls -la'
alias ghci='ghci -XScopedTypeVariables'
alias gopen='gnome-open'
alias global-cabal-install='cabal install --global -p --enable-documentation --root-cmd=sudo'
alias user-cabal-install='cabal install --user -p --enable-documentation --root-cmd=sudo'
alias head-cabal-install='cabal install --with-compiler=/home/coconnor/Development/ghc/inplace/bin/ghc-stage2 --with-hc-pkg=/home/coconnor/Development/ghc/inplace/bin/ghc-pkg'
alias gcam='git commit -a -m'
alias gp='git push'
alias xh='xterm -ls &'

alias td='todo.sh'
alias ta='todo.sh add'
alias tl='todo.sh list'
alias e='emacs -nw'
