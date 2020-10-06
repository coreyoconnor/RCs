if [[ -z $SCREENRC_HACK ]] ; then
   source $HOME/.bashrc
fi

source $RCS_DIR/shell_env

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

source $RCS_DIR/shell_aliases
