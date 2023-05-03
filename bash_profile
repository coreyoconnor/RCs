if [[ -z $SCREENRC_HACK ]] ; then
   source $HOME/.bashrc
fi

source $RCS_DIR/shell_env

if [[ -f ${RCS_PRIVATE_DIR}/profile_private ]] ; then
  source ${RCS_PRIVATE_DIR}/profile_private
fi

source $RCS_DIR/shell_aliases
