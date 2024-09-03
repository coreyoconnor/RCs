export RCS_DIR=${RCS_DIR:-$HOME/Documents/RCs}
source $RCS_DIR/shell_rc

if [[ -f ${RCS_PRIVATE_DIR}/profile_private ]] ; then
  source ${RCS_PRIVATE_DIR}/profile_private
fi

source $RCS_DIR/shell_aliases

if [[ -f $HOME/.kube/completion.bash.inc ]] ; then
  source $HOME/.kube/completion.bash.inc
fi

if [[ -f $RCS_DIR/mill-completion.bash.inc ]] ; then
  function mill() {
    if [[ -f ./millw ]] ; then
      ./millw "$@"
    else
      if [[ -f ./mill ]] ; then
        ./mill "$@"
      else
        echo "Not in a mill project base directory" >&2
      fi
    fi
  }
  alias m=mill
  source $RCS_DIR/mill-completion.bash.inc
fi

export PS1='\n\[\033[1;32m\][\w]\$\[\033[0m\] '

