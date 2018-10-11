export RCS_DIR=${HOME}/Development/RCs

if [[ -f ${HOME}/Development/RCs_private/rc_private ]] ; then
  export RCS_PRIVATE_DIR=${HOME}/Development/RCs_private
  source ${RCS_PRIVATE_DIR}/rc_private
fi

export PATH=${HOME}/bin:${HOME}/.cabal/bin:$RCS_DIR/bin:$PATH

export GHC_HEAD=/home/coconnor/Development/ghc/inplace/bin/ghc-stage2

export EDITOR=vim

export P4CONFIG=${HOME}/.p4settings
export SBT_OPTS="-Xmx10g"
export PS1='\n\[\033[1;32m\][\w]\$\[\033[0m\] '

if [[ -z $SCREENRC_HACK && -n $STY ]] ; then
   export SCREENRC_HACK=HACK
   source ${HOME}/.bash_profile
fi
