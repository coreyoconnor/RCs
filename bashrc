if [[ -f ${HOME}/Development/RCs_private/rc_private ]] ; then
  export RCS_PRIVATE_DIR=${HOME}/Development/RCs_private
  source ${RCS_PRIVATE_DIR}/rc_private
fi
export PATH=${HOME}/bin:${HOME}/.cabal/bin:$PATH

export GHC_HEAD=/home/coconnor/Development/ghc/inplace/bin/ghc-stage2

export EDITOR=vim

export P4CONFIG=${HOME}/.p4settings
