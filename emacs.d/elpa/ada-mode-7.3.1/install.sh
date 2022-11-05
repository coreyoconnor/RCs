#!/bin/sh
# Install executables for Ada mode.
#
# See build.sh for build (must be run before install).

# $1 : optional --prefix=<dir>
#
# If you don't have write permission in the GNAT installation
# directory, you need to use --prefix=<dir>, or run with root priviledges.

if [ -d ../wisi-4.0.? ]; then
    WISI_DIR=`ls -d ../wisi-4.0.?`
fi

gprinstall -f -p -P ada_mode_wisi_parse.gpr -aP $WISI_DIR --install-name=ada_mode_wisi_parse $1 

# end of file.
