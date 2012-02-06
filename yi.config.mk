
# preferred frontend
frontend = vty

# where to install executable and packages
prefix = $(HOME)/Library/yi

# The following are web-publishing options.

tmp-dir = /tmp
user = coconnor

configure-dirs = --prefix=$(prefix) 

# extra-configure-args = -f$(frontend) --user --enable-executable-profiling --enable-library-profiling --ghc-options=-auto-all
extra-configure-args = -f$(frontend) --user 

# You should never need to change the following.

top-src-dir =

