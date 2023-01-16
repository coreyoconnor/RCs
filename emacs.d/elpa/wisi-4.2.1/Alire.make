# Build Ada parts of Emacs wisi with Alire; see build/Makefile for non-Alire build

STEPHES_ADA_LIBRARY_ALIRE_PREFIX ?= $(CURDIR)/../org.stephe_leake.sal

include $(STEPHES_ADA_LIBRARY_ALIRE_PREFIX)/build/alire_rules.make

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj-wisi.el"))
# End:
# end of file
