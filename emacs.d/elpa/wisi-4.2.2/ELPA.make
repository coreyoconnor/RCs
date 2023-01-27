# For compiling wisi code in elpa worktree

#export Standard_Common_Build := Debug

.PHONY : all force

all : build_ada byte-compile

build_ada : wisi.gpr force
	gprbuild -p -j8 wisi.gpr

wisi.gpr : wisi.gpr.gp
	gnatprep -DELPA="yes" wisi.gpr.gp wisi.gpr

BYTE_COMPILE := "(progn (setq package-load-list '((wisi) (ada-mode) (gnat-compiler) all)) (package-initialize)(setq byte-compile-error-on-warn t)(batch-byte-compile))"
byte-compile : byte-compile-clean
	emacs -Q -batch -L . --eval $(BYTE_COMPILE) *.el

byte-compile-clean :
	rm -f *.elc

clean : force
	rm -rf wisi.gpr obj *parse_table*

recursive-clean : force
	gprclean -r -P wisi.gpr

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj-wisi.el"))
# End:
# end of file
