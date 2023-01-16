;;; gnat-compiler-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gnat-alire" "gnat-alire.el" (0 0 0 0))
;;; Generated autoloads from gnat-alire.el

(autoload 'create-alire-prj "gnat-alire" "\
Return an initial wisi project for the current Alire workspace.

\(fn &key NAME GPR-FILE COMPILE-ENV FILE-ENV XREF-LABEL)" nil nil)

(register-definition-prefixes "gnat-alire" '("alire-get-env"))

;;;***

;;;### (autoloads nil "gnat-compiler" "gnat-compiler.el" (0 0 0 0))
;;; Generated autoloads from gnat-compiler.el

(autoload 'create-gnat-compiler "gnat-compiler" "\


\(fn &key GPR-FILE RUN-BUFFER-NAME PROJECT-PATH TARGET RUNTIME GNAT-STUB-OPTS GNAT-STUB-CARGS)" nil nil)

(register-definition-prefixes "gnat-compiler" '("gnat"))

;;;***

;;;### (autoloads nil "gnat-xref" "gnat-xref.el" (0 0 0 0))
;;; Generated autoloads from gnat-xref.el

(autoload 'create-gnat-xref "gnat-xref" "\


\(fn &key GPR-FILE RUN-BUFFER-NAME PROJECT-PATH TARGET RUNTIME GNAT-STUB-OPTS GNAT-STUB-CARGS)" nil nil)

(register-definition-prefixes "gnat-xref" '("gnat-"))

;;;***

;;;### (autoloads nil nil ("gnat-compiler-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gnat-compiler-autoloads.el ends here
