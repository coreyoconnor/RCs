;;; ada-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ada-build" "ada-build.el" (0 0 0 0))
;;; Generated autoloads from ada-build.el

(autoload 'ada-build-prompt-select-prj-file "ada-build" "\
Prompt for a project file, parse and select it.
The file must have an extension from `wisi-prj-file-extensions'.
Returns the project if a file is selected, nil otherwise." t nil)

(autoload 'ada-build-check "ada-build" "\
Run the check_cmd project variable.
By default, this checks the current file for syntax errors.
If CONFIRM is non-nil, prompt for user confirmation of the command.

\(fn &optional CONFIRM)" t nil)

(autoload 'ada-build-make "ada-build" "\
Run the make_cmd project variable.
By default, this compiles and links the main program.
If CONFIRM is non-nil, prompt for user confirmation of the command.

\(fn &optional CONFIRM)" t nil)

(autoload 'ada-build-set-make "ada-build" "\
Set the main project variable to the current file, then run the make_cmd project variable.
By default, this compiles and links the new main program.
If CONFIRM is non-nil, prompt for user confirmation of the command.

\(fn &optional CONFIRM)" t nil)

(autoload 'ada-build-run "ada-build" "\
Run the run_cmd project variable.
By default, this runs the main program.
If CONFIRM is non-nil, prompt for user confirmation of the command.

\(fn &optional CONFIRM)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-build" '("ada-build-")))

;;;***

;;;### (autoloads nil "ada-compiler-gnat" "ada-compiler-gnat.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ada-compiler-gnat.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-compiler-gnat" '("ada-gnat-")))

;;;***

;;;### (autoloads nil "ada-core" "ada-core.el" (0 0 0 0))
;;; Generated autoloads from ada-core.el

(autoload 'create-ada-prj "ada-core" "\


\(fn &key NAME COMPILE-ENV (COMPILER-LABEL ada-compiler) (XREF-LABEL ada-xref-tool) SOURCE-PATH PLIST FILE-PRED)" nil nil)

(autoload 'ada-prj-default "ada-core" "\
Return the default `ada-prj' object.
If SRC-DIR is non-nil, use it as the default for project.source-path.

\(fn &optional NAME SRC-DIR)" nil nil)

(autoload 'ada-prj-make-compiler "ada-core" "\


\(fn LABEL)" nil nil)

(autoload 'ada-select-prj-file "ada-core" "\
Select PRJ-FILE as the current project file, parsing it if necessary.
Deselects the current project first.

\(fn PRJ-FILE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-core" '("ada-")))

;;;***

;;;### (autoloads nil "ada-gnat-xref" "ada-gnat-xref.el" (0 0 0 0))
;;; Generated autoloads from ada-gnat-xref.el

(autoload 'create-gnat-xref "ada-gnat-xref" "\


\(fn &key GPR-FILE RUN-BUFFER-NAME PROJECT-PATH TARGET RUNTIME GNAT-STUB-OPTS GNAT-STUB-CARGS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-gnat-xref" '("ada-gnat-" "gnatxref-buffer-name-prefix")))

;;;***

;;;### (autoloads nil "ada-imenu" "ada-imenu.el" (0 0 0 0))
;;; Generated autoloads from ada-imenu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-imenu" '("ada--imenu-")))

;;;***

;;;### (autoloads nil "ada-indent-user-options" "ada-indent-user-options.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ada-indent-user-options.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-indent-user-options" '("ada-")))

;;;***

;;;### (autoloads nil "ada-mode" "ada-mode.el" (0 0 0 0))
;;; Generated autoloads from ada-mode.el

(autoload 'ada-add-extensions "ada-mode" "\
Define SPEC and BODY as being valid extensions for Ada files.
SPEC and BODY are two regular expressions that must match against
the file name.

\(fn SPEC BODY)" nil nil)

(autoload 'ada-fix-compiler-error "ada-mode" nil t nil)

(autoload 'ada-parse-prj-file "ada-mode" "\


\(fn PRJ-FILE)" nil nil)

(autoload 'ada-select-prj-file "ada-mode" "\


\(fn PRJ-FILE)" nil nil)

(defalias 'ada-project-current #'wisi-prj-current-cached)

(autoload 'ada-mode "ada-mode" "\
The major mode for editing Ada code.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.ad[abs]\\'" . ada-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-mode" '("ada-")))

;;;***

;;;### (autoloads nil "ada-process" "ada-process.el" (0 0 0 0))
;;; Generated autoloads from ada-process.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-process" '("ada-process-")))

;;;***

;;;### (autoloads nil "ada-skel" "ada-skel.el" (0 0 0 0))
;;; Generated autoloads from ada-skel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-skel" '("ada-skel-")))

;;;***

;;;### (autoloads nil "gnat-core" "gnat-core.el" (0 0 0 0))
;;; Generated autoloads from gnat-core.el

(autoload 'create-gnat-compiler "gnat-core" "\


\(fn &key GPR-FILE RUN-BUFFER-NAME PROJECT-PATH TARGET RUNTIME GNAT-STUB-OPTS GNAT-STUB-CARGS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnat-core" '("ada-gnat-debug-run" "gnat")))

;;;***

;;;### (autoloads nil "gpr-indent-user-options" "gpr-indent-user-options.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from gpr-indent-user-options.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gpr-indent-user-options" '("gpr-indent")))

;;;***

;;;### (autoloads nil "gpr-mode" "gpr-mode.el" (0 0 0 0))
;;; Generated autoloads from gpr-mode.el

(autoload 'gpr-mode "gpr-mode" "\
The major mode for editing GNAT project files." t nil)

(add-to-list 'auto-mode-alist '("\\.gpr\\'" . gpr-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gpr-mode" '("gpr-")))

;;;***

;;;### (autoloads nil "gpr-process" "gpr-process.el" (0 0 0 0))
;;; Generated autoloads from gpr-process.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gpr-process" '("gpr-process-")))

;;;***

;;;### (autoloads nil "gpr-query" "gpr-query.el" (0 0 0 0))
;;; Generated autoloads from gpr-query.el

(autoload 'create-gpr_query-xref "gpr-query" "\


\(fn &key GPR-FILE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gpr-query" '("gpr-query")))

;;;***

;;;### (autoloads nil "gpr-skel" "gpr-skel.el" (0 0 0 0))
;;; Generated autoloads from gpr-skel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gpr-skel" '("gpr-skel-")))

;;;***

;;;### (autoloads nil nil ("ada-mode-pkg.el" "ada-prj.el" "ada-stmt.el"
;;;;;;  "ada-xref.el" "prj.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ada-mode-autoloads.el ends here
