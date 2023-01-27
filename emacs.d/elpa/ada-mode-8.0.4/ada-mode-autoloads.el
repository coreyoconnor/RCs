;;; ada-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ada-build" "ada-build.el" (0 0 0 0))
;;; Generated autoloads from ada-build.el

(autoload 'ada-build-prompt-select-prj-file "ada-build" "\
Prompt for a project file, parse and select it.
The file must have an extension from `wisi-prj-file-extensions'.
Returns the project if a file is selected, nil otherwise.

\(fn FILENAME)" t nil)

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
Set the main project variable to the current file, then run make_cmd.
By default, this compiles and links the new main program.
If CONFIRM is non-nil, prompt for user confirmation of the command.

\(fn &optional CONFIRM)" t nil)

(autoload 'ada-build-run "ada-build" "\
Run the run_cmd project variable.
By default, this runs the main program.
If CONFIRM is non-nil, prompt for user confirmation of the command.

\(fn &optional CONFIRM)" t nil)

(register-definition-prefixes "ada-build" '("ada-build-"))

;;;***

;;;### (autoloads nil "ada-core" "ada-core.el" (0 0 0 0))
;;; Generated autoloads from ada-core.el

(autoload 'create-ada-prj "ada-core" "\


\(fn &key NAME COMPILE-ENV (COMPILER-LABEL ada-compiler) (XREF-LABEL ada-xref-backend) SOURCE-PATH PLIST)" nil nil)

(autoload 'ada-prj-default "ada-core" "\
Return the default `ada-prj' object.
If SRC-DIR is non-nil, use it as the default for project.source-path.

\(fn &optional NAME SRC-DIR)" nil nil)

(autoload 'ada-select-prj-file "ada-core" "\
Select PRJ-FILE as the current project file, parsing it if necessary.
Deselects the current project first.

\(fn PRJ-FILE)" nil nil)

(register-definition-prefixes "ada-core" '("ada-"))

;;;***

;;;### (autoloads nil "ada-eglot" "ada-eglot.el" (0 0 0 0))
;;; Generated autoloads from ada-eglot.el

(autoload 'ada-eglot-setup "ada-eglot" "\
Configure elgot settings for Ada." nil nil)

(autoload 'create-eglot-xref "ada-eglot" nil nil nil)

(register-definition-prefixes "ada-eglot" '("ada-eglot-" "eglot-ada"))

;;;***

;;;### (autoloads nil "ada-imenu" "ada-imenu.el" (0 0 0 0))
;;; Generated autoloads from ada-imenu.el

(register-definition-prefixes "ada-imenu" '("ada--imenu-"))

;;;***

;;;### (autoloads nil "ada-indent-user-options" "ada-indent-user-options.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ada-indent-user-options.el

(register-definition-prefixes "ada-indent-user-options" '("ada-"))

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

(autoload 'ada-parse-require-process "ada-mode" "\
Start the Ada parser in an external process, if not already started.
Unless WAIT, does not wait for parser to respond. Returns the parser object.

\(fn &key WAIT)" t nil)

(autoload 'ada-mode "ada-mode" "\
The major mode for editing Ada code.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.ad[abs]\\'" . ada-mode))

(register-definition-prefixes "ada-mode" '("ada-"))

;;;***

;;;### (autoloads nil "ada-skel" "ada-skel.el" (0 0 0 0))
;;; Generated autoloads from ada-skel.el

(register-definition-prefixes "ada-skel" '("ada-skel-"))

;;;***

;;;### (autoloads nil "ada_annex_p-process" "ada_annex_p-process.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ada_annex_p-process.el

(register-definition-prefixes "ada_annex_p-process" '("ada_annex_p-process-"))

;;;***

;;;### (autoloads nil "benchmark-xref" "benchmark-xref.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from benchmark-xref.el

(register-definition-prefixes "benchmark-xref" '("ada-mode-test" "bench"))

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
