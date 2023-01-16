;;; gnat-compiler.el --- Support for running GNAT tools  -*- lexical-binding:t -*-
;;
;; GNAT is provided by AdaCore; see https://www.adacore.com/community
;;
;;; Copyright (C) 2012 - 2023  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;; Version: 1.0.1
;; package-requires: ((emacs "25.3") (wisi "4.2.0"))
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'wisi-prj)

;;;;; code

(defgroup gnat-compiler nil
  "Interface to AdaCore gnat compiler via a wisi project."
  :group 'programming)

(defcustom gnat-debug-run nil
  "If t or integer > 0, buffers containing a GNAT command will show
the command.  Otherwise, they will show only the output of the
command.  Higher integers show more information (environment vars etc)."
  :type 'integer
  :safe  #'integerp)

(defun gnat-debug-enabled (level)
  "Return t if gnat-debug-run is t or > LEVEL."
  (cond
   ((integerp gnat-debug-run)
    (> gnat-debug-run level))

   (t gnat-debug-run)))

;;;;; project file handling

(cl-defstruct gnat-compiler
  "Used with wisi-compiler-* generic functions."

  gpr-file 	  ;; absolute file name of GNAT project file.
  run-buffer-name ;; string; some compiler objects have no gpr file
  project-path    ;; list of directories from GPR_PROJECT_PATH
  target 	  ;; gnat --target argument.
  runtime 	  ;; gnat --RTS argument.
  gnat-stub-opts  ;; options for gnat stub
  gnat-stub-cargs ;; cargs options for gnat stub
  )

;;;###autoload
(cl-defun create-gnat-compiler
    (&key
     gpr-file
     run-buffer-name
     project-path
     target
     runtime
     gnat-stub-opts
     gnat-stub-cargs)
  ;; We declare and autoload this because we can't autoload
  ;; make-gnat-compiler in emacs < 27. We also can't use (defalias
  ;; 'create-gnat-compiler 'make-gnat-compiler); then
  ;; make-gnat-compiler is not defined by autoload.
  (make-gnat-compiler
   :gpr-file gpr-file
   :run-buffer-name run-buffer-name
   :project-path project-path
   :target target
   :runtime runtime
   :gnat-stub-opts gnat-stub-opts
   :gnat-stub-cargs gnat-stub-cargs
   ))

(defun gnat-run-buffer-name (prj-file-name &optional prefix)
  ;; We don't use (gnat-compiler-gpr-file compiler), because multiple
  ;; wisi-prj files can use one gpr-file.
  (concat (or prefix " *gnat-run-")
	  prj-file-name
	  "*"))

(defun gnat-compiler-require-prj ()
  "Return current `gnat-compiler' object from current project compiler.
Throw an error if current project does not have a gnat-compiler."
  (let* ((wisi-prj (wisi-prj-require-prj))
	 (compiler (wisi-prj-compiler wisi-prj)))
    (if (gnat-compiler-p compiler)
	compiler
      (error "no gnat-compiler in current project"))))

(defun gnat-prj-add-prj-dir (project compiler dir)
  "Add DIR to COMPILER.project_path, and to GPR_PROJECT_PATH in PROJECT.file-env"
  ;; We maintain two project values for this;
  ;; project-path - a list of directories, for elisp find file
  ;; GPR_PROJECT_PATH in environment, for gnat-run
  (when (file-directory-p dir)
    (let ((process-environment (copy-sequence (wisi-prj-file-env project))))
      (cl-pushnew dir (gnat-compiler-project-path compiler) :test #'string-equal)

      (setenv "GPR_PROJECT_PATH"
	      (mapconcat #'identity
			 (gnat-compiler-project-path compiler) path-separator))
      (setf (wisi-prj-file-env project) (copy-sequence process-environment))
      )))

(cl-defun gnat-get-paths (project &key ignore-prj-paths)
  "Set source and project paths in PROJECT from \"gnat list\"."
  (let* ((compiler (wisi-prj-compiler project))
	 (src-dirs (unless ignore-prj-paths (wisi-prj-source-path project)))
	 (prj-dirs nil))

    ;; Don't need project plist obj_dirs if using a project file, so
    ;; not setting obj-dirs.

    (condition-case-unless-debug nil
	(with-current-buffer (gnat-run-buffer compiler (gnat-compiler-run-buffer-name compiler))
	  ;; gnat list -v -P can return status 0 or 4; always lists compiler dirs

	  (gnat-run-gnat project "list" (list "-v") '(0 4))

	  (goto-char (point-min))

	  ;; Source path
	  (search-forward "Source Search Path:")
	  (forward-line 1)
	  (while (not (looking-at "^$")) ;; terminate on blank line
	    (back-to-indentation) ;; skip whitespace forward

	    ;; we use 'cl-pushnew here, and nreverse later, to
	    ;; preserve the directory order. Directory order matters
	    ;; for extension projects, which can have duplicate file
	    ;; names, and for project paths, which can contain two
	    ;; compiler libraries (ie Alire and system).
            (cl-pushnew
	     (if (looking-at "<Current_Directory>")
		 (directory-file-name default-directory)
	       (expand-file-name ; Canonicalize path part.
		(directory-file-name
		 (buffer-substring-no-properties (point) (line-end-position)))))
	     src-dirs
	     :test
	     #'string-equal)
	    (forward-line 1))

          ;; Project path
	  ;;
	  ;; These are also added to src-dirs, so compilation errors
	  ;; reported in project files are found.
	  (search-forward "Project Search Path:")
	  (forward-line 1)
	  (while (not (looking-at "^$"))
	    (back-to-indentation)
	    (let ((f
		   (if (looking-at "<Current_Directory>")
                       (directory-file-name default-directory)
		     (expand-file-name
                      (buffer-substring-no-properties (point) (line-end-position))))))
	      (cl-pushnew f src-dirs :test #'string-equal)
	      (cl-pushnew f prj-dirs :test #'string-equal))
	    (forward-line 1))

	  )
      (error
       ;; search-forward failed. Possible causes:
       ;;
       ;; missing dirs in GPR_PROJECT_PATH => user error
       ;; missing Object_Dir => gprbuild not run yet; it will be run soon
       ;; some files are missing string quotes => user error
       ;;
       ;; We used to call gpr_query to get src-dirs, prj-dirs here; it
       ;; is tolerant of the above errors. But ignoring the errors, to
       ;; let gprbuild run with GPR_PROJECT_PATH set, is simpler.
       (pop-to-buffer (gnat-run-buffer compiler (gnat-compiler-run-buffer-name compiler)))
       (message "project search path: %s" prj-dirs)
       (message "parse gpr failed")
       ))

    (setf (wisi-prj-source-path project) (nreverse src-dirs))
    (setf (gnat-compiler-project-path compiler) nil)
    (mapc (lambda (dir) (gnat-prj-add-prj-dir project compiler dir))
	  (nreverse prj-dirs))
    ))

(defun gnat-parse-gpr (gpr-file project)
  "Parse GPR-FILE, append to PROJECT (a `wisi-prj' object).
GPR-FILE must be absolute file name.
source-path will include compiler runtime."
  (let ((compiler (wisi-prj-compiler project)))
    (if (gnat-compiler-gpr-file compiler)
	;; gpr-file previously set; new one must match
	(when (not (string-equal gpr-file (gnat-compiler-gpr-file compiler)))
	  (error "project file %s defines a different GNAT project file than %s"
		 (gnat-compiler-gpr-file compiler)
		 gpr-file))

      (setf (gnat-compiler-gpr-file compiler) gpr-file)))

  (gnat-get-paths project :ignore-prj-paths t))

(defun gnat-parse-gpr-1 (gpr-file project)
  "For `wisi-prj-parser-alist'."
  (let ((compiler (wisi-prj-compiler project)))
    (setf (gnat-compiler-run-buffer-name compiler) (gnat-run-buffer-name gpr-file))
    (gnat-parse-gpr gpr-file project)))

;;;; command line tool interface

(defun gnat-run-buffer (compiler name)
  "Return a buffer suitable for running gnat command line tools for COMPILER."
  (let ((buffer (get-buffer name)))

    (unless (buffer-live-p buffer)
      (setq buffer (get-buffer-create name))
      (when (gnat-compiler-gpr-file compiler)
	;; Otherwise assume `default-directory' is already correct (or
	;; doesn't matter).
	(with-current-buffer buffer
	  (setq default-directory
		(file-name-directory
		 (gnat-compiler-gpr-file compiler))))
	))
    buffer))

(defun gnat-run (project exec command &optional err-msg expected-status)
  "Run a gnat command line tool, as \"EXEC COMMAND\".
PROJECT  is a `wisi-prj' object.
EXEC must be an executable found on `exec-path'.
COMMAND must be a list of strings.
ERR-MSG must be nil or a string.
EXPECTED-STATUS must be nil or a list of integers; throws an error if
process status is not a member.

Return process status.
Assumes current buffer is (gnat-run-buffer)"
  (set 'buffer-read-only nil)
  (erase-buffer)

  (setq command (cl-delete-if 'null command))

  ;; We can't just append file-env and compile-env to
  ;; process-environment, because they might have values that
  ;; override what's already in process-environment(for example alire sets PATH and
  ;; GPR_PROJECT_PATH). So we use (setenv ...).
  (let ((process-environment (copy-sequence process-environment))
	(process-list
	 (lambda (list)
	   (dolist (var list)
	     (unless (string-match "\\(.*\\)=\\(.*\\)$" var)
	       (error "malformed environment entry: %s" var))
	     (setenv (match-string-no-properties 1 var) (match-string-no-properties 2 var)))))
	status)

    (funcall process-list (wisi-prj-compile-env project))
    (funcall process-list (wisi-prj-file-env project))

    (let ((exec-path (split-string (getenv "PATH") path-separator)))
      (when (gnat-debug-enabled 0)
	(insert (format "GPR_PROJECT_PATH=%s\n%s " (getenv "GPR_PROJECT_PATH")
			(executable-find exec)))
	(mapc (lambda (str) (insert (concat str " "))) command)
	(newline))

      (when (gnat-debug-enabled 1)
	(dolist (item process-environment)
	  (insert item)(insert "\n")))

      (setq status (apply #'call-process exec nil t nil command)))

    (cond
     ((memq status (or expected-status '(0))); success
      nil)

     (t ; failure
      (pop-to-buffer (current-buffer))
      (if err-msg
	  (error "%s %s failed; %s" exec (car command) err-msg)
	(error "%s %s failed" exec (car command))
	))
     )))

(defun gnat-run-gnat (project command &optional switches-args expected-status)
  "Run the \"gnat\" command line tool, as \"gnat COMMAND -P<prj> SWITCHES-ARGS\".
COMMAND must be a string, SWITCHES-ARGS a list of strings.
EXPECTED-STATUS must be nil or a list of integers.
Return process status.
Assumes current buffer is (gnat-run-buffer)"
  (let* ((compiler (wisi-prj-compiler project))
	 (gpr-file (gnat-compiler-gpr-file compiler))
	 (project-file-switch
	  (when gpr-file
	    (concat "-P" (file-name-nondirectory gpr-file))))
         (target-gnat (concat (gnat-compiler-target compiler) "gnat"))
         ;; gnat list understands --RTS without a fully qualified
         ;; path, gnat find (in particular) doesn't (but it doesn't
         ;; need to, it uses the ALI files found via the GPR)
         (runtime
          (when (and (gnat-compiler-runtime compiler) (string= command "list"))
            (list (concat "--RTS=" (gnat-compiler-runtime compiler)))))
	 (cmd (append (list command) (list project-file-switch) runtime switches-args)))

    (gnat-run project target-gnat cmd nil expected-status)
    ))

(defun gnat-run-no-prj (command &optional dir)
  "Run \"gnat COMMAND\", with DIR as current directory.
Return process status.  Process output goes to current buffer,
which is displayed on error."
  (set 'buffer-read-only nil)
  (erase-buffer)

  (when gnat-debug-run
    (setq command (cl-delete-if 'null command))
    (mapc (lambda (str) (insert (concat str " "))) command)
    (newline))

  (let ((default-directory (or dir default-directory))
	status)

    (setq status (apply #'call-process "gnat" nil t nil command))
    (cond
     ((= status 0); success
      nil)

     (t ; failure
      (pop-to-buffer (current-buffer))
      (error "gnat %s failed" (car command)))
     )))

;;;; gnatprep utils

(defun gnatprep-indent ()
  "If point is on a gnatprep keyword, return indentation column
for it. Otherwise return nil.  Intended to be added to
`wisi-indent-calculate-functions' or other indentation function
list."
  ;; gnatprep keywords are:
  ;;
  ;; #if identifier [then]
  ;; #elsif identifier [then]
  ;; #else
  ;; #end if;
  ;;
  ;; they are all indented at column 0.
  (when (equal (char-after) ?\#) 0))

(defun gnatprep-syntax-propertize (start end)
  (goto-char start)
  (save-match-data
    (while (re-search-forward
	    "^[ \t]*\\(#\\(?:if\\|else\\|elsif\\|end\\)\\)"; gnatprep keywords.
	    end t)
      (cond
       ((match-beginning 1)
	(put-text-property
	 (match-beginning 1) (match-end 1) 'syntax-table '(11 . ?\n)))
       )
      )))

(defconst gnatprep-preprocessor-keywords
   (list (list "^[ \t]*\\(#.*\n\\)"  '(1 font-lock-preprocessor-face t))))

;; We assume that if this file is loaded, any ada buffer may have
;; gnatprep syntax; even with different compilers; all must run
;; gnatprep first. If support for another preprocessor is added, we'll
;; need wisi-prj-preprocessor, along with -compiler and -xref.
(defun gnatprep-setup ()
  (add-to-list 'wisi-indent-calculate-functions 'gnatprep-indent)
  (add-hook 'ada-syntax-propertize-hook #'gnatprep-syntax-propertize)
  (font-lock-add-keywords 'ada-mode gnatprep-preprocessor-keywords)
  ;; caller must call font-lock-refresh-defaults after this
  )

;;;; compiler message handling

(defconst gnat-predefined-package-alist
  '(
    ("a-calend" . "Ada.Calendar")
    ("a-chahan" . "Ada.Characters.Handling")
    ("a-comlin" . "Ada.Command_Line")
    ("a-contai" . "Ada.Containers")
    ("a-direct" . "Ada.Directories")
    ("a-except" . "Ada.Exceptions")
    ("a-ioexce" . "Ada.IO_Exceptions")
    ("a-finali" . "Ada.Finalization")
    ("a-numeri" . "Ada.Numerics")
    ("a-nuflra" . "Ada.Numerics.Float_Random")
    ("a-stream" . "Ada.Streams")
    ("a-ststio" . "Ada.Streams.Stream_IO")
    ("a-string" . "Ada.Strings")
    ("a-strfix" . "Ada.Strings.Fixed")
    ("a-strmap" . "Ada.Strings.Maps")
    ("a-strunb" . "Ada.Strings.Unbounded")
    ("a-stwiun" . "Ada.Strings.Wide_Unbounded")
    ("a-textio" . "Ada.Text_IO")
    ("g-comlin" . "GNAT.Command_Line")
    ("g-dirope" . "GNAT.Directory_Operations")
    ("g-socket" . "GNAT.Sockets")
    ("i-c"      . "Interfaces.C")
    ("i-cstrin" . "Interfaces.C.Strings")
    ("interfac" . "Interfaces")
    ("s-stoele" . "System.Storage_Elements")
    )
  "Alist (filename . package name) of GNAT file names for predefined Ada packages.")

(defun gnat-compilation-filter ()
  "Filter to add text properties to secondary file references.
For `compilation-filter-hook'."
  (save-excursion
    (goto-char compilation-filter-start)

    ;; primary references are handled by font-lock functions; see
    ;; `compilation-mode-font-lock-keywords'.
    ;;
    ;; compilation-filter might insert partial lines, or it might insert multiple lines
    (goto-char (line-beginning-position))
    (while (not (eobp))
      ;; We don't want 'next-error' to always go to secondary
      ;; references, so we _don't_ set 'compilation-message text
      ;; property. Instead, we set 'gnat-secondary-error, so
      ;; `gnat-show-secondary-error' will handle it. We also set
      ;; fonts, so the user can see the reference.

      ;; typical secondary references look like:
      ;;
      ;; trivial_productions_test.adb:57:77:   ==> in call to "Get" at \
      ;;    opentoken-token-enumerated-analyzer.ads:88, instance at line 41
      ;;
      ;; c:/foo/bar/lookahead_test.adb:379:14: found type access to "Standard.String" defined at line 379
      ;;
      ;; lookahead_test.ads:23:09: "Name" has been inherited from subprogram at aunit-simple_test_cases.ads:47
      ;;
      ;; lalr.adb:668:37: non-visible declaration at analyzer.ads:60, instance at parser.ads:38
      ;;
      ;; save the file from the primary reference, look for "*.ad?:nn", "at line nnn"

      (let (file)
	(when (looking-at "^\\(\\(.:\\)?[^ :\n]+\\):")
	  (setq file (match-string-no-properties 1)))

	(skip-syntax-forward "^-"); space following primary reference

	(while (search-forward-regexp "\\s-\\(\\([^[:blank:]]+\\.[[:alpha:]]+\\):\\([0-9]+\\):?\\([0-9]+\\)?\\)"
				      (line-end-position) t)

	  (goto-char (match-end 0))
	  (with-silent-modifications
	    (compilation--put-prop 2 'font-lock-face compilation-info-face); file
	    (compilation--put-prop 3 'font-lock-face compilation-line-face); line
	    (compilation--put-prop 4 'font-lock-face compilation-line-face); col
	    (put-text-property
	     (match-beginning 0) (match-end 0)
	     'gnat-secondary-error
	     (list
	      (match-string-no-properties 2); file
	      (string-to-number (match-string-no-properties 3)); line
	      (if (match-string 4)
		  (1- (string-to-number (match-string-no-properties 4)))
		0); column
	      ))
	    ))

	(when (search-forward-regexp "\\(at line \\)\\([0-9]+\\)" (line-end-position) t)
	  (with-silent-modifications
	    (compilation--put-prop 1 'font-lock-face compilation-info-face); "at line" instead of file
	    (compilation--put-prop 2 'font-lock-face compilation-line-face); line
	    (put-text-property
	     (match-beginning 1) (match-end 1)
	     'gnat-secondary-error
	     (list
	      file
	      (string-to-number (match-string-no-properties 2)); line
	      1)); column
	    ))
	(forward-line 1))
      )
    ))

(defun gnat-show-secondary-error ()
  "Show the next secondary file reference in the compilation buffer.
A secondary file reference is defined by text having text
property `gnat-secondary-error', set by
`gnat-compilation-filter'."
  (interactive)

  ;; preserving the current window works only if the frame
  ;; doesn't change, at least on Windows.
  (let ((start-buffer (current-buffer))
	pos item file)
    (when (eq major-mode 'compilation-mode)
      (setq next-error-last-buffer (current-buffer)))
    ;; We use `pop-to-buffer', not `set-buffer', so point is correct
    ;; for the current window showing compilation-last-buffer, and
    ;; moving point in that window works. But that might eat an
    ;; `other-frame-window-mode' prefix, which the user means to apply
    ;; to ’ada-goto-source’ below; disable that temporarily.
    (let ((display-buffer-overriding-action nil))
      (pop-to-buffer next-error-last-buffer nil t)
      (setq pos (next-single-property-change (point) 'gnat-secondary-error))
      (unless pos
	;; probably at end of compilation-buffer, in new compile
	(goto-char (point-min))
	(setq pos (next-single-property-change (point) 'gnat-secondary-error)))

      (when pos
	(setq item (get-text-property pos 'gnat-secondary-error))
	;; file-relative-name handles absolute Windows paths from
	;; g++. Do this in compilation buffer to get correct
	;; default-directory.
	(setq file (file-relative-name (nth 0 item)))

	;; Set point in compilation buffer past this secondary error, so
	;; user can easily go to the next one.
	(goto-char (next-single-property-change (1+ pos) 'gnat-secondary-error)))

      (pop-to-buffer start-buffer nil t);; for windowing history
      )
    (when item
      (wisi-goto-source
       file
       (nth 1 item); line
       (nth 2 item); column
       ))
    ))

(defun gnat-debug-filter ()
  ;; call gnat-compilation-filter with `compilation-filter-start' bound
  (interactive)
  (beginning-of-line)
  (let ((compilation-filter-start (point)))
    (gnat-compilation-filter)))

;;;;; auto fix compilation errors

(defconst gnat-name-regexp "\\(\\(?:\\sw\\|[_.]\\)+\\)")

(defconst gnat-file-name-regexp
  "\\([a-z-_.]+\\)"
  "regexp to extract a file name")

(defconst gnat-quoted-name-regexp
  "\"\\([[:alnum:]_.']+\\)\""
  "regexp to extract the quoted names in error messages")

(defconst gnat-quoted-punctuation-regexp
  "\"\\([,:;=()|]+\\)\""
  "regexp to extract quoted punctuation in error messages")

(defun gnat-misspelling ()
  "Return correct spelling from current compiler error.
Prompt user if more than one."
  ;; wisi-output.adb:115:41: no selector "Productions" for type "RHS_Type" defined at wisi.ads:77
  ;; wisi-output.adb:115:41: invalid expression in loop iterator
  ;; wisi-output.adb:115:42: possible misspelling of "Production"
  ;; wisi-output.adb:115:42: possible misspelling of "Production"
  ;;
  ;; GNAT Community 2021 adds "error: " to the above (a misspelling is never a warning):
  ;; wisi-output.adb:115:41: error: invalid expression in loop iterator
  ;; wisi-output.adb:115:42: error: possible misspelling of "Production"
  ;; wisi-output.adb:115:42: error: possible misspelling of "Production"
  ;;
  ;; column number can vary, so only check the line number
  (save-excursion
    (let* ((start-msg (get-text-property (line-beginning-position) 'compilation-message))
	   (start-line (nth 1 (compilation--message->loc start-msg)))
	   done choices)
      (while (not done)
	(forward-line 1)
	(let ((msg (get-text-property (line-beginning-position) 'compilation-message)))
	  (setq done (or (not msg)
			 (not (equal start-line (nth 1 (compilation--message->loc msg)))))))
	(when (and (not done)
		   (progn
		     (skip-syntax-forward "^-")
		     (forward-char 1)
		     (when (looking-at "error: ")
		       (goto-char (match-end 0)))
		     (looking-at (concat "possible misspelling of " gnat-quoted-name-regexp))))
	  (push (match-string 1) choices)))

      ;; return correct spelling
      (cond
       ((= 0 (length choices))
	nil)

       ((= 1 (length choices))
	(car choices))

       (t ;; multiple choices
	(completing-read "correct spelling: " choices))
       ))))

(defun gnat-qualified ()
  "Return qualified name from current compiler error, if there is one offered."
  (save-excursion
    (forward-line 1)
    (skip-syntax-forward "^ ")
    (when (looking-at " use fully qualified name starting with \\([[:alnum:]_]+\\) to make")
      (match-string 1))
    ))

(defun gnat-file-name-from-ada-name (compiler ada-name)
  (let ((result nil))

    (while (string-match "\\." ada-name)
      (setq ada-name (replace-match "-" t t ada-name)))

    (setq ada-name (downcase ada-name))

    (with-current-buffer (gnat-run-buffer compiler (gnat-compiler-run-buffer-name compiler))
      (gnat-run-no-prj
       (list
	"krunch"
	ada-name
	;; "0" means only krunch GNAT library names
	"0"))

      (goto-char (point-min))
      (when gnat-debug-run (forward-line 1)); skip  cmd
      (setq result (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
      )
    result))

(defun gnat-ada-name-from-file-name (file-name)
  (let* ((ada-name (file-name-sans-extension (file-name-nondirectory file-name)))
	 (predefined (cdr (assoc ada-name gnat-predefined-package-alist))))

    (if predefined
        predefined
      (while (string-match "-" ada-name)
	(setq ada-name (replace-match "." t t ada-name)))
      ada-name)))

(defun gnat-make-package-body (project body-file-name)
  ;; gnatstub always creates the body in the current directory (in the
  ;; process where gnatstub is running); the -o parameter may not
  ;; contain path info. So we bind default-directory here.
  (let* ((compiler (wisi-prj-compiler project))
	 (start-file (buffer-file-name))
	(opts (when (gnat-compiler-gnat-stub-opts compiler)
		(split-string (gnat-compiler-gnat-stub-opts compiler))))
	(cargs (when (gnat-compiler-gnat-stub-cargs compiler)
		(append (list "-cargs") (split-string (gnat-compiler-gnat-stub-cargs compiler)))))
	(process-environment
	 (append
	   (wisi-prj-compile-env project)
	   (wisi-prj-file-env project)
	   (copy-sequence process-environment)))
	)

    ;; Make sure all relevant files are saved to disk.
    (save-some-buffers t)

    (with-current-buffer (gnat-run-buffer compiler (gnat-compiler-run-buffer-name compiler))
      (let ((default-directory (file-name-directory body-file-name)))
	(gnat-run-gnat
	 project
	 "stub"
	 (append opts (list start-file) cargs))

	(find-file body-file-name)
	(indent-region (point-min) (point-max))
	(save-buffer)))
    nil))

(defun gnat-syntax-propertize (start end)
  (goto-char start)
  (save-match-data
    (while (re-search-forward
	    (concat
	     "[^[:alnum:])]\\('\\)\\[[\"a-fA-F0-9]+\"\\]\\('\\)"; 1, 2: non-ascii character literal, not attributes
	     "\\|\\(\\[\"[a-fA-F0-9]+\"\\]\\)"; 3: non-ascii character in identifier
	     )
	    end t)
      (cond
       ((match-beginning 1)
	(put-text-property
	 (match-beginning 1) (match-end 1) 'syntax-table '(7 . ?'))
	(put-text-property
	 (match-beginning 2) (match-end 2) 'syntax-table '(7 . ?')))

       ((match-beginning 3)
	(put-text-property
	 (match-beginning 3) (match-end 3) 'syntax-table '(2 . nil)))
       )
      )))

(defun gnat-insert-unit-name (unit-name)
  "Insert UNIT-NAME at point and capitalize it."
  ;; unit-name is normally gotten from a file-name, and is thus all lower-case.
  (let ((start-point (point))
        search-bound)
    (insert unit-name)
    (setq search-bound (point))
    (insert " ") ; separate from following words, if any, for wisi-case-adjust-identifier
    (goto-char start-point)
    (while (search-forward "." search-bound t)
      (forward-char -1)
      (wisi-case-adjust-identifier)
      (forward-char 1))
    (goto-char search-bound)
    (wisi-case-adjust-identifier)
    (delete-char 1)))

(defun gnat-context-clause-region ()
  (if (fboundp 'ada-context-clause-region);; in ada-mode
      (ada-context-clause-region)
    (user-error "ada-context-clause-region not defined; can't find context clause")))

(defun gnat-extend-with-clause (partial-parent-name child-name)
  "Assuming point is in a selected name, just before CHILD-NAME, add or
extend a with_clause to include CHILD-NAME."
  ;; In GNAT Community 2020, point is before partial-parent-name; in
  ;; earlier gnat, it is after.
  (search-forward partial-parent-name (line-end-position) t)
  (let ((parent-name-end (point)))
    ;; Find the full parent name; skip back to whitespace, then match
    ;; the name forward.
    (skip-syntax-backward "w_.")
    (search-forward-regexp gnat-name-regexp parent-name-end t)
    (let ((parent-name (match-string 0))
	  (context-clause (gnat-context-clause-region)))
      (goto-char (car context-clause))
      (if (search-forward-regexp (concat "^with " parent-name ";") (cdr context-clause) t)
	  ;; found existing 'with' for parent; extend it
	  (progn
	    (forward-char -1) ; skip back over semicolon
	    (insert "." child-name))

	;; not found; we are in a package body, with_clause for parent is in spec.
	;; insert a new one
	(gnat-add-with-clause (concat parent-name "." child-name)))
      )))

(defun gnat-add-use (unit-name)
  (if (fboundp 'ada-fix-add-use);; in ada-mode
      (ada-fix-add-use unit-name)
    (user-error "ada-fix-add-use not defined; add use clause manually")))

(defun gnat-add-use-type (type)
  (if (fboundp 'ada-fix-add-use-type);; in ada-mode
      (ada-fix-add-use-type type)
    (user-error "ada-fix-add-use-type not defined; add use type clause manually")))

(defun gnat-add-with-clause (unit-name)
  (if (fboundp 'ada-fix-add-with-clause);; in ada-mode
      (ada-fix-add-with-clause unit-name)
    (user-error "ada-fix-add-with-clause not defined; add with clause manually")))

(defun gnat-align ()
  (if (fboundp 'ada-align);; in ada-mode
      (ada-align)
    (user-error "ada-align not defined; align manually")))

(defcustom gnat-lsp-server-exec nil
  "Location of an Ada language server.
If non-nil, should be an absolute path to an executable for the
server; this allows specifying a development version. See
`gnat-find-als' for default behaviour."
  :group 'gnat-compiler
  :type 'string)

(defun gnat-find-als (&optional _interactive no-error)
  ;; in eglot 1.8, eglot--connect calls CONTACT with 1 arg
  ;; in devel eglot, eglot--connect calls CONTACT with no args
  "Find the language server executable.
If `gnat-lsp-server-exec' is set, uses that. Otherwise defaults
to AdaCore ada_language_server in `exec-path', then in a gnat
installation found in `exec-path'.  If NO-ERROR, return nil if
server executable not found; otherwise signal user-error."
  (if gnat-lsp-server-exec
      (let ((tmp (locate-file gnat-lsp-server-exec exec-path exec-suffixes)))
        (if (and tmp
            (file-readable-p tmp))
            (setq gnat-lsp-server-exec tmp)
	  (user-error "gnat-lsp-server-exec '%s' not a readable file"
		      gnat-lsp-server-exec)))

    ;; else look for AdaCore ada_language_server
    ;;
    ;; ada_language_server is provided by a GNAT compiler installation, in a
    ;; directory under GNAT/libexec, which is typically not in PATH.
    (let ((gnat (executable-find "gnat"))
	(path exec-path))
    (when gnat
      (setq path (append
		  path
		  (list
		   (expand-file-name
		    "../libexec/gnatstudio/als"
		    (file-name-directory gnat))))))
    (let ((guess (locate-file "ada_language_server" path exec-suffixes)))
      (if guess
	  guess
	(unless no-error
	  (user-error "ada_language_server not found; set gnat-lsp-server-exec?")))))))

;;;;; wisi compiler generic methods

(cl-defmethod wisi-compiler-root-dir ((compiler gnat-compiler))
  "Return the directory containing the project file."
  ;; eglot starts the language server in this directory;
  ;; ada_language_server searches for a project file.
  (and (gnat-compiler-gpr-file compiler)
       (file-name-directory (gnat-compiler-gpr-file compiler))))

(cl-defmethod wisi-compiler-parse-one ((compiler gnat-compiler) project name value)
  (cond
   ((or
     (string= name "ada_project_path") ;; backward compatibility
     (string= name "gpr_project_path"))
    (let ((process-environment
	   (append
	    (wisi-prj-compile-env project)
	    (wisi-prj-file-env project))));; reference, for substitute-in-file-name
      (gnat-prj-add-prj-dir project compiler (expand-file-name (substitute-in-file-name value)))))

   ((string= name "gnat-stub-cargs")
    (setf (gnat-compiler-gnat-stub-cargs compiler) value))

   ((string= name "gnat-stub-opts")
    (setf (gnat-compiler-gnat-stub-opts compiler) value))

   ((string= name "gpr_file")
    ;; The gpr file is parsed in `wisi-compiler-parse-final' below, so
    ;; it sees all file environment vars. We store the absolute gpr
    ;; file name, so we can get the correct default-directory from
    ;; it. Note that gprbuild requires the base name be found on
    ;; GPR_PROJECT_PATH.
    (let* ((process-environment
	    (append
	     (wisi-prj-compile-env project)
	     (wisi-prj-file-env project)));; reference, for substitute-in-file-name
	   (gpr-file (substitute-env-vars value)))

      (if (= (aref gpr-file 0) ?$)
	  ;; An environment variable that was not resolved, possibly
	  ;; because the env var is later defined in the project file;
	  ;; it may be resoved in `wisi-compiler-parse-final'.
	  (setf (gnat-compiler-gpr-file compiler) gpr-file)

	;; else get the absolute path
	(setf (gnat-compiler-gpr-file compiler)
	      (or (locate-file gpr-file (gnat-compiler-project-path compiler))
		  (expand-file-name (substitute-env-vars gpr-file))))))
    t)

   ((string= name "runtime")
    (setf (gnat-compiler-runtime compiler) value))

   ((string= name "target")
    (setf (gnat-compiler-target compiler) value))

   ))

(cl-defmethod wisi-compiler-parse-final ((compiler gnat-compiler) project prj-file-name)
  (setf (gnat-compiler-run-buffer-name compiler) (gnat-run-buffer-name prj-file-name))

  (let ((gpr-file (gnat-compiler-gpr-file compiler)))
    (if gpr-file
	(progn
	  (when (= (aref gpr-file 0) ?$)
	    ;; An environment variable that was not resolved earlier,
	    ;; because the env var is defined in the project file.
	    (let ((process-environment
		   (append
		    (wisi-prj-compile-env project)
		    (wisi-prj-file-env project))));; reference, for substitute-in-file-name

	      (setq gpr-file
		    (or
		     (locate-file (substitute-env-vars gpr-file)
				  (gnat-compiler-project-path compiler))
		     (expand-file-name (substitute-env-vars gpr-file))))

	      (setf (gnat-compiler-gpr-file compiler) gpr-file)))

	  (gnat-parse-gpr gpr-file project)
	  )

    ;; else add the compiler libraries to project.source-path
    (gnat-get-paths project :ignore-prj-paths nil)
    )))

(defvar ada-syntax-propertize-hook) ;; actually declared in ada-core.el in ada-mode package

(cl-defmethod wisi-compiler-select-prj :after ((_compiler gnat-compiler) _project)
  (add-to-list 'completion-ignored-extensions ".ali") ;; gnat library files
  (setq compilation-error-regexp-alist
	;; gnu matches the summary line from make:
	;; make: *** [rules.make:143: wisitoken-bnf-generate.exe] Error 4
	;; which is just annoying, but should be up to the user.
	'(gnu)
	)
  (add-hook 'compilation-filter-hook #'gnat-compilation-filter)
  (add-hook 'ada-syntax-propertize-hook #'gnat-syntax-propertize)

  ;; We should call `syntax-ppss-flush-cache' here, to force ppss with
  ;; the new hook function. But that must be done in all ada-mode
  ;; buffers, which is tedious. So we're ignoring it until it becomes
  ;; a problem; normally, the compiler is selected before any Ada
  ;; files are visited, so it's not an issue.
  )

(cl-defmethod wisi-compiler-deselect-prj :before ((_compiler gnat-compiler) _project)
  (setq completion-ignored-extensions (delete ".ali" completion-ignored-extensions))
  (setq compilation-error-regexp-alist (mapcar #'car compilation-error-regexp-alist-alist))
  (remove-hook 'ada-syntax-propertize-hook #'gnat-syntax-propertize)
  (remove-hook 'compilation-filter-hook #'gnat-compilation-filter))

(cl-defmethod wisi-compiler-prj-path ((compiler gnat-compiler))
    (gnat-compiler-project-path compiler)
    )

(cl-defmethod wisi-compiler-fix-error ((_compiler gnat-compiler) source-buffer)
  (let ((start-pos (point))
	message-column
	result)
    ;; Move to start of error message text. GNAT Community 2021 puts
    ;; warning: | error: after the file:line:column; earlier compilers
    ;; only put "warning: ".
    ;;
    ;; test_incremental.adb:657:20: error: "Checks" not declared in "WisiToken"
    (skip-syntax-forward "^-") ;; file:line:column
    (forward-char 1)
    (when (looking-at "warning: \\|error: ")
      (goto-char (match-end 0)))
    (setq message-column (current-column))

    ;; recognize it, handle it
    (setq
     result
     (unwind-protect
	 (cond
	  ;; It is tempting to define an alist of (MATCH . ACTION), but
	  ;; that is too hard to debug
	  ;;
	  ;; This list will get long, so let's impose some order.
	  ;;
	  ;; First expressions that start with a named regexp,
	  ;; alphabetical by variable name and following string.
	  ;;
	  ;; Then expressions that start with a string, alphabetical by string.
	  ;;
	  ;; Then style errors.

	  ((looking-at (concat gnat-quoted-name-regexp " is not a component of "))
	 	   (save-excursion
	     (let ((child-name (match-string 1))
		   (correct-spelling (gnat-misspelling)))
	       (setq correct-spelling (match-string 1))
	       (pop-to-buffer source-buffer)
	       (search-forward child-name)
	       (replace-match correct-spelling))
	     t))

	  ((looking-at (concat gnat-quoted-name-regexp " is not visible"))
	   (let* ((done nil)
		  (err-msg (get-text-property (line-beginning-position) 'compilation-message))
		  (file-line-struct err-msg)
		  pos choices unit-name)
	     ;; next line may contain a reference to where ident is
	     ;; defined; if present, it will have been marked by
	     ;; gnat-compilation-filter:
	     ;;
    ;; gnatquery.adb:255:13: error: "Has_Element" is not visible
    ;; gnatquery.adb:255:13: error: non-visible declaration at a-convec.ads:68, instance at gnatcoll-arg_lists.ads:157
    ;; gnatquery.adb:255:13: error: non-visible declaration at a-coorse.ads:62, instance at gnatcoll-xref.ads:912
    ;; gnatquery.adb:255:13: error: non-visible declaration at a-coorse.ads:62, instance at gnatcoll-xref.ads:799
    ;; gnatquery.adb:255:13: error: non-visible declaration at gnatcoll-xref.ads:314
	     ;;
	     ;; or the next line may contain "multiple use clauses cause hiding"
	     ;;
	     ;; the lines after that may contain alternate matches;
	     ;; collect all, let user choose.
	     ;;
	     ;; However, a line that contains 'gnat-secondary-error may be from the next error message:
	     ;; parser_no_recover.adb:297:60: no selector "Tree" for type "Parser_State" defined at lists.ads:96
	     (forward-line 1)
	     (when (looking-at ".* multiple use clauses cause hiding")
	       (forward-line 1))
	     (while (not done)
	       (let ((limit (1- (line-end-position))))
		 ;; 1- because next compilation error is at next line beginning
		 (setq done (not
			     (and
			      (equal file-line-struct err-msg) ;; same error message?
			      (setq pos (next-single-property-change (point) 'gnat-secondary-error nil limit))
			      (<= pos limit))))
		 (when (not done)
		   (let* ((item (get-text-property pos 'gnat-secondary-error))
			  (unit-file (nth 0 item))
			  (choice (gnat-ada-name-from-file-name unit-file)))
		     (unless (member choice choices) (push choice choices))
		     (goto-char (1+ pos))
		     (goto-char (1+ (next-single-property-change (point) 'gnat-secondary-error nil limit)))
		     (when (eolp)
		       (forward-line 1)
		       (setq file-line-struct (get-text-property (point) 'compilation-message)))
		     ))
		 ))

	     (setq unit-name
		   (cond
		    ((= 0 (length choices)) nil)
		    ((= 1 (length choices)) (car choices))
		    (t ;; multiple choices
		     (completing-read "package name: " choices))))

	     (when unit-name
	       (pop-to-buffer source-buffer)
	       ;; We either need to add a with_clause for a package, or
	       ;; prepend the package name here (or add a use clause, but I
	       ;; don't want to do that automatically).
	       ;;
	       ;; If we need to add a with_clause, unit-name may be only
	       ;; the prefix of the real package name, but in that case
	       ;; we'll be back after the next compile; no way to get the
	       ;; full package name (without the function/type name) now.
	       ;; Note that we can't use gnat find, because the code
	       ;; doesn't compile.
	       (cond
		((looking-at (concat unit-name "\\."))
		 (gnat-add-with-clause unit-name))
		(t
		 (gnat-insert-unit-name unit-name)
		 (insert ".")))
	       t) ;; success, else nil => fail
	     ))

	  ((or (looking-at (concat gnat-quoted-name-regexp " is undefined"))
	       (looking-at (concat gnat-quoted-name-regexp " is not a predefined library unit")))
	   ;; We either need to add a with_clause for a package, or
	   ;; something is spelled wrong.
	   (save-excursion
	     (let ((unit-name (match-string 1))
		   (correct-spelling (gnat-misspelling)))
	       (if correct-spelling
		   (progn
		     (pop-to-buffer source-buffer)
		     (search-forward unit-name)
		     (replace-match correct-spelling))

		 ;; else assume missing with
		 (pop-to-buffer source-buffer)
		 (gnat-add-with-clause unit-name))))
	   t)

	  ((looking-at (concat gnat-quoted-name-regexp " not declared in " gnat-quoted-name-regexp))
	   (save-excursion
	     (let ((child-name (match-string 1))
		   (partial-parent-name (match-string 2))
		   (correct-spelling (gnat-misspelling))
		   (qualified (gnat-qualified)))
	       (cond
		(correct-spelling
		 (pop-to-buffer source-buffer)
		 (search-forward child-name)
		 (replace-match correct-spelling))

		(qualified
		 (pop-to-buffer source-buffer)
		 (search-forward child-name)
		 (skip-syntax-backward "w_.")
		 (insert qualified "."))

		(t
		 ;; else guess that "child" is a child package, and extend the with_clause
		 (pop-to-buffer source-buffer)
		 (gnat-extend-with-clause partial-parent-name child-name))))
	   t))

	  ((looking-at (concat gnat-quoted-punctuation-regexp
			       " should be "
			       gnat-quoted-punctuation-regexp))
	   (let ((bad (match-string-no-properties 1))
		 (good (match-string-no-properties 2)))
	     (pop-to-buffer source-buffer)
	     (looking-at bad)
	     (delete-region (match-beginning 0) (match-end 0))
	     (insert good))
	   t)

;;;; strings
	  ((looking-at (concat "aspect \"" gnat-name-regexp "\" requires 'Class"))
	   (pop-to-buffer source-buffer)
	   (forward-word 1)
	   (insert "'Class")
	   t)

	  ((looking-at (concat "\"end " gnat-name-regexp ";\" expected"))
	   (let ((expected-name (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (if (looking-at (concat "end " gnat-name-regexp ";"))
		 (progn
		   (goto-char (match-end 1))   ; just before ';'
		   (delete-region (match-beginning 1) (match-end 1)))
	       ;; else we have just 'end;'
	       (forward-word 1)
	       (insert " "))
	     (insert expected-name))
	   t)

	  ((looking-at (concat "\"end loop " gnat-name-regexp ";\" expected"))
	   (let ((expected-name (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (if (looking-at (concat "end loop " gnat-name-regexp ";"))
		 (progn
		   (goto-char (match-end 1))   ; just before ';'
		   (delete-region (match-beginning 1) (match-end 1)))
	       ;; else we have just 'end loop;'
	       (forward-word 2)
	       (insert " "))
	     (insert expected-name))
	   t)

	  ((looking-at "expected an access type")
	   (progn
	     (set-buffer source-buffer)
	     (backward-char 1)
	     (when (looking-at "\\.all")
	       (delete-char 4)
	       t)))

	  ((looking-at (concat "expected \\(private \\)?type " gnat-quoted-name-regexp))
	   (forward-line 1)
	   (move-to-column message-column)
	   (cond
	    ((looking-at "found procedure name")
	     (pop-to-buffer source-buffer)
	     (forward-word 1)
	     (insert "'Access")
	     t)
	    ((looking-at "found type access")
	     (pop-to-buffer source-buffer)
	     (if (looking-at "'Access")
		 (kill-word 1)
	       (forward-symbol 1)
	       (insert ".all"))
	     t)
	    ((looking-at "found type .*_Access_Type")
	     ;; assume just need '.all'
	     (pop-to-buffer source-buffer)
	     (forward-word 1)
	     (insert ".all")
	     t)
	    ))

	  ((looking-at "extra \".\" ignored")
	   (set-buffer source-buffer)
	   (delete-char 1)
	   t)

	  ((looking-at (concat "keyword " gnat-quoted-name-regexp " expected here"))
	   (let ((expected-keyword (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (insert " " expected-keyword))
	   t)

	  ((looking-at "\\(?:possible \\)?missing \"with \\([[:alnum:]_.]+\\);")
	   ;; also 'possible missing "with Ada.Text_IO; use Ada.Text_IO"' - ignoring the 'use'
	   (let ((package-name (match-string-no-properties 1)))
	     (pop-to-buffer source-buffer)
	     ;; Could check if prefix is already with'd, extend
	     ;; it. But that's not easy. This message only occurs for
	     ;; compiler-provided Ada and GNAT packages.
	     (gnat-add-with-clause package-name))
	   t)

	  ;; must be after above
	  ;;
	  ;; missing "end;" for "begin" at line 234
	  ((looking-at "missing \"\\([^ ]+\\)\"")
	   (let ((stuff (match-string-no-properties 1)))
	     (set-buffer source-buffer)
	     (insert (concat stuff)));; if missing ")", don't need space; otherwise do?
	   t)

	  ((looking-at (concat "\\(?:possible \\)?misspelling of " gnat-quoted-name-regexp))
	   (let ((expected-name (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (looking-at gnat-name-regexp)
	     (delete-region (match-beginning 1) (match-end 1))
	     (insert expected-name))
	   t)

	  ((looking-at "No legal interpretation for operator")
	   (forward-line 1)
	   (move-to-column message-column)
	   (looking-at (concat "use clause on " gnat-quoted-name-regexp))
	   (let ((package (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (gnat-add-use package))
	   t)

	  ((looking-at (concat "no selector " gnat-quoted-name-regexp))
	   ;; Check next line for spelling error.
	   (save-excursion
	     (let ((unit-name (match-string 1))
		   (correct-spelling (gnat-misspelling)))
	       (when correct-spelling
		 (pop-to-buffer source-buffer)
		 (search-forward unit-name)
		 (replace-match correct-spelling)
		 t))))

	  ((looking-at (concat "operator for \\(?:private \\)?type " gnat-quoted-name-regexp
			       "\\(?: defined at " gnat-file-name-regexp "\\)?"))
	   (let ((type (match-string 1))
		 (package-file (match-string 2))
		 ;; IMPROVEME: we'd like to handle ", instance at
		 ;; <file:line:column>", but gnatcoll.xref does not
		 ;; support looking up an entity by location alone; it
		 ;; requires the name, and this error message does not
		 ;; give the name of the instance. When we implement
		 ;; adalang xref, or if the error message improves,
		 ;; try again.
		 )
	     (when package-file
	       (setq type (concat
			   (gnat-ada-name-from-file-name package-file)
			   "." type)))
	     (pop-to-buffer source-buffer)
	     (gnat-add-use-type type)
	   t))

	  ((looking-at "package \"Ada\" is hidden")
	   (pop-to-buffer source-buffer)
	   (forward-word -1)
	   (insert "Standard.")
	   t)

	  ((looking-at "parentheses required for unary minus")
	   (set-buffer source-buffer)
	   (insert "(")
	   (forward-word 1)
	   (insert ")")
	   t)

	  ((looking-at "prefix of dereference must be an access type")
	   (pop-to-buffer source-buffer)
	   ;; point is after '.' in '.all'
	   (delete-region (- (point) 1) (+ (point) 3))
	   t)

;;;; warnings
	  ((looking-at (concat gnat-quoted-name-regexp " is already use-visible"))
	   ;; just delete the 'use'; assume it's on a line by itself.
	   (pop-to-buffer source-buffer)
	   (beginning-of-line)
	   (delete-region (point) (progn (forward-line 1) (point)))
	   t)

	  ((looking-at (concat gnat-quoted-name-regexp " is not modified, could be declared constant"))
	   (pop-to-buffer source-buffer)
	   (search-forward ":")
	   (forward-comment (- (point-max) (point)))
	   ;; "aliased" must be before "constant", so check for it
	   (when (looking-at "aliased")
	     (forward-word 1)
	     (forward-char 1))
	   (insert "constant ")
	   t)

	  ((looking-at (concat "constant " gnat-quoted-name-regexp " is not referenced"))
	   (let ((constant (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (end-of-line)
	     (newline-and-indent)
	     (insert "pragma Unreferenced (" constant ");"))
	   t)

	  ((looking-at (concat "formal parameter " gnat-quoted-name-regexp " is not referenced"))
	   (let ((param (match-string 1))
		 cache)
	     (pop-to-buffer source-buffer)
	     ;; Point is in a subprogram parameter list;
	     ;; ada-goto-declarative-region-start goes to the package,
	     ;; not the subprogram declarative_part (this is a change
	     ;; from previous wisi versions).
	     (setq cache (wisi-goto-statement-start))
	     (while (not (eq 'IS (wisi-cache-token cache)))
	       (forward-sexp)
	       (setq cache (wisi-get-cache (point))))
	     (forward-word)
	     (newline-and-indent)
	     (insert "pragma Unreferenced (" param ");"))
	   t)

	  ((looking-at (concat "formal parameter " gnat-quoted-name-regexp " is not modified"))
	   (let ((mode-regexp "\"\\([in out]+\\)\"")
		 new-mode
		 old-mode)
	     (forward-line 1)
	     (search-forward-regexp
	      (concat "mode could be " mode-regexp " instead of " mode-regexp))
	     (setq new-mode (match-string 1))
	     (setq old-mode (match-string 2))
	     (pop-to-buffer source-buffer)
	     (search-forward old-mode)
	     (replace-match new-mode)
	     (gnat-align)
	     )
	   t)

	  ((looking-at (concat "variable " gnat-quoted-name-regexp " is not referenced"))
	   (let ((param (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (forward-sexp);; end of declaration
	     (forward-char);; skip semicolon
	     (newline-and-indent)
	     (insert "pragma Unreferenced (" param ");"))
	   t)

	  ((or
	    (looking-at (concat "no entities of " gnat-quoted-name-regexp " are referenced"))
	    (looking-at (concat "unit " gnat-quoted-name-regexp " is never instantiated"))
	    (looking-at (concat "renamed constant " gnat-quoted-name-regexp " is not referenced"))
	    (looking-at "redundant with clause"))
	   ;; just delete the declaration; assume it's on a line by itself.
	   (pop-to-buffer source-buffer)
	   (beginning-of-line)
	   (delete-region (point) (progn (forward-line 1) (point)))
	   t)

	  ((looking-at (concat "variable " gnat-quoted-name-regexp " is assigned but never read"))
	   (let ((param (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (wisi-goto-statement-end) ;; leaves point before semicolon
	     (forward-char 1)
	     (newline-and-indent)
	     (insert "pragma Unreferenced (" param ");"))
	   t)

	  ((looking-at (concat "unit " gnat-quoted-name-regexp " is not referenced"))
	   ;; just delete the 'with'; assume it's on a line by itself.
	   (pop-to-buffer source-buffer)
	   (beginning-of-line)
	   (delete-region (point) (progn (forward-line 1) (point)))
	   t)

	  ((looking-at (concat "use clause for \\(package\\|type\\|private type\\) " gnat-quoted-name-regexp
			       " \\(defined at\\|from instance at\\|has no effect\\)"))
	   ;; delete the 'use'; assume it's on a line by itself.
	   (pop-to-buffer source-buffer)
	   (beginning-of-line)
	   (delete-region (point) (progn (forward-line 1) (point)))
	   t)

;;;; style errors
	  ((or (looking-at "(style) \".*\" in wrong column")
	       (looking-at "(style) this token should be in column"))
	   (set-buffer source-buffer)
	   (funcall indent-line-function)
	   t)

	  ((looking-at "(style) bad capitalization, mixed case required")
	   (set-buffer source-buffer)
	   (forward-word)
	   (wisi-case-adjust-identifier)
	   t)

	  ((looking-at (concat "(style) bad casing of " gnat-quoted-name-regexp))
	   (let ((correct (match-string-no-properties 1))
		 end)
	     ;; gnat leaves point on first bad character, but we need to replace the whole word
	     (set-buffer source-buffer)
	     (skip-syntax-backward "w_")
	     (setq end (point))
	     (skip-syntax-forward "w_")
	     (delete-region (point) end)
	     (insert correct))
	   t)

	  ((or
	    (looking-at "(style) bad column")
	    (looking-at "(style) bad indentation")
	    (looking-at "(style) incorrect layout"))
	   (set-buffer source-buffer)
	   (funcall indent-line-function)
	   t)

	  ((looking-at "(style) \"exit \\(.*\\)\" required")
	   (let ((name (match-string-no-properties 1)))
	     (set-buffer source-buffer)
	     (forward-word 1)
	     (insert (concat " " name))
	   t))

	  ((looking-at "(style) misplaced \"then\"")
	   (set-buffer source-buffer)
	   (delete-indentation)
	   t)

         ((looking-at "(style) missing \"overriding\" indicator")
          (set-buffer source-buffer)
          (cond
           ((looking-at "\\(procedure\\)\\|\\(function\\)")
            (insert "overriding ")
	    t)
           (t
            nil)))

	  ((looking-at "(style) reserved words must be all lower case")
	   (set-buffer source-buffer)
	   (downcase-word 1)
	   t)

	  ((looking-at "(style) space not allowed")
	   (set-buffer source-buffer)
	   ;; Error places point on space. More than one trailing space
	   ;; should be fixed by delete-trailing-whitespace in
	   ;; before-save-hook, once the file is modified.
	   (delete-char 1)
	   t)

	  ((looking-at "(style) space required")
	   (set-buffer source-buffer)
	   (insert " ")
	   t)
	  )));; end of setq unwind-protect cond
    (if result
	t
      (goto-char start-pos)
      nil)
    ))

;;;; Initialization

(add-to-list 'wisi-prj-file-extensions  "gpr")
(add-to-list 'wisi-prj-parser-alist  '("gpr" . gnat-parse-gpr-1))

(add-to-list
 'compilation-error-regexp-alist-alist
 '(gnat
   ;; typical:
   ;;   cards_package.adb:45:32: expected private type "System.Address"
   ;;
   ;; with full path Source_Reference pragma :
   ;;   d:/maphds/version_x/1773/sbs-abi-dll_lib.ads.gp:39:06: file "interfaces_c.ads" not found
   ;;
   ;; gnu cc1: (gnatmake can invoke the C compiler)
   ;;   foo.c:2: `TRUE' undeclared here (not in a function)
   ;;   foo.c:2 : `TRUE' undeclared here (not in a function)
   ;;
   ;; we can't handle secondary errors here, because a regexp can't distinguish "message" from "filename"
   "^\\(\\(.:\\)?[^ :\n]+\\):\\([0-9]+\\)\\s-?:?\\([0-9]+\\)?" 1 3 4))

(eval-after-load 'ada-mode '(add-hook 'ada-mode-hook #'gnatprep-setup))

(provide 'gnat-compiler)

;;; gnat-compiler.el ends here
