;;; wisi-run-indent-test.el --- utils for automating indentation and casing tests  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 - 2022  Free Software Foundation, Inc.
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

(require 'wisi-prj)
(require 'wisi-process-parse)

;; user can set these to t in an EMACSCMD
(defvar skip-cmds nil)
(defvar skip-reindent-test nil)
(defvar skip-recase-test nil)
(defvar skip-write nil)

(defvar save-parser-log nil
  "If non-nil, a file name telling where to save wisi parser transaction log")

(defvar save-edited-text nil
  "If non-nil, a file name telling where to save wisi parser edited
text, after each edit in an incremental parse, and before each partial parse.")

(defun test-in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun wisi-wait-parser()
  (while (wisi-process--parser-busy wisi-parser-shared)
    (accept-process-output nil wisi-process-time-out)))

(defvar test-face-wait-fn nil
  "Function to call after `font-lock-ensure' to wait for face to actually be set.")

(defun test-face (token face)
  "Test if all of TOKEN in next code line has FACE.
FACE may be a list."
  (save-excursion
    (when (test-in-comment-p)
      (beginning-of-line); forward-comment doesn't move if inside a comment!
      (forward-comment (point-max)))
    (condition-case nil
	(search-forward token (line-end-position 5))
      (error
       (error "can't find '%s'" token)))

    (when (not skip-recase-test) ;; should be t when wisi-disable-face is t
      (let ((token (match-string-no-properties 0))
	    (test-pos (match-beginning 0)))

	(when wisi-parser-shared
	  ;; it may be busy doing initial parse in another file opened by an xref command.
	  (wisi-wait-parser)
	  (wisi-validate-cache (line-beginning-position) (line-end-position) nil 'face))

	(font-lock-ensure (line-beginning-position) (line-end-position))

	(when test-face-wait-fn
	  (funcall test-face-wait-fn))

	;; We don't use face-at-point, because it doesn't respect
	;; font-lock-face set by the parser! And we want to check for
	;; conflicts between font-lock-keywords and the parser.

	;; font-lock-keywords sets 'face property, parser sets 'font-lock-face.

	;; In emacs < 27, if we use (get-text-property (point) 'face), we
	;; also get 'font-lock-face, but not vice-versa. So we have to use
	;; text-properties-at to check for both.
	(let ((props (text-properties-at test-pos))
	      key
	      token-face)
	  (cond
	   ((plist-get props 'font-lock-face)
	    (setq key 'font-lock-face)
	    (setq token-face (plist-get props 'font-lock-face)))

	   ((plist-get props 'face)
	    (setq key 'face)
	    (setq token-face (plist-get props 'face)))
	   )

	  (when (and (memq 'font-lock-face props)
		     (memq 'face props))
	    (describe-text-properties test-pos)
	    (error "mixed font-lock-keyword and parser faces for '%s'" token))

	  (when (text-property-not-all test-pos (+ test-pos (length token)) key token-face)
       	    (error "mixed faces, expecting %s for '%s'" face token))

	  (unless (equal token-face face)
	    (error "found face %s, expecting %s for '%s'" token-face face token))
	  )))))

(defun test-face-1 (search token face)
  "Move to end of comment, search for SEARCH, call `test-face'."
  (save-excursion
    (when (test-in-comment-p)
      (beginning-of-line); forward-comment doesn't move if inside a comment!
      (forward-comment (point-max)))
    (search-forward search)
    (test-face token face)
    ))

(defun test-left-fringe-mark (search-string present)
  "Search for SEARCH-STRING; if PRESENT is non-nil, assert that
left fringe mark is present on that line.  Otherwise, assert it
is not present."
  (save-excursion
    (when (test-in-comment-p)
      (beginning-of-line); forward-comment doesn't move if inside a comment!
      (forward-comment (point-max)))
    (search-forward search-string)
    (let* ((mark (overlays-in (line-end-position) (1+ (line-end-position)))))
      (cond
       (present
	(unless mark
	  (error "expecting left fringe mark line %d" (line-number-at-pos))))

       ((not present)
	(when mark
	  (error "expecting no left fringe mark line %d" (line-number-at-pos))))
       ))))

(defun test-cache-class (token class)
  "Test if TOKEN in next code line has wisi-cache with class CLASS."
  (save-excursion
    (wisi-validate-cache (line-beginning-position 0) (line-end-position 3) nil 'navigate)
    (beginning-of-line); forward-comment doesn't move if inside a comment!
    (forward-comment (point-max))
    (condition-case nil
	(search-forward token (line-end-position 5))
      (error
       (error "can't find '%s'" token)))

    (let ((cache (get-text-property (match-beginning 0) 'wisi-cache)))

      (unless cache (error "no cache"))
      (unless (eq (wisi-cache-class cache) class)
	(error "expecting class %s, found '%s'" class (wisi-cache-class cache)))
    )))

(defun test-cache-containing (containing contained)
  "Test if CONTAINING in next code line has wisi-cache that contains CONTAINED."
  (save-excursion
    (wisi-validate-cache (line-beginning-position 0) (line-end-position 3) nil 'navigate)
    (beginning-of-line)
    (forward-comment (point-max))
    (let (containing-pos contained-cache)
      (search-forward containing (line-end-position 5))
      (setq containing-pos (match-beginning 0))

      (search-forward contained (line-end-position 5))
      (setq contained-cache (get-text-property (match-beginning 0) 'wisi-cache))

      (unless contained-cache (error "no cache on %s" contained))
      (unless (= containing-pos (wisi-cache-containing contained-cache))
	(error "expecting %d, got %d" containing-pos (wisi-cache-containing contained-cache)))
    )))

(defvar test-refactor-markers nil
  "Stores positions altered by `test-refactor-1' for `test-refactor-2'.
Each item is a list (ACTION PARSE-BEGIN PARSE-END EDIT-BEGIN)")

(defun test-refactor-1 (action inverse-action search-string refactor-string)
  (beginning-of-line)
  (forward-comment (point-max)) ;; forward-comment does not work from inside comment
  (when search-string
    (search-forward search-string (line-end-position 7)))
  (wisi-validate-cache (line-end-position -7) (line-end-position 7) t 'navigate)
  (search-forward refactor-string (line-end-position 7))
  (let ((edit-begin (match-beginning 0)))
    (push (list
	   inverse-action
	   (copy-marker edit-begin nil))
	  test-refactor-markers)
    (wisi-refactor wisi-parser-shared action edit-begin)
    ))

(defun test-refactor-inverse ()
  "Reverse refactors done by recent set of `test-refactor-1'."
  ;; Force parse of forward refactor for partial parse
  (wisi-validate-cache (line-end-position -7) (line-end-position 7) t 'navigate)
  (save-excursion
    (dolist (item test-refactor-markers)
      (wisi-refactor wisi-parser-shared
		     (nth 0 item)
		     (marker-position (nth 1 item))))
    (setq test-refactor-markers nil)))

(defun wisi-test-save-log-1 (buffer log-file-name)
    (with-current-buffer buffer
      (message "saving parser transaction log '%s' to '%s'" (buffer-name) log-file-name)
      (write-region nil nil log-file-name)))

(defun wisi-test-save-log ()
  (interactive)
  (cond
   ((stringp save-parser-log)
    (when (buffer-live-p (wisi-parser-transaction-log-buffer wisi-parser-shared))
      (wisi-test-save-log-1 (wisi-parser-transaction-log-buffer wisi-parser-shared) save-parser-log)))

   (t ;; save-parser-log is a list of (LOG-BUFFER-NAME LOG-FILE-NAME)
    (dolist (item save-parser-log)
      (wisi-test-save-log-1 (get-buffer (nth 0 item)) (nth 1 item))))
    ))

(defvar eglot-send-changes-idle-time)
(declare-function jsonrpc--debug "jsonrpc.el")

(defun run-test-here ()
  "Run an indentation and casing test on the current buffer."
  (interactive)
  (when wisi-incremental-parse-enable
    ;; wait for the parser to finish the initial parse
    (wisi-wait-parser))

  (condition-case err
      (progn
	(setq indent-tabs-mode nil)

	;; for test-face
	(setq jit-lock-context-time 0.0)
	(setq-local font-lock-ensure-function 'jit-lock-fontify-now) ;; it's not at all clear what's resetting this
	(setq-local eglot-send-changes-idle-time 0.0) ;; FIXME: did not help test-face

	;; Test files use wisi-prj-select-cached to parse and select a project file.
	(setq project-find-functions (list #'wisi-prj-current-cached))

	(unless (memq 'eglot-xref-backend xref-backend-functions)
	  (setq xref-backend-functions (list #'wisi-prj-xref-backend)))

	(when (stringp save-edited-text)
	  (wisi-process-parse-save-text wisi-parser-shared save-edited-text t))

	(let ((error-count 0)
	      (error-lines '())
	      (pass-count 0)
	      (test-buffer (current-buffer))
	      cmd-line
	      last-result last-cmd expected-result force-fail)
	  ;; Look for EMACS* comments in the file:
	  ;;
	  ;; EMACSCMD: <form>
	  ;;    Executes the lisp form inside a save-excursion, saves the result as a lisp object.
	  ;;
	  ;; EMACSRESULT: <form>
	  ;;    point is moved to end of line, <form> is evaluated inside
	  ;;    save-excursion and compared (using `equal') with the result
	  ;;    of the previous EMACSCMD, and the test fails if they don't
	  ;;    match.
	  ;;
	  ;; EMACSRESULT_START:<first list element>
	  ;; EMACSRESULT_ADD:  <list element>
	  ;; EMACSRESULT_FINISH:
	  ;;    build a list, compare it to the result of the previous EMACSCMD.
	  ;;
	  ;; EMACS_SKIP_UNLESS: <form>
	  ;;   skip entire test if form evals nil
	  ;;
	  ;; EMACSDEBUG: <form>
	  ;;    Eval form, display result. Also used for setting breakpoint.

	  (goto-char (point-min))
	  (while (and (not skip-cmds)
		      (re-search-forward (concat comment-start "EMACS\\([^:]+\\):") nil t))
	    (cond
	     ((string= (match-string 1) "CMD")
	      (looking-at ".*$")
	      (setq cmd-line (line-number-at-pos)
		    last-cmd (match-string-no-properties 0)
		    force-fail nil)
	      (let ((msg (format "%s:%d: test %s" (buffer-file-name) cmd-line last-cmd)))
		(when wisi-parser-shared (wisi-parse-log-message wisi-parser-shared msg))
		(message "%s" msg)
		(when (and (fboundp 'eglot-current-server) (eglot-current-server))
		  (jsonrpc--debug (eglot-current-server) msg))
		(save-excursion
		  (setq last-result
			(condition-case-unless-debug err
			    (prog1
			      (eval (car (read-from-string last-cmd)) t)
			      (when (> wisi-debug 1)
			        (setq msg (concat msg " ... done"))
                                (when wisi-parser-shared (wisi-parse-log-message wisi-parser-shared msg))
                                (message msg)))
			  ((error wisi-parse-error)
			   (setq error-count (1+ error-count))
			   (push cmd-line error-lines)
			   (setq msg (concat msg " ... signaled"))
			   (setq force-fail t)
			   (when wisi-parser-shared (wisi-parse-log-message wisi-parser-shared msg))
			   (message msg)
			   (setq msg (format "... %s: %s" (car err) (cdr err)))
			   (when wisi-parser-shared (wisi-parse-log-message wisi-parser-shared msg))
			   (when (and (fboundp 'eglot-current-server) (eglot-current-server))
			     (jsonrpc--debug (eglot-current-server) msg))
			   (message msg)
			   nil)))
		  ))
		;; save-excursion does not preserve mapping of buffer to
		;; window, but some tests depend on that. For example,
		;; execute-kbd-macro doesn’t work properly if current buffer
		;; is not visible.
		(pop-to-buffer test-buffer))

	     ((string= (match-string 1) "RESULT")
	      (looking-at ".*$")
	      (setq expected-result
	            (save-excursion
	              (end-of-line 1)
	              (eval (car (read-from-string (match-string 0))) t)))
	      (if (and (not force-fail)
		       (equal expected-result last-result))
		  (let ((msg (format "test passes %s:%d:\n" (buffer-file-name) (line-number-at-pos))))
		    (setq pass-count (1+ pass-count))
		    (when wisi-parser-shared (wisi-parse-log-message wisi-parser-shared msg))
		    (message msg))

		(setq error-count (1+ error-count))
		(push (line-number-at-pos) error-lines)

		(let ((msg (concat
			    (format "error: %s:%d:\n" (buffer-file-name) (line-number-at-pos))
			    (if force-fail
				"... failed due to signal"
			      (format "... result of '%s' does not match.\n... Got    '%s',\n... expect '%s'"
				      last-cmd
				      last-result
				      expected-result)))))
		  (when wisi-parser-shared (wisi-parse-log-message wisi-parser-shared msg))
		  (message "%s" msg))))

	     ((string= (match-string 1) "RESULT_START")
	      (looking-at ".*$")
	      (let ((val (save-excursion
		          (end-of-line 1)
		          (eval (car (read-from-string (match-string 0))) t))))
		(when val
		  (setq expected-result (list val)))))

	     ((string= (match-string 1) "RESULT_ADD")
	      (looking-at ".*$")
	      (let ((val (save-excursion
			   (end-of-line 1)
			   (eval (car (read-from-string (match-string 0))) t))))
		(when val
		  (setq expected-result (append expected-result (list val))))))

	     ((string= (match-string 1) "RESULT_FINISH")
	      (unless (equal (length expected-result) (length last-result))
		(setq error-count (1+ error-count))
		(push (line-number-at-pos) error-lines)
		;; this is used for gpr-query tests, not parser tests,
		;; so we don't write to the parser log.
		(message
		 (concat
		  (format "error: %s:%d:\n" (buffer-file-name) (line-number-at-pos))
		  (format "Length of result of '%s' does not match.\nGot    '%s',\nexpect '%s'"
			  last-cmd
			  (length last-result)
			  (length expected-result)))))

	      (let ((i 0))
		(while (< i (length expected-result))
		  (unless (equal (nth i expected-result) (nth i last-result))
		    (setq error-count (1+ error-count))
		    (push (line-number-at-pos) error-lines)
		    (message
		     (concat
		      (format "error: %s:%d:\n" (buffer-file-name) (line-number-at-pos))
		      (format "Nth (%d) result of '%s' does not match.\nGot    '%s',\nexpect '%s'"
			      i
			      last-cmd
			      (nth i last-result)
			      (nth i expected-result))
		      )))
		  (setq i (1+ i)))))

	     ((string= (match-string 1) "_SKIP_UNLESS")
	      (looking-at ".*$")
	      (unless (eval (car (read-from-string (match-string 0))) t)
		(setq skip-cmds t)
		(setq skip-reindent-test t)
		(setq skip-recase-test t)
		;; We don’t set ‘skip-write’ t here, so the *.diff Make target succeeds.
		))

	     ((string= (match-string 1) "DEBUG")
	      (looking-at ".*$")
	      (message "DEBUG: %s:%d %s"
		       (current-buffer)
		       (line-number-at-pos)
		       (save-excursion
			 (eval (car (read-from-string (match-string 0))) t))))

	     (t
	      (setq error-count (1+ error-count))
	      (push (line-number-at-pos) error-lines)
	      (error (concat "Unexpected EMACS test command " (match-string 1))))))

	  (let ((msg (format "%s:%d tests passed %d"
			     (buffer-file-name) (line-number-at-pos (point)) pass-count)))
	    (when wisi-parser-shared (wisi-parse-log-message wisi-parser-shared msg))
	    (message msg))

	  (when (> error-count 0)
	    (message "errors on lines: %s" error-lines)
	    (error
	     "%s:%d: aborting due to previous errors (%d)"
	     (buffer-file-name) (line-number-at-pos (point)) error-count))
	  )

	(unless skip-reindent-test
	  ;; Reindent the buffer
	  (message "indenting")

	  ;; first unindent; if the indentation rules do nothing, the test
	  ;; would pass, otherwise!  Only unindent by 1 column, so comments
	  ;; not currently in column 0 are still not in column 0, in case
	  ;; the mode supports a special case for comments in column 0.
	  (indent-rigidly (point-min) (point-max) -1)

	  ;; indent-region uses save-excursion, so we can't goto an error location
	  (indent-region (point-min) (point-max))

	  ;; Cleanup the buffer; indenting often leaves trailing whitespace;
	  ;; files must be saved without any.
	  (delete-trailing-whitespace)
	  )

	(when (and wisi-auto-case (not skip-recase-test))
	  (message "casing")
	  (wisi-case-adjust-buffer))

	(wisi-test-save-log))
    (error
     (wisi-test-save-log)
     (signal (car err) (cdr err)))
    ))

;; Let edebug display strings full-length, and show internals of records
(defvar cl-print-readably t); cl-print.el, used by edebug
(setq read-buffer-completion-ignore-case t) ;; for "*Messages*"

(defun wisi-half-screen ()
  (interactive)
  (modify-frame-parameters
   nil
   (cl-case system-type
     (gnu/linux
      (list
       (cons 'font "DejaVu Sans Mono-11")
       (cons 'width 120) ;; characters; fringe extra
       (cons 'height 73) ;; characters
       (cons 'left 0)
       (cons 'top 0)))
     (windows-nt
      (list
       (cons 'font "DejaVu Sans Mono-8")
       (cons 'width 120) ;; characters; fringe extra
       (cons 'height 103) ;; characters
       (cons 'left 0)
       (cons 'top 0))))))
(define-key global-map "\C-cp" 'wisi-half-screen)

(defun wisi-first-error ()
  (interactive)
  (pop-to-buffer "*Messages*")
  (goto-char (point-min))
  (search-forward "error:"))
(define-key global-map [f6] 'wisi-first-error)

(defun wisi-prev-window ()
  "move to previous window"
  (interactive)
  (other-window -1))
(define-key global-map [M-C-up] 'wisi-prev-window)
(define-key global-map [M-C-down] 'other-window)
(define-key global-map [f11] 'switch-to-buffer)

(defun run-test (file-name)
  "Run an indentation and casing test on FILE-NAME."
  (interactive "f")

  (setq-default indent-tabs-mode nil) ;; no tab chars in files
  (setq message-log-max most-positive-fixnum)

  ;; we'd like to run emacs from a makefile as:
  ;;
  ;; emacs -Q --batch -l runtest.el -f run-test-here <filename>
  ;;
  ;; However, the function specified with -f is run _before_
  ;; <filename> is visited. So we try this instead:
  ;;
  ;; emacs -Q --batch -l runtest.el --eval '(run-test "<filename>")'
  ;;
  ;; And then we discover that processes spawned with start-process
  ;; don't run when emacs is in --batch mode. So we try this:
  ;;
  ;; emacs -Q -l runtest.el --eval '(progn (run-test "<filename>")(kill-emacs))'
  ;;
  ;; Then we must use (font-lock-ensure) to force immediate fontification.

  (setq xref-prompt-for-identifier nil)

  (let ((dir default-directory))
    ;; Always wait for initial full parse to complete.
    (setq wisi-parse-full-background nil)

    (find-file file-name) ;; sets default-directory

    (run-test-here)

    (unless skip-write
      ;; Write the result file; makefile will diff.
      (when skip-reindent-test
	;; user sets skip-reindent-test when testing interactive editing
	;; commands, so the diff would fail. Revert to the original file,
	;; save a copy of that.
	(revert-buffer t t))

      (delete-trailing-whitespace)
      (write-file (concat dir (file-name-nondirectory file-name) ".tmp")) )
    )
  )

(provide 'wisi-run-indent-test)
;; end of file
