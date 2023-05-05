;;; openai-complete --- stuff -*- lexical-binding: t; -*-
;;; Commentary: copyright (c) 2023 Corey O'Connor
;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'seq)

(defvar openai-complete-model nil "")
(setq openai-complete-model "gpt-4")

(defvar openai-complete-context-size nil "")
(setq openai-complete-context-size 64)

(defvar openai-complete-max-tokens nil "")
(setq openai-complete-max-tokens 256)

(defvar openai-complete-system-prompt nil "")
(setq openai-complete-system-prompt "
You are a helpful coding AI.
The user will provide three sections of content: 'GLOBAL', 'CONTEXT', 'LOCAL'.
'GLOBAL' describes the general structure.
'CONTEXT' describes the context of the 'LOCAL' code.
'LOCAL' is the incomplete code.
Respond with exactly the substitution for '???' in the 'LOCAL' section code.
The 'LOCAL' section code contains a single '???'.
Respond with only the exact code that should replace the '???'.
Any response that is not code must be in comments.
")

(defvar openai-complete-user-prompt nil "")
(setq openai-complete-user-prompt "
Provide only the code substitution for the '???'.
GLOBAL
%s

CONTEXT
%s

LOCAL
%s
")

(cl-defstruct openai-complete-context
  local ;; list of strings
  context ;; list of strings
  global ;; list of strings
  meta ;; metadata depends on mode
  )

(cl-defstruct openai-complete-prompts
  system ;; no format required
  user ;; format with GLOBAL, CONTEXT, LOCAL strings
  )

(defun mopt-first-non-nil (arg funcs)
  ""
  (cl-some (lambda (f) (funcall f arg)) funcs)
  )

(defun make-regex-local-context  (up-to-regex goal-comment-leader-regex)
"Grabs no more than openai-complete-context-size lines, up to UP-TO-REGEX.
With GOAL-COMMENT-LEADER-REGEX used to match the goal from the current/previous line."
  (lambda (point)
    (pcase (local-context-0 openai-complete-context-size)
      (`( ,prior ,next )
       (let* ((trimmed-prior (seq-drop-while
                              (lambda (s) (not (string-match-p up-to-regex s)))
                              prior))
              (clean-prior (if (null trimmed-prior) prior trimmed-prior))
              (last-line (car (last clean-prior)))
              )
         (if (null (string-match (concat "^\s*" goal-comment-leader-regex "+\s*\\(.*\\)$") last-line))
             (list (append clean-prior (list "???") next) nil)
           (list (append (butlast clean-prior) (list "???") next) (match-string 1 last-line))
           )
         )
       )
      )
    )
  )

(defun make-regex-context-context (regex)
  ""
  (lambda (local-metadata)
    (let (
          (result (list))
          (context-size (/ openai-complete-context-size 2))
          )
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward regex nil t)
          (push (match-string 1) result)))
      (if (seq-empty-p result)
          (list (list "") local-metadata)
        (list (append (list "Local namespace includes:") (seq-take result context-size)) local-metadata)
        )
      )
    )
  )

(defun make-file-based-global-context (name)
  ""
  (lambda (context-metadata)
    (let* ((context-size (/ openai-complete-context-size 4))
           (project-context
            (if (null (buffer-file-name))
                '()
              (let ((other-files (seq-take (remove "." (remove ".." (remove (buffer-name)
                                                                            (directory-files (file-name-directory (buffer-file-name)))

                                                                            )
                                                               ))
                                           context-size
                                           )
                                 )
                    )
                (append (list (concat "The code is " name " in a file named " (string-trim (buffer-name))) ". With other files in the project:") other-files)
                )
              )
            )
           (goal-statement (if context-metadata
                               (list "The goal is to " context-metadata ". ")
                             '()
                             )
                           )
           )
      (list (append goal-statement project-context) context-metadata)
      )
    )
  )

(defun openai-complete-elisp-context (point)
  "provide context for elisp"
  (pcase-let* ((`( ,local ,local-metadata) (mopt-first-non-nil point
                                                               (list
                                                                 (make-regex-local-context "(defun" ";;")
                                                                 )))
               (`( ,context ,context-metadata) (mopt-first-non-nil local-metadata
                                                                   (list
                                                                     (make-regex-context-context "(defun \\(\\(?:\\w\\|-\\)+\s*(.*)$\\)")
                                                                     )))
               (`( ,global ,global-metadata) (mopt-first-non-nil context-metadata
                                                                 (list
                                                                   (make-file-based-global-context "Emacs Lisp")
                                                                   )))
               )
    ;; (message "local-metadata %s" local-metadata)
    ;; (message "context-metadata %s" context-metadata)
    ;; (message "global-metadata %s" global-metadata)
    (make-openai-complete-context :local local :context context :global global :meta global-metadata)
    )
  )

(defun openai-complete-elisp-prompts ()
  ""
  (make-openai-complete-prompts
   :system openai-complete-system-prompt
   :user openai-complete-user-prompt
   )
  )

(defun openai-complete-elisp-continue ()
  ""
  (interactive)
  (make-openai-complete-generic-continue
   'openai-complete-elisp-context
   (openai-complete-elisp-prompts)
   )
  )

(defun openai-complete-scala-context (point)
  "provide context for scala"
  (pcase-let* ((`( ,local ,local-metadata) (mopt-first-non-nil point
                                                               (list
                                                                 (make-regex-local-context "\sdef" "//")
                                                                 )))
               (`( ,context ,context-metadata) (mopt-first-non-nil local-metadata
                                                                   (list
                                                                     (make-regex-context-context "\s\\(?:def\\|val\\)\s+\\(\\w+.*\\)=.*$")
                                                                     )))
               (`( ,global ,global-metadata) (mopt-first-non-nil context-metadata
                                                                 (list
                                                                   (make-file-based-global-context "Scala")
                                                                   )))
               )
    ;; (message "local-metadata %s" local-metadata)
    ;; (message "context-metadata %s" context-metadata)
    ;; (message "global-metadata %s" global-metadata)
    (make-openai-complete-context :local local :context context :global global :meta global-metadata)
    )
  )

(defun openai-complete-scala-prompts ()
  ""
  (make-openai-complete-prompts
   :system openai-complete-system-prompt
   :user openai-complete-user-prompt
   )
  )

(defun openai-complete-scala-continue ()
  ""
  (interactive)
  (make-openai-complete-generic-continue
   'openai-complete-scala-context
   (openai-complete-scala-prompts)
   )
  )

(defun openai-complete-php-context (point)
  "provide three levels of context (local mid far) for php"
  (pcase-let* ((`( ,local ,local-metadata) (mopt-first-non-nil point
                                                               (list
                                                                 (make-regex-local-context "^\s*class\\|^\s*function" "\\(?://\\|#\\)")
                                                                 )))
               (`( ,context ,context-metadata) (mopt-first-non-nil local-metadata
                                                                   (list
                                                                     (make-regex-context-context "^\s*\\(function\s+\\w+.*\\|class\s+\\w+.*\\)\\(?:$\\|{\\)")
                                                                     )))
               (`( ,global ,global-metadata) (mopt-first-non-nil context-metadata
                                                                 (list
                                                                   (make-file-based-global-context "PHP 8")
                                                                   )))
               )
    ;; (message "local-metadata %s" local-metadata)
    ;; (message "context-metadata %s" context-metadata)
    ;; (message "global-metadata %s" global-metadata)
    (make-openai-complete-context :local local :context context :global global :meta global-metadata)
    )
  )

(defun openai-complete-php-prompts ()
  ""
  (make-openai-complete-prompts
   :system openai-complete-system-prompt
   :user openai-complete-user-prompt
   )
  )

(defun openai-complete-php-continue ()
  ""
  (interactive)
  (make-openai-complete-generic-continue
   'openai-complete-php-context
   (openai-complete-php-prompts)
   )
  )

;; TODO: annotate
(defun buffer-text-lines (start end)
  ""
  (let* ((lines (split-string (buffer-substring-no-properties start end) "\n"))
         (no-trailing-empties (nreverse
                               (seq-drop-while
                                (lambda (s) (string-match-p "^\\s*$" s))
                                (nreverse lines))
                               )
                              )
         )
    no-trailing-empties
    )
  )

(defun local-context-0-prior (n)
  "Prior n lines of text"
  (save-excursion
    (let ((end (line-end-position))
          (start (progn
                   (forward-line (* -1 n))
                   (beginning-of-line)
                   (point)))
          )
      (buffer-text-lines start end))))

(defun local-context-0-next (n)
  "next n lines of text"
  (save-excursion
    (let ((start (progn
                   (forward-line 1)
                   (line-beginning-position)
                   )
                 )
          (end (progn
                 (forward-line (- n 1))
                 (end-of-line)
                 (point)
                 )
               )
          )
      (buffer-text-lines start end))))

(defun local-context-0 (n)
  "Prior n lines of text and up to n/2 next lines"
  (let ((prior (local-context-0-prior n))
        (next (local-context-0-next (/ n 2))))
    (list prior next)))


(defun openai-complete-response-substitution (content meta)
  ""
  (let* ((close-start (save-excursion (forward-line -2) (point)))
         (close-end (save-excursion (forward-line 2) (point))))
    (if (and (not meta)
             (save-excursion
               (goto-char close-start)
               (search-forward "???" close-end t)))
        (replace-match content t t)
      (if (and (eolp)
               (not (looking-back "^[[:space:]]*" 1)))
          (progn
            (newline-and-indent)
            (insert content))
        (insert content))))
  )

(defun openai-complete--select-text-result (data)
  ""
  (let* ((choices (let-alist data .choices)))
    (when (seq-empty-p choices)
      (user-error "No completion found"))
    (let* ((choice (seq-first choices)))
      (let-alist choice (let-alist .message (string-trim .content)))
      )
    )
  )

(defun openai-complete--select-code-block (text)
  ""
  (if (null (string-match "^.*```\\(.*?\\)```.*$" text))
      text
    (match-string 1 text)
    )
  )

(defun make-openai-complete-generic-continue (context-f prompts)
  ""
  (let* ((context (funcall context-f (point)))
         (local-text (mapconcat 'identity (openai-complete-context-local context) "\n"))
         (context-text (mapconcat 'identity (openai-complete-context-context context) "\n"))
         (global-text (mapconcat 'identity (openai-complete-context-global context) "\n"))
         (system-text (openai-complete-prompts-system prompts))
         (user-text (format (openai-complete-prompts-user prompts) global-text context-text local-text))
         (request (vector
                   (list (cons "role" "system")
                         (cons "content" system-text))
                   (list (cons "role" "user")
                         (cons "content" user-text))
                   )
           )
         )
    ;; (message "SYSTEM START\n%s\nSYSTEM END" system-text)
    ;; (message "USER START\n%s\nUSER END" user-text)
    ;;(message "request %s" request)
    (openai-chat request
                 (lambda (data)
                   ;; (message "response %s" data)
                   (let* ((result (openai-complete--select-text-result data))
                          (code (openai-complete--select-code-block result))
                          )
                       (message "result %s" result)
                       (message "code %s" code)
                       (openai-complete-response-substitution code (openai-complete-context-meta context))
                       )
                     )
                 :max-tokens openai-complete-max-tokens
                 :model openai-complete-model
                 )
    )
  )

(defun openai-complete-symbol-size-comparator (a b)
  ""
  (> (- (nth 3 a) (nth 2 a))
     (- (nth 3 b) (nth 2 b))))

(defun openai-complete-sort-symbols-by-size-desc (symbols)
  ""
  (sort symbols 'openai-complete-symbol-size-comparator))

(defun openai-complete-to-line-char (hash-table)
  ""
  (let ((line (gethash "line" hash-table))
        (character (gethash "character" hash-table)))
    (vector line character)))

(defun openai-complete-get-line (hash-table)
  ""
  (gethash "line" hash-table))

(defun openai-complete-get-text-from-lines (start end)
  ""
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- start))
    (let ((start-pos (point)))
      (forward-line (1+ (- end start)))
      (buffer-substring-no-properties start-pos (point)))))

(defun openai-complete-symbols-with-line-in-range (symbols line max)
  ""
  (let ((result '()))
    (dolist (symbol symbols)
      (let ((start (nth 2 symbol))
            (end (nth 3 symbol)))
        (when (and (<= start line) (<= line end) (<= (- end start) max))
          (push symbol result))))
    (nreverse result)))

(defun openai-complete-flatten-document-symbols (symbols)
  ""
  (let (result)
    (dolist (symbol (append symbols nil) result)
      (let ((kind (gethash "kind" symbol))
            (name (gethash "name" symbol))
            (selection-range (gethash "selectionRange" symbol))
            (range (gethash "range" symbol))
            (children (gethash "children" symbol)))
        (push (list kind
                    name
                    (+ 1 (min
                          (openai-complete-get-line (gethash "start" selection-range))
                          (openai-complete-get-line (gethash "start" range))))
                    (+ 1 (max
                          (openai-complete-get-line (gethash "end" selection-range))
                          (openai-complete-get-line (gethash "end" range)))))
              result)
        (setq result (append result (openai-complete-flatten-document-symbols children)))))))

(defun openai-complete-scala-auto-region-fill-in ()
  (interactive)
  (let* ((line (line-number-at-pos))
         (params (lsp--text-document-position-params))
         (response (lsp-request "textDocument/documentSymbol" params))
         (all-symbols (openai-complete-flatten-document-symbols response))
         (in-range-symbols (openai-complete-symbols-with-line-in-range
                            all-symbols
                            line
                            openai-complete-scala-complete-max-context))
         (relevant-symbols (openai-complete-sort-symbols-by-size-desc in-range-symbols))
         (best-symbol (car relevant-symbols))
         )
    (when (not best-symbol)
      (user-error "No context is available that is less than %d lines" openai-complete-scala-complete-max-context))
    (let* ((start (nth 2 best-symbol))
           (end (nth 3 best-symbol))
           (context-text (openai-complete-get-text-from-lines start end))
           (code (concat openai-complete-scala-complete-prompt context-text))
           (handle-response (lambda (data)
                              (let* ((choices (openai--data-choices data))
                                     (result (string-trim (openai--get-choice choices)))
                                     )
                                (when (string-empty-p result)
                                  (user-error "No completion found"))
                                (save-excursion
                                  (goto-char (point-min))
                                  (forward-line (1- start))
                                  (let ((start-pos (point)))
                                    (forward-line (1+ (- end start)))
                                    (let ((end-pos (point)))
                                      (goto-char start-pos)
                                      (search-forward "???" end-pos t)
                                      (replace-match (replace-regexp-in-string "^\n+" "" result) t t)
                                      )
                                    )
                                  )
                                )
                              )
                            )
           )
      (openai-completion code handle-response :max-tokens 256 :model openai-complete-scala-model)
      )
    )
  )

(defun openai-complete-ensure-placeholder ()
  (interactive)
  (let ((previous-text (buffer-substring-no-properties (max (point-min) (- (point) 3)) (point))))
    (unless (string= previous-text "???")
      (insert "???"))))

(defun openai-complete-scala-continue-alt ()
  (interactive)
  (call-interactively 'openai-complete-ensure-placeholder)
  (let* ((line (line-number-at-pos))
         (params (lsp--text-document-position-params))
         (response (lsp-request "textDocument/documentSymbol" params))
         (all-symbols (openai-complete-flatten-document-symbols response))
         (in-range-symbols (openai-complete-symbols-with-line-in-range
                            all-symbols
                            line
                            openai-complete-scala-complete-max-context))
         (relevant-symbols (openai-complete-sort-symbols-by-size-desc in-range-symbols))
         (best-symbol (car relevant-symbols))
         )
    (when (not best-symbol)
      (user-error "No context is available that is less than %d lines" openai-complete-scala-complete-max-context))
    (let* ((start (nth 2 best-symbol))
           (end (nth 3 best-symbol))
           (context-text (openai-complete-get-text-from-lines start end))
           (code (concat openai-complete-scala-complete-prompt context-text))
           (handle-response (lambda (data)
                              (let* ((choices (openai--data-choices data))
                                     (result (string-trim (openai--get-choice choices)))
                                     )
                                (when (string-empty-p result)
                                  (user-error "No completion found"))
                                (save-excursion
                                  (goto-char (point-min))
                                  (forward-line (1- start))
                                  (let ((start-pos (point)))
                                    (forward-line (1+ (- end start)))
                                    (let ((end-pos (point)))
                                      (goto-char start-pos)
                                      (search-forward "???" end-pos t)
                                      (replace-match (replace-regexp-in-string "^\n+" "" result) t t)
                                      )
                                    )
                                  )
                                )
                              )
                            )
           )
      (openai-completion code handle-response :max-tokens 256 :model openai-complete-scala-model)
      )
    )
  )

(provide 'openai-complete)
;;; openai-complete.el ends here
