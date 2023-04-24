;;; openai-complete --- stuff -*- lexical-binding: t; -*-
;;; Commentary: copyright (c) 2023 Corey O'Connor
;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'seq)

(defvar openai-complete-model nil "")
(setq openai-complete-model "gpt-3.5-turbo")

(defvar openai-complete-context-lines nil "")
(setq openai-complete-context-lines 64)

(defvar openai-complete-max-tokens nil "")
(setq openai-complete-max-tokens 256)

(defvar openai-complete-scala-complete-max-context nil "")
(setq openai-complete-scala-complete-max-context 256)

(defvar openai-complete-context-context-size nil "")
(setq openai-complete-context-context-size 16)

(defvar openai-complete-php-subst-prompt nil "")
(setq openai-complete-php-subst-prompt "
The following is incomplete PHP 8 code.
The code contains a single ???.
You are a helpful AI that provides the best substitution for the ???.
Respond with only the exact text that should replace the ??? to complete the code.
Only the exact substitution for ??? is useful.
No text before or after the replacement for ??? is useful.

")

(defvar openai-complete-php-continue-prompt nil "")
(setq openai-complete-php-continue-prompt "
The following is incomplete PHP 8 code.
You are a helpful coding AI that provides the best completion with the objective: %s.

")

(defvar openai-complete-scala-model nil "")
(setq openai-complete-scala-model "text-davinci-003")

(defvar openai-complete-scala-complete-prompt nil "")
(setq openai-complete-scala-complete-prompt "
The following is incomplete Scala 2.13 code.
The code contains a single ???.
You are a helpful AI that provides the best substitution for the ???.
Respond with only the exact text that should replace the ??? to complete the code.
Only the exact substitution for ??? is useful.
No text before or after the replacement for ??? is useful.

")

(defvar openai-complete-elisp-system-prompt nil "")
(setq openai-complete-elisp-system-prompt "
You are a helpful coding AI.
The user will provide three sections of content: 'GLOBAL', 'CONTEXT', 'LOCAL'.
'GLOBAL' describes the general structure.
'CONTEXT' describes the context of the 'LOCAL' code.
'LOCAL' is the incomplete code.
You will provide the best substitution for the '???' in the 'LOCAL' section code.
The 'LOCAL' section code contains a single '???'.
Respond with only the exact text that should replace the '???' to complete the code.
Only the exact substitution for '???' is useful.
Any response that is not code must be in comments.
")

(defvar openai-complete-elisp-user-prompt nil "")
(setq openai-complete-elisp-user-prompt "
Provide only the code substitution for the '???'; no backticks.
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

(defun elisp-local-context-0 (point)
  ""
  (pcase (local-context-0 openai-complete-context-lines)
    (`( ,prior ,next )
     (let* ((trimmed-prior (seq-drop-while
                            (lambda (s) (not (string-match-p "(defun" s)))
                            prior))
            (clean-prior (if (null trimmed-prior) prior trimmed-prior))
            (last-line (car (last clean-prior)))
            )
       (if (null (string-match "^\s*;;+\s*\\(.*\\)$" last-line))
           (list (append clean-prior (list "???") next) nil)
         (list (append (butlast clean-prior) (list "???") next) (match-string 1 last-line))
         )
       )
      )
     )
    )

(defun elisp-context-context-0 (local-metadata)
  ""
  (let ((regex "(defun \\(\\(?:\\w\\|-\\)+\s*(.*)$\\)")
        (result (list)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (push (match-string 1) result)))
    (if (seq-empty-p result)
        (list (list "Emacs packages are available.") local-metadata)
      (list (append (list "Emacs packages are available. Local functions include:") (seq-take result openai-complete-context-context-size)) local-metadata)
    )
  )
  )

(defun elisp-global-context-0 (context-metadata)
  ""
  (let* ((project-context
          (if (null (buffer-file-name))
              '()
            (let ((other-files (seq-take (remove "." (remove ".." (remove (buffer-name)
                                                 (directory-files (file-name-directory (buffer-file-name)))

                                              )
                                         ))
                               openai-complete-context-context-size
                               )
                               )
                  )
              (append (list (concat "This file is " (string-trim (buffer-name))) ". With other files in the project:") other-files)
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

(defun openai-complete-elisp-context (point)
  "provide context for elisp"
  (pcase-let* ((`( ,local ,local-metadata) (mopt-first-non-nil point '(elisp-local-context-0)))
               (`( ,context ,context-metadata) (mopt-first-non-nil local-metadata '(elisp-context-context-0)))
               (`( ,global ,global-metadata) (mopt-first-non-nil context-metadata '(elisp-global-context-0)))
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
   :system openai-complete-elisp-system-prompt
   :user openai-complete-elisp-user-prompt
   )
  )

(defun openai-complete-scala-context ()
  "provide three levels of context (local mid far) for scala"
  '()
  )

(defun openai-complete-php-context ()
  "provide three levels of context (local mid far) for php"
  '()
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

(defun openai-complete-elisp-continue ()
  ""
  (interactive)
  (openai-complete-generic-continue
   'openai-complete-elisp-context
   (openai-complete-elisp-prompts)
   )
  )

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

(defun openai-complete-generic-continue (context-f prompts)
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
                           (let* ((choices (let-alist data .choices))
                                  )
                             (when (seq-empty-p choices)
                               (user-error "No completion found"))
                             (let* ((choice (seq-first choices))
                                    (result (let-alist choice (let-alist .message (string-trim .content))))
                                    )
                               ;; (message "result %s" result)
                               (openai-complete-response-substitution result (openai-complete-context-meta context))
                               )
                             )
                           )
                         :max-tokens openai-complete-max-tokens
                         :model openai-complete-model
                         )
      )
    )

(defun openai-complete-php-region-fill-in (start end)
  ""
  (interactive "r")
  (let ((code (concat openai-complete-php-subst-prompt (string-trim (buffer-substring-no-properties start end)))))
    (openai-completion code
                       (lambda (data)
                         (let* ((choices (openai--data-choices data))
                                (result (string-trim (openai--get-choice choices)))
                                )
                           (when (string-empty-p result)
                             (user-error "No completion found"))
                           (save-excursion
                             (goto-char start)
                             (search-forward "???" end t)
                             (replace-match (replace-regexp-in-string "^\n+" "" result) t t)
                             )
                           )
                         )
                       :max-tokens 256
                       :model openai-complete-php-subst-model
                       )
    )
  )

(defun openai-complete-php-continue ()
  ""
  (interactive)
  (let* ((previous-lines (local-context-0-prior 128))
         (probable-pre-context (seq-drop-while
                                 (lambda (s) (not (string-match-p "\\(class\\|function\\)" s)))
                                 previous-lines)
                                )
         (last-line (car (last probable-pre-context)))
         (context-lines (butlast probable-pre-context))
         )
    (let* ((code-context (mapconcat 'identity context-lines "\n"))
           (prompt (concat
                    (format openai-complete-php-continue-prompt last-line)
                    code-context))
           )
      (openai-completion prompt
                         (lambda (data)
                           (let* ((choices (openai--data-choices data))
                                  (result (string-trim (openai--get-choice choices)))
                                  )
                             (when (string-empty-p result)
                               (user-error "No completion found"))
                             (delete-region (line-beginning-position) (line-end-position))
                             (insert result)
                             (indent-according-to-mode)
                             )
                           )
                         :max-tokens 256
                         :model openai-complete-php-subst-model
                         )
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

(defun openai-complete-scala-region-fill-in (start end)
  ""
  (interactive "r")
  (let ((code (concat openai-complete-scala-complete-prompt (string-trim (buffer-substring-no-properties start end)))))
    (openai-completion code
                       (lambda (data)
                         (let* ((choices (openai--data-choices data))
                                (result (string-trim (openai--get-choice choices)))
                                )
                           (when (string-empty-p result)
                             (user-error "No completion found"))
                           (save-excursion
                             (goto-char start)
                             (while (search-forward "???" end t)
                               (replace-match (replace-regexp-in-string "^\n+" "" result) t t)
                               )
                             )
                           )
                         )
                       :max-tokens 256
                       :model openai-complete-scala-model
                       )
    )
  )

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

(defun openai-complete-scala-continue ()
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
