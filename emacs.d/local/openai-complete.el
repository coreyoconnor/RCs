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
Respond with exactly the substitution for 'XXX' in the 'LOCAL' section code.
The 'LOCAL' section code contains a single 'XXX'.
Respond with only the exact code that should replace the 'XXX'.
")

(defvar openai-complete-user-prompt nil "")
(setq openai-complete-user-prompt "
Provide only the code substitution for the 'XXX'.
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

(cl-defstruct openai-complete-lsp-symbol
  kind ;; lsp symbol kind
  name ;; string
  decl ;; string
  range-start ;; int
  range-end
  )

(defun mopt-first-non-nil (arg funcs)
  ""
  (cl-some (lambda (f) (funcall f arg)) funcs)
  )

(defun make-regex-local-context  (up-to-regex)
  "Grabs no more than openai-complete-context-size lines, up to UP-TO-REGEX."
  (lambda (point)
    (pcase (local-context-0 openai-complete-context-size)
      (`( ,prior ,next )
       (let* ((trimmed-prior (seq-drop-while
                              (lambda (s) (not (string-match-p up-to-regex s)))
                              prior))
              (clean-prior (if (null trimmed-prior) prior trimmed-prior))
              )
         (list clean-prior nil)
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
              (let* ((other-files
                      (remove "."
                              (remove ".."
                                      (remove (buffer-name)
                                              (directory-files (file-name-directory (buffer-file-name)))
                                              )
                                      )
                              )
                      )
                     (files-sample (seq-take other-files context-size))
                     (this-file-intro (concat "The code is " name " in a file named " (string-trim (buffer-name))))
                     )
                (if (null files-sample) (list this-file-intro)
                  (append (list this-file-intro ". With other files in the project:") files-sample)
                  )
                )
              )
            )
           )
      (list project-context context-metadata)
      )
    )
  )

(defun openai-complete-elisp-context (point)
  "provide context for elisp"
  (pcase-let* ((`( ,local ,local-metadata) (mopt-first-non-nil point
                                                               (list
                                                                (make-regex-local-context "(defun")
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
                                                                'local-context-from-lsp
                                                                (make-regex-local-context "\sdef")
                                                                )))
               (`( ,context ,context-metadata) (mopt-first-non-nil local-metadata
                                                                   (list
                                                                    'context-context-from-lsp
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
    ;; (message "local %s" local)
    ;; (message "context %s" context)
    ;; (message "global %s" global)
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
                                                                (make-regex-local-context "^\s*class\\|^\s*function")
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


(defun openai-complete--response-substitution (content meta)
  ""
  (let* ((close-start (save-excursion (forward-line -2) (point)))
         (close-end (save-excursion (forward-line 2) (point))))
    (save-excursion
      (goto-char close-start)
      (search-forward "XXX" close-end t)
      (let ((match-start (match-beginning 0)))
        (replace-match content t t)
        (indent-region match-start (point))
        )
      )
    )
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
  (call-interactively 'openai-complete-ensure-placeholder)
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
    (message "SYSTEM START\n%s\nSYSTEM END" system-text)
    (message "USER START\n%s\nUSER END" user-text)
    ;;(message "request %s" request)
    (openai-chat request
                 (lambda (data)
                   ;; (message "response %s" data)
                   (let* ((result (openai-complete--select-text-result data))
                          (code (openai-complete--select-code-block result))
                          )
                     ;;(message "result %s" result)
                     ;;(message "code %s" code)
                     (openai-complete--response-substitution code (openai-complete-context-meta context))
                     )
                   )
                 :max-tokens openai-complete-max-tokens
                 :model openai-complete-model
                 )
    )
  )

(defun openai-complete--symbol-size-comparator (a b)
  ""
  (> (- (openai-complete-lsp-symbol-range-end a) (openai-complete-lsp-symbol-range-start a))
     (- (openai-complete-lsp-symbol-range-end b) (openai-complete-lsp-symbol-range-start b))))

(defun openai-complete--sort-symbols-by-size-desc (symbols)
  ""
  (sort symbols 'openai-complete--symbol-size-comparator))

(defun openai-complete--lsp-range-to-pos (range)
  ""
  (let ((line (gethash "line" range))
        (character (gethash "character" range)))
    (save-excursion
      (goto-char (point-min))
      (forward-line line)
      (move-to-column character)
      (point)
      )
    )
  )

(defun openai-complete--buffer-text-for-lsp-lines (start end)
  ""
  (save-excursion
    (goto-char (point-min))
    (forward-line start)
    (let ((start-pos (point)))
      (forward-line (+ 1 (- end start)))
      (let* ((lines (split-string (buffer-substring-no-properties start-pos (point)) "\n"))
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
    )
  )

(defun openai-complete--symbols-with-line-in-range (symbols line max)
  ""
  (let ((result '()))
    (dolist (symbol symbols)
      (let ((start (openai-complete-lsp-symbol-range-start symbol))
            (end (openai-complete-lsp-symbol-range-end symbol))
            )
        ;; (message "symbol %s" symbol)
        (when (and (<= start line) (<= line end) (<= (- end start) max))
          (push symbol result))))
    (nreverse result)))

(defun openai-complete--symbols-from-response (response)
  ""
  (let (result)
    (dolist (symbol (append response nil) result)
      (let* ((kind (gethash "kind" symbol))
             (name (gethash "name" symbol))
             (selection-range (gethash "selectionRange" symbol))
             (range (gethash "range" symbol))
             (decl (nth 0 (openai-complete--buffer-text-for-lsp-lines
                           (gethash "line" (gethash "start" range)) ;; yes range
                           (gethash "line" (gethash "end" selection-range))
                           )
                        )
                   )
             (children (gethash "children" symbol))
             )
        (push (make-openai-complete-lsp-symbol :kind kind
                                               :name name
                                               :decl (string-trim decl)
                                               :range-start (+ 1 (gethash "line" (gethash "start" range)))
                                               :range-end (+ 1 (gethash "line" (gethash "end" range)))
                                               )
              result)
        (setq result (append result (openai-complete--symbols-from-response children)))
        )
      )
    result
    )
  )

(defun openai-complete-ensure-placeholder ()
  (interactive)
  (let ((previous-text (buffer-substring-no-properties (max (point-min) (- (point) 3)) (point))))
    (unless (string= previous-text "XXX")
      (insert "XXX"))))

(defun openai-complete--lsp-buffer-symbols (point)
  ""
  (let*
      ((params (lsp--text-document-position-params))
       (response (lsp-request "textDocument/documentSymbol" params))
       (all-symbols (openai-complete--symbols-from-response response))
       )
    ;; (message "%s" all-symbols)
    all-symbols
    )
  )

(defun local-context-from-lsp (point)
  ""
  (let*
      ((all-symbols (openai-complete--lsp-buffer-symbols point))
       (in-range-symbols (openai-complete--symbols-with-line-in-range
                          all-symbols
                          (line-number-at-pos point)
                          openai-complete-context-size))
       (relevant-symbols (openai-complete--sort-symbols-by-size-desc in-range-symbols))
       (best-symbol (car relevant-symbols))
       )
    ;; (message "best %s" in-range-symbols)
    (when (not (null best-symbol))
      (let* ((text-lines (openai-complete--buffer-text-for-lsp-lines
                          (openai-complete-lsp-symbol-range-start best-symbol)
                          (openai-complete-lsp-symbol-range-end best-symbol))
                         )
             )
        (list text-lines all-symbols)
        )
      )
    )
  )

(defun context-context-from-lsp (all-symbols)
  ""
  (when (not (null all-symbols))
    (let* ((context-size (/ openai-complete-context-size 2))
           (symbol-decls (mapcar (lambda (symbol) (concat " - `" (openai-complete-lsp-symbol-decl symbol) "`")) all-symbols))
           )
      (list (append (list "Local symbols include:") (seq-take symbol-decls context-size)) all-symbols)
      )
    )
  )

(provide 'openai-complete)
;;; openai-complete.el ends here
