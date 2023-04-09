;;; openai-complete --- stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar openai-complete-php-subst-model nil "")
(setq openai-complete-php-subst-model "text-davinci-003")

(defvar openai-complete-php-subst-prompt nil "")
(setq openai-complete-php-subst-prompt "
The following is incomplete PHP 8 code.
The code contains a single ???.
You are a helpful AI that provides the best substitution for the ???.
Respond with only the exact text that should replace the ??? to complete the code.
Only the exact substitution for ??? is useful. No text before or after the replacement for ??? is useful.

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
Only the exact substitution for ??? is useful. No text before or after the replacement for ??? is useful.

")

(defun openai-complete-drop-trailing-whitespace-lines (strings)
  (nreverse
   (seq-drop-while
    (lambda (s) (string-match-p "^\\s*$" s))
    (nreverse strings))))

(defun openai-complete-get-previous-lines-text (n)
  (save-excursion
    (let ((end (line-end-position))
          (start (progn
                   (forward-line (* -1 n))
                   (beginning-of-line)
                   (point)))
          )
      (split-string (buffer-substring-no-properties start end) "\n"))))

(defun pop-end (strings)
  (let ((last-element (car (last strings)))
        (remaining-list (butlast strings)))
    (cl-values last-element remaining-list)))

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
  (let* ((previous-lines (openai-complete-get-previous-lines-text 128))
         (probable-pre-context (openai-complete-drop-trailing-whitespace-lines
                                (seq-drop-while
                                 (lambda (s) (not (string-match-p "\\(class\\|function\\)" s)))
                                 previous-lines)
                                )
                               )
         )
    (cl-multiple-value-bind (last-line context-lines) (pop-end probable-pre-context)

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
  )

(defvar openai-complete-scala-complete-max-context nil "")
(setq openai-complete-scala-complete-max-context 256)

(defun openai-complete-symbol-size-comparator (a b)
  (> (- (nth 3 a) (nth 2 a))
     (- (nth 3 b) (nth 2 b))))

(defun openai-complete-sort-symbols-by-size-desc (symbols)
  (sort symbols 'openai-complete-symbol-size-comparator))

(defun openai-complete-to-line-char (hash-table)
  (let ((line (gethash "line" hash-table))
        (character (gethash "character" hash-table)))
    (vector line character)))

(defun openai-complete-get-line (hash-table) (gethash "line" hash-table))

(defun openai-complete-get-text-from-lines (start end)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- start))
    (let ((start-pos (point)))
      (forward-line (1+ (- end start)))
      (buffer-substring-no-properties start-pos (point)))))

(defun openai-complete-symbols-with-line-in-range (symbols line max)
  (let ((result '()))
    (dolist (symbol symbols)
      (let ((start (nth 2 symbol))
            (end (nth 3 symbol)))
        (when (and (<= start line) (<= line end) (<= (- end start) max))
          (push symbol result))))
    (nreverse result)))

(defun openai-complete-flatten-document-symbols (symbols)
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
  (openai-complete-ensure-placeholder)
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

(provide 'openai-complete)
;;; openai-complete.el ends here
