;;; openai-scala-complete --- stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar openai-scala-model nil "")
(setq openai-scala-model "text-davinci-003")

(defvar openai-scala-complete-prompt nil "")
(setq openai-scala-complete-prompt "
The following is incomplete Scala 2.13 code.
The code contains a single ???.
You are a helpful AI that provides the best substitution for the ???.
Respond with only the exact text that should replace the ??? to complete the code.
Only the exact substitution for ??? is useful. No text before or after the replacement for ??? is useful.

")

(defvar openai-scala-complete-max-context nil "")
(setq openai-scala-complete-max-context 256)

(defun symbol-size-comparator (a b)
  (> (- (nth 3 a) (nth 2 a))
     (- (nth 3 b) (nth 2 b))))

(defun sort-symbols-by-size-desc (symbols)
  (sort symbols 'symbol-size-comparator))

(defun to-line-char (hash-table)
  (let ((line (gethash "line" hash-table))
        (character (gethash "character" hash-table)))
    (vector line character)))

(defun get-line (hash-table) (gethash "line" hash-table))

(defun get-text-from-lines (start end)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- start))
    (let ((start-pos (point)))
      (forward-line (1+ (- end start)))
      (buffer-substring-no-properties start-pos (point)))))

(defun symbols-with-line-in-range (symbols line max)
  (let ((result '()))
    (dolist (symbol symbols)
      (let ((start (nth 2 symbol))
            (end (nth 3 symbol)))
        (when (and (<= start line) (<= line end) (<= (- end start) max))
          (push symbol result))))
    (nreverse result)))

(defun lsp-symbols-to-line-ranges (symbols)
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
                          (get-line (gethash "start" selection-range))
                          (get-line (gethash "start" range))))
                    (+ 1 (max
                          (get-line (gethash "end" selection-range))
                          (get-line (gethash "end" range)))))
              result)
        (setq result (append result (flatten-document-symbols children)))))))

(defun openai-scala-region-fill-in (start end)
  ""
  (interactive "r")
  (let ((code (concat openai-scala-complete-prompt (string-trim (buffer-substring-no-properties start end)))))
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
                       :model openai-scala-model
                       )
    )
  )

(defun openai-scala-auto-region-fill-in ()
  (interactive)
  (let* ((line (line-number-at-pos))
         (column (current-column))
         (params (lsp--text-document-position-params))
         (response (lsp-request "textDocument/documentSymbol" params))
         (line-ranges (lsp-symbols-to-line-ranges response))
         (all-symbols (symbols-with-line-in-range
                       line-ranges
                       line
                       openai-scala-complete-max-context))
         (relevant-symbols (sort-symbols-by-size-desc all-symbols))
         (best-symbol (car relevant-symbols))
         )
    (when (not best-symbol)
      (user-error "No context is available that is less than %d lines" openai-scala-complete-max-context))
    (let* ((start (nth 2 best-symbol))
           (end (nth 3 best-symbol))
           (context-text (get-text-from-lines start end))
           (code (concat openai-scala-complete-prompt context-text))
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
      (openai-completion code handle-response :max-tokens 256 :model openai-scala-model)
      )
    )
  )

(provide 'openai-scala-complete)
;;; openai-scala-complete.el ends here
