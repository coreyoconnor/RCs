;;; openai-php-complete --- stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar openai-php-subst-model nil "")
(setq openai-php-subst-model "text-davinci-003")

(defvar openai-php-subst-prompt nil "")
(setq openai-php-subst-prompt "
The following is incomplete PHP 7.4 code.
The code contains a single ???.
You are a helpful AI that provides the best substitution for the ???.
Respond with only the exact text that should replace the ??? to complete the code.
Only the exact substitution for ??? is useful. No text before or after the replacement for ??? is useful.

")

(defvar openai-php-continue-prompt nil "")
(setq openai-php-continue-prompt "
The following is incomplete PHP 7.4 code.
You are a helpful coding AI that provides the best completion with the objective: %s.

")

(defun openai-php-region-fill-in (start end)
  ""
  (interactive "r")
  (let ((code (concat openai-php-subst-prompt (string-trim (buffer-substring-no-properties start end)))))
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
                       :model openai-php-subst-model
                       )
    )
  )

(defun drop-trailing-whitespace-lines (strings)
  (nreverse
   (seq-drop-while
    (lambda (s) (string-match-p "^\\s*$" s))
    (nreverse strings))))

(defun get-previous-lines-text (n)
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

(defun openai-php-continue ()
  ""
  (interactive)
  (let* ((previous-lines (get-previous-lines-text 128))
         (probable-pre-context (drop-trailing-whitespace-lines
                                (seq-drop-while
                                 (lambda (s) (not (string-match-p "\\(class\\|function\\)" s)))
                                 previous-lines)
                                )
                               )
         )
    (cl-multiple-value-bind (last-line context-lines) (pop-end probable-pre-context)

      (let* ((code-context (mapconcat 'identity context-lines "\n"))
             (prompt (concat
                      (format openai-php-continue-prompt last-line)
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
                           :model openai-php-subst-model
                           )
        )
      )
    )
  )

(provide 'openai-php-complete)
;;; openai-php-complete.el ends here
