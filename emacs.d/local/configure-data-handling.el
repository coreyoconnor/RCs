;; configuration of data handling -*- lexical-binding: t; -*-
(require 'cl)

(setq browse-url-browser-function 'eww-browse-url)

(setq-default buffer-file-coding-system 'utf-8-unix)

(setq create-lockfiles nil)
(global-auto-revert-mode t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Originally from stevey, adapted to support moving to a new directory.
;; http://stackoverflow.com/questions/384284/how-do-i-rename-an-open-file-in-emacs#384612
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     (buffer-file-name)))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
    (message "Renamed to %s." new-name)))

(use-package magit
  :ensure t
  :after (:all evil)
  :bind (("C-c g" . magit-file-dispatch))
  )

(use-package codegpt
  :ensure t
  :config
  (setq-default openai-scala-mode "text-davinci-003")

  (setq-default openai-scala-complete-prompt "
The following is incomplete Scala 2.13 code.
The code contains a single ???.
Respond with all the text that should replace the ??? to complete the code.
Only the exact text replacement for ??? is useful. No text before or after the replacement for ??? is useful.

")

  (defun openai-scala-complete (start end)
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
                         )
      )
    )
  )

(provide 'configure-data-handling)

;;; configure-data-handling.el ends here
