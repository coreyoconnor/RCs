;; configuration of data handling
(setq browse-url-browser-function 'eww-browse-url)

(setq-default buffer-file-coding-system 'utf-8-unix)

(setq create-lockfiles nil)

(projectile-global-mode)

(setq projectile-enable-caching t)
(when (string-equal system-type "windows-nt")
  (setq projectile-indexing-method 'native)
  (scroll-bar-mode -1)
  )

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

(provide 'configure-data-handling)
