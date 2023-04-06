;; configuration of file navigation features

(defun configure-nav-package ()
  (autoload 'nav "nav" "nav" t)
  (enable-evil-nav)
  )

(defun enable-evil-nav ()
  (eval-after-load 'nav
    '(progn
       (nav-disable-overeager-window-splitting)
       (evil-make-overriding-map nav-mode-map 'normal t)
       (evil-define-key 'normal nav-mode-map
         "j" 'evil-next-line
         "k" 'evil-previous-line)
       )
    )
  )

(defun configure-outline-minor-mode ()
  (define-prefix-command 'cm-map nil "outline-")
  (define-key cm-map "f" 'outline-minor-mode)
  (define-key cm-map "t" 'outline-toggle-children)
  (global-set-key "\M-f" cm-map)
  )

(use-package projectile
  :ensure t
  :diminish
  :config
    (projectile-global-mode)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'ido)
    ;; always index using native. otherwise .projectile is ignored?
    (setq projectile-indexing-method 'native)
    (when (string-equal system-type "windows-nt")
    (setq projectile-indexing-method 'native)
    (scroll-bar-mode -1)
    )

    (setq projectile-globally-ignored-directories
        (append '(".git"
                    ".svn"
                    "out"
                    "repl"
                    "target"
                    "venv"
                    ".bloop"
                    ".gradle"
                    ".idea"
                    ".metals")
                projectile-globally-ignored-directories
                )
        )
  )

;;(use-package window-numbering
;;  :config
;;  (window-numbering-mode)
;;  )

(configure-nav-package)
(configure-outline-minor-mode)
(enable-evil-nav)

(require 'subr-x)

; hack to add back string-trim that was in 25
(defun string-trim (string)
  (string-trim-right string))


(provide 'configure-navigation)
