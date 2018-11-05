;; configuration of mode agnostic display and UI

(setq ring-bell-function 'ignore)
(setq warning-minimum-level :emergency)
(setq message-log-max t)
(setq term-setup-hook
      (lambda ()
          (setq keyboard-translate-table "\C-@\C-a\C-b\C-c\C-d\C-e\C-f\C-g\C-?")
          (global-set-key "\M-h" 'help-for-help)
          )
      )

(when (string-equal system-type "windows-nt")
  (setq projectile-indexing-method 'native)
  (set-default-font "Consolas 14")
  (scroll-bar-mode -1)
  )

(setq scroll-conservatively 5)
(setq scroll-margin 5)
(setq blink-matching-paren nil)

(setq column-number-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

(use-package origami :ensure t
  :after (:all evil)
  :config (progn
            (global-origami-mode 1)
            (add-hook 'prog-mode-hook
                      (lambda ()
                        (origami-mode)
                        ;; I'd like this hook added
                        ;; (origami-close-all-nodes (current-buffer))
                        ;; but that exacerbates issues using find/replace with mark based folds.
                        ))

            (evil-define-key 'normal prog-mode-map (kbd "TAB") 'origami-forward-toggle-node)

            (define-key evil-normal-state-map "za" 'origami-forward-toggle-node)
            (define-key evil-normal-state-map "zR" 'origami-open-all-nodes)
            (define-key evil-normal-state-map "zM" 'origami-close-all-nodes)
            (define-key evil-normal-state-map "zr" 'origami-close-node-recursively)
            (define-key evil-normal-state-map "zm" 'origami-open-node-recursively)
            (define-key evil-normal-state-map "zo" 'origami-show-node)
            (define-key evil-normal-state-map "zc" 'origami-close-node)
            (define-key evil-normal-state-map "zj" 'origami-forward-fold)
            (define-key evil-normal-state-map "zk" 'origami-previous-fold)

            (setq origami-parser-alist (cons '(scala-mode . origami-c-style-parser) origami-parser-alist))
            (setq origami-parser-alist (cons '(glsl-mode . origami-c-style-parser) origami-parser-alist))
            )
  )

(provide 'configure-display)
