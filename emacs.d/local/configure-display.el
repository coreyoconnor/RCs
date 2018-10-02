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

;; from https://www.reddit.com/r/emacs/comments/5ei7wa/awesome_vimlike_folding_for_evilmode_with_markers/
(defun nin-origami-toggle-node ()
  (interactive)
  (if (equal major-mode 'org-mode)
      (org-cycle)
    (save-excursion ;; leave point where it is
      (goto-char (point-at-eol))             ;; then go to the end of line
      (origami-toggle-node (current-buffer) (point)))))                 ;; and try to fold

(use-package origami :ensure t
  :after (:all evil)
  :config (progn
            (global-origami-mode 1)
            (add-hook 'prog-mode-hook
                      (lambda ()
                        (origami-mode)
                        (origami-close-all-nodes (current-buffer))))
            (evil-define-key 'normal prog-mode-map (kbd "TAB") 'nin-origami-toggle-node)

            (define-key evil-normal-state-map "za" 'origami-forward-toggle-node)
            (define-key evil-normal-state-map "zR" 'origami-close-all-nodes)
            (define-key evil-normal-state-map "zM" 'origami-open-all-nodes)
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
