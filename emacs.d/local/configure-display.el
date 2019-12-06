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

(when nil
  (use-package evil-tabs :ensure t
    :after (:all evil)
    :config (progn
              (define-key evil-normal-state-map "gt" 'elscreen-next)
              (define-key evil-normal-state-map "gT" 'elscreen-previous)
              )
    )

                                        ; from: https://zuttobenkyou.wordpress.com/2012/06/15/emacs-vimlike-tabwindow-navigation/
  (defun vimlike-quit (old-fun &rest args)
    (interactive)
    (let ((one-elscreen (elscreen-one-screen-p))
          (one-window (one-window-p))
          )
      (cond
                                        ; if current tab has split windows in it, close the current live window
       ((not one-window)
        (delete-window) ; delete the current window
                                        ; (balance-windows) ; balance remaining windows
        nil)
                                        ; if there are multiple elscreens (tabs), close the current elscreen
       ((not one-elscreen)
        (elscreen-kill)
        nil)
                                        ; if there is only one elscreen, just try to quit (calling elscreen-kill
                                        ; will not work, because elscreen-kill fails if there is only one
                                        ; elscreen)
       (one-elscreen
        (evil-quit)
        nil)
       )))


  (use-package elscreen :ensure t
    :after (:all evil evil-tabs)
    :config (progn
              (elscreen-start)
              (advice-add #'evil-tab-sensitive-quit' :around #'vimlike-quit)
              (add-hook 'elscreen-goto-hook (lambda () (set-window-vscroll nil 0)))
              )
    )
  )

(use-package centaur-tabs
  :ensure t
  :after (:all evil)
  :config (progn
            (setq centaur-tabs-cycle-scope 'tabs)
            (centaur-tabs-mode t)
            (define-key evil-normal-state-map "gt" 'centaur-tabs-forward)
            (define-key evil-normal-state-map "gT" 'centaur-tabs-backward)
            (evil-ex-define-cmd "quit" 'kill-buffer)
            )
  )

(provide 'configure-display)
