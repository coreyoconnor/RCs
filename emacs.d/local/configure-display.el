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
  (set-default-font "Consolas 14")
  (scroll-bar-mode -1)
  )

(when (string-equal system-type "darwin")
  (when (member "Menlo" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Menlo-16"))
    (add-to-list 'default-frame-alist '(font . "Menlo-16"))))

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

  )

(use-package centaur-tabs
  :ensure t
  :after (:all evil)
  :config
  (setq centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-mode t)
  (define-key evil-normal-state-map "gt" 'centaur-tabs-forward)
  (define-key evil-normal-state-map "gT" 'centaur-tabs-backward)
  (evil-ex-define-cmd "quit" 'kill-buffer)
  )

(use-package elscreen :ensure t
  :after (:all evil centaur-tabs)
  :config
  (elscreen-start)

  (evil-ex-define-cmd "tabnew" 'elscreen-create)
  (evil-ex-define-cmd "tabn" 'elscreen-create)

  (define-key evil-normal-state-map (kbd "C-c f") 'elscreen-next)
  (define-key evil-normal-state-map (kbd "C-c C-f") 'elscreen-next)
  (define-key evil-normal-state-map (kbd "C-c b") 'elscreen-previous)
  (define-key evil-normal-state-map (kbd "C-c C-b") 'elscreen-previous)
  )

(use-package diminish
  :ensure t
  :config
    (diminish 'auto-fill-mode)
  )

(use-package eldoc
  :ensure t
  :diminish
  )

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  )

(provide 'configure-display)
