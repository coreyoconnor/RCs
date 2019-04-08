(setq-default evil-want-integration t)
(setq-default evil-want-keybinding nil)

(use-package evil
  :ensure t
  :config
  (progn
    (evil-mode 1)

    (require 'evil-collection)
    (require 'evil-numbers)
    (evil-collection-init)

    (dolist (mode '(diff-mode))
      (setq-default evil-collection-mode-list (delq mode evil-collection-mode-list)))

    (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
    (define-key evil-normal-state-map (kbd "ti") 'imenu)
    )
  )

(provide 'configure-interface)
