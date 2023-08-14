(setq-default evil-want-integration t)
(setq-default evil-want-keybinding nil)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night t)
  )

(use-package undo-tree
  :ensure t
  :config
  (setq-default undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  )

(use-package evil
  :ensure t
  :after (:all company selectrum)
  :config
  (evil-mode 1)

  (require 'evil-numbers)

  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "ti") 'imenu)

  (defun describe-function-and-focus ()
    (interactive)
    (call-interactively 'describe-function)
    (switch-to-buffer-other-window "*Help*"))

  (define-key evil-normal-state-map (kbd "C-c h") 'describe-function-and-focus)

  (global-undo-tree-mode)
  (setq evil-undo-system 'undo-tree)
  )

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init)

  (dolist (mode '(diff-mode))
    (setq-default evil-collection-mode-list (delq mode evil-collection-mode-list)))
  )

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-minibuffer)
  )

(provide 'configure-interface)
