;; coreyoconnor: I don't have a good understanding of Lisp or Emacs
(defun configure ()
  (configure-package-manager)
  (configure-data-handling)
  (configure-modes)
  (configure-formatting)
  (configure-display)
  (configure-navigation)
  (configure-local-overrides)
  )

(defun configure-package-manager ()
  (setq inhibit-default-init t)
  (configure-load-path)
  (enable-package-package)
  (configure-package-repos)
  (enable-use-package-package)
  )

(defun configure-load-path ()
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/local"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/evil-numbers"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/lsp-scala"))
  )

(defun enable-package-package ()
  (require 'package)
  (setq package-enable-at-startup nil)
  )

(defun configure-package-repos ()
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives
               '("org" . "http://orgmode.org/elpa/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives
               '("melpa-stable" . "http://stable.melpa.org/packages/"))

  (package-initialize)
  )

(defun enable-use-package-package ()
  (when (not package-archive-contents)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t)
  )

(defun configure-data-handling ()
  (require 'configure-data-handling)
  )

(defun configure-modes ()
  (require 'configure-modes)
  )

(defun configure-formatting ()
  (require 'configure-formatting)
  )

(defun configure-display ()
  (require 'configure-display)
  )

(defun configure-navigation ()
  (require 'configure-navigation)
  )

(defun configure-local-overrides ()
  (when (file-readable-p (expand-file-name "~/.emacs-local.el"))
    (load-file (expand-file-name "~/.emacs-local.el"))
    )
  )

(configure)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Inferior ruby process" t)
(require 'inf-ruby)
(require 'web-mode)
(setq-default web-mode-code-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(setq-default evil-want-integration t)
(setq-default evil-want-keybinding nil)

(use-package evil
  :ensure t
  :config
  (progn
    (require 'evil-collection)
    (require 'evil-numbers)
    (evil-collection-init)

    (dolist (mode '(diff-mode))
      (setq-default evil-collection-mode-list (delq mode evil-collection-mode-list)))

    (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

    )
  )

(add-hook 'c-mode-common-hook (lambda() (cleanup-on-save)))

; keyboard interface options
(evil-mode 1)

; GUI options
(add-to-list 'default-frame-alist  '(width . 80) )

(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))

;; default to unified diffs
(setq diff-switches "-u")

(add-hook 'compilation-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'compilation-minor-mode-hook (lambda () (visual-line-mode 1)))

(setq dabbrev-case-replace nil)

(setq ac-ignore-case nil)
