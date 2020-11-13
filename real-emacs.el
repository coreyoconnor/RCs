;;; RCs --- coreyoconnor emacs RC
;;; Commentary:
;; I don't have a good understanding of Lisp or Emacs
;;; Code:

(defun configure ()
  (configure-package-manager)
  (configure-data-handling)
  (configure-interface)
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
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(defun configure-load-path ()
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/evil-numbers"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/local"))
  )

(defun enable-package-package ()
  (require 'package)
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

  (setq package-enable-at-startup nil)
  )

(defun configure-interface ()
  (require 'configure-interface)
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

;; unsorted

; GUI options
(add-to-list 'default-frame-alist  '(width . 80) )

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

(configure)
