;;; RCs --- coreyoconnor emacs RC
;;; Commentary:
;; I don't have a good understanding of Lisp or Emacs
;;; Code:
(if (memq window-system '(mac ns))
    (progn
      (setq-default max-lisp-eval-depth 10000)
      (setq-default max-specpdl-size 10000)
      )
  (setq-default max-lisp-eval-depth 200000)
  (setq-default max-specpdl-size 200000)
  )

(defun configure ()
  (configure-package-manager)

  (when (memq window-system '(mac ns x))
    (use-package exec-path-from-shell
      :ensure t
      :config
      (exec-path-from-shell-initialize))
    )

  (configure-local-overrides)

  (add-to-list 'exec-path (expand-file-name "~/.local/share/coursier/bin"))

  (configure-data-handling)
  (configure-interface)
  (configure-modes)
  (configure-formatting)
  (configure-display)
  (configure-navigation)
  )

(defun configure-package-manager ()
  (setq inhibit-default-init t)
  (configure-load-path)
  (enable-package-package)
  (configure-package-repos)
  )

(defun configure-load-path ()
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/evil-numbers"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/local"))
  )

(defun enable-package-package ()
  (require 'package)
  )

(defun configure-package-repos ()
  (add-to-list 'package-archives
               '("gnu" . "https://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives
               '("org" . "https://orgmode.org/elpa/"))
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-archives
               '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)

  (setq package-archive-priorities '(("melpa"    . 5)
                                     ("jcs-elpa" . 0)))

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
  (when (file-readable-p (expand-file-name "~/.config/emacs-local.el"))
    (load-file (expand-file-name "~/.config/emacs-local.el"))
    )
  )

;; unsorted

                                        ; GUI options

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))

(add-to-list 'term-file-aliases
             '("screen.xterm-256color" . "xterm-256color")
             '("foot" . "foot-direct"))

;; default to unified diffs
(setq diff-switches "-u")

(add-hook 'compilation-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'compilation-minor-mode-hook (lambda () (visual-line-mode 1)))

(setq dabbrev-case-replace nil)

(setq ac-ignore-case nil)

(configure)
