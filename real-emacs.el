(add-to-list 'load-path "~/.emacs.d")

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; default text formatting options
(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)
(setq-default fill-column 101)

(add-hook 'after-change-major-mode-hook
          (function
            (lambda ()
                (fci-mode)
                (turn-on-auto-fill)
            )
          )
)

;; ruby 

(add-hook 'ruby-mode-hook
  (function (lambda ()
              (setq evil-shift-width ruby-indent-level)
              ;; (evil-define-key 'insert 
              ;;                  ruby-mode-map
              ;;                  (kbd "C-n")
              ;;                  'rsense-complete
              ;;   )
            )
  )
)

(setq auto-mode-alist (cons '("\\.rake\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Gemfile" . ruby-mode) auto-mode-alist))

; use rsense autocomplete for C-n in insert

; C
(setq-default c-indent-level 4)

; keyboard interface options
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(setq scroll-conservatively 5)
(setq scroll-margin 5)

; GUI options
(add-to-list 'default-frame-alist  '(width . 100) )
(require 'fill-column-indicator)

(add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme)

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-dark-laptop)
     )
  )

; plugins
(autoload 'coq-mode "ProofGeneral" "~/.emacs.d/ProofGeneral/generic/proof-site.el"
  "ProofGeneral for coq")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(coq-prog-args (quote ("-I" "/home/corey/Development/cpdt_coconnor/cpdt/src")))
 '(ecb-auto-activate t)
 '(ecb-display-default-dir-after-start t)
 '(ecb-fix-window-size (quote width))
 '(ecb-layout-name "dironly")
 '(ecb-layout-window-sizes (quote (("basic" (ecb-directories-buffer-name 0.25728155339805825 . 0.48214285714285715) (ecb-analyse-buffer-name 0.25728155339805825 . 0.5)) ("left8" (ecb-directories-buffer-name 0.1796116504854369 . 0.26785714285714285) (ecb-sources-buffer-name 0.1796116504854369 . 0.25) (ecb-methods-buffer-name 0.1796116504854369 . 0.2857142857142857) (ecb-history-buffer-name 0.1796116504854369 . 0.17857142857142858)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-show-sources-in-directories-buffer (quote always))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 40)
 '(inhibit-startup-screen t)
 '(nav-width 40))

(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

(autoload 'markdown-mode "~/.emacs.d/markdown-mode/markdown-mode.el"
   "Major mode for editing Markdown files" t)

(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;; (load-file "~/.emacs.d/cedet/common/cedet.elc")

;; (global-ede-mode 1)
;;(require 'cedit)
;;(semantic-load-enable-minimum-features)
;;(semantic-load-enable-code-helpers)

;; (add-to-list 'load-path "~/.emacs.d/ecb")
;; (load-file "~/.emacs.d/ecb/ecb.el")

; (setq rsense-home (expand-file-name "~/Development/rsense"))
; (add-to-list 'load-path (concat rsense-home "/etc"))
; (require 'rsense)
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)

(autoload 'dirtree "dirtree" "dirtree" t)

(setq inhibit-default-init t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

(add-to-list 'load-path "/apollo/env/EmacsAmazonLibs/share/emacs/site-lisp")
(require 'amz-common)

(add-to-list 'load-path "~/.emacs.d/jdee-2.4.1/lisp")
(load "jde")
(require 'nav)
(nav-disable-overeager-window-splitting)
