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
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/evil-tabs"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/evil-numbers"))
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

(eval-after-load 'compilation-mode
  '(progn
    (evil-make-overriding-map compilation-mode 'normal t)
    (evil-define-key 'normal nav-mode-map
      "gt" 'elscreen-next
      "gT" 'elscreen-previous)))

(use-package scala-mode
             :interpreter
             ("scala" . scala-mode))

(require 'ensime)
(setq ensime-startup-snapshot-notification nil)

(defadvice ensime-config (around ensime-config-path-fixup activate)
  (let* ((config ad-do-it)
         (subprojects (plist-get config :subprojects))
         (new-subprojects
          (-map (lambda (subproject)
                  (let* ((source-roots (plist-get subproject :source-roots))
                         (new-source-roots
                          (-map (lambda (source-root) (file-truename source-root))
                                source-roots)))
                    (plist-put subproject :source-roots new-source-roots)))
                subprojects)))
    (plist-put config :subprojects new-subprojects)))

(autoload 'inf-ruby-minor-mode "inf-ruby" "Inferior ruby process" t)
(require 'inf-ruby)
(require 'web-mode)
(setq-default web-mode-code-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(require 'evil)
(require 'evil-tabs)
(require 'evil-numbers)
(loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
                              (nrepl-mode . insert)
                              (pylookup-mode . emacs)
                              (comint-mode . normal)
                              (shell-mode . insert)
                              (git-commit-mode . insert)
                              (git-rebase-mode . emacs)
                              (term-mode . emacs)
                              (help-mode . emacs)
                              (helm-grep-mode . emacs)
                              (grep-mode . emacs)
                              (bc-menu-mode . emacs)
                              (magit-branch-manager-mode . emacs)
                              (rdictcc-buffer-mode . emacs)
                              (dired-mode . emacs)
                              (wdired-mode . normal)
                              (ensime-inf-mode . emacs))
      do (evil-set-initial-state mode state))

(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

(add-hook 'c-mode-common-hook (lambda() (cleanup-on-save)))

(setq scala-indent:align-parameters t)
(add-hook 'scala-mode-hook 'cleanup-on-save)
(add-hook 'scala-mode-hook 'ensime-mode)

(eval-after-load 'js-mode
  (add-hook 'js-mode-hook
            (lambda()
              (setq js-indent-level 2)
              (setq tab-width 2)
              (setq c-basic-offset 2)
              (setq evil-shift-width 2)
              (cleanup-on-save)
              )))

(eval-after-load 'js-mode
  (add-hook 'js-mode-hook
            (lambda() (cleanup-on-save))))

(eval-after-load 'haml-mode
  (add-hook 'haml-mode-hook
            (lambda ()
              (setq-local electric-indent-chars (remq ?\n electric-indent-chars))))
  )

;; ruby
(eval-after-load 'ruby-mode
 '(progn
    (setq-default ruby-indent-level 2)
    (setq-default evil-shift-width 2)
    (add-hook 'ruby-mode-hook
      (lambda () (setq-default evil-shift-width ruby-indent-level)))
    (add-hook 'ruby-mode-hook
      (lambda() (cleanup-on-save)))))

(setq auto-mode-alist (cons '("\\.rake\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Gemfile" . ruby-mode) auto-mode-alist))

; C
(setq-default c-indent-level 4)

; keyboard interface options
(evil-mode 1)
;; (define-key evil-normal-state-map [escape] 'keyboard-quit)
;; (define-key evil-visual-state-map [escape] 'keyboard-quit)
;; (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-evil-tabs-mode t)

; GUI options
(add-to-list 'default-frame-alist  '(width . 100) )

(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; Java stuff
(add-hook 'java-mode-hook 'ensime-mode)

(defvar java-src-dir "src/")
(defun java-src-stack-trace-regexp-to-filename ()
  "Generates a relative filename from java-stack-trace regexp match data."
  (concat java-src-dir
          (replace-regexp-in-string "\\." "/" (match-string 1))
          (match-string 2)))

(require 'compile)

;; regexps are not case sensitive so we jump through hoops to get this regex to match as expected
(add-to-list 'compilation-error-regexp-alist 'java-src-stack-trace)
(add-to-list 'compilation-error-regexp-alist-alist
  '(java-src-stack-trace .
    ("at \\(\\(?:[[:alnum:]]+\\.\\)+\\)+[[:alnum:]]+\\..+(\\([[:alnum:]]+\\.java\\):\\([[:digit:]]+\\))$"
     java-src-stack-trace-regexp-to-filename 3)))

(defvar java-tst-dir "tst/")
(defun java-tst-stack-trace-regexp-to-filename ()
  "Generates a relative filename from java-stack-trace regexp match data."
  (concat java-tst-dir
          (replace-regexp-in-string "\\." "/" (match-string 1))
          (match-string 2)))

(add-to-list 'compilation-error-regexp-alist 'java-tst-stack-trace)
(add-to-list 'compilation-error-regexp-alist-alist
  '(java-tst-stack-trace .
    ("at \\(\\(?:[[:alnum:]]+\\.\\)+\\)+[[:alnum:]]+\\..+(\\([[:alnum:]]+\\Test.java\\):\\([[:digit:]]+\\))$"
     java-tst-stack-trace-regexp-to-filename 3)))

(add-hook 'compilation-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'compilation-minor-mode-hook (lambda () (visual-line-mode 1)))

; from: https://zuttobenkyou.wordpress.com/2012/06/15/emacs-vimlike-tabwindow-navigation/
(defun vimlike-quit ()
  "Vimlike ':q' behavior: close current window if there are split windows;
otherwise, close current tab (elscreen)."
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

(evil-ex-define-cmd "q[uit]" 'vimlike-quit)

(setq dabbrev-case-replace nil)

(setq ac-ignore-case nil)
