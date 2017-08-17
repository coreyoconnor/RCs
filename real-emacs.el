;; coreyoconnor: I don't have a good understanding of Lisp or Emacs

(defun configure-nav ()
  (autoload 'nav "nav" "nav" t)
  (enable-evil-nav)
  )

(defun enable-evil-nav ()
  (eval-after-load 'nav
    '(progn
       (nav-disable-overeager-window-splitting)
       (evil-make-overriding-map nav-mode-map 'normal t)
       (evil-define-key 'normal nav-mode-map
         "j" 'evil-next-line
         "k" 'evil-previous-line)
       )
    )
  )

(defun configure-UI ()
  (setq ring-bell-function 'ignore)
  (setq warning-minimum-level :emergency)
  (setq message-log-max t)
  (setq term-setup-hook
        '(lambda ()
            (setq keyboard-translate-table "\C-@\C-a\C-b\C-c\C-d\C-e\C-f\C-g\C-?")
            (global-set-key "\M-h" 'help-for-help)
            )
        )
  (configure-nav)
  )

(defun configure-elisp ()
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/local"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/evil-tabs"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/evil-numbers"))
  )

(defun configure-data-handling ()
  (setq create-lockfiles nil)
  )

(defun configure-default-formatting ()
  (setq-default indent-tabs-mode nil
                tab-width 4
                c-basic-offset 4)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  )

(defun configure-packages ()
  (setq use-package-always-ensure t)
  )

(defun configure ()
  (configure-elisp)
  (configure-data-handling)
  (configure-default-formatting)
  (configure-packages)
  (configure-UI)
  )

(configure)

(defun cleanup-on-save ()
  (add-hook 'write-contents-functions
            (lambda()
              (save-excursion
                (delete-trailing-whitespace))))
  )

(eval-after-load 'compilation-mode
  '(progn
    (evil-make-overriding-map compilation-mode 'normal t)
    (evil-define-key 'normal nav-mode-map
      "gt" 'elscreen-next
      "gT" 'elscreen-previous)))

(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

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

(window-numbering-mode)
(projectile-global-mode)

(setq projectile-enable-caching t)
(when (string-equal system-type "windows-nt")
  (setq projectile-indexing-method 'native)
  (set-default-font "Consolas 14")
  )

;; default text formatting options
(setq make-backup-files nil)
(setq-default fill-column 101)
(setq column-number-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; Enable electric indent but disable ?\n in some modes.
(electric-indent-mode 1)
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

(setq scroll-conservatively 5)
(setq scroll-margin 5)

; GUI options
(add-to-list 'default-frame-alist  '(width . 100) )
(require 'fill-column-indicator)

; plugins
(load-file (expand-file-name "~/.emacs.d/ProofGeneral/generic/proof-site.el"))

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
 '(nav-width 40)
 '(nxml-slash-auto-complete-flag t))

(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(when (file-readable-p (expand-file-name "~/.emacs-local.el"))
  (load-file (expand-file-name "~/.emacs-local.el"))
)

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
; (semantic-mode 1)

(autoload 'dirtree "dirtree" "dirtree" t)

(setq inhibit-default-init t)

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
(add-hook 'haskell-mode-hook
          '(lambda ()
             (turn-on-haskell-indentation)
             (haskell-indentation-enable-show-indentations)))

(setq blink-matching-paren nil)
(setq dabbrev-case-replace nil)

(setq ac-ignore-case nil)

;; from http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun bf-pretty-print-xml-region (begin end)
    "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
    (interactive "r")
    (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
        (message "Ah, much better!"))

;; Originally from stevey, adapted to support moving to a new directory.
;; http://stackoverflow.com/questions/384284/how-do-i-rename-an-open-file-in-emacs#384612
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     (buffer-file-name)))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
      (message "Renamed to %s." new-name)))
