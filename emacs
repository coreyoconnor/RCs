; -*-Lisp-*-

(when (memq window-system '(mac ns x))
;; overriding image.el function image-type-available-p

(defun image-type-available-p (type)
    "Return t if image type TYPE is available. Image types are symbols like `xbm' or `jpeg'."
    (if (eq 'svg type)
        nil
    (and (fboundp 'init-image-library)
            (init-image-library type))))
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(background-color nil)
 '(background-mode dark)
 '(column-number-mode t)
 '(company-idle-delay nil)
 '(connection-local-criteria-alist
   '(((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "ip-172-16-11-59.ec2.internal")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)
     ((:application eshell)
      eshell-connection-default-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))
     (eshell-connection-default-profile
      (eshell-path-env-list))))
 '(cursor-color nil)
 '(custom-safe-themes
   '("ec815e06ead0ec81514ec142bc3dbf89f822d7389bfe0ddc3b474c78b5eb0426" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))
 '(ecb-auto-activate t)
 '(ecb-display-default-dir-after-start t)
 '(ecb-fix-window-size 'width)
 '(ecb-layout-name "dironly")
 '(ecb-layout-window-sizes
   '(("basic"
      (ecb-directories-buffer-name 0.25728155339805825 . 0.48214285714285715)
      (ecb-analyse-buffer-name 0.25728155339805825 . 0.5))
     ("left8"
      (ecb-directories-buffer-name 0.1796116504854369 . 0.26785714285714285)
      (ecb-sources-buffer-name 0.1796116504854369 . 0.25)
      (ecb-methods-buffer-name 0.1796116504854369 . 0.2857142857142857)
      (ecb-history-buffer-name 0.1796116504854369 . 0.17857142857142858))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2)
 '(ecb-show-sources-in-directories-buffer 'always)
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 40)
 '(elscreen-display-tab nil)
 '(evil-undo-system 'undo-tree)
 '(foreground-color nil)
 '(helm-minibuffer-history-key "M-p")
 '(inhibit-startup-screen t)
 '(lsp-ui-flycheck-enable t)
 '(lsp-ui-sideline-show-diagnostics t)
 '(nav-width 40)
 '(nxml-slash-auto-complete-flag t)
 '(package-selected-packages
   '(helm-lsp selectrum-prescient selectrum 2bit markdown-mode treemacs shell-maker lv magit magit-popup mmm-mode nix-mode openai origami pfuture php-mode pkg-info popup posframe powerline projectile queue request rich-minority s sbt-mode scala-mode smart-mode-line spinner ssass-mode tablist tblui transient treemacs-evil treemacs-projectile undo-tree vue-html-mode which-key xcscope yaml ac-php-core ace-window annalist async avy avy-menu bui ccls centaur-tabs cfrs codegpt compat dash diminish edit-indirect epl evil-avy f flycheck git-commit goto-chg helm helm-core helm-projectile ht js1-mode json-mode json-snatcher lsp-origami lsp-treemacs lsp-ui dap-mode lsp-java lsp-metals lsp-docker chatgpt-shell vue-mode helm-ag treemacs-magit company-php phps-mode chatgpt jetbrains-darcula-theme javap-mode elscreen elscreen-buffer-group elscreen-fr mmm-jinja2 poly-ansible ac-php composer php-scratch ada-mode rjsx-mode js-format js-import js-react-redux-yasnippets exec-path-from-shell jinja2-mode yatemplate yasnippet-snippets fold-this seq glsl-mode nerdtab go-mode evil-collection gdscript-mode js2-highlight-vars string-inflection yasnippet yard-mode yaml-mode window-numbering web-mode swiper smex rubocop robe projectile-rails paredit omniref nav mediawiki ido-ubiquitous idle-highlight-mode hydra hindent haml-mode groovy-mode gradle-mode fringe-helper flymake-ruby evil enh-ruby-mode ecb dirtree company color-theme-solarized color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized auto-complete))
 '(safe-local-variable-values
   '((haskell-indentation-where-post-offset . 4)
     (haskell-indentation-where-pre-offset . 4)
     (haskell-indentation-ifte-offset . 4)
     (haskell-indentation-left-offset . 4)
     (haskell-indentation-starter-offset . 4)
     (haskell-indentation-layout-offset . 4)))
 '(treemacs-no-png-images t)
 '(warning-suppress-log-types '((comp) (comp) (comp)))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)

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
      (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "JAVA_HOME"))
        (add-to-list 'exec-path-from-shell-variables var))
      (exec-path-from-shell-initialize)
      )
    )

  (configure-local-overrides)

  (add-to-list 'exec-path (expand-file-name "~/.local/share/coursier/bin"))

  (configure-local-overrides)
  (configure-data-handling)
  (configure-interface)
  (configure-modes)
  (configure-formatting)
  (configure-display)
  (configure-navigation)
  )

(defun configure-package-manager ()
  ;; (setq inhibit-default-init t)
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

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)
  (setq use-package-always-ensure 't)
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
