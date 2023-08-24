;; configuration of modes
;;; Code:

(add-to-list 'completion-styles 'flex)

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              c-indent-level 4
              fill-column 101)

(electric-indent-mode 1)

(use-package groovy-mode
  :ensure t
  :after (:all evil)
  :mode (("\\.groovy\\'" . groovy-mode) ("Jenkinsfile" . groovy-mode))
  :config
  (setq evil-shift-width 2)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq c-indent-level 2)
  (setq groovy-indent-offset 2)
  )

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  )

(use-package nix-mode
  :ensure t
  :after (:all evil)
  :mode "\\.nix\\'"
  :config
  (setq evil-shift-width 2)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq indent-line-function 'insert-tab)
  (setq indent-line-function 'indent-relative)
  )

(eval-after-load 'haskell-mode
  (add-hook 'haskell-mode-hook
            (lambda ()
              (turn-on-haskell-indentation)
              (haskell-indentation-enable-show-indentations)))
  )

(eval-after-load 'gdscript-mode
  (add-hook 'gdscript-mode-hook
            (lambda ()
              (setq evil-shift-width 4)
              (setq tab-width 4)
              (setq c-basic-offset 4)
              (setq indent-line-function 'insert-tab)
              (setq tabs-always-indent t)
              )
            )
  )

(use-package ht
  :ensure t)

(use-package spinner
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  )

(use-package dash
  :ensure t)

(use-package helm-core
  :ensure t)

(use-package wfnames
  :ensure t
  )

(use-package helm
  :ensure t
  :after (:all wfnames helm-core)
  :diminish
  :config
  (helm-mode t)
  )

(use-package helm-projectile
  :after (:all projectile helm)
  :diminish
  :ensure t)

(use-package helm-ag
  :after (:all helm)
  :ensure t)

(use-package evil-avy
  :ensure t
  :after (:all evil avy-menu))

(use-package avy-menu
  :ensure t)

(use-package company
  :ensure t
  :init
  (setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf
         company-yasnippet
         )
        (company-abbrev company-dabbrev)
        ))
  :config
  (global-company-mode 't)
  )

(use-package yasnippet
  :ensure t
  :after (:all company)
  :config
  (yas-global-mode)
  ;;(add-to-list 'company-yasnippet 'company-backends)
  )

(use-package lsp-mode
  :ensure t
  :after (:all evil company)
  :hook lsp-lens-mode
  :hook lsp-ui-mode
  :hook helm-mode
  :hook (
         (scala-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :config

  (setq-default lsp-file-watch-threshold nil)
  (setq-default lsp-prefer-flymake nil)
  (setq-default lsp-enable-on-type-formatting nil)
  (setq-default lsp-debounce-full-sync-notifications-interval 2.0)
  (setq-default lsp-idle-delay 1.5)
  (setq-default lsp-response-timeout 120)
  (setq-default lsp-completion-provider :capf)
  (push "[/\\\\]\\alldocs\\\\" lsp-file-watch-ignored-directories)

  (setq gc-cons-threshold 100000000) ;; 100mb
  (setq read-process-output-max (* 4096 1024)) ;; 4mb
  ;; (setq lsp-log-io nil)

  )

(use-package lsp-ui
  :ensure t
  :after (:all evil lsp-mode company helm)
  :commands lsp-ui-mode
  :config

  (add-hook 'lsp-ui-flycheck-list-mode-hook (lambda()
                                              (setq truncate-lines nil)
                                              (visual-line-mode)
                                              )
            )

  (defun fix-flycheck-list-size ()
    (let ((window (get-buffer-window lsp-ui-flycheck-list--buffer t)))
      (fit-window-to-buffer window 9 10 nil nil t)
      )
    )

  (setq-default lsp-ui-flycheck-list-position 'right)

  ;;(advice-add 'lsp-ui-flycheck-list :after #'fix-flycheck-list-size)
  (define-key lsp-ui-flycheck-list-mode-map (kbd "RET") 'lsp-ui-flycheck-list--visit)
  (define-key lsp-ui-flycheck-list-mode-map (kbd "q") 'lsp-ui-flycheck-list--quit)
  (evil-make-overriding-map lsp-ui-flycheck-list-mode-map nil)

  (setq-default lsp-ui-sideline-diagnostic-max-lines 30)

  ;; (setq-default lsp-eldoc-enable-signature-help nil)
  (setq-default lsp-eldoc-enable-hover t)
  (setq-default lsp-ui-sideline-show-diagnostics t)
  (setq-default lsp-ui-sideline-enable t)
  ;;
  ;; (setq-default lsp-ui-flycheck-live-reporting t)
  (define-key evil-normal-state-map (kbd "t t") 'helm-imenu)
  (define-key evil-normal-state-map (kbd "g d") 'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "g e") 'lsp-treemacs-errors-list)
  (define-key evil-normal-state-map (kbd "g a") 'lsp-ui-sideline-apply-code-actions)
  (define-key evil-normal-state-map (kbd "g l") 'lsp-avy-lens)
  (define-key evil-normal-state-map (kbd "g s") 'lsp-ui-imenu)
  (define-key evil-normal-state-map (kbd "C-c h") 'lsp-ui-doc-glance)
  (define-key evil-insert-state-map (kbd "C-n") 'company-complete)
  )

(use-package lsp-metals
  :ensure t
  :after (:all lsp scala-mode)
  :custom
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
  )

(use-package origami
  :ensure t
  )

(use-package lsp-origami
  :ensure t
  :after (:all origami lsp-mode)
  :config
  (add-hook 'origami-mode-hook #'lsp-origami-mode)
  )

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package scala-mode
  :ensure t
  :interpreter "scala"
  :mode ("\\.scala\\'" "\\.sc\\'")
  :magic-fallback ("/usr/bin/env amm" "/usr/bin/env -S amm")
  :diminish
  :hook  ((scala-mode . origami-mode) (scala-mode . flyspell-prog-mode))
  :config

  (setq-default tab-width 2)
  (setq-default c-basic-offset 2)
  (setq-default evil-shift-width 2)
  (setq-default scala-indent:align-parameters t)
  (evil-define-key 'insert scala-mode-map (kbd "C-c c") 'openai-complete-scala-continue)
  (evil-define-key 'normal scala-mode-map (kbd "g ?") 'openai-complete-scala-auto-region-fill-in)
  (cleanup-on-save)
  )

(use-package sbt-mode
  :ensure t
  :after scala-mode
  :commands sbt-start sbt-command
  :diminish
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )

(use-package llvm-mode
  :load-path "llvm-mode"
  :demand t
  :mode "\\.ll\\'"
  )

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  :ensure t
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )

(use-package lsp-docker
  :ensure t
  )

(use-package dap-mode
  :ensure t
  :after (:all lsp-mode lsp-docker)
  :hook ((lsp-mode . dap-mode) (lsp-mode . dap-ui-mode))
  :config (setq dap-auto-configure-features (remove 'controls dap-auto-configure-features))
  )

(use-package treemacs
  :ensure t
  :after (:all evil)
  :init
  (customize-set-variable 'treemacs-no-png-images t)
  :config
  (setq treemacs-collapse-dirs 5)
  (setq treemacs-width 55)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode nil)
  (treemacs-fringe-indicator-mode 'always)
  (define-key evil-normal-state-map (kbd "TAB") 'treemacs-TAB-action)
  (define-key evil-normal-state-map (kbd "RET") 'treemacs-RET-action)
  )

;; Use the Tree View Protocol for viewing the project structure and triggering compilation
(use-package lsp-treemacs
  :ensure t
  :after (:all lsp-mode treemacs)
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode t))

(use-package treemacs-projectile
  :ensure t
  :after (:all treemacs projectile)
  )

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-evil
  :ensure t
  :after treemacs
  :bind (:map evil-normal-state-map
              ("T" . treemacs)))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq js-indent-level 2)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq evil-shift-width 2)
  (setq js2-strict-missing-semi-warning nil)
  (cleanup-on-save))

(eval-after-load 'haml-mode
  (add-hook 'haml-mode-hook
            (lambda ()
              (setq-local electric-indent-chars (remq ?\n electric-indent-chars))))
  )

;; ruby
(use-package ruby-mode
  :ensure t
  :mode (("\\.rake\\'" . ruby-mode)
         ("Rakefile" . ruby-mode)
         ("Gemfile" . ruby-mode)
         )
  :config
    (setq-default ruby-indent-level 2)
    (add-hook 'ruby-mode-hook
      (lambda () (setq evil-shift-width ruby-indent-level)))
    (add-hook 'ruby-mode-hook
              (lambda () (cleanup-on-save)))
    (autoload 'inf-ruby-minor-mode "inf-ruby" "Inferior ruby process" t)
    (require 'inf-ruby)
    (require 'web-mode)
    (setq-default web-mode-code-indent-offset 2)
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
 )

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  )

;(use-package prolog-mode
;  :ensure t
;  :mode "\\.pl\\'"
;  )

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  )

(use-package ccls
  :ensure t
  )

(use-package mmm-mode
  :ensure t
  :config
  (setq mmm-global-mode 'maybe)
  (mmm-add-classes '((markdown-scala
                      :submode scala-mode
                      :face mmm-declaration-submode-face
                      :front "^```scala.*$"
                      :back "^```.*$")))
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-scala)
  )

(use-package vue-mode
  :ensure t
  )

(use-package yaml-mode
  :ensure t
  :mode "\\.yml.j2\\'"
  )

(use-package mmm-jinja2
  :ensure t
  :config
  (mmm-add-mode-ext-class 'yaml-mode "\\.yml.j2\\'" 'jinja2)
  )

(use-package lsp-java
  :ensure t
  :after (:all treemacs lsp-mode dap-mode)
  :config
  (add-hook 'java-mode-hook 'lsp)
  )

(use-package shell-maker
  :ensure t
  )

(use-package chatgpt
  :ensure t
  :after (:all shell-maker)
  )

;; (use-package phps-mode
;;   :after flycheck
;;   :ensure t
;;   :mode ("\\.php\\'" "\\.phtml\\'")
;;   :config
;;   (phps-mode-flycheck-setup)
;;   (setq phps-mode-async-process t)
;;   (setq phps-mode-async-process-using-async-el t))

(use-package php-mode
  :ensure t
  :after (:all evil)
  :config
  (evil-define-key 'insert php-mode-map (kbd "C-c c") 'openai-complete-php-continue)
  )

(use-package company-php
  :ensure t
  :after (:all company php-mode)
  )

(provide 'configure-modes)
