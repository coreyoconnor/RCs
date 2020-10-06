;; configuration of modes

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              c-indent-level 4
              fill-column 101)

(electric-indent-mode 1)

(use-package json-mode
  :ensure t)

(use-package nix-mode
             :mode ("\\.nix\\'" . nix-mode)
             :config
             (progn
               (add-hook 'nix-mode-hook
                         (lambda ()
                           (setq-local evil-shift-width 2)
                           (setq-local tab-width 2)
                           (setq-local c-basic-offset 2)
                           (setq-local indent-line-function 'insert-tab)
                           (setq-local indent-line-function 'indent-relative)
                           )
                         )
               )
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
              (setq-local evil-shift-width 4)
              (setq-local tab-width 4)
              (setq-local c-basic-offset 4)
              (setq-local indent-line-function 'insert-tab)
              (setq-local tabs-always-indent t)
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

(setq is-lsp-enabled nil)

(defun enable-for-session ()
  (if (not is-lsp-enabled)
      (progn
        (add-hook 'scala-mode-hook 'lsp)
        (setq is-lsp-enabled t)
        )
    )
  )

(use-package dash-functional
  :ensure t)

(use-package helm
  :ensure t
  :config
  (progn
    (require 'helm-config)))

(use-package helm-projectile
  :after (:all projectile helm)
  :ensure t)

(require 'radian-autocomplete)

(use-package evil-avy
  :ensure t)

(use-package avy-menu
  :ensure t)

(use-package company
  :ensure t)

(use-package lsp-mode
  :ensure t
  :after (:all evil company)
  :config
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/lsp-ui"))
  (require 'lsp-ui)
  (require 'lsp-ui-flycheck)
  (require 'yasnippet)
  (add-hook 'lsp-mode-hook (lambda ()
                             (enable-for-session)
                             (lsp-ui-mode)
                             (helm-mode 1)
                             )
            )
  ;;(add-hook 'lsp-after-open-hook (lambda ()
  ;;                                    (lsp-ui-flycheck-enable t)
  ;;                                    )
  ;;            )

  (setq-default lsp-ui-sideline-diagnostic-max-lines 30)

  ;; (setq-default lsp-eldoc-enable-signature-help nil)
  (setq-default lsp-eldoc-enable-hover t)
  (setq-default lsp-ui-sideline-show-diagnostics t)
  (setq-default lsp-ui-sideline-enable t)
  (setq-default lsp-file-watch-threshold 1000000)
  (setq-default lsp-prefer-flymake nil)
  (push "[/\\\\]\\nixpkgs$" lsp-file-watch-ignored)

  ;; (setq-default lsp-ui-flycheck-live-reporting t)
  (define-key evil-normal-state-map (kbd "t t") 'helm-imenu)
  (define-key evil-normal-state-map (kbd "g d") 'xref-find-definitions)
  (define-key evil-insert-state-map "\C-n" 'company-complete))

(use-package lsp-metals
  :ensure t
  )

(use-package origami
  :load-path "origami"
  :demand t)

(use-package lsp-origami
  :load-path "lsp-origami"
  :demand t
  :after (:all origami lsp-mode)
  :config
  (progn
    (add-hook 'origami-mode-hook #'lsp-origami-mode)
    ))

(defun setup-scala-format ()
  (setq-local tab-width 2)
  (setq-local c-basic-offset 2)
  (setq-local evil-shift-width 2)
  (setq-local scala-indent:align-parameters t)
  )

(use-package scala-mode
  :ensure t
  :interpreter "scala"
  :mode ("\\.scala\\'" "\\.sc\\'")
  :magic-fallback ("/usr/bin/env amm" "/usr/bin/env -S amm")
  :config
  (progn
    (add-hook 'scala-mode-hook (lambda ()
                                 (cleanup-on-save)
                                 (flycheck-mode)
                                 (setup-scala-format)
                                 (auto-fill-mode)
                                 (origami-mode)
                                 ))
    )
  )

(use-package sbt-mode
  :ensure t
  :after scala-mode
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

(use-package dap-mode
  :ensure t
  :hook ((lsp-mode . dap-mode) (lsp-mode . dap-ui-mode))
  :config (setq dap-auto-configure-features (remove 'controls dap-auto-configure-features))
  )

;; Use the Tree View Protocol for viewing the project structure and triggering compilation
(use-package lsp-treemacs
  :ensure t
  :config
  ;; (lsp-metals-treeview-enable t)
  ;; (setq lsp-metals-treeview-show-when-views-received t)
  )

(eval-after-load 'js-mode
  (add-hook 'js-mode-hook
            (lambda ()
              (setq-local js-indent-level 2)
              (setq-local tab-width 2)
              (setq-local c-basic-offset 2)
              (setq-local evil-shift-width 2)
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
(use-package ruby-mode
  :ensure t
  :mode (("\\.rake\\'" . ruby-mode)
         ("Rakefile" . ruby-mode)
         ("Gemfile" . ruby-mode)
         )
  :config
  (progn
    (setq-default ruby-indent-level 2)
    (add-hook 'ruby-mode-hook
      (lambda () (setq-local evil-shift-width ruby-indent-level)))
    (add-hook 'ruby-mode-hook
              (lambda () (cleanup-on-save)))
    (autoload 'inf-ruby-minor-mode "inf-ruby" "Inferior ruby process" t)
    (require 'inf-ruby)
    (require 'web-mode)
    (setq-default web-mode-code-indent-offset 2)
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    )
 )

;(use-package proof-general
;  :ensure t
;  )
;
;(use-package coq-mode
;  :demand proof-general
;  :mode "\\.v\\'"
;  )

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

(provide 'configure-modes)
