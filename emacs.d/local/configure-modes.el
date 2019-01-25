;; configuration of modes

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              c-indent-level 4
              fill-column 101)

(electric-indent-mode 1)

(use-package nix-mode
             :mode ("nix" . nix-mode)
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

(defun scalafmt-scala-format ()
  (setq-local tab-width 2)
  (setq-local c-basic-offset 2)
  (setq-local evil-shift-width 2)
  (setq-local scala-indent:align-parameters t)
  )

(use-package ht
  :ensure t)

(use-package spinner
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (progn
    (global-flycheck-mode)
    )
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

(use-package lsp
  :load-path "lsp-mode"
  :demand t
  :config (progn
            (add-to-list 'load-path (expand-file-name "~/.emacs.d/lsp-ui"))
            (require 'lsp-ui)
            (require 'lsp-ui-flycheck)
            (add-hook 'lsp-mode-hook 'lsp-ui-mode)
            (add-hook 'lsp-mode-hook 'enable-for-session)
            (setq lsp-ui-sideline-show-diagnostics t)
            (setq lsp-ui-sideline-enable t)
            )
  )

(use-package lsp-mode
  :demand lsp
  )

(use-package scala-mode
  :ensure t
  :interpreter "scala"
  :mode "\\.scala\\'"
  :config
  (progn
    (add-hook 'scala-mode-hook 'cleanup-on-save)
    (add-hook 'scala-mode-hook 'scalafmt-scala-format)
    (add-hook 'scala-mode-hook 'flycheck-mode)
    )
  )

(use-package sbt-mode
  :ensure t
  :after scala-mode
  )

(use-package lsp-scala
  :load-path "lsp-scala"
  :demand t
  :after (:all scala-mode sbt-mode lsp)
  )

(use-package llvm-mode
  :load-path "llvm-mode"
  :demand t
  :mode "\\.ll\\'"
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

(use-package prolog-mode
  :ensure t
  :mode "pl"
  )

(use-package markdown-mode
  :ensure t
  :mode "md"
  )

(provide 'configure-modes)
