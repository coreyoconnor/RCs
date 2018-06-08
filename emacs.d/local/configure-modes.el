;; configuration of modes

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              c-indent-level 4
              fill-column 101)

(electric-indent-mode 1)

(eval-after-load 'nix-mode
  (add-hook 'nix-mode-hook
            (lambda ()
              (setq-local indent-line-function 'indent-relative))))

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

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'cleanup-on-save)
(defun scalafmt-scala-format ()
  (setq-local tab-width 2)
  (setq-local c-basic-offset 2)
  (setq-local evil-shift-width 2)
  (setq-local scala-indent:align-parameters t)
  )

(add-hook 'scala-mode-hook 'scalafmt-scala-format)

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
(eval-after-load 'ruby-mode
 '(progn
    (setq-default ruby-indent-level 2)
    (add-hook 'ruby-mode-hook
      (lambda () (setq-local evil-shift-width ruby-indent-level)))
    (add-hook 'ruby-mode-hook
      (lambda () (cleanup-on-save)))))

(setq auto-mode-alist (cons '("\\.rake\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Gemfile" . ruby-mode) auto-mode-alist))

(provide 'configure-modes)
