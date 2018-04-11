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

(provide 'configure-modes)
