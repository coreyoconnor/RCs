;; configuration of modes

(eval-after-load 'nix-mode
  (add-hook 'nix-mode-hook
            (lambda ()
              (setq-local indent-line-function #'indent-relative))))

(eval-after-load 'haskell-mode
  (add-hook 'haskell-mode-hook
            '(lambda ()
              (turn-on-haskell-indentation)
              (haskell-indentation-enable-show-indentations)))
  )

(eval-after-load 'gdscript-mode 
  (add-hook 'gdscript-mode-hook
	    '(lambda ()
		(setq evil-shift-width 4)
		(setq tab-width 4)
		(setq c-basic-offset 4)
		(setq indent-tabs-mode t)))
  )

(provide 'configure-modes)
