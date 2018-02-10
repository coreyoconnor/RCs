;; configuration of file navigation features

(defun configure-nav-package ()
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

(defun configure-outline-minor-mode ()
  (define-prefix-command 'cm-map nil "outline-")
  (define-key cm-map "f" 'outline-minor-mode)
  (define-key cm-map "t" 'outline-toggle-children)
  (global-set-key "\M-f" cm-map)
  )

(window-numbering-mode)

(configure-nav-package)
(configure-outline-minor-mode)
(enable-evil-nav)

(provide 'configure-navigation)
