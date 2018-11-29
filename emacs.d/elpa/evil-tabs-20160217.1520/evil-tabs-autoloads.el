;;; evil-tabs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-tabs" "../../../../../.emacs.d/elpa/evil-tabs-20160217.1520/evil-tabs.el"
;;;;;;  "8e488b8fb77de86ae765a1ba49d4757f")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/evil-tabs-20160217.1520/evil-tabs.el

(defvar evil-tabs-mode nil "\
Non-nil if Evil-Tabs mode is enabled.
See the `evil-tabs-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `evil-tabs-mode'.")

(custom-autoload 'evil-tabs-mode "evil-tabs" nil)

(autoload 'evil-tabs-mode "evil-tabs" "\
Integrating Vim-style tabs for Evil mode users.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-tabs-mode "evil-tabs" "\
Enable `evil-tabs-mode' in the current buffer.

\(fn)" nil nil)

(autoload 'turn-off-evil-tabs-mode "evil-tabs" "\
Disable `evil-tabs-mode' in the current buffer.

\(fn)" nil nil)

(defvar global-evil-tabs-mode nil "\
Non-nil if Global Evil-Tabs mode is enabled.
See the `global-evil-tabs-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-tabs-mode'.")

(custom-autoload 'global-evil-tabs-mode "evil-tabs" nil)

(autoload 'global-evil-tabs-mode "evil-tabs" "\
Toggle Evil-Tabs mode in all buffers.
With prefix ARG, enable Global Evil-Tabs mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Tabs mode is enabled in all buffers where
`turn-on-evil-tabs-mode' would do it.
See `evil-tabs-mode' for more information on Evil-Tabs mode.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "evil-tabs" "../../../../../.emacs.d/elpa/evil-tabs-20160217.1520/evil-tabs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../.emacs.d/elpa/evil-tabs-20160217.1520/evil-tabs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-tabs" '("evil-tabs-mode-map")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/evil-tabs-20160217.1520/evil-tabs-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/evil-tabs-20160217.1520/evil-tabs.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-tabs-autoloads.el ends here
