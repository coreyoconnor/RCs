;;; idle-highlight-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "idle-highlight-mode" "idle-highlight-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from idle-highlight-mode.el

(autoload 'idle-highlight-mode "idle-highlight-mode" "\
Idle-Highlight Minor Mode.

If called interactively, enable Idle-Highlight mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-idle-highlight-mode 'globalized-minor-mode t)

(defvar global-idle-highlight-mode nil "\
Non-nil if Global Idle-Highlight mode is enabled.
See the `global-idle-highlight-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-idle-highlight-mode'.")

(custom-autoload 'global-idle-highlight-mode "idle-highlight-mode" nil)

(autoload 'global-idle-highlight-mode "idle-highlight-mode" "\
Toggle Idle-Highlight mode in all buffers.
With prefix ARG, enable Global Idle-Highlight mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Idle-Highlight mode is enabled in all buffers where
`idle-highlight--turn-on' would do it.
See `idle-highlight-mode' for more information on Idle-Highlight mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "idle-highlight-mode" '("global-idle-highlight-ignore-buffer" "idle-highlight-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; idle-highlight-mode-autoloads.el ends here
