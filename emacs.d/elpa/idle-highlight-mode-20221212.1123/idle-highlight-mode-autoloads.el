;;; idle-highlight-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "idle-highlight-mode" "idle-highlight-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from idle-highlight-mode.el

(autoload 'idle-highlight-mode "idle-highlight-mode" "\
Idle-Highlight Minor Mode.

This is a minor mode.  If called interactively, toggle the
`Idle-Highlight mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `idle-highlight-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

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
With prefix ARG, enable Global Idle-Highlight mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Idle-Highlight mode is enabled in all buffers where `idle-highlight--turn-on' would do it.

See `idle-highlight-mode' for more information on Idle-Highlight mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "idle-highlight-mode" '("global-idle-highlight-ignore-buffer" "idle-highlight-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; idle-highlight-mode-autoloads.el ends here
