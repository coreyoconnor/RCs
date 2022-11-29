;;; evil-avy-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-avy" "evil-avy.el" (0 0 0 0))
;;; Generated autoloads from evil-avy.el

(defvar evil-avy-mode nil "\
Non-nil if Evil-Avy mode is enabled.
See the `evil-avy-mode' command
for a description of this minor mode.")

(custom-autoload 'evil-avy-mode "evil-avy" nil)

(autoload 'evil-avy-mode "evil-avy" "\
Toggle evil-avy-mode.
Interactively with no argument, this command toggles the mode. A
positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode,`toggle' toggles the state.

This is a minor mode.  If called interactively, toggle the
`Evil-Avy mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='evil-avy-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When evil-avy-mode is active, it replaces some the normal, visual, operator
and motion state keybindings to invoke avy commands.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "evil-avy" '("avy-forward-char-in-line"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-avy-autoloads.el ends here
