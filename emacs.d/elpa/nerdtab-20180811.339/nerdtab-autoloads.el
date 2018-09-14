;;; nerdtab-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nerdtab" "nerdtab.el" (0 0 0 0))
;;; Generated autoloads from nerdtab.el

(defvar nerdtab-mode nil "\
Non-nil if Nerdtab mode is enabled.
See the `nerdtab-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `nerdtab-mode'.")

(custom-autoload 'nerdtab-mode "nerdtab" nil)

(autoload 'nerdtab-mode "nerdtab" "\
A global minor mode that provide tabs and activly update tab list.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nerdtab" '("nerdtab-" "define-nerdtab-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nerdtab-autoloads.el ends here
