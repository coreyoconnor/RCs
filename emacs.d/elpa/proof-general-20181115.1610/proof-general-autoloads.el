;;; proof-general-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pg-init" "pg-init.el" (0 0 0 0))
;;; Generated autoloads from pg-init.el

(eval-and-compile (defvar pg-init--script-full-path (or (and load-in-progress load-file-name) (bound-and-true-p byte-compile-current-file) (buffer-file-name))) (defvar pg-init--pg-root (file-name-directory pg-init--script-full-path)))

(unless (bound-and-true-p byte-compile-current-file) (require 'proof-site (expand-file-name "generic/proof-site" pg-init--pg-root)))

;;;***

;;;### (autoloads nil nil ("proof-general-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; proof-general-autoloads.el ends here