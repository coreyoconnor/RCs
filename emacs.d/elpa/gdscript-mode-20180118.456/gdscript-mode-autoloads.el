;;; gdscript-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gdscript-mode" "gdscript-mode.el" (0 0 0 0))
;;; Generated autoloads from gdscript-mode.el

(add-to-list 'auto-mode-alist '("\\.gd\\'" . gdscript-mode))

(autoload 'gdscript-mode "gdscript-mode" "\
Major mode for editing Godot GDScript files

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gdscript-mode" '("gdscript-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gdscript-mode-autoloads.el ends here
