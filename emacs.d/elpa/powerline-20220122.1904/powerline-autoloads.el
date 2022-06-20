;;; powerline-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "powerline" "../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline.el"
;;;;;;  "4e17b19febf196748a8b46df50922709")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline.el

(autoload 'powerline-hud "powerline" "\
Return XPM of relative buffer location using FACE1 and FACE2 of optional WIDTH.

\(fn FACE1 FACE2 &optional WIDTH)" nil nil)

(autoload 'powerline-mouse "powerline" "\
Return mouse handler for CLICK-GROUP given CLICK-TYPE and STRING.

\(fn CLICK-GROUP CLICK-TYPE STRING)" nil nil)

(autoload 'powerline-concat "powerline" "\
Concatonate STRINGS and pad sides by spaces.

\(fn &rest STRINGS)" nil nil)

(autoload 'defpowerline "powerline" "\
Create function NAME by wrapping BODY with powerline padding an propetization.

\(fn NAME BODY)" nil t)

(autoload 'powerline-raw "powerline" "\
Render STR as mode-line data using FACE and optionally PAD import.
PAD can be left (`l') or right (`r').

\(fn STR &optional FACE PAD)" nil nil)

(autoload 'powerline-fill "powerline" "\
Return empty space using FACE and leaving RESERVE space on the right.

\(fn FACE RESERVE)" nil nil)
 (autoload 'powerline-major-mode "powerline")
 (autoload 'powerline-minor-modes "powerline")
 (autoload 'powerline-narrow "powerline")
 (autoload 'powerline-vc "powerline")
 (autoload 'powerline-encoding "powerline")
 (autoload 'powerline-buffer-size "powerline")
 (autoload 'powerline-buffer-id "powerline")
 (autoload 'powerline-process "powerline")
 (autoload 'powerline-selected-window-active "powerline")

;;;### (autoloads "actual autoloads are elsewhere" "powerline" "../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "powerline" '("pl/" "powerline-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "powerline-separators"
;;;;;;  "../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline-separators.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline-separators.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "powerline-separators" '("pl/" "powerline-image-apple-rgb")))

;;;***

;;;### (autoloads nil "powerline-themes" "../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline-themes.el"
;;;;;;  "81fa94bb242707ccc900931ca36984d1")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline-themes.el

(autoload 'powerline-default-theme "powerline-themes" "\
Setup the default mode-line." t nil)

(autoload 'powerline-center-theme "powerline-themes" "\
Setup a mode-line with major and minor modes centered." t nil)

(autoload 'powerline-vim-theme "powerline-themes" "\
Setup a Vim-like mode-line." t nil)

(autoload 'powerline-nano-theme "powerline-themes" "\
Setup a nano-like mode-line." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "powerline-themes"
;;;;;;  "../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline-themes.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline-themes.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "powerline-themes" '("powerline-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline-pkg.el"
;;;;;;  "../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline-separators.el"
;;;;;;  "../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline-themes.el"
;;;;;;  "../../../../../.emacs.d/elpa/powerline-20220122.1904/powerline.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; powerline-autoloads.el ends here
