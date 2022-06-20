;;; avy-menu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "avy-menu" "../../../../../.emacs.d/elpa/avy-menu-20210321.1732/avy-menu.el"
;;;;;;  "24937323b26b18aca3c497bdb60bbf27")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/avy-menu-20210321.1732/avy-menu.el

(autoload 'avy-menu "avy-menu" "\
Show a popup menu in a temporary window and return user's selection.

BUFFER-OR-NAME specifies the name of the buffer (or the buffer
itself) that hosts the menu options.  MENU should be a list of
the form (TITLE PANE1 PANE2 …), where each pane is a list of the
form (TITLE ITEM1 ITEM2 …).  Each item is normally a cons
cell (STRING . VALUE), but a string can appear as an item—that
adds a non-selectable item in the menu.  Also, empty strings
start new sub-sections.

If SHOW-PANE-HEADER is not NIL, show pane headers (titles),
otherwise hide them.

The returned value is VALUE if user has selected something and
NIL if they have cancelled the menu or pressed a key that does
not correspond to an option in the menu.

\(fn BUFFER-OR-NAME MENU &optional SHOW-PANE-HEADER)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "avy-menu" "../../../../../.emacs.d/elpa/avy-menu-20210321.1732/avy-menu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../.emacs.d/elpa/avy-menu-20210321.1732/avy-menu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "avy-menu" '("avy-menu--insert-strings")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/avy-menu-20210321.1732/avy-menu-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/avy-menu-20210321.1732/avy-menu.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; avy-menu-autoloads.el ends here
