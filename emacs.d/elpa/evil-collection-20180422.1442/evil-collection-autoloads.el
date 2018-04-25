;;; evil-collection-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "evil-collection" "evil-collection.el" (23262
;;;;;;  49873 499054 461000))
;;; Generated autoloads from evil-collection.el

(autoload 'evil-collection-translate-key "evil-collection" "\
Translate keys in the keymap(s) corresponding to STATES and KEYMAPS.
STATES should be the name of an evil state, a list of states, or nil. KEYMAPS
should be a symbol corresponding to the keymap to make the translations in or a
list of keymap symbols. Like `evil-define-key', when a keymap does not exist,
the keybindings will be deferred until the keymap is defined, so
`with-eval-after-load' is not neccessary. TRANSLATIONS corresponds to a list of
key replacement pairs. For example, specifying \"a\" \"b\" will bind \"a\" to
\"b\"'s definition in the keymap. Specifying nil as a replacement will unbind a
key. If DESTRUCTIVE is nil, a backup of the keymap will be stored on the initial
invocation, and future invocations will always look up keys in the backup
keymap. When no TRANSLATIONS are given, this function will only create the
backup keymap without making any translations. On the other hand, if DESTRUCTIVE
is non-nil, the keymap will be destructively altered without creating a backup.
For example, calling this function multiple times with \"a\" \"b\" \"b\" \"a\"
would continue to swap and unswap the definitions of these keys. This means that
when DESTRUCTIVE is non-nil, all related swaps/cycles should be done in the same
invocation.

\(fn STATES KEYMAPS &rest TRANSLATIONS &key DESTRUCTIVE &allow-other-keys)" nil nil)

(function-put 'evil-collection-translate-key 'lisp-indent-function 'defun)

(autoload 'evil-collection-swap-key "evil-collection" "\
Wrapper around `evil-collection-translate-key' for swapping keys.
STATES, KEYMAPS, and ARGS are passed to `evil-collection-translate-key'. ARGS
should consist of key swaps (e.g. \"a\" \"b\" is equivalent to \"a\" \"b\" \"b\"
\"a\" with `evil-collection-translate-key') and optionally keyword arguments for
`evil-collection-translate-key'.

\(fn STATES KEYMAPS &rest ARGS)" nil t)

(function-put 'evil-collection-swap-key 'lisp-indent-function 'defun)

(autoload 'evil-collection-init "evil-collection" "\
Register the Evil bindings for all modes in `evil-collection-mode-list'.

Alternatively, you may register select bindings manually, for
instance:

  (with-eval-after-load 'calendar
    (require 'evil-collection-calendar)
    (evil-collection-calendar-setup))

If MODES is specified (as either one mode or a list of modes), use those modes
instead of the modes in `evil-collection-mode-list'.

\(fn &optional MODES)" t nil)

;;;***

;;;### (autoloads nil nil ("evil-collection-ace-jump-mode.el" "evil-collection-ag.el"
;;;;;;  "evil-collection-alchemist.el" "evil-collection-anaconda-mode.el"
;;;;;;  "evil-collection-arc-mode.el" "evil-collection-avy.el" "evil-collection-bookmark.el"
;;;;;;  "evil-collection-buff-menu.el" "evil-collection-calc.el"
;;;;;;  "evil-collection-calendar.el" "evil-collection-cider.el"
;;;;;;  "evil-collection-cmake-mode.el" "evil-collection-comint.el"
;;;;;;  "evil-collection-company.el" "evil-collection-compile.el"
;;;;;;  "evil-collection-cus-theme.el" "evil-collection-custom.el"
;;;;;;  "evil-collection-daemons.el" "evil-collection-debbugs.el"
;;;;;;  "evil-collection-debug.el" "evil-collection-diff-mode.el"
;;;;;;  "evil-collection-dired.el" "evil-collection-doc-view.el"
;;;;;;  "evil-collection-edebug.el" "evil-collection-eldoc.el" "evil-collection-elfeed.el"
;;;;;;  "evil-collection-elisp-mode.el" "evil-collection-elisp-refs.el"
;;;;;;  "evil-collection-emms.el" "evil-collection-epa.el" "evil-collection-ert.el"
;;;;;;  "evil-collection-eshell.el" "evil-collection-etags-select.el"
;;;;;;  "evil-collection-eval-sexp-fu.el" "evil-collection-evil-search.el"
;;;;;;  "evil-collection-eww.el" "evil-collection-flycheck.el" "evil-collection-free-keys.el"
;;;;;;  "evil-collection-geiser.el" "evil-collection-ggtags.el" "evil-collection-git-timemachine.el"
;;;;;;  "evil-collection-go-mode.el" "evil-collection-guix.el" "evil-collection-helm.el"
;;;;;;  "evil-collection-help.el" "evil-collection-ibuffer.el" "evil-collection-image+.el"
;;;;;;  "evil-collection-image.el" "evil-collection-indium.el" "evil-collection-info.el"
;;;;;;  "evil-collection-integration.el" "evil-collection-ivy.el"
;;;;;;  "evil-collection-js2-mode.el" "evil-collection-kotlin-mode.el"
;;;;;;  "evil-collection-log-view.el" "evil-collection-lsp-ui-imenu.el"
;;;;;;  "evil-collection-lua-mode.el" "evil-collection-macrostep.el"
;;;;;;  "evil-collection-magit.el" "evil-collection-man.el" "evil-collection-minibuffer.el"
;;;;;;  "evil-collection-neotree.el" "evil-collection-notmuch.el"
;;;;;;  "evil-collection-nov.el" "evil-collection-occur.el" "evil-collection-outline.el"
;;;;;;  "evil-collection-p4.el" "evil-collection-package-menu.el"
;;;;;;  "evil-collection-paren.el" "evil-collection-pass.el" "evil-collection-pdf.el"
;;;;;;  "evil-collection-pkg.el" "evil-collection-popup.el" "evil-collection-proced.el"
;;;;;;  "evil-collection-prodigy.el" "evil-collection-profiler.el"
;;;;;;  "evil-collection-python.el" "evil-collection-quickrun.el"
;;;;;;  "evil-collection-racer.el" "evil-collection-realgud.el" "evil-collection-reftex.el"
;;;;;;  "evil-collection-rjsx-mode.el" "evil-collection-robe.el"
;;;;;;  "evil-collection-rtags.el" "evil-collection-ruby-mode.el"
;;;;;;  "evil-collection-settings.el" "evil-collection-simple.el"
;;;;;;  "evil-collection-slime.el" "evil-collection-term.el" "evil-collection-tide.el"
;;;;;;  "evil-collection-transmission.el" "evil-collection-typescript-mode.el"
;;;;;;  "evil-collection-util.el" "evil-collection-vc-annotate.el"
;;;;;;  "evil-collection-vdiff.el" "evil-collection-view.el" "evil-collection-vlf.el"
;;;;;;  "evil-collection-wdired.el" "evil-collection-wgrep.el" "evil-collection-which-key.el"
;;;;;;  "evil-collection-woman.el" "evil-collection-xref.el" "evil-collection-ztree.el")
;;;;;;  (23262 49873 584055 366000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-collection-autoloads.el ends here
