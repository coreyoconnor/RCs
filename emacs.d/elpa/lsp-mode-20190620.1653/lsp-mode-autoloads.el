;;; lsp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "lsp-mode" "lsp-mode.el" (23819 62265 910579
;;;;;;  173000))
;;; Generated autoloads from lsp-mode.el

(autoload 'lsp "lsp-mode" "\
Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be openned in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("lsp-clients.el" "lsp-clojure.el" "lsp-css.el"
;;;;;;  "lsp-dart.el" "lsp-elm.el" "lsp-fsharp.el" "lsp-go.el" "lsp-html.el"
;;;;;;  "lsp-intelephense.el" "lsp-metals.el" "lsp-mode-pkg.el" "lsp-pyls.el"
;;;;;;  "lsp-rust.el" "lsp-solargraph.el" "lsp-vetur.el" "lsp-xml.el"
;;;;;;  "lsp.el") (23819 62267 118579 173000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; lsp-mode-autoloads.el ends here
