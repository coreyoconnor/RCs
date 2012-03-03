;;; contrib-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (eassist-list-methods eassist-switch-h-cpp) "eassist"
;;;;;;  "../../../../../.emacs.d/cedet/contrib/eassist.el" (20304
;;;;;;  37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/contrib/eassist.el

(defvar eassist-header-switches '(("h" "cpp" "cc" "c") ("hpp" "cpp" "cc") ("cpp" "h" "hpp") ("c" "h") ("C" "H") ("H" "C" "CPP" "CC") ("cc" "h" "hpp")) "\
This variable defines possible switches for `eassist-switch-h-cpp' function.
Its format is list of (from . (to1 to2 to3...)) elements.  From and toN are
strings which are extentions of the files.")

(autoload 'eassist-switch-h-cpp "eassist" "\
Switch header and body file according to `eassist-header-switches' var.
The current buffer's file name extention is searched in
`eassist-header-switches' variable to find out extention for file's counterpart,
for example *.hpp <--> *.cpp.

\(fn)" t nil)

(autoload 'eassist-list-methods "eassist" "\
Show method/function list of current buffer in a newly created buffer.
This function is recommended to be bound to some convinient hotkey.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ede-gnustep" "../../../../../.emacs.d/cedet/contrib/ede-gnustep.el"
;;;;;;  (20304 37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/contrib/ede-gnustep.el

(add-to-list 'ede-project-class-files (ede-project-autoload "edegnustep" :name "GNUstep-Make" :file 'ede-gnustep :proj-file "ProjStep.ede" :load-type 'ede-step-load :class-sym 'ede-step-project) t)

(add-to-list 'ede-project-class-files (ede-project-autoload "gnustep-root" :name "GNUstep-make Top Most" :file 'ede-gnustep :proj-file "RootProjStep.ede" :initializers '(:project-mode scanner) :load-type 'ede-gnustep-load :class-sym 'ede-step-project) t)

(add-to-list 'ede-project-class-files (ede-project-autoload "gnustep" :name "GNUstep-Make in scanner mode" :file 'ede-gnustep :proj-file "ProjStep.ede" :initializers '(:project-mode scanner) :load-type 'ede-gnustep-load :class-sym 'ede-step-project) t)

(add-to-list 'auto-mode-alist '("\\(Root\\)?ProjStep\\.ede" . emacs-lisp-mode))

;;;***

;;;### (autoloads (semantic-tag-folding-mode global-semantic-tag-folding-mode
;;;;;;  global-semantic-tag-folding-mode) "semantic-tag-folding"
;;;;;;  "../../../../../.emacs.d/cedet/contrib/semantic-tag-folding.el"
;;;;;;  (20304 37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/contrib/semantic-tag-folding.el

(defvar global-semantic-tag-folding-mode nil "\
*If non-nil enable global use of variable `semantic-tag-folding-mode'.
With this mode enabled, a new folding decoration mode is added.
Clicking on a + or - in the fringe will fold that tag.")

(custom-autoload 'global-semantic-tag-folding-mode "semantic-tag-folding" nil)

(autoload 'global-semantic-tag-folding-mode "semantic-tag-folding" "\
Toggle global use of option `semantic-tag-folding-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(autoload 'semantic-tag-folding-mode "semantic-tag-folding" "\
Minor mode mark semantic tags for folding.
This mode will display +/- icons in the fringe.  Clicking on them
will fold the current tag.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (wisent-csharp-default-setup) "wisent-csharp" "../../../../../.emacs.d/cedet/contrib/wisent-csharp.el"
;;;;;;  (20304 37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/contrib/wisent-csharp.el

(autoload 'wisent-csharp-default-setup "wisent-csharp" "\
Not documented

\(fn)" nil nil)

(add-hook 'csharp-mode-hook #'wisent-csharp-default-setup)

;;;***

;;;### (autoloads (wisent-php-default-setup) "wisent-php" "../../../../../.emacs.d/cedet/contrib/wisent-php.el"
;;;;;;  (20304 37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/contrib/wisent-php.el

(autoload 'wisent-php-default-setup "wisent-php" "\
Hook run to setup Semantic in `php-mode'.
Use the alternate LALR(1) parser.

\(fn)" nil nil)

(add-hook 'php-mode-hook #'wisent-php-default-setup)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/cedet/contrib/cedet-contrib-load.el"
;;;;;;  "../../../../../.emacs.d/cedet/contrib/cedet-contrib.el"
;;;;;;  "../../../../../.emacs.d/cedet/contrib/eassist.el" "../../../../../.emacs.d/cedet/contrib/ede-gnustep.el"
;;;;;;  "../../../../../.emacs.d/cedet/contrib/semantic-ectag-scala.el"
;;;;;;  "../../../../../.emacs.d/cedet/contrib/semantic-tag-folding.el"
;;;;;;  "../../../../../.emacs.d/cedet/contrib/wisent-csharp-wy.el"
;;;;;;  "../../../../../.emacs.d/cedet/contrib/wisent-csharp.el"
;;;;;;  "../../../../../.emacs.d/cedet/contrib/wisent-php-wy.el"
;;;;;;  "../../../../../.emacs.d/cedet/contrib/wisent-php.el" "../../../../../.emacs.d/cedet/contrib/wisent-ruby-wy.el"
;;;;;;  "../../../../../.emacs.d/cedet/contrib/wisent-ruby.el") (20305
;;;;;;  30510 993234))

;;;***

(provide 'contrib-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; contrib-loaddefs.el ends here
