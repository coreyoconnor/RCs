;;; eieio-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (chart-test-it-all) "chart" "../../../../../.emacs.d/cedet/eieio/chart.el"
;;;;;;  (20304 37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/eieio/chart.el

(autoload 'chart-test-it-all "chart" "\
Test out various charting features.

\(fn)" t nil)

;;;***

;;;### (autoloads (data-debug-show data-debug-insert-object-button
;;;;;;  data-debug-insert-object-slots) "eieio-datadebug" "../../../../../.emacs.d/cedet/eieio/eieio-datadebug.el"
;;;;;;  (20304 37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/eieio/eieio-datadebug.el

(autoload 'data-debug-insert-object-slots "eieio-datadebug" "\
Insert all the slots of OBJECT.
PREFIX specifies what to insert at the start of each line.

\(fn OBJECT PREFIX)" nil nil)

(autoload 'data-debug-insert-object-button "eieio-datadebug" "\
Insert a button representing OBJECT.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between PREFIX and the object button.

\(fn OBJECT PREFIX PREBUTTONTEXT)" nil nil)

(autoload 'data-debug-show "eieio-datadebug" "\
Run ddebug against any EIEIO object OBJ

\(fn (OBJ eieio-default-superclass))" nil nil)

;;;***

;;;### (autoloads (eieio-describe-generic eieio-build-class-alist
;;;;;;  eieio-describe-constructor eieio-describe-class eieio-browse)
;;;;;;  "eieio-opt" "../../../../../.emacs.d/cedet/eieio/eieio-opt.el"
;;;;;;  (20304 37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/eieio/eieio-opt.el

(autoload 'eieio-browse "eieio-opt" "\
Create an object browser window to show all objects.
If optional ROOT-CLASS, then start with that, otherwise start with
variable `eieio-default-superclass'.

\(fn &optional ROOT-CLASS)" t nil)

(defalias 'describe-class 'eieio-describe-class)

(autoload 'eieio-describe-class "eieio-opt" "\
Describe a CLASS defined by a string or symbol.
If CLASS is actually an object, then also display current values of that obect.
Optional HEADERFCN should be called to insert a few bits of info first.

\(fn CLASS &optional HEADERFCN)" t nil)

(autoload 'eieio-describe-constructor "eieio-opt" "\
Describe the constructor function FCN.
Uses `eieio-describe-class' to describe the class being constructed.

\(fn FCN)" t nil)

(autoload 'eieio-build-class-alist "eieio-opt" "\
Return an alist of all currently active classes for completion purposes.
Optional argument CLASS is the class to start with.
If INSTANTIABLE-ONLY is non nil, only allow names of classes which
are not abstract, otherwise allow all classes.
Optional argument BUILDLIST is more list to attach and is used internally.

\(fn &optional CLASS INSTANTIABLE-ONLY BUILDLIST)" nil nil)

(defalias 'describe-method 'eieio-describe-generic)

(defalias 'describe-generic 'eieio-describe-generic)

(defalias 'eieio-describe-method 'eieio-describe-generic)

(autoload 'eieio-describe-generic "eieio-opt" "\
Describe the generic function GENERIC.
Also extracts information about all methods specific to this generic.

\(fn GENERIC)" t nil)

;;;***

;;;### (autoloads (eieio-perftest-onemethodcall eieio-perftest-methodcall)
;;;;;;  "eieio-perftest" "../../../../../.emacs.d/cedet/eieio/eieio-perftest.el"
;;;;;;  (20304 37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/eieio/eieio-perftest.el

(autoload 'eieio-perftest-methodcall "eieio-perftest" "\
Test and time performance of method invocation.

\(fn)" t nil)

(autoload 'eieio-perftest-onemethodcall "eieio-perftest" "\
Test and time performance of method invocation.

\(fn)" t nil)

;;;***

;;;### (autoloads (enable-visual-studio-bookmarks) "linemark" "../../../../../.emacs.d/cedet/eieio/linemark.el"
;;;;;;  (20304 37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/eieio/linemark.el

(autoload 'enable-visual-studio-bookmarks "linemark" "\
Bind the viss bookmark functions to F2 related keys.
\\<global-map>
\\[viss-bookmark-toggle]     - To=ggle a bookmark on this line.
\\[viss-bookmark-next-buffer]   - Move to the next bookmark.
\\[viss-bookmark-prev-buffer]   - Move to the previous bookmark.
\\[viss-bookmark-clear-all-buffer] - Clear all bookmarks.

\(fn)" t nil)

;;;***

;;;### (autoloads (lmcompile-do-highlight) "lmcompile" "../../../../../.emacs.d/cedet/eieio/lmcompile.el"
;;;;;;  (20304 37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/eieio/lmcompile.el

(autoload 'lmcompile-do-highlight "lmcompile" "\
Do compilation mode highlighting.
Works on grep, compile, or other type mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/cedet/eieio/chart.el"
;;;;;;  "../../../../../.emacs.d/cedet/eieio/eieio-base.el" "../../../../../.emacs.d/cedet/eieio/eieio-comp.el"
;;;;;;  "../../../../../.emacs.d/cedet/eieio/eieio-custom.el" "../../../../../.emacs.d/cedet/eieio/eieio-datadebug.el"
;;;;;;  "../../../../../.emacs.d/cedet/eieio/eieio-doc.el" "../../../../../.emacs.d/cedet/eieio/eieio-load.el"
;;;;;;  "../../../../../.emacs.d/cedet/eieio/eieio-opt.el" "../../../../../.emacs.d/cedet/eieio/eieio-perftest.el"
;;;;;;  "../../../../../.emacs.d/cedet/eieio/eieio-speedbar.el" "../../../../../.emacs.d/cedet/eieio/eieio-test-methodinvoke.el"
;;;;;;  "../../../../../.emacs.d/cedet/eieio/eieio-test-mro.el" "../../../../../.emacs.d/cedet/eieio/eieio-tests.el"
;;;;;;  "../../../../../.emacs.d/cedet/eieio/eieio.el" "../../../../../.emacs.d/cedet/eieio/linemark.el"
;;;;;;  "../../../../../.emacs.d/cedet/eieio/lmcompile.el") (20305
;;;;;;  30485 291690))

;;;***

(provide 'eieio-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eieio-loaddefs.el ends here
