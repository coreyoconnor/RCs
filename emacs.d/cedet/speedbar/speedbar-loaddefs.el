;;; speedbar-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (rpm) "rpm" "../../../../../.emacs.d/cedet/speedbar/rpm.el"
;;;;;;  (20304 37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/speedbar/rpm.el

(autoload 'rpm "rpm" "\
Red Hat Package Management in Emacs.

\(fn)" t nil)

;;;***

;;;### (autoloads (gud-speedbar-buttons) "sb-gud" "../../../../../.emacs.d/cedet/speedbar/sb-gud.el"
;;;;;;  (20304 37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/speedbar/sb-gud.el

(autoload 'gud-speedbar-buttons "sb-gud" "\
Create a speedbar display based on the current state of GUD.
If the GUD BUFFER is not running a supported debugger, then turn
off the specialized speedbar mode.

\(fn BUFFER)" nil nil)

;;;***

;;;### (autoloads (Info-speedbar-buttons Info-speedbar-browser) "sb-info"
;;;;;;  "../../../../../.emacs.d/cedet/speedbar/sb-info.el" (20304
;;;;;;  37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/speedbar/sb-info.el

(autoload 'Info-speedbar-browser "sb-info" "\
Initialize speedbar to display an info node browser.
This will add a speedbar major display mode.

\(fn)" t nil)

(autoload 'Info-speedbar-buttons "sb-info" "\
Create a speedbar display to help navigation in an Info file.
BUFFER is the buffer speedbar is requesting buttons for.

\(fn BUFFER)" nil nil)

(eval-after-load "info" '(require 'sb-info))

;;;***

;;;### (autoloads (rmail-speedbar-buttons) "sb-rmail" "../../../../../.emacs.d/cedet/speedbar/sb-rmail.el"
;;;;;;  (20304 37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/speedbar/sb-rmail.el

(autoload 'rmail-speedbar-buttons "sb-rmail" "\
Create buttons for BUFFER containing rmail messages.
Click on the address under Reply to: to reply to this person.
Under Folders: Click a name to read it, or on the <M> to move the
current message into that RMAIL folder.

\(fn BUFFER)" nil nil)

;;;***

;;;### (autoloads (w3-speedbar-buttons) "sb-w3" "../../../../../.emacs.d/cedet/speedbar/sb-w3.el"
;;;;;;  (20304 37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/speedbar/sb-w3.el

(autoload 'w3-speedbar-buttons "sb-w3" "\
Create speedbar buttons for the current web BUFFER displayed in w3 mode.

\(fn BUFFER)" nil nil)

;;;***

;;;### (autoloads (speedbar-get-focus speedbar-frame-mode) "speedbar"
;;;;;;  "../../../../../.emacs.d/cedet/speedbar/speedbar.el" (20304
;;;;;;  37590))
;;; Generated autoloads from ../../../../../.emacs.d/cedet/speedbar/speedbar.el

(defalias 'speedbar 'speedbar-frame-mode)

(autoload 'speedbar-frame-mode "speedbar" "\
Enable or disable speedbar.  Positive ARG means turn on, negative turn off.
A nil ARG means toggle.  Once the speedbar frame is activated, a buffer in
`speedbar-mode' will be displayed.  Currently, only one speedbar is
supported at a time.
`speedbar-before-popup-hook' is called before popping up the speedbar frame.
`speedbar-before-delete-hook' is called before the frame is deleted.

\(fn &optional ARG)" t nil)

(autoload 'speedbar-get-focus "speedbar" "\
Change frame focus to or from the speedbar frame.
If the selected frame is not speedbar, then speedbar frame is
selected.  If the speedbar frame is active, then select the attached frame.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/cedet/speedbar/bigclock.el"
;;;;;;  "../../../../../.emacs.d/cedet/speedbar/dframe.el" "../../../../../.emacs.d/cedet/speedbar/rpm.el"
;;;;;;  "../../../../../.emacs.d/cedet/speedbar/sb-ant.el" "../../../../../.emacs.d/cedet/speedbar/sb-gud.el"
;;;;;;  "../../../../../.emacs.d/cedet/speedbar/sb-html.el" "../../../../../.emacs.d/cedet/speedbar/sb-image.el"
;;;;;;  "../../../../../.emacs.d/cedet/speedbar/sb-info.el" "../../../../../.emacs.d/cedet/speedbar/sb-rmail.el"
;;;;;;  "../../../../../.emacs.d/cedet/speedbar/sb-texinfo.el" "../../../../../.emacs.d/cedet/speedbar/sb-w3.el"
;;;;;;  "../../../../../.emacs.d/cedet/speedbar/speedbar-load.el"
;;;;;;  "../../../../../.emacs.d/cedet/speedbar/speedbar.el") (20305
;;;;;;  30484 473104))

;;;***

(provide 'speedbar-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; speedbar-loaddefs.el ends here
