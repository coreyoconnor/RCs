;;; composer-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "composer" "composer.el" (0 0 0 0))
;;; Generated autoloads from composer.el

(autoload 'composer-get-config "composer" "\
Return config value by `NAME'.

\(fn NAME)" nil nil)

(autoload 'composer-get-bin-dir "composer" "\
Retrurn path to Composer bin directory." nil nil)

(autoload 'composer-install "composer" "\
Execute `composer.phar install' command." t nil)

(autoload 'composer-dump-autoload "composer" "\
Execute `composer.phar install' command." t nil)

(autoload 'composer-require "composer" "\
Execute `composer require' command.

When IS-DEV is not-NIL, add `--dev' to option.
Require PACKAGE is package name.

\(fn IS-DEV &optional PACKAGE)" t nil)

(autoload 'composer-update "composer" "\
Execute `composer.phar update' command." t nil)

(autoload 'composer-find-json-file "composer" "\
Open composer.json of the project." t nil)

(autoload 'composer-view-lock-file "composer" "\
Open composer.lock of the project." t nil)

(autoload 'composer-run-vendor-bin-command "composer" "\
Run command `COMMAND' in `vendor/bin' of the composer project.

\(fn COMMAND)" t nil)

(autoload 'composer-run-script "composer" "\
Run script `SCRIPT` as defined in the composer.json.

\(fn SCRIPT)" t nil)

(autoload 'composer-setup-managed-phar "composer" "\
Setup `composer.phar'.  Force re-setup when `FORCE' option is non-NIL.

\(fn &optional FORCE)" t nil)

(autoload 'composer "composer" "\
Execute `composer' SUB-COMMAND with OPTION arguments.

When called with prefix argument GLOBAL, execute in global context.

\(fn GLOBAL &optional SUB-COMMAND OPTION)" t nil)

(register-definition-prefixes "composer" '("composer-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; composer-autoloads.el ends here
