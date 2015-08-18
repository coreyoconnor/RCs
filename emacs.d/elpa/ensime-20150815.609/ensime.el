;;; ensime.el --- ENhanced Scala Interaction Mode for Emacs

;; Copyright (C) 2003 - 2015 the SLIME and ENSIME authors
;; License: http://www.gnu.org/licenses/gpl.html

;; Homepage: https://github.com/ensime/ensime-emacs
;; Keywords: languages
;; Package-Version:  0.9.10
;; Package-Requires: ((scala-mode2 "0.21") (sbt-mode "0.03") (popup "0.5.0") (yasnippet "0.8.0") (company "0.8.7") (auto-complete "1.5.0") (dash "2.10.0") (s "1.3.0"))

;;; Commentary:
;;
;;  ENSIME has a server component which can read the AST of your
;;  project and its dependencies, providing features that are simply
;;  not possible with emacs-lisp pattern matching.
;;
;;; Code:

(eval-and-compile
  (when (<= emacs-major-version 21)
    (error "Ensime requires Emacs 22 or higher")))

(eval-and-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'url-gw)
(require 'dash)
(require 'arc-mode)
(require 'thingatpt)
(require 'comint)
(require 'timer)
(require 'tooltip)
(require 'pp)
(require 'hideshow)
(require 'flymake)
(require 'font-lock)
(require 'auto-complete)
(require 'easymenu)
(require 'ensime-client)
(require 'ensime-util)
(require 'ensime-vars)
(require 'ensime-config)
(require 'ensime-completion-util)
(require 'ensime-auto-complete)
(require 'ensime-company)
(require 'ensime-sbt)
(require 'ensime-inf)
(require 'ensime-stacktrace)
(require 'ensime-debug)
(require 'ensime-editor)
(require 'ensime-goto-testfile)
(require 'ensime-inspector)
(require 'ensime-mode)
(require 'ensime-model)
(require 'ensime-notes)
(require 'ensime-popup)
(require 'ensime-refactor)
(require 'ensime-startup)
(require 'ensime-undo)
(require 'ensime-search)
(require 'ensime-doc)
(require 'ensime-semantic-highlight)
(require 'ensime-ui)
(require 'ensime-http)
(require 'timer)

(defvar ensime-protocol-version "0.7")

(defvar ensime-prefer-noninteractive nil
  "State variable used for regression testing.")

(defvar ensime-popup-in-other-frame nil)


;;;###autoload
(defun ensime ()
  "Read config file for settings. Then start an inferior
   ENSIME server and connect to its Swank server."
  (interactive)
  (condition-case ex
      (if ensime-auto-generate-config
          (ensime--maybe-refresh-config
           nil
           'ensime--maybe-update-and-start
           '(lambda (reason) (ensime--maybe-update-and-start)))
        (ensime--maybe-update-and-start))
    ('error (error (format
                    "check that sbt is on your PATH and that your config is compatible with %s [%s]"
                    "http://github.com/ensime/ensime-server/wiki/Example-Configuration-File" ex)))))

;;;###autoload
(defun ensime-remote (host port)
  "Read config file for settings. Then connect to an existing ENSIME server."
  (interactive "shost: \nnport: ")

  (if ensime-auto-generate-config
      (ensime--maybe-refresh-config
       nil
       `(lambda () (ensime--maybe-update-and-start (url-gateway-nslookup-host ,host) ,port))
       `(lambda (reason) (ensime--maybe-update-and-start (url-gateway-nslookup-host ,host) ,port)))))

(provide 'ensime)

;;; ensime.el ends here
