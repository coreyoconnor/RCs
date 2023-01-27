;;; gnat-alire.el --- Support for building with Alire -*- lexical-binding:t -*-
;;
;;; Copyright (C) 2012 - 2023  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;; See https://alire.ada.dev/

(require 'gnat-compiler)

(cl-defstruct
    (alire-prj
     (:include wisi-prj)
     (:copier nil))
  xref-label
  )

(defun alire-get-env (project)
  "Set PROJECT slots from Alire as needed."

  ;; alire inherits GPR_PROJECT_PATH (see
  ;; https://github.com/alire-project/alire/issues/1147). So empty it
  ;; here.
  ;;
  ;; gnat list does not respect ADA_PROJECT_PATH, so if the
  ;; alire.toml specifies ADA_PROJECT_PATH (ada_language_server does
  ;; this), append that to GPR_PROJECT_PATH here.
  ;;
  ;; We need all of the alire settings for "gnat list" and "gpr_query"
  ;; to properly process complex projects (like Alire).
  (let ((process-environment (copy-sequence process-environment))
	ada-project-path
	gpr-project-path)
    (setenv "GPR_PROJECT_PATH" "")

    (with-temp-buffer
      (let ((status (call-process "alr" nil (current-buffer) nil "printenv")))
	(cond
	 ((= 0 status)
	  (goto-char (point-min))
	  (while (not (eobp))
            ;; Sometimes alr puts comments in this output:
            ;; Note: Running post_fetch actions for xmlada=23.0.0...
            ;; checking build system type... x86_64-pc-linux-gnu
	    (when (looking-at "export \\(.*\\)=\"\\(.*\\)\"$")
	      (setf (wisi-prj-file-env project)
		    (append (wisi-prj-file-env project)
			    (list (concat (match-string-no-properties 1) "=" (match-string-no-properties 2)))))

	      (let ((name  (match-string-no-properties 1))
		    (value (match-string-no-properties 2)))
	        (when (string= name "ADA_PROJECT_PATH")
		  (setq ada-project-path value))
	        (when (string= name "GPR_PROJECT_PATH")
		  (setq gpr-project-path value)))
	      )
            (forward-line 1)
	    ))

	 (t
	  (user-error "alr printenv failed; bad or missing alire.toml?"))
	 )))
    (when ada-project-path
      (setf (wisi-prj-file-env project)
	    (delete (concat "GPR_PROJECT_PATH=" gpr-project-path) (wisi-prj-file-env project)))
      (setf (wisi-prj-file-env project)
	    (append (wisi-prj-file-env project)
		    (list (concat "GPR_PROJECT_PATH="
				  (concat gpr-project-path ":" ada-project-path))))))
    ))

;;;###autoload
(cl-defun create-alire-prj (&key name gpr-file compile-env file-env xref-label)
  ;; We could use "alr exec -P -- echo" to get the project file (also
  ;; see https://github.com/alire-project/alire/issues/1151), but that
  ;; doesn't work when there are multiple project files listed in
  ;; alire.toml. And if there are multiple project files, the user
  ;; needs to pick one anyway.  So we require it as an argument; must
  ;; be absolute or relative to Alire root directory.
  ;;
  ;; prj-file should _not_ specify the gpr-file or gpr-project-path;
  ;; it is only used for casing. We get GPR_PROJECT_PATH from the
  ;; Alire environment.
  "Return an initial wisi project for the current Alire workspace."
  (let ((temp (locate-dominating-file default-directory "alire.toml")))
    (when (null temp)
      (user-error "no alire.toml found in or above %s" default-directory))
    (let* ((default-directory temp)
	   (abs-gpr-file (expand-file-name gpr-file))
	   (project (make-alire-prj :name name
                                    :compile-env compile-env
                                    :file-env file-env
                                    :xref-label xref-label)))

      (alire-get-env project)

      ;; We need a gnat-compiler to set compilation-search-path; this
      ;; must run after alire-get-env because it uses GPR_PROJECT_PATH.
      (setf (wisi-prj-compiler project)
	    (create-gnat-compiler
	     :gpr-file abs-gpr-file
	     :run-buffer-name (gnat-run-buffer-name abs-gpr-file)))

      (when (null xref-label)
        (user-error "create-alire-prj: no xref backend specified; add :xref-label"))
      (setf (wisi-prj-xref project)
	    (funcall (intern (format "create-%s-xref" (symbol-name xref-label)))))

      (wisi-compiler-parse-one (wisi-prj-compiler project) project "gpr_file" abs-gpr-file)
      (wisi-xref-parse-one     (wisi-prj-xref project)     project "gpr_file" abs-gpr-file)

      project)))

(cl-defmethod wisi-prj-default ((project alire-prj))
  (let* ((gpr-file (gnat-compiler-gpr-file (wisi-prj-compiler project)))
         (default-directory (file-name-directory gpr-file)))
    (create-alire-prj
     :name        (wisi-prj-name project)
     :gpr-file    gpr-file
     :compile-env (wisi-prj-compile-env project)
     :file-env    (wisi-prj-file-env project)
     :xref-label  (alire-prj-xref-label project))))

(provide 'gnat-alire)
;;; gnat-alire.el ends here
