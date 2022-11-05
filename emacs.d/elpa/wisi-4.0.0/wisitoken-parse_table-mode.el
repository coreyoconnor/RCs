;; wisitoken-parse_table-mode.el --- For navigating in a parse table as output by wisitoken-bnf-generate. -*- lexical-binding:t -*-
;;
;; Copyright (C) 2017 - 2022  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: parser
;; Version: 1.0
;; package-requires: ((emacs "25.1"))
;; URL: http://www.nongnu.org/ada-mode/wisi/wisi.html
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'xref)

(defvar wisitoken-parse_table-last-buffer nil
  "Last buffer in which a wisitoken-parse_table operation was performed")

(defun wisitoken-parse_table--xref-backend () 'wisitoken-parse_table)

(cl-defgeneric xref-backend-identifier-completion-table (_backend)
  (let ((names nil))
    (save-excursion
      (goto-char (point-min))
      (search-forward "Productions:")
      (forward-line)
      (while (looking-at "[0-9.]+: \\([a-z_]+\\) <=")
	(push (cons (match-string 1) (list (buffer-file-name) (line-number-at-pos) 0)) names)
	(forward-line))
      names)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql wisitoken-parse_table)))
  ;; if we are on one of:
  ;; - ’goto state nnn’ in a state action
  ;; => return nnn state
  ;;
  ;; or
  ;; - foo <= bar baz
  ;; => return nonterminal name at point
  ;;
  ;; - 'reduce n tokens to <nonterminal> <prod_id>'
  ;; => return 'prod_id: name'
  (setq wisitoken-parse_table-last-buffer (current-buffer))
  (cond
   ((save-excursion
      (beginning-of-line)
      ;; "go to" for bison output
      (search-forward-regexp "go ?to state \\([0-9]+\\)" (line-end-position) t))
    (match-string 1))

   ((save-excursion
      (beginning-of-line)
      (search-forward-regexp "reduce [0-9]+ tokens to \\([[:alnum:]_]+\\) \\([0-9.]+\\)" (line-end-position) t))
    (concat (match-string 2) ": " (match-string 1)))

   (t
    (thing-at-point 'symbol))))

(cl-defgeneric xref-backend-definitions (_backend identifier)
  ;; IDENTIFIER is from xref-back-identifier-at-point; a state number or a nonterminal
  (setq wisitoken-parse_table-last-buffer (current-buffer))
  (let ((state-p (string-match "\\`[0-9]+\\'" identifier))
	(prod_id-p (string-match "\\`[0-9.]+: " identifier)))
    (save-excursion
      (goto-char (point-min))
      (cond
       (state-p
	(search-forward-regexp (concat "^State " identifier ":$")))

       (prod_id-p
	(search-forward-regexp (concat identifier " <=")))

       (t
	(search-forward-regexp (concat "^[0-9.]+: " identifier " <=")))
       )
      (list (xref-make identifier (xref-make-buffer-location (current-buffer) (match-beginning 0))))
      )))

;;;###autoload
(defun wisitoken-parse_table-goto ()
  "Get symbol at point, goto symbol's definition.
Symbol can be a nonterminal name, or a state number."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (pop-to-buffer wisitoken-parse_table-last-buffer)
    (xref-find-definitions symbol)))

(defun wisitok-p_t-nonterm-alist ()
  (let ((names nil))
    (save-excursion
      (goto-char (point-min))
      (search-forward "Productions:")
      (forward-line)
      (while (looking-at "\\([0-9]+\\.[0-9]+\\): \\([a-z_]+\\) <=")
	(push (cons (match-string 1) (match-string 2)) names)
	(forward-line))
      names)))

(defconst wisitok-p_t-conflict-reduce-regexp
  "\\(reduce\\) [0-9]+ tokens to \\([[:alnum:]_]+\\)")

(defun wisitok-p_t-conflict-alist ()
  (let ((conflicts nil)
	(nonterms (wisitok-p_t-nonterm-alist))
	line)

    (save-excursion
      (goto-char (point-min))
      (search-forward "Parse Table:")
      (while (search-forward-regexp (concat "^ +" wisitok-p_t-conflict-reduce-regexp) nil t)
	(let ((conflict (concat "REDUCE " (match-string 2)))
	      (on-token nil))
	  (goto-char (line-beginning-position 0))
	  (setq line (line-number-at-pos))
	  (back-to-indentation)
	  (looking-at "\\([A-Z_]+\\) +=> ")
	  (setq on-token (match-string 1))
	  (goto-char (match-end 0))
	  (looking-at
	   (concat "\\(?:" wisitok-p_t-conflict-reduce-regexp
		   "\\)\\|\\(?:\\(shift\\) and goto state [0-9]+ \\([0-9]+\\.[0-9]+\\)\\)"))
	  (cond
	   ((match-beginning 1)
	    (setq conflict (concat "REDUCE " (match-string 2) " | " conflict)))
	   ((match-beginning 3)
	    (setq conflict (concat "SHIFT " (cdr (assoc (match-string 4) nonterms)) " | " conflict)))
	   )

	  (forward-line 2)
	  (while (looking-at (concat "^ +" wisitok-p_t-conflict-reduce-regexp))
	    (setq conflict (concat conflict " | REDUCE " (match-string 2)))
	    (forward-line 1))

	  (setq conflict (concat conflict " on token " on-token))

	  (push (cons conflict (list (buffer-file-name) line 0)) conflicts)
	  )))
    conflicts))

(defconst wisitok-p_t-action-nonterm-regexp "\\(?:SHIFT\\|REDUCE\\) [[:alnum:]_]+")

(defun wisitoken-parse_table--get-conflict ()
  (save-excursion
    (goto-char (line-beginning-position))
    (when (looking-at "%conflict ")
     (goto-char (match-end 0))
     (looking-at
      (concat
       wisitok-p_t-action-nonterm-regexp
       "\\(?: | " wisitok-p_t-action-nonterm-regexp "\\)+ on token [[:alnum:]_]+"))
     (match-string-no-properties 0))))

;;;###autoload
(defun wisitoken-parse_table-conflict-goto (&optional prompt)
  "Get conflict at point, goto first occurance.
With user arg, prompt for parse table buffer."
  (interactive "P")
  (when prompt
    (setq wisitoken-parse_table-last-buffer
	  (get-buffer
	   (read-buffer
	    (format "parse table buffer (%s): " wisitoken-parse_table-last-buffer)
	    wisitoken-parse_table-last-buffer
	    t))))
  (let ((conflict (wisitoken-parse_table--get-conflict)))
    (pop-to-buffer wisitoken-parse_table-last-buffer)
    ;; IMPROVEME: we may need to cache the completion table in a large buffer
    (let ((loc (cdr (assoc conflict (wisitok-p_t-conflict-alist)))))
      (if loc
	  (wisi-goto-source (nth 0 loc) (nth 1 loc) (nth 2 loc))
	(user-error "conflict not found")))))

;;;###autoload
(define-minor-mode wisitoken-parse_table-mode
  "Provides navigation in wisi-generate parse table output."
  :lighter ":parse_table"
  (add-hook 'xref-backend-functions #'wisitoken-parse_table--xref-backend nil t)

  (if wisitoken-parse_table-mode
      (read-only-mode 0)
    (read-only-mode 1)
  ))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.parse_table.*\\'" . wisitoken-parse_table-mode))

(provide 'wisitoken-parse_table-mode)
;; end of file
