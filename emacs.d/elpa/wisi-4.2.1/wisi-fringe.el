;;; wisi-fringe.el --- show approximate error locations in the fringe  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 - 2019, 2021 - 2022  Free Software Foundation, Inc.
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
;;
;;; Design:
;;
;; Bitmaps are displayed in the fringe by putting a 'display property
;; on buffer text. However, just doing that also hides the buffer
;; text. To avoid that, we put the ’display property on a string, and
;; then an overlay containing that string as ’before-string or
;; ’after-string on the newline of a buffer line.
;;
;; We show approximate error positions in the entire buffer with
;; single-pixel lines in the right fringe, and mark error lines with
;; ’!!’ in the left fringe.

;;; Code:

(require 'wisi-parse-common)            ;For `wisi-debug'

(define-fringe-bitmap 'wisi-fringe--double-exclaim-bmp
  (vector
   #b00000000
   #b01100110
   #b01100110
   #b01100110
   #b01100110
   #b01100110
   #b00000000
   #b01100110
   #b01010110
   #b00000000))

(defun wisi-fringe--put-left (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))

    (when (< 1 wisi-debug)
      (wisi-parse-log-message wisi-parser-shared (format "wisi-fringe--put-left line %d" line)))

    (let* ((endpos (line-end-position))
	   (ov (make-overlay endpos (1+ endpos)))
	   (bmp 'wisi-fringe--double-exclaim-bmp))
      (overlay-put ov 'before-string (propertize "-" 'display (list 'left-fringe bmp 'compilation-error)))
      (overlay-put ov 'wisi-fringe t)
      )))

(defun wisi-fringe-clean ()
  "Remove all wisi-fringe marks."
  (remove-overlays (point-min) (point-max) 'wisi-fringe t))

(defun wisi-fringe-display-errors (positions)
  "Display markers in the fringe for each buffer position in POSITIONS.
The buffer containing POSITIONS must be current, and the window
displaying that buffer must be current."
  (wisi-fringe-clean)
  (when positions
    (let ((window-pos-first (window-start))
	  (window-pos-last  (window-end)))

      (when (< 1 wisi-debug)
	(wisi-parse-log-message wisi-parser-shared
				(format "wisi-fringe-display-errors %d %d %d"
					(length positions)
					window-pos-first
					window-pos-last)))

      (dolist (pos positions)
	(let* ((line (line-number-at-pos (max (point-min) (min (point-max) pos)) t)))
	  (when (and (>= pos window-pos-first)
		     (<= pos window-pos-last))
	    (wisi-fringe--put-left line))
	  ))
      )))

(provide 'wisi-fringe)
