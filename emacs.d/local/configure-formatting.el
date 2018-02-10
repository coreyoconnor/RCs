;; configuration of mode agnostic formatting

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4)
(electric-indent-mode 1)
(setq-default fill-column 101)
(setq column-number-mode t)

(defun cleanup-on-save ()
  (add-hook 'write-contents-functions
            (lambda()
              (save-excursion
                (delete-trailing-whitespace))))
  )

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; from http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun bf-pretty-print-xml-region (begin end)
    "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
    (interactive "r")
    (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
        (message "Ah, much better!"))

(provide 'configure-formatting)
