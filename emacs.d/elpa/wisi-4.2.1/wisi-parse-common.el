;;; wisi-parse-common.el --- declarations used by wisi-parse.el, wisi-ada-parse.el, and wisi.el -*- lexical-binding:t -*-
;;
;; Copyright (C) 2014, 2015, 2017 - 2022  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
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

;;; Code:
(require 'cl-lib)

(defcustom wisi-incremental-parse-enable nil
  "If non-nil, use incremental parse when possible."
  :type 'boolean
  :group 'wisi
  :safe #'booleanp)

(defcustom wisi-partial-parse-threshold 100001
  "Minimum size that will be parsed by each call to the parser.
A parse is always requested at a point (or on a region); the
point is first expanded to a start point before the region and an
end point after the region, that the parser can gracefully
handle. If the final region covers the entire buffer, a complete
parse is done. Indent assumes the start point of the parse region
is properly indented. Most navigate parses ignore this setting
and parse the whole buffer."
  :type 'integer
  :group 'wisi
  :safe #'integerp)
(make-variable-buffer-local 'wisi-partial-parse-threshold)

(defcustom wisi-save-all-changes nil
  "When non-nil, save all changes sent to the parser for each
buffer, to aid in reproducing bugs. Also writes a copy of the
buffer to a file at each full parse, to give the starting point
for the changes. The filename is the visited file name with
\"-wisi-change-start\"' appended."
  :type 'boolean
  :group 'wisi
  :safe 'booleanp)

(defcustom wisi-save-text-tree nil
  "When non-nil, save the parser's copy of the full text and the
current parse tree before each change, to aid in reproducing
bugs. The full text is written to a file whose name is the
visited file name with \"-wisi-prev-text\"' appended. The tree
can be dumped to a file via the tree query dump-prev."
  :type 'boolean
  :group 'wisi
  :safe #'booleanp)

(cl-defstruct (wisi--lexer-error)
  pos ;; position (integer) in buffer where error was detected.
  message  ;; string error message
  inserted ;; char inserted after pos.
  )

(cl-defstruct (wisi--parse-error-repair)
  pos ;; position (integer) in buffer where insert/delete is done.
  inserted ;; list of token IDs that were inserted before pos
  deleted  ;; list of token IDs that were deleted after pos
  deleted-region ;; buffer (cons FIRST LAST) region deleted
  )

(cl-defstruct (wisi--parse-error)
  ;; Includes information derived from compiler error recovery to edit
  ;; text to fix one error. Used by ’wisi-repair-error’ to edit buffer.
  pos      ;; position (integer or marker) in buffer where error was detected.
  pos-2    ;; secondary error position
  message  ;; string error message
  repair   ;; list of wisi--parse-error-repair.
  )

(defconst wisi-parser-transaction-log-buffer-size-default 300000)

(cl-defstruct wisi-parser
  ;; Per-language values for a wisi parser. Also holds transient
  ;; values set by the current parse, that must be used before the
  ;; next parse starts.

  repair-image
  ;; alist of (TOKEN-ID . STRING); used by repair error

  transaction-log-buffer
  ;; Buffer holding history of communications with parser; one log per
  ;; parser instance.

  (transaction-log-buffer-size wisi-parser-transaction-log-buffer-size-default)
  ;; Max character count to retain in transaction-log-buffer. Set to 0
  ;; to disable log. Default is large enough for all transactions in
  ;; test/ada_mode-incremental_parse.adb with lots of verbosity.
)

(cl-defstruct wisi-parser-local
  ;; Per-buffer values set by the wisi parser, that must persist past
  ;; the current parse.

  ;; Separate lists for lexer and parse errors, because lexer errors
  ;; must be repaired first, before parse errors can be repaired. And
  ;; they have different structures.
  lexer-errors
  ;; list of wisi--lexer-errors from last parse.  Can be more than one if
  ;; lexer supports error recovery.
  parse-errors
  ;; List of wisi--parse-errors from last parse. Can be more than one if
  ;; parser supports error recovery.

  (all-changes -1)
  ;; List of all changes sent to parser for the current buffer since
  ;; the last full parse for that buffer (last change at head of
  ;; list). Used to reproduce bugs. Value of -1 means keeping this
  ;; list is disabled; it can easily get huge if enabled. See
  ;; `wisi-save-all-changes', `wisi-process-all-changes-to-cmd'.
)

(cl-defgeneric wisi-parser-transaction-log-buffer-name (parser)
  "Return a buffer name for the transaction log buffer.")

(defun wisi-parse-log-message (parser message)
  "Write MESSAGE (a string) to PARSER transaction-log-buffer.
Text properties on MESSAGE are preserved,"
  (let ((max (wisi-parser-transaction-log-buffer-size parser)))
    (when (> max 0)
      (unless (buffer-live-p (wisi-parser-transaction-log-buffer parser))
	(setf (wisi-parser-transaction-log-buffer parser)
	      (get-buffer-create (wisi-parser-transaction-log-buffer-name parser)))
	(with-current-buffer (wisi-parser-transaction-log-buffer parser)
	  (read-only-mode 1)
	  (buffer-disable-undo)))
      (with-current-buffer (wisi-parser-transaction-log-buffer parser)
	(goto-char (point-max))
	(let ((inhibit-read-only t))
	  (insert (format "%s:\n%s\n" (format-time-string "%H:%M:%S.%3N") message))
	  (when (> (buffer-size) max)
	    (save-excursion
	      (goto-char (- (buffer-size) max))
	      ;; search for tail of time stamp "mm:ss.mmm:\n"
	      (search-forward-regexp ":[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]:$" nil t)
	      (forward-line -1)
	      (delete-region (point-min) (point)))))))))

(cl-defgeneric wisi-parse-format-language-options (parser)
  "Return a string to be sent to the parser, containing settings
for the language-specific parser options.")

(cl-defgeneric wisi-parse-expand-region (_parser begin end)
  "Return a cons SEND-BEGIN . SEND-END that is an expansion of
region BEGIN END that starts and ends at points the parser can
handle gracefully."
  (cons begin end))

(cl-defgeneric wisi-parse-save-text-tree-auto (parser enable)
  "Implement `wisi-save-text-tree'.")

(defvar-local wisi-parser-shared nil
  "The current shared wisi parser; a ‘wisi-parser-shared’ object.
There is one parser object per language; `wisi-parser-shared' is a
buffer-local reference to that shared object.")

(defvar-local wisi-parser-local nil
  "Buffer-local values used by the wisi parser; a ‘wisi-parser-local’ object.")

(defvar-local wisi--changes nil
  "Cached list of args to wisi-after-change, for incremental parse.
Each element is
(INSERT-BEGIN-BYTE-POS INSERT-BEGIN-CHAR-POS
 INSERT-END-BYTE-POS INSERT-END-CHAR-POS
 DELETED-BYTE-COUNT DELETED-CHAR-COUNT INSERTED-TEXT)")

(defconst wisi-post-parse-actions '(navigate face indent none refactor query debug)
  "Actions that the parser can perform after parsing.
Only navigate thru indent are valid for partial parse.")

(defun wisi-read-parse-action ()
  "Read a parse action symbol from the minibuffer."
  (intern-soft (completing-read "parse action (indent): " wisi-post-parse-actions nil t nil nil 'indent)))

(defun wisi-search-backward-skip (regexp skip-p)
  "Search backward for REGEXP. If SKIP-P returns non-nil, search again.
SKIP-P is a function taking no parameters.
Return nil if no match found before bob."
  (let ((maybe-found-p (search-backward-regexp regexp nil t)))
    (while (and maybe-found-p
		(funcall skip-p)
		(setq maybe-found-p (search-backward-regexp regexp nil t))))
    maybe-found-p))

(defun wisi-search-forward-skip (regexp skip-p)
  "Search forward for REGEXP. If SKIP-P returns non-nil, search again.
SKIP-P is a function taking no parameters.
Return nil if no match found before eob."
  (let ((maybe-found-p (search-forward-regexp regexp nil t)))
    (while (and maybe-found-p
		(funcall skip-p)
		(setq maybe-found-p (search-forward-regexp regexp nil t))))
    maybe-found-p))

(defun wisi-show-expanded-region ()
  "For debugging. Expand currently selected region."
  (interactive)
  (let ((region (wisi-parse-expand-region wisi-parser-shared (region-beginning) (region-end))))
    (message "pre (%d . %d) post %s" (region-beginning) (region-end) region)
    (set-mark (car region))
    (goto-char (cdr region))
    ))

(cl-defgeneric wisi-parse-adjust-indent (_parser indent _repair)
  "Adjust INDENT for REPAIR (a wisi--parse-error-repair struct). Return new indent."
  indent)

(cl-defgeneric wisi-parse-require-process (parser &key nowait)
    "If PARSER uses an external process, start the process for PARSER.
If NOWAIT is non-nil, does not wait for the process to respond.")

(cl-defgeneric wisi-parse-current (parser parse-action begin send-end parse-end)
  "Parse current buffer starting at BEGIN, continuing at least thru PARSE-END.
Send the parser BEGIN thru SEND-END, which does a full or partial
parse, and performs post-parse action PARSE-ACTION (one of
`wisi-post-parse-actions') on region BEGIN PARSE-END.  Returns
parsed region.")

(defvar wisi-parse-full-active nil
  ;; Only one buffer can be doing a full parse.
  "Non-nil if `wisi-parse-incremental was called with full and nowait.
The value is a list (source-buffer (font-lock-begin
. font-lock-end)), where (FONT-LOCK-BEGIN . FONT-LOCK-END) is the
region font-lock attempted to fontify while the parser was
busy.")

(cl-defgeneric wisi-parse-incremental (parser parser-action &key full nowait)
  "Incrementally parse current buffer.
PARSER-ACTION (one of `wisi-post-parse-actions') is used to
decide whether to wait if parser is busy.  If FULL, do initial
full parse.  If FULL and NOWAIT, don't wait for parse to
complete; buffer is read-only until full parse completes.  Text
changes for incremental parse are stored in `wisi--changes',
created by `wisi-after-change'.")

(cl-defgeneric wisi-post-parse (parser parse-action begin end)
  "Perform PARSE-ACTION on region BEGIN END.
PARSE-ACTION is one of `wisi-post-parse-actions'. Buffer must
have been previously parsed by `wisi-parse-current' or
`wisi-parse-incremental'");

(cl-defgeneric wisi-refactor (parser refactor-action pos)
  "Perform REFACTOR-ACTION at point POS")

(defconst wisi-parse-tree-queries
  ;; Must match wisi.ads Query_Label. Results defined in doc string of `wisi-parse-tree-query'.
  '((node                   .   0)
    (containing-statement   .	1)
    (ancestor		    .	2)
    (parent		    .	3)
    (child		    .	4)
    (print		    .	5)
    (dump		    .	6) ;; dump-prev is not here because it queries a different tree.
    )
  "Query values for `wisi-parse-tree-query'.")

(cl-defstruct wisi-tree-node
  "A syntax tree node"
  address     ;; hexadecimal string
  id          ;; token_id, a symbol
  char-region ;; cons (start_pos . end_pos)
  )

(cl-defgeneric wisi-parse-tree-query (parser query &rest args)
  "Return result of parse tree query QUERY with ARGS:

- node: ARGS is a buffer position. Return the wisi-tree-node for
  the terminal at ARGS. If ARGS is in whitespace or comment,
  preceding terminal.

- containing-statement: ARGS is a buffer position. Return the
  wisi-tree-node for the statement ancestor of the terminal at
  ARGS, or nil if no such ancestor. A \"statement\" is one of the
  statement ids declared by the language-specific grammar
  backend.

- ancestor: ARGS are a buffer position and a list of ids. Return
  the wisi-tree-node for the ancestor of the terminal at that pos
  that is one of the ids, or nil if no such ancestor.

- parent: ARGS are (node-address n). Return the wisi-tree-node
  for the nth parent of the node, or nil if no such parent.

- child: ARGS are (node-address n). Return wisi-tree-node for the
  nth child of the node, or nil if no such child.

- print: ARGS ignored. Output parse tree and any errors in the
  tree to the trace log. Returns t.

- dump: ARGS are (file-name). Dump text representation of parse
  tree to file file-name, overwriting any existing
  file. Returns t.

\"terminal at pos\" means pos is in the region defined by the
terminal token text plus following non_grammar and whitespace."
  ;; wisi-indent-statement requires this definition of 'terminal at pos'.
  )

(cl-defgeneric wisi-parse-kill-buffer (parser)
  "Tell parser the current buffer is being deleted.
Called by `wisi-parse-kill-buf'.")

(defun wisi-parse-kill-buf ()
  "Tell parser the current buffer is being deleted.
For `kill-buffer-hook'."
  (when wisi-parser-shared
    (wisi-parse-kill-buffer wisi-parser-shared)))

(cl-defgeneric wisi-parse-reset (parser)
  "Ensure parser is ready to process a new parse.")

(cl-defgeneric wisi-parse-kill (parser)
  "Kill any external process associated with parser.")

(cl-defgeneric wisi-parse-enable-memory-report (parser)
  "Configure parser to report memory use.")

(cl-defgeneric wisi-parse-memory-report-reset (parser)
  "Reset memory report base.")

(cl-defgeneric wisi-parse-memory-report (parser)
  "Display memory use since last reset.")

(cl-defstruct
  (wisi-cache
   (:constructor wisi-cache-create)
   (:copier nil))
  nonterm;; nonterminal from parse

  token
  ;; terminal symbol from wisi-keyword-table or
  ;; wisi-punctuation-table, or lower-level nonterminal from parse

  last ;; pos of last char in token, relative to first (0 indexed)

  class ;; one of wisi-class-list

  containing
  ;; Marker at the start of the containing statement for this token.
  ;; nil for outermost containing.

  prev ;; marker at previous motion token in statement; nil if none
  next ;; marker at next motion token in statement; nil if none
  end  ;; marker at token at end of current statement
  )

(defun wisi-get-cache (pos)
  "Return `wisi-cache' struct from the `wisi-cache' text property at POS."
  (get-text-property pos 'wisi-cache))

(defun wisi-prev-cache (pos)
  "Return previous cache before POS, or nil if none."
  (let (cache)
    ;; If pos is not near cache, p-s-p-c will return pos just after
    ;; cache, so 1- is the beginning of cache.
    ;;
    ;; If pos is just after end of cache, p-s-p-c will return pos at
    ;; start of cache.
    ;;
    ;; So we test for the property before subtracting 1.
    (setq pos (previous-single-property-change pos 'wisi-cache))
    (cond
     ((null pos)
       nil)

     ((setq cache (get-text-property pos 'wisi-cache))
      cache)

     (t
      (setq pos (1- pos))
      (setq cache (get-text-property pos 'wisi-cache))
      cache)
     )))

(defun wisi-backward-cache ()
  "Move point backward to the first token cache preceding point.
Returns cache, or nil if at beginning of buffer.
Assumes the buffer is fully parsed."
  (let ((pos (previous-single-property-change (point) 'wisi-cache))
	cache)
    (cond
     ((null pos)
      (goto-char (point-min))
      nil)

     ((setq cache (get-text-property pos 'wisi-cache))
      (goto-char pos)
      cache)

     (t
      (setq pos (1- pos))
      (setq cache (get-text-property pos 'wisi-cache))
      (goto-char pos)
      cache)
     )))

(defun wisi-next-cache (pos)
  "Return next cache after POS, or nil if none."
  (let (cache)
    (when (get-text-property pos 'wisi-cache)
      ;; on a cache; get past it
      (setq pos (1+ pos)))

    (setq cache (get-text-property pos 'wisi-cache))
    (unless cache
      (setq pos (next-single-property-change pos 'wisi-cache))
      (when pos
	(setq cache (get-text-property pos 'wisi-cache)))
      )
    cache
    ))

(defun wisi-forward-cache ()
  "Move point forward to the first token cache after point.
Returns cache, or nil if at end of buffer.
Assumes the buffer is fully parsed."
  (let (cache pos)
    (when (get-text-property (point) 'wisi-cache)
      ;; on a cache; get past it
      (goto-char (1+ (point))))

    (setq cache (get-text-property (point) 'wisi-cache))
    (if cache
	nil

      (setq pos (next-single-property-change (point) 'wisi-cache))
      (if pos
	  (progn
	    (goto-char pos)
	    (setq cache (get-text-property pos 'wisi-cache)))
	;; at eob
	(goto-char (point-max))
	(setq cache nil))
      )
    cache
    ))

(defun wisi-cache-region (cache &optional start)
  "Return region designated by START (default point) to cache last."
  (unless start (setq start (point)))
  (cons start (+ start (wisi-cache-last cache))))

(defcustom wisi-debug 0
  "wisi debug mode:
0 : normal - ignore parse errors, for indenting new code
1 : report parse errors (for running tests)"
  :type 'integer
  :group 'wisi
  :safe #'integerp)

;; The following parameters are easily changeable for debugging.
(defcustom wisi-parser-verbosity ""
  "WisiToken trace config; empty string for none.
See WisiToken Trace_Enable for complete set of options.
Examples:
debug=1 lexer=1 parse=2 action=3"
  :type 'string
  :group 'wisi
  :safe #'stringp)
(make-variable-buffer-local 'wisi-parser-verbosity)

(defcustom wisi-mckenzie-zombie-limit nil
  "If integer, overrides %mckenzie_zombie_limit.
This sets the number of tokens past the error point that other
parsers accept before the error parser is terminated.  Higher
value gives better solutions, but may cause too many parsers to
be active at once.  If nil, uses %mckenzie_zombie_limit value from grammar file."
  :type 'integer
  :group 'wisi
  :safe #'integerp)
(make-variable-buffer-local 'wisi-mckenzie-zombie-limit)

(defcustom wisi-mckenzie-enqueue-limit nil
  "If integer, overrides %mckenzie_enqueue_limit.
This sets the maximum number of solutions that will be considered.
Higher value has more recover power, but will be slower to fail.
If nil, uses %mckenzie_enqueue_limit value from grammar file."
  :type 'integer
  :group 'wisi
  :safe #'integerp)
(make-variable-buffer-local 'wisi-mckenzie-enqueue-limit)

(defcustom wisi-parse-max-parallel nil
  "Maximum number of parallel parsers during regular parsing.
Parallel parsers are used to resolve conflicts in the grammar.
If a file needs more than this, it's probably an indication that
the grammar has excessive conflicts. If nil, uses %max_parallel
value from grammar file (default 15)"
  :type 'integer
  :group 'wisi
  :safe #'integerp)
(make-variable-buffer-local 'wisi-parse-max-parallel)

;; end of easily changeable parameters

(defvar-local wisi-indent-comment-col-0 nil
  "If non-nil, comments currently starting in column 0 are left in column 0.
Otherwise, they are indented with previous comments or code.
Normally set from a language-specific option.")

(defconst wisi-class-list
  [motion ;; motion-action
   statement-end
   statement-override
   statement-start
   misc ;; other stuff
   ]
  "array of valid token classes.")

(defun wisi-error-msg (message &rest args)
  (let ((line (line-number-at-pos))
	(col (- (point) (line-beginning-position))))
    (format
     "%s:%d:%d: %s"
       (buffer-name) ;; buffer-file-name is sometimes nil here!?
       line col
       (apply #'format message args))))

(defvar wisi-parse-error nil)
(put 'wisi-parse-error
     'error-conditions
     '(error wisi-parse-error))
(put 'wisi-parse-error
     'error-message
     "wisi parse error")

(cl-defstruct wisi-tok
  token  ;; symbol from a token table ;; IMPROVEME: rename to ’id’?
  region ;; cons giving buffer region containing token text

  nonterminal ;; t if a nonterminal

  line ;; Line number at start of token. Nil for empty nonterminals

  first
  ;; For terminals, t if token is the first token on a line.
  ;;
  ;; For nonterminals, line number of first contained line (not
  ;; including trailing comments) that needs indenting; it is a
  ;; comment, or begins with a contained token.
  ;;
  ;; Otherwise nil.

  ;; The following are non-nil if token (terminal or non-terminal) is
  ;; followed by blank or comment lines
  comment-line ;; first blank or comment line following token
  comment-end ;; position at end of blank or comment lines
  )

(defun wisi-token-text (token)
  "Return buffer text from token range."
  (let ((region (wisi-tok-region token)))
    (and region
       (buffer-substring-no-properties (car region) (cdr region)))))

(defun wisi-and-regions (left right)
  "Return region enclosing both LEFT and RIGHT."
  (if left
      (if right
	  (cons (min (car left) (car right))
		(max (cdr left) (cdr right)))
	left)
    right))

(defun wisi--set-line-begin (line-count)
  "Return a vector of line-beginning positions, with length LINE-COUNT."
  (let ((result (make-vector line-count 0)))
    (save-excursion
      (goto-char (point-min))

      (dotimes (i line-count)
	(aset result i (point))
	(forward-line 1)))
    result))

;;;; debugging
(defun wisi-tok-debug-image (tok)
  "Return id and region from TOK, as string."
  (cond
   ((wisi-tok-region tok)
    (format "(%s %d . %d)"
	    (wisi-tok-token tok)
	    (car (wisi-tok-region tok))
	    (cdr (wisi-tok-region tok))))
   (t
    (format "(%s)" (wisi-tok-token tok)))
   ))

(defun wisi-save-kbd-macro (file-name)
  "Write `last-kbd-macro' to FILE_NAME."
  (interactive "F")
  (with-temp-buffer
    (insert (format "%s" last-kbd-macro))
    (write-file file-name))
  (message "keyboard macro saved to file '%s'" file-name))

(defun wisi-parse-incremental-none ()
  "Force an incremental parse.
Signals an error if `wisi-incremental-parse-enable' is nil."
  (unless wisi-incremental-parse-enable
    (user-error "wisi-parse-incremental-none with wisi-incremental-parse-enable nil"))
  (save-excursion (wisi-parse-incremental wisi-parser-shared 'none)))

(defun wisi-replay-undo (count)
  "Execute `undo' COUNT times, delaying in between each."
  (let ((i 0))
    (undo-start)
    (while (< i count)
      (undo-more 1)
      (sit-for 0.1)
      (setq i (1+ i)))))

(provide 'wisi-parse-common)
