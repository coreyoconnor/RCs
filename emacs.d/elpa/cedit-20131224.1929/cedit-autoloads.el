;;; cedit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (cedit-raise cedit-splice-killing-backward cedit-barf
;;;;;;  cedit-wrap-brace cedit-slurp cedit-up-block-forward cedit-up-block-backward
;;;;;;  cedit-down-block cedit-beginning-of-statement cedit-end-of-statement
;;;;;;  cedit-backward-char cedit-forward-char) "cedit" "cedit.el"
;;;;;;  (21371 50890 981813 874000))
;;; Generated autoloads from cedit.el

(autoload 'cedit-forward-char "cedit" "\
balanced forward-char / returns point
foo|; {bar;} baz;  =>  foo;| {bar;} baz;
foo;| {bar;} baz;  =>  foo; {bar;}| baz;
foo; {bar;|} baz;  =>  ERROR
foo; {bar;} baz;|  =>  ERROR

\(fn &optional NEST)" t nil)

(autoload 'cedit-backward-char "cedit" "\
balanced backward-char / returns point
foo; {bar;}| baz;  =>  foo; |{bar;} baz;
foo;| {bar;} baz;  =>  foo|; {bar;} baz;
foo; {|bar;} baz;  =>  ERROR
|foo; {bar;} baz;  =>  ERROR

\(fn &optional NEST)" t nil)

(autoload 'cedit-end-of-statement "cedit" "\
goto end of statement
when THIS is non-nil, do not move to next statement
when fail, point is never moved
foo;| {bar;} baz;  =>  foo; {bar;}| baz;
foo; {bar;}| baz;  =>  foo; {bar;} baz;|
foo; {bar;} baz;|  =>  ERROR
foo; {bar;|} baz;  =>  ERROR

\(fn &optional THIS)" t nil)

(autoload 'cedit-beginning-of-statement "cedit" "\
goto beginning of statement
when THIS is non-nil, do not move to previous statement
when fail, point is never moved
foo; {bar;} |baz;  =>  foo; |{bar;} baz;
foo; |{bar;} baz;  =>  |foo; {bar;} baz;
|foo; {bar;} baz;  =>  ERROR
foo; {|bar;} baz;  =>  ERROR

\(fn &optional THIS)" t nil)

(autoload 'cedit-down-block "cedit" "\
go down into block
|else{foo; bar;}  =>  else{|foo; bar;}
|foo;  =>  ERROR

\(fn)" t nil)

(autoload 'cedit-up-block-backward "cedit" "\
go backward out of block.
if called at top-level, goto beginning of the first statement.
do{foo; bar; b|az;}  =>  |do{foo; bar; baz;}
 foo; bar; b|az;   =>   |foo; bar; baz;

\(fn)" t nil)

(autoload 'cedit-up-block-forward "cedit" "\
go forward out of block.
if called at top-level, goto end of the last statement.
do{foo; bar; b|az;}  =>  do{foo; bar; baz;}|
 foo; bar; b|az;   =>   foo; bar; baz;|

\(fn)" t nil)

(autoload 'cedit-slurp "cedit" "\
slurp statement
{fo|o; bar;} baz;  =>  {fo|o, bar;} baz;
                   =>  {fo|o, bar; baz;}
                   =>  {fo|o, bar, baz;}

\(fn)" t nil)

(autoload 'cedit-wrap-brace "cedit" "\
wrap statement with brace
to wrap two or more statements, mark them

\(fn)" t nil)

(autoload 'cedit-barf "cedit" "\
barf statement
{fo|o, bar; baz;}  =>  {fo|o; bar; baz;}
                   =>  {fo|o; bar;} baz;
                   =>  {fo|o;} bar; baz;

\(fn)" t nil)

(autoload 'cedit-splice-killing-backward "cedit" "\
splice statements killing preceding statements
{foo; bar, b|az, foobar;}  =>  {foo; |baz, foobar;}
                           =>  {|baz, foobar;}
                           =>  baz, foobar;

\(fn)" t nil)

(autoload 'cedit-raise "cedit" "\
raise statement
{foo; bar, b|az, foobar;}  =>  {foo; |baz;}
                           =>  baz;
to raise statement, in case comma-expr is also able to be raise, mark it.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("cedit-pkg.el") (21371 50891 75833 216000))

;;;***

(provide 'cedit-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cedit-autoloads.el ends here
