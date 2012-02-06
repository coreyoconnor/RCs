au FileType haskell let b:comment_leader = '-- '
au FileType vim let b:comment_leader = '\" '
au FileType vhdl let b:comment_leader = '-- '
au FileType c let b:comment_leader = '// '
au FileType ada let b:comment_leader = '-- '
au FileType sh let b:comment_leader = '# '
au FileType java let b:comment_leader = '// '
au FileType make let b:comment_leader = '# '

fun! Comment()
    let savedreport = &report
    let &report = 100000
    let str = escape(b:comment_leader, '/')
    exe 'sm/^/'.str.'/e'
    let &report = savedreport
endfun

fun! Uncomment()
    let savedreport = &report
    let &report = 100000
    let str = escape(b:comment_leader, '/')
    exe 'sm/^'.str.'//e'
    let &report = savedreport
endfun

" Comment / uncomment lines
map <silent> ,c :call Comment()<CR>
map <silent> ,u :call Uncomment()<CR>

