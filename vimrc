set nocompatible

autocmd GUIEnter * set lines=45 columns=143
autocmd GUIEnter * set guifont=Menlo:h18

set backspace=indent,eol,start
set shiftwidth=2
set tabstop=2
set softtabstop=2
set expandtab
set autoindent
set laststatus=2
set ruler
set formatoptions=croqt
set textwidth=100
set encoding=utf-8
set fileformat=unix
set scrolloff=3
set wildmode=longest,list
set tildeop
filetype indent on
set foldmethod=syntax

filetype plugin on
colorscheme darkblue

" setlocal spell spelllang=en_us
map <F4> :let &lines=&lines-1
map <S-F4> :let &lines=&lines+1

map t :tabnew 

" Workaround the cursor disappearing bug :-(
map <C-L> :let &guifont=&guifont
runtime ftplugin/man.vim


set shellcmdflag=-ic

function! s:HsSearch(regex_txt)
    try
        execute 'vim' . a:regex_txt . ' **/*.*hs'
	catch /.*/
    endtry
    cope
endfunction

function! s:TypeSearch(type_txt)
    call s:HsSearch('/\%(newtype|type|data\)\s\+' . a:type_txt . '\W/')
endfunction

function! s:FunSearch(fun_regex)
    call s:HsSearch('/\%(\s\+\)' . a:fun_regex . '\W/')
endfunction

command! -nargs=+ HsSearch :call s:HsSearch('<args>')
command! -nargs=+ TypeSearch :call s:TypeSearch('<args>')
command! -nargs=+ DefSearch :call s:FunSearch('<args>')

function! s:CSearch(regex_txt)
    try
        execute 'vim' . a:regex_txt . ' **/*.*c **/*.cpp **/*.h'
	catch /.*/
    endtry
    cope
endfunction
command! -nargs=+ CSearch :call s:CSearch('<args>')

let g:haddock_browser = "/usr/bin/chromium-browser"
let g:haddock_docdir = "/usr/local/share/doc/ghc/"

au BufEnter *.hs compiler ghc
let g:ghc = "/usr/local/bin/ghc"

autocmd BufRead *.vala set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
autocmd BufRead *.vapi set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
au BufRead,BufNewFile *.vala    setfiletype vala
au BufRead,BufNewFile *.vapi    setfiletype vala

function! s:SetGHCStaticoptions()
    let b:ghc_staticoptions = "-hide-all-packages "
    let b:ghc_staticoptions .= "-package transformers "
    let b:ghc_staticoptions .= "-package monads-tf "
    let b:ghc_staticoptions .= "-package regex-tdfa "
    let b:ghc_staticoptions .= "-package process "
    let b:ghc_staticoptions .= "-package unix "
    let b:ghc_staticoptions .= "-package base-4.2.0.0 "
    let b:ghc_staticoptions .= "-package filepath "
    let b:ghc_staticoptions .= "-package directory "
    let b:ghc_staticoptions .= "-package extensible-exceptions "
    let b:ghc_staticoptions .= "-package ghc-paths "
    let b:ghc_staticoptions .= "-package containers "
    let b:ghc_staticoptions .= "-package pureMD5 "
    let b:ghc_staticoptions .= "-package bytestring "
    let b:ghc_staticoptions .= "-package vty-4.2.1.0 "
    let b:ghc_staticoptions .= "-ignore-package dev_system "
    let b:ghc_staticoptions .= "-XPackageImports "
    let b:ghc_staticoptions .= "-XNoImplicitPrelude "
endfunction

au BufRead,BufNewFile *.hs call s:SetGHCStaticoptions()
au BufRead,BufNewFile *.hs compiler ghc

let g:NERDTreeIgnore = ['\.o$', '\.hi$', '\~$']
let g:NERDChristmasTree = 1
let g:NERDTreeWinSize = 40

"here is a more exotic version of my original Kwbd script
"delete the buffer; keep windows; create a scratch buffer
function s:Kwbd(kwbdStage)
  if(a:kwbdStage == 1)
    if(!buflisted(winbufnr(0)))
      bd!
      return
    endif
    let s:kwbdBufNum = bufnr("%")
    let s:kwbdWinNum = winnr()
    windo call s:Kwbd(2)
    execute s:kwbdWinNum . 'wincmd w'
    let s:buflistedLeft = 0
    let s:bufFinalJump = 0
    let l:nBufs = bufnr("$")
    let l:i = 1
    while(l:i <= l:nBufs)
      if(l:i != s:kwbdBufNum)
        if(buflisted(l:i))
          let s:buflistedLeft = s:buflistedLeft + 1
        else
          if(bufexists(l:i) && !strlen(bufname(l:i)) && !s:bufFinalJump)
            let s:bufFinalJump = l:i
          endif
        endif
      endif
      let l:i = l:i + 1
    endwhile
    if(!s:buflistedLeft)
      if(s:bufFinalJump)
        windo if(buflisted(winbufnr(0))) | execute "b! " . s:bufFinalJump | endif
      else
        enew
        let l:newBuf = bufnr("%")
        windo if(buflisted(winbufnr(0))) | execute "b! " . l:newBuf | endif
      endif
      execute s:kwbdWinNum . 'wincmd w'
    endif
    if(buflisted(s:kwbdBufNum) || s:kwbdBufNum == bufnr("%"))
      execute "bd! " . s:kwbdBufNum
    endif
    if(!s:buflistedLeft)
      set buflisted
      set bufhidden=delete
      set buftype=nofile
      setlocal noswapfile
    endif
  else
    if(bufnr("%") == s:kwbdBufNum)
      let prevbufvar = bufnr("#")
      if(prevbufvar > 0 && buflisted(prevbufvar) && prevbufvar != s:kwbdBufNum)
        b #
      else
        bn
      endif
    endif
  endif
endfunction

command! Kwbd call <SID>Kwbd(1)
nnoremap <silent> <Plug>Kwbd :<C-u>Kwbd<CR>

" ScratchMarkBuffer
" Mark a buffer as scratch
function! g:ScratchMarkBuffer()
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    setlocal buflisted
endfunction

set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundleCheck

syn on

autocmd VimEnter * NERDTree
