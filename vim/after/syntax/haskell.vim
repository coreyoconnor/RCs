" Vim syntax file
" based on:
"
" Language: haskell with embedded hamlet
" Author:   Patrick Brisbin <me@pbrisbin.com>
" License:  as-is

" store and remove current syntax value
let old_syntax = b:current_syntax
unlet b:current_syntax

syn include @hamlet bundle/html-template-syntax/syntax/hamlet.vim
unlet b:current_syntax

syn region hmBlock   matchgroup=quasiQuote start=/\[\$\?dhamlet|/ end=/|\]/ contains=@hamlet
syn region csBlock   matchgroup=quasiQuote start=/\[\$\?dcassius|/       end=/|\]/ contains=@cassius
syn region lcBlock   matchgroup=quasiQuote start=/\[\$\?dlucius|/        end=/|\]/ contains=@lucius
syn region jsBlock   matchgroup=quasiQuote start=/\[\$\?djulius|/        end=/|\]/ contains=@julius

" restore current syntax value
let b:current_syntax = old_syntax
