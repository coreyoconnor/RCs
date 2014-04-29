function! ToCamelCase(str)
  return substitute(a:str,'_\(\w\)','\u\1','g')
endfunction

function! ReplaceAllWith(x,y)
  exe '%s/'.a:x.'/'.a:y.'/g'
  normal ''
endfunction

vnoremap ~ y:call ReplaceAllWith(@", ToCamelCase(@"))
