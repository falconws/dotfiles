" vim tab
setlocal expandtab tabstop=8 shiftwidth=4 softtabstop=4

" python
filetype plugin on
autocmd FileType python setl expandtab tabstop=8 shiftwidth=4 softtabstop=4
autocmd FileType python setl omnifunc=pysmell#Complete

" neocomplcache
let g:neocomplcache_enable_at_startup = 1

set number

" Execute python script Ctrl+P
function! s:ExecPy()
exe "!" . &ft . " %"
    :endfunction
    command! Exec call <SID>ExecPy()
    autocmd FileType python map <silent> <C-P> :call <SID>ExecPy()<CR>
