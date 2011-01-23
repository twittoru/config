colorscheme darkblue
set t_Co=256
set guioptions=
set guifont=Menlo\ Regular:h14
set ambiwidth=double
if has('kaoriya')
    set transparency=40
    set imdisable
    set imdisableactivate
    set cmdheight=1
endif

"from http://vim-users.jp/2010/01/hack120/
let g:save_window_file = expand('~/.vim/.Winpos')
augroup SaveWindow
    autocmd!
    autocmd VimLeavePre * call s:save_window()
    function! s:save_window()
        let options = [
                    \ 'set columns=' . &columns,
                    \ 'set lines=' . &lines,
                    \ 'winpos ' . getwinposx() . ' ' . getwinposy(),
                    \ ]
        call writefile(options, g:save_window_file)
    endfunction
augroup END

if filereadable(g:save_window_file)
    execute 'source' g:save_window_file
endif

