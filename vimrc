scriptencoding utf-8
" vim vim vim !
set nocompatible
set helplang=ja

syntax enable
filetype plugin indent on

let colorlist = map(split(globpath(&runtimepath, 'colors/*.vim'), '\n'), 'fnamemodify(v:val, ":t:r")')
if count(colorlist,"ap_dark8") != 0
    colorscheme ap_dark8
else
    colorscheme desert
endif

" color
set t_Co=8


" indent 
set autoindent
set smartindent

set backspace=indent,eol,start

set nobackup
"set backupcopy&
"set backupdir=$HOME/.vim/backup
"let &directory=&backupdir
set directory=~/tmp

set clipboard=unnamed

set viminfo=


" search
set ignorecase
set smartcase
set noincsearch
set nohlsearch

" look feel
set showcmd
set number
set showmatch
set foldmethod=marker
set laststatus=2
set virtualedit=block
set cmdheight=1
set visualbell
set t_vb=

function! GetB()
  let c = matchstr(getline('.'), '.', col('.') - 1)
  let c = iconv(c, &enc, &fenc)
  return String2Hex(c)
endfunction
" :help eval-examples
" The function Nr2Hex() returns the Hex string of a number.
func! Nr2Hex(nr)
  let n = a:nr
  let r = ""
  while n
    let r = '0123456789ABCDEF'[n % 16] . r
    let n = n / 16
  endwhile
  return r
endfunc
" The function String2Hex() converts each character in a string to a two
" character Hex string.
func! String2Hex(str)
  let out = ''
  let ix = 0
  while ix < strlen(a:str)
    let out = out . Nr2Hex(char2nr(a:str[ix]))
    let ix = ix + 1
  endwhile
  return out
endfunc


set statusline=[%n]%1*%m%*%r%h%w%{'['.(&fenc!=''?&fenc:&enc).':'.&ff.']'}%y\ %f%=[%<%{fnamemodify(getcwd(),':~')}]%-8([%{GetB()}]%)\ %-11(%l,%c%V%)\ %4P
"set rulerformat=%=%1*%m%*%r%h%w%{'['.(&fenc!=''?&fenc:&enc).':'.&ff.']'}%y%f[%<%{fnamemodify(getcwd(),':~')}][%n]%4P

" tab
" コマンドライン補完するときに強化されたものを使う(参照 :help wildmenu)
set wildmenu
" バッファが編集中でもその他のファイルを開けるように
set hidden
" 外部のエディタで編集中のファイルが変更されたら自動で読み直す
set autoread

set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4


set tags+=~/.vim/tags/cpp

nnoremap <silent> j gj
nnoremap <silent> k gk
nnoremap ; :
nnoremap : ;

" always ready to edge my sword
nnoremap <silent> ,s :<C-u>source $MYVIMRC<CR>
nnoremap <silent> ,e :<C-u>split $MYVIMRC<CR>

" mimic it emacsen
nnoremap <C-h> :<C-u>help<Space>
"" autocmds {{{ 
" clear autocmds when load vimrc
"autocmd!
" calc SENTO-RYOKU
autocmd FileType vim nnoremap <buffer> ,p :<C-u>echo len(filter(readfile($MYVIMRC), 'v:val !~ "^\\s*$"'))<CR>
" view helpfile like less
"autocmd FileType help nnoremap <buffer> q <C-w>q
autocmd FileType help nnoremap <buffer> q <C-w>c
" goodbye auto comment-out
autocmd FileType * setlocal formatoptions-=cro
"}}}
" 文字コードの自動認識 {{{
" form  http://www.kawaz.jp/pukiwiki/?vim#content_1_7
if !has('kaoriya')
    if &encoding !=# 'utf-8'
        set encoding=japan
        set fileencoding=japan
    endif
    if has('iconv')
        let s:enc_euc = 'euc-jp'
        let s:enc_jis = 'iso-2022-jp'
        " iconvがeucJP-msに対応しているかをチェック
        if iconv("\x87\x64\x87\x6a", 'cp932', 'eucjp-ms') ==# "\xad\xc5\xad\xcb"
            let s:enc_euc = 'eucjp-ms'
            let s:enc_jis = 'iso-2022-jp-3'
            " iconvがJISX0213に対応しているかをチェック
        elseif iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
            let s:enc_euc = 'euc-jisx0213'
            let s:enc_jis = 'iso-2022-jp-3'
        endif
        " fileencodingsを構築
        if &encoding ==# 'utf-8'
            let s:fileencodings_default = &fileencodings
            let &fileencodings = s:enc_jis .','. s:enc_euc .',cp932'
            let &fileencodings = &fileencodings .','. s:fileencodings_default
            unlet s:fileencodings_default
        else
            let &fileencodings = &fileencodings .','. s:enc_jis
            set fileencodings+=utf-8,ucs-2le,ucs-2
            if &encoding =~# '^\(euc-jp\|euc-jisx0213\|eucjp-ms\)$'
                set fileencodings+=cp932
                set fileencodings-=euc-jp
                set fileencodings-=euc-jisx0213
                set fileencodings-=eucjp-ms
                let &encoding = s:enc_euc
                let &fileencoding = s:enc_euc
            else
                let &fileencodings = &fileencodings .','. s:enc_euc
            endif
        endif
        " 定数を処分
        unlet s:enc_euc
        unlet s:enc_jis
    endif
    " 日本語を含まない場合は fileencoding に encoding を使うようにする
    if has('autocmd')
        function! AU_ReCheck_FENC()
            if &fileencoding =~# 'iso-2022-jp' && search("[^\x01-\x7e]", 'n') == 0
                let &fileencoding=&encoding
            endif
        endfunction
        autocmd BufReadPost * call AU_ReCheck_FENC()
    endif
    " 改行コードの自動認識 shebang書くときにmacだと失敗するので基本unix
    set fileformats=unix,mac,dos
    " □とか○の文字があってもカーソル位置がずれないようにする
    " はずだったがrxvt-unicodeではムリポなので.vimrcでは=single
    "if exists('&ambiwidth')
    "  set ambiwidth=double
    "endif
endif
"}}}

call pathogen#runtime_append_all_bundles()
call pathogen#helptags()
" matchit
let b:match_ignorecase = 1
" neocomplcache
let g:neocomplcache_enable_at_startup = 1
" eskk.vim
if has('vim_starting')
    let g:eskk_dictionary = '~/.skk-jisyo'

    if has('mac')
        let g:eskk_large_dictionary = "~/Library/Application\ Support/AquaSKK/SKK-JISYO.L"
    elseif has('win32') || has('win64')
        let g:eskk_large_dictionary = "~/SKK_JISYO.L"
    else
    endif
endif
"skk.vim
let g:skk_large_jisyo          = "/Users/tor/Library/Application\ Support/AquaSKK/SKK-JISYO.L"
let g:skk_jisyo_encoding       = "utf-8"
let g:skk_large_jisyo_encoding = "euc-jp"
let g:skk_kutouten_type = "en"
let g:skk_kutouten_en = "．，"
let g:skk_auto_save_jisyo      = 1
let g:skk_keyboard_layout ='azik'
"unite.vim
autocmd FileType unite call s:unite_my_settings()
function! s:unite_my_settings()
  " 単語単位からパス単位で削除するように変更
  imap <buffer> <C-w> <Plug>(unite_delete_backward_path)
endfunction
" 起動時にインサートモードで開始
let g:unite_enable_start_insert = 1
" バッファ一覧
nnoremap <silent> ,ub :<C-u>Unite buffer<CR>
" ファイル一覧
nnoremap <silent> ,uf :<C-u>UniteWithBufferDir -buffer-name=files file<CR>
" レジスタ一覧
nnoremap <silent> ,ur :<C-u>Unite -buffer-name=register register<CR>
" 最近使用したファイル一覧
nnoremap <silent> ,um :<C-u>Unite file_mru<CR>
" 全部乗せ
nnoremap <silent> ,ua :<C-u>UniteWithBufferDir -buffer-name=files buffer file_mru bookmark file<CR>
