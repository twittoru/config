" for [x]html

" about tabs
" always possibility that indent is deep
setlocal shiftwidth=2
setlocal tabstop=2

" for make
" double quotation for % ("%") is needed for Windows
setlocal makeprg=tidy\ -raw\ -quiet\ -errors\ --gnu-emacs\ yes\ \"%\"

" check, fix, form document and write it back
setlocal autoread
function! ModifyByHTMLTidy()
    update
    !tidy  -config ~/.tidyrc -quiet -modify "%"
endfunction

" function to close tag
function! InsertHTMLCloseTag()
" inserts the appropriate closing HTML tag; used for the \hc operation defined
" above;
" requires ignorecase to be set, or to type HTML tags in exactly the same case
" that I do;
" doesn't treat <P> as something that needs closing;
" clobbers register z and mark z
"
" by Smylers http://www.stripey.com/vim/
" 2000 May 3

    if &filetype == 'html' || &filetype == 'xhtml'

        " list of tags which shouldn't be closed:
        "let UnaryTags = ' Area Base Br DD DT HR Img Input LI Link Meta P Param '
        let UnaryTags = ' area base br hr img input link meta param '

        " remember current position:
        normal mz

        " loop backwards looking for tags:
        let Found = 0
        while Found == 0
            " find the previous <, then go forwards one character and grab the first
            " character plus the entire word:
            execute "normal ?\<LT>\<CR>l"
            normal "zyl
            let Tag = expand('<cword>')

            " if this is a closing tag, skip back to its matching opening tag:
            if @z == '/'
                execute "normal ?\<LT>" . Tag . "\<CR>"

            " if this is a unary tag, then position the cursor for the next
            " iteration:
            elseif match(UnaryTags, ' ' . Tag . ' ') > 0
                normal h

            " otherwise this is the tag that needs closing:
            else
                let Found = 1

            endif
        endwhile " not yet found match

        " create the closing tag and insert it:
        let @z = '</' . Tag . '>'
        normal `z"zp

    else " filetype is not HTML
        echohl ErrorMsg
        echo 'The InsertCloseTag() function is only intended to be used in HTML ' .
                    \ 'files.'
        sleep
        echohl None
    endif " check on filetype

endfunction " InsertHTMLCloseTag()