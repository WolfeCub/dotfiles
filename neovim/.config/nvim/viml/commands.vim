" Use :W to sudo write file
command! W w !sudo tee % > /dev/null

" Useful to grab the extra space abbreviations can add
func s:eatChar(pat)
    let c = nr2char(getchar(0))
    return (c =~ a:pat) ? '' : c
endfunc

function LaunchFileBrowser()
    lua require('telescope.builtin').file_browser(get_small_ivy({previewer = false}))
    call s:eatChar('\s')
endfunction

cnoreabbrev e call LaunchFileBrowser()<cr>

" Command to remove trailing whitespace
function! TrimWhitespace()
    exec "%s/\\s\\+$//e"
    exec "normal! \<C-o>"
endfunction
command! -nargs=0 TrimWhitespace :call TrimWhitespace()
