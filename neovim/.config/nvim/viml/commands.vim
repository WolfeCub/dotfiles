" Use :W to sudo write file
command! W w !sudo tee % > /dev/null

cnoreabbrev e lua require('telescope.builtin').file_browser(get_small_ivy({previewer = false}))<cr>

" Command to remove trailing whitespace
function! TrimWhitespace()
    exec "%s/\\s\\+$//e"
    exec "normal! \<C-o>"
endfunction
command! -nargs=0 TrimWhitespace :call TrimWhitespace()
