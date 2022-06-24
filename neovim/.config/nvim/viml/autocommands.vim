" Have have working directory follow buffer
autocmd BufEnter * silent! lcd %:p:h

" Disable line numbers in terminal buffers
autocmd TermOpen * setlocal nonumber norelativenumber
