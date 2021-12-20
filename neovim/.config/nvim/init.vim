lua require('plugins')

colorscheme jellybeans

set tabstop=4
set shiftwidth=4
set expandtab
set smartindent

" Keybindings {{{

nnoremap <Space> <nop>
let g:mapleader = "\<Space>"
let g:maplocalleader = "\\"

" Fast saving
nnoremap <leader>w :<C-u>update<cr>

" Make j and k behave like they should for wrapped lines
nnoremap j gj
nnoremap k gk

" Buffer navigation keybinds
nnoremap <leader>b :b#<cr>
nnoremap <leader>k :bd<cr>

" Don't lose visual selection with < >
xnoremap < <gv
xnoremap > >gv

" Telescope
nnoremap <leader>m <cmd>Telescope buffers<cr>
nnoremap <leader>g <cmd>Telescope live_grep<cr>
nnoremap <leader>f <cmd>Telescope find_files<cr>

" }}}
