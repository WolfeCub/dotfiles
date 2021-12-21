lua require('plugins')

colorscheme jellybeans

set tabstop=4
set shiftwidth=4
set expandtab
set smartindent
set number relativenumber

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
nnoremap <leader>p <cmd>Telescope projects<cr>

" Lsp
nnoremap <leader>lx <cmd>lua require'telescope.builtin'.lsp_code_actions(require('telescope.themes').get_cursor({}))<cr>
nnoremap <leader>lr <cmd>lua vim.lsp.buf.rename()<cr>
nnoremap <leader>lu <cmd>Telescope lsp_references<cr>
nnoremap <leader>li <cmd>Telescope lsp_implementations<cr>

" }}}
