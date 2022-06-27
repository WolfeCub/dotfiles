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

" Shift + J/K moves selected lines down/up in visual mode
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Fzf
nnoremap <leader>f <cmd>lua fzf_find_files_dwim()<cr>
nnoremap <leader>m <cmd>FzfLua buffers<cr>
nnoremap <leader>g <cmd>FzfLua live_grep<cr>
nnoremap <leader>r <cmd>FzfLua resume<cr>

" Lsp
nnoremap <leader>lx <cmd>FzfLua lsp_code_actions<cr>
nnoremap <leader>lr <cmd>lua vim.lsp.buf.rename()<cr>
nnoremap <leader>lu <cmd>FzfLua lsp_references<cr>
nnoremap <leader>li <cmd>FzfLua lsp_implementations<cr>

" Edit relative to current buffer
nnoremap <leader>e :e <C-R>=expand("%:h") . "/" <CR>

" Git good
nnoremap <leader>G <cmd>Neogit<cr>

" Terminal
tnoremap <Esc> <C-\><C-n>
nnoremap <leader>t <cmd>lua open_toggle_term()<cr>
nnoremap <leader>T <cmd>ToggleTerm direction=float<cr>
