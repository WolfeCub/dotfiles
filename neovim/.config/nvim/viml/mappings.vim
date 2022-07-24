nnoremap <Space> <nop>
let g:mapleader = "\<Space>"
let g:maplocalleader = "\\"

" Fast saving
nnoremap <leader>w :<C-u>silent update<cr>

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

" Telescope
nnoremap <leader>f <cmd>lua telescope_find_files_dwim()<cr>
nnoremap <leader>m <cmd>Telescope buffers<cr>
nnoremap <leader>g <cmd>Telescope live_grep<cr>
nnoremap <leader>r <cmd>Telescope resume<cr>
nnoremap <leader>e <cmd>lua require('telescope').extensions.file_browser.file_browser({path = vim.fn.expand('%:p:h')})<cr>
nnoremap <M-x> <cmd>Telescope commands<cr>

" Git good
nnoremap <leader>G <cmd>Neogit<cr>

" Terminal
tnoremap <Esc> <C-\><C-n>
nnoremap <leader>t <cmd>lua open_toggle_term()<cr>
nnoremap <leader>T <cmd>ToggleTerm direction=float<cr>
