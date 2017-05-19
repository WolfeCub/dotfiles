" __          __   _  __     _             _
" \ \        / /  | |/ _|   ( )           (_)
"  \ \  /\  / /__ | | |_ ___|/ ___  __   ___ _ __ ___  _ __ ___
"   \ \/  \/ / _ \| |  _/ _ \ / __| \ \ / / | '_ ` _ \| '__/ __|
"    \  /\  / (_) | | ||  __/ \__ \  \ V /| | | | | | | | | (__
"     \/  \/ \___/|_|_| \___| |___/ (_)_/ |_|_| |_| |_|_|  \___|


" Vim Plug {{{

call plug#begin('~/.vim/plugged')

" Visual
Plug 'chriskempson/base16-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Fuzzy Finding
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Utils
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-scriptease'
Plug 'tpope/vim-afterimage'

Plug 'haya14busa/incsearch.vim'
Plug 'vim-scripts/matchit.zip'
Plug 'junegunn/vim-easy-align'

" Completion
Plug 'ervandew/supertab'
Plug 'Rip-Rip/clang_complete'
Plug 'davidhalter/jedi-vim'

" Misc
Plug 'WolfeCub/vim-markdown-format', { 'for': ['md', 'markdown'] }

call plug#end()

" }}}

" Basic Configuration {{{

set nocompatible   " be iMproved, required
set tabstop=4      " Show existing tab with 4 spaces width
set shiftwidth=4   " When indenting with '>', use 4 spaces width
set expandtab      " Basic tab options
set hlsearch       " Highlights search options
set laststatus=2   " Displays statusline by default
set backspace=2    " Allows for free backspacing
set incsearch      " Incremental search
set ignorecase     " Case insensitive matching
set smartcase      " Smartcase matching
set showmatch      " Show matching brackets when text indicator is over them
set splitbelow     " Open new splits below
set splitright     " Open new splits right
set shortmess=atI  " No intro message
set title          " Show the filename in the window titlebar
set scrolloff=3    " Start scrolling three lines before the horizontal window border
set visualbell     " Disable screen flash bells
set t_vb=          " Disable audio bells
set nostartofline  " Don’t reset cursor to start of line when moving around.
set foldenable     " Turn on folds
set fdm=indent     " Fold on syntax
set foldlevel=999  " Make it really high, so they're not displayed by default
set hidden         " Allow buffers with pending changes to be sent to background
set timeoutlen=500 ttimeoutlen=0

" Completion options
set wildmode=list:longest
set wildmenu                "enable ctrl-n and ctrl-p to scroll thru matches
set wildignore=*.o,*.obj,*~ "stuff to ignore when tab completing
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=vendor/rails/**
set wildignore+=vendor/cache/**
set wildignore+=*.gem
set wildignore+=log/**
set wildignore+=tmp/**
set wildignore+=*.png,*.jpg,*.gif

" }}}

" Visual Configuration {{{

set relativenumber
set number
set background=dark
let base16colorspace=256  " Access colors present in 256 colorspace
colorscheme base16-default-dark
syntax enable
set t_Co=256
filetype on
set showmatch
set cursorline
hi CursorLine cterm=bold ctermbg=NONE

" }}}

" Custom Keybinds {{{

nnoremap <Space> <nop>
let g:mapleader = "\<Space>"
let g:maplocalleader = "\\"

" Make j and k behave like they should for wrapped lines
nnoremap j gj
nnoremap k gk
" Quick access to vimrc file
nnoremap <leader>init :e ~/.vimrc<cr>
" Fast saving
nnoremap <leader>w :<C-u>update<cr>
" Spellcheck
noremap <leader>ss :setlocal spell!<cr>
" Toggle folds
nnoremap <silent> <Tab> :call Foldtoggle()<cr>
" Source current file
nnoremap <leader>S :source %<cr>
" Source visual selection
vnoremap <leader>S y:execute @@<cr>
" Buffer navigation keybinds
nnoremap <leader>b :b#<cr>
nnoremap <leader>k :bd<cr>
nnoremap <leader>m :Buffers<cr>
" Don't lose visual selection with < >
xnoremap < <gv
xnoremap > >gv
" Better redraw. Clears search and fixes syntax
nnoremap <c-l> :nohlsearch<cr>:diffupdate<cr>:syntax sync fromstart<cr><c-l>
" Quickly edit macros
nnoremap <leader>@  :<c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>
" FZF ag search
nnoremap <leader>f :Ag<cr>
" FZF search tags file
nnoremap <leader>t :Tags<cr>
nnoremap <leader>T :call fzf#vim#tags(expand('<cword>'))<cr>

nnoremap <localleader><localleader> :b#<cr>

" }}}

" Custom Configuration {{{

" Use :W to sudo write file
command! W w !sudo tee % > /dev/null

" Command to remove trailing whitespace
function! TrimWhitespace()
    exec "%s/\\s\\+$//e"
    exec "normal! \<C-o>"
endfunction
command! -nargs=0 TrimWhitespace :call TrimWhitespace()

" Adds the :Shell command to execute a command via the shell and place the result in a buffer
function! s:ExecuteInShell(command)
    let command = join(map(split(a:command), 'expand(v:val)'))
    let winnr = bufwinnr('^' . command . '$')
    silent! execute  winnr < 0 ? 'botright new ' . fnameescape(command) : winnr . 'wincmd w'
    setlocal buftype=nowrite bufhidden=wipe nobuflisted noswapfile nowrap number
    echo 'Execute ' . command . '...'
    silent! execute 'silent %!'. command
    silent! execute 'resize ' . line('$')
    silent! redraw
    silent! execute 'au BufUnload <buffer> execute bufwinnr(' . bufnr('#') . ') . ''wincmd w'''
    silent! execute 'nnoremap <silent> <buffer> <LocalLeader>r :call <SID>ExecuteInShell(''' . command . ''')<CR>'
    echo 'Shell command ' . command . ' executed.'
endfunction
command! -complete=shellcmd -nargs=+ Shell call s:ExecuteInShell(<q-args>)

" Hides the error message when toggling folds
function! Foldtoggle()
    try
        normal! za
    catch /E490/
    endtry
endfunction

" }}}

" Plugin Configs {{{

let g:airline_powerline_fonts = 1 " Sets the powerline font to work properly
let g:airline#extensions#tabline#enabled = 1

" FZF Settings
" Insert mode completion
inoremap <c-x><c-k> <Plug>(fzf-complete-word)
inoremap <c-x><c-f> <Plug>(fzf-complete-path)
inoremap <c-x><c-j> <Plug>(fzf-complete-file-ag)
inoremap <c-x><c-l> <Plug>(fzf-complete-line)
" [Tags] Command to generate tags file
let g:fzf_tags_command = 'ctags -R'

" incsearch
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" Easy Align
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Supertab
let g:SuperTabDefaultCompletionType = "<C-X><C-O>"
" If you prefer the Omni-Completion tip window to close when a selection is
" made, these lines close it on movement in insert mode or when leaving
" insert mode
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" Set clang path for completion
let g:clang_library_path='/usr/lib64/libclang.so.3.9'

" Keybinds for Markdown Format
nnoremap <leader>h1 :MakeHeader 1<cr>
nnoremap <leader>h2 :MakeHeader 2<cr>
nnoremap <leader>h3 :MakeHeader 3<cr>
nnoremap <leader>h4 :MakeHeader 4<cr>
nnoremap <leader>h5 :MakeHeader 5<cr>
nnoremap <leader>h6 :MakeHeader 6<cr>
vnoremap <leader>ll :<C-u>MakeList<cr>
vnoremap <leader>nl :<C-u>MakeNumberedList<cr>
vnoremap <leader>cb :<C-u>FencedCodeBlock<cr>
vnoremap <leader>bq :<C-u>BlockQuote<cr>
nnoremap <leader>li :MakeLink n<cr>
vnoremap <leader>li :<C-u>MakeLink v<cr>


" }}}

" Temp & Testing {{{

function! GetSyntaxID()
    return synID(line('.'), col('.'), 1)
endfunction

function! GetSyntaxParentID()
    return synIDtrans(GetSyntaxID())
endfunction

function! GetSyntax()
    echo synIDattr(GetSyntaxID(), 'name')
    exec "hi ".synIDattr(GetSyntaxParentID(), 'name')
endfunction

" }}}

" Modeline {{{
" vim: set fdm=marker foldlevel=0:
"" }}}

