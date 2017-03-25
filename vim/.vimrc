" __          __   _  __     _             _
" \ \        / /  | |/ _|   ( )           (_)
"  \ \  /\  / /__ | | |_ ___|/ ___  __   ___ _ __ ___  _ __ ___
"   \ \/  \/ / _ \| |  _/ _ \ / __| \ \ / / | '_ ` _ \| '__/ __|
"    \  /\  / (_) | | ||  __/ \__ \  \ V /| | | | | | | | | (__
"     \/  \/ \___/|_|_| \___| |___/ (_)_/ |_|_| |_| |_|_|  \___|


" Vim Plug {{{

call plug#begin('~/.vim/plugged')

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-surround'
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
set wildchar=<TAB> " Start wild expansion in the command line using <TAB>
set wildmenu       " Wild char completion menu
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

if has('macunix')
    set clipboard=exclude:.*  " Improve startup time by ignoring x server clipboard
end

" }}}

" Visual Configuration {{{

set relativenumber
set number
set background=dark
colorscheme jellybeans
syntax enable
set t_Co=256
filetype on
set showmatch

" }}}

" Custom Keybinds {{{

nnoremap <Space> <nop>
let g:mapleader = "\<Space>"

nnoremap <leader>vimrc :vs ~/.vimrc<cr><C-w>r

" Make j and k behave like they should for wrapped lines
nnoremap j gj
nnoremap k gk
" Fast saving
nnoremap <leader>w :<C-u>update<cr>
" Spellcheck
noremap <leader>ss :setlocal spell!<cr>
" Ctrl+A goes to the beginning of the command line
cnoremap <C-A>	<Home>
" Ctrl+E goes to the end of the command line
cnoremap <C-E>	<End>
" Toggle folds
nnoremap <silent> <Tab> :call Foldtoggle()<cr>
" Source current file
nnoremap <leader>so :source %<cr>
" Source current line
nnoremap <leader>S ^vg_y:execute @@<cr>
" Source visual selection
vnoremap <leader>S y:execute @@<cr>
" Remove search highlights
nnoremap <leader>n :set hlsearch! hlsearch?<cr>
" Buffer navigation keybinds
nnoremap <leader>b :b#<cr>
nnoremap <leader>k :bd<cr>

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

let g:airline_powerline_fonts = 1 " Sets the powerline font to work properly
let g:airline#extensions#tabline#enabled = 1

nnoremap <leader>p :FZF<cr>
" Insert mode completion
inoremap <c-x><c-k> <Plug>(fzf-complete-word)
inoremap <c-x><c-f> <Plug>(fzf-complete-path)
inoremap <c-x><c-j> <Plug>(fzf-complete-file-ag)
inoremap <c-x><c-l> <Plug>(fzf-complete-line)

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

