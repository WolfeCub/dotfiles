" __          __   _  __     _             _
" \ \        / /  | |/ _|   ( )           (_)
"  \ \  /\  / /__ | | |_ ___|/ ___  __   ___ _ __ ___  _ __ ___
"   \ \/  \/ / _ \| |  _/ _ \ / __| \ \ / / | '_ ` _ \| '__/ __|
"    \  /\  / (_) | | ||  __/ \__ \  \ V /| | | | | | | | | (__
"     \/  \/ \___/|_|_| \___| |___/ (_)_/ |_|_| |_| |_|_|  \___|


set foldmethod=marker         " Enables marker folding for this file
if has('nvim') == 0
    set clipboard=exclude:.*  " Improve startup time by ignoring x server clipboard
    set nocompatible          " be iMproved, required
endif

" Vim Plug {{{

call plug#begin('~/.vim/plugged')

Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/syntastic'
Plug 'scrooloose/nerdtree'
Plug 'SirVer/ultisnips'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'godlygeek/tabular'
Plug 'WolfeCub/vim-markdown-format'
Plug 'terryma/vim-expand-region'
Plug 'suan/vim-instant-markdown', { 'for': ['md', 'markdown'] }
Plug 'matze/vim-move'

if has("lua") == 1 && has('nvim') == 0
    Plug 'Shougo/neocomplete.vim'
elseif has('nvim') == 1
    Plug 'Shougo/deoplete.nvim'
endif

call plug#end()

" }}}

" Basic Configuration {{{

set expandtab      " Basic tab options
set smarttab       " Basic tab options
set hlsearch       " Highlights search options
set laststatus=2   " Displays statusline by default
set backspace=2    " Allows for free backspacing
set incsearch      " incremental search
set wildchar=<TAB> " start wild expansion in the command line using <TAB>
set wildmenu       " wild char completion menu
set ignorecase     " case insensitive matching
set smartcase      " smartcase matching
set showmatch      " show matching brackets when text indicator is over them
set splitbelow     " Open new splits below
set splitright     " Open new splits right
set timeoutlen=1000 ttimeoutlen=0

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

if $TERM_PROGRAM =~ "iTerm" && has('nvim') == 0
    let &t_SI = "\<Esc>]50;CursorShape=1\x7" " Vertical bar in insert mode
    let &t_EI = "\<Esc>]50;CursorShape=0\x7" " Block in normal mode
elseif has('nvim') == 1
    let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
endif

" }}}

" Custom Configuration {{{

" Use :W to sudo write file
command! W w !sudo tee % > /dev/null

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

" }}}

" Custom Keybinds {{{

let g:mapleader = "\<Space>"

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
nnoremap <Tab> za
" Source current file
nnoremap <leader>so :source %<cr>
" Source current line
nnoremap <leader>S ^vg_y:execute @@<cr>
" Source visual selection
vnoremap <leader>S y:execute @@<cr>
" Remove search highlights
nnoremap <leader>n :set hlsearch! hlsearch?<cr>
" Buffer navigation keybinds
nnoremap <leader>bb :b#<cr>
nnoremap <leader>bn :bn<cr>
nnoremap <leader>bp :bp<cr>
nnoremap <leader>bd :bd<cr>

if has('nvim') == 1
    inoremap <Tab> <C-n>
endif
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

" Change default ctrlp binding
let g:ctrlp_map = '<leader>p'

" NERDtree Options
nnoremap <C-n> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

"Set up syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
"Have syntastic use Python 3 for syntax checking
let g:syntastic_python_python_exec = '/usr/local/bin/python3'

" Ultisnips Keybinds
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" Neocomplete {{{
if has("lua") == 1 && has('nvim') == 0
    " Note: This option must set it in .vimrc(_vimrc).  NOT IN .gvimrc(_gvimrc)!
    " Disable AutoComplPop.
    let g:acp_enableAtStartup = 0
    " Use neocomplete.
    let g:neocomplete#enable_at_startup = 1
    " Use smartcase.
    let g:neocomplete#enable_smart_case = 1
    " Set minimum syntax keyword length.
    let g:neocomplete#sources#syntax#min_keyword_length = 3
    let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

    " Define dictionary.
    let g:neocomplete#sources#dictionary#dictionaries = {
        \ 'default' : '',
        \ 'vimshell' : $HOME.'/.vimshell_hist',
        \ 'scheme' : $HOME.'/.gosh_completions'
            \ }

    " Define keyword.
    if !exists('g:neocomplete#keyword_patterns')
        let g:neocomplete#keyword_patterns = {}
    endif
    let g:neocomplete#keyword_patterns['default'] = '\h\w*'

    " Plugin key-mappings.
    inoremap <expr><C-g>     neocomplete#undo_completion()
    inoremap <expr><C-l>     neocomplete#complete_common_string()

    " Recommended key-mappings.
    " <CR>: close popup and save indent.
    inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
    function! s:my_cr_function()
      return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
      " For no inserting <CR> key.
      "return pumvisible() ? "\<C-y>" : "\<CR>"
    endfunction
    " <TAB>: completion.
    inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
    " <C-h>, <BS>: close popup and delete backword char.
    inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
    inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
    " Close popup by <Space>.
    "inoremap <expr><Space> pumvisible() ? "\<C-y>" : "\<Space>"

    " AutoComplPop like behavior.
    "let g:neocomplete#enable_auto_select = 1

    " Shell like behavior(not recommended).
    "set completeopt+=longest
    "let g:neocomplete#enable_auto_select = 1
    "let g:neocomplete#disable_auto_complete = 1
    "inoremap <expr><TAB>  pumvisible() ? "\<Down>" : "\<C-x>\<C-u>"

    " Enable omni completion.
    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
    autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

    " Enable heavy omni completion.
    if !exists('g:neocomplete#sources#omni#input_patterns')
      let g:neocomplete#sources#omni#input_patterns = {}
    endif
    "let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
    "let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
    "let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

    " For perlomni.vim setting.
    " https://github.com/c9s/perlomni.vim
    let g:neocomplete#sources#omni#input_patterns.perl = '\h\w*->\h\w*\|\h\w*::'
endif
" }}}

" }}}

" Temp & Testing {{{

" To improve vim habits
" Unbind the cursor keys in insert, normal and visual modes.
for prefix in ['i', 'n', 'v']
	for key in ['<Up>', '<Down>', '<Left>', '<Right>']
		exe prefix . "noremap " . key . " <Nop>"
	endfor
endfor

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

" Neovim Only {{{
if has('nvim') == 1

    let g:deoplete#enable_at_startup = 1
    tnoremap <Esc> <C-\><C-n>

endif
" }}}

