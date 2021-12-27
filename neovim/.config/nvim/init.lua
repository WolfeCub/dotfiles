require 'functions'
require 'plugins'

vim.g.base16colorspace = 256
vim.cmd 'colorscheme base16-default-dark'

require 'options'

require_viml 'commands.vim'
require_viml 'autocommands.vim'
require_viml 'mappings.vim'
