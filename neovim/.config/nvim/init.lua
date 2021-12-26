require 'functions'
require 'plugins'

vim.g.base16colorspace = 256
vim.cmd 'colorscheme base16-default-dark'

require 'options'

vim.cmd "source ~/.config/nvim/viml/commands.vim"
vim.cmd "source ~/.config/nvim/viml/autocommands.vim"
vim.cmd "source ~/.config/nvim/viml/mappings.vim"
