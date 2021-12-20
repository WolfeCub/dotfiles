local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end
vim.api.nvim_command 'packadd packer.nvim'

return require('packer').startup(function()
  use 'wbthomason/packer.nvim'

  use 'nanotech/jellybeans.vim'

  use 'tpope/vim-surround'
  use 'tpope/vim-repeat'
  use 'tpope/vim-rsi'

  use {
    'nvim-lualine/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true},
    config = function() 
      require('lualine').setup{
        options = {
          theme = 'auto'
        }
      }
    end
  }

  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  use 'neovim/nvim-lspconfig'
  use {
      'ms-jpq/coq_nvim',
      config = function()
        require'lspconfig'.tsserver.setup{}
      end
  }

  if packer_bootstrap then
    require('packer').sync()
  end
end)
