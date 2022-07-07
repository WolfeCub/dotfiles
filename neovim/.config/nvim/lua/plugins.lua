local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end
vim.api.nvim_command 'packadd packer.nvim'

return require('packer').startup(function()
    use 'wbthomason/packer.nvim'
    use 'nvim-lua/plenary.nvim'

    use {
        'folke/tokyonight.nvim',
        config = function()
            vim.g.tokyonight_style = 'night'
            vim.cmd('colorscheme tokyonight');
        end
    }

    use 'tpope/vim-surround'
    use 'tpope/vim-repeat'
    use 'tpope/vim-rsi'
    use 'nelstrom/vim-visual-star-search'
    use 'tommcdo/vim-lion'

    use {
        'ggandor/leap.nvim',
        config = function() require('leap').set_default_keymaps() end
    }

    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
        config = function()
            require('nvim-treesitter.configs').setup({
                sync_install = false,
                highlight = {
                    enable = true,
                    additional_vim_regex_highlighting = false,
                },
            });
            require('plugins.treesitter');
        end
    }

    use {
        'nvim-lualine/lualine.nvim',
        requires = {'kyazdani42/nvim-web-devicons', opt = true},
        config = function() require('plugins.lualine') end
    }

	use { 
		'ibhagwan/fzf-lua',
		requires = {'kyazdani42/nvim-web-devicons'},
        config = function() require('plugins.fzf') end
	}

    use {
        'hrsh7th/nvim-cmp',
        requires = {
            'neovim/nvim-lspconfig',
            'hrsh7th/cmp-nvim-lsp',
            'L3MON4D3/LuaSnip',
            'saadparwaiz1/cmp_luasnip',
            'onsails/lspkind.nvim',
            'hrsh7th/cmp-path',
            'hrsh7th/cmp-cmdline',
        },
        config = function() require('plugins.cmp') end
    }

    use {
        'folke/trouble.nvim',
        requires = 'kyazdani42/nvim-web-devicons',
        config = function()
            require('trouble').setup();
        end
    }

    use {
        'neovim/nvim-lspconfig',
        requires = {'ms-jpq/coq_nvim'},
        config = function()
            require('plugins.lsp-config')
            require('plugins.lsp-ui-config')
        end
    }

    use {
        'williamboman/nvim-lsp-installer',
        requires = {'neovim/nvim-lspconfig'},
    }

    use {
        'j-hui/fidget.nvim',
        requires = {'neovim/nvim-lspconfig'},
        config = function()
            require('fidget').setup({})
        end
    }

    use {
        'akinsho/toggleterm.nvim', 
        tag = 'v1.*', 
        config = function()
            require('toggleterm').setup()
        end
    }

    use {
        'TimUntersberger/neogit',
        config = function()
            require('neogit').setup{}
        end
    }

    use {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup()
        end
    }

    use {
        'maxmellon/vim-jsx-pretty',
        requires = {'leafgarland/typescript-vim'},
    }

    use 'jparise/vim-graphql'

    if packer_bootstrap then
        require('packer').sync()
    end
end)
