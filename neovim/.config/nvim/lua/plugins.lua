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
            vim.g.tokyonight_style = 'night';
            vim.g.tokyonight_italic_comments = false;
            vim.g.tokyonight_italic_keywords = false;
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
		'nvim-telescope/telescope.nvim',
		requires = {
            'nvim-lua/plenary.nvim',
            'nvim-telescope/telescope-ui-select.nvim',
            'kyazdani42/nvim-web-devicons',
            {'nvim-telescope/telescope-fzf-native.nvim', run = 'make' },
            'nvim-telescope/telescope-file-browser.nvim',
        },
        config = function()
            require('telescope').setup({
                pickers = {
                    git_files = {
                        show_untracked = true,
                    },
                },
            });
            require('telescope').load_extension('ui-select');
            require('telescope').load_extension('fzf');
            require('telescope').load_extension('file_browser');
        end
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

    use 'folke/lua-dev.nvim'


    use {
        'mfussenegger/nvim-dap',
        requires = { 'rcarriga/nvim-dap-ui' },
        config = function() require('plugins.dap') end,
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
