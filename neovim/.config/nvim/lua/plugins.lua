local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end
vim.api.nvim_command 'packadd packer.nvim'

return require('packer').startup(function()
    use 'wbthomason/packer.nvim'
    use 'nvim-lua/plenary.nvim'

    use 'nanotech/jellybeans.vim'
    use {
        'chriskempson/base16-vim',
        config = function()
            vim.g.base16colorspace = 256;
            vim.cmd('colorscheme base16-default-dark');
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
            require('plugins.treesitter-config');
        end
    }

    use {
        'nvim-lualine/lualine.nvim',
        requires = {'kyazdani42/nvim-web-devicons', opt = true},
        config = function() require('plugins.lualine-config') end
    }

	use { 
		'ibhagwan/fzf-lua',
		requires = {'kyazdani42/nvim-web-devicons'},
        config = function()
            local actions = require "fzf-lua.actions"
            require('fzf-lua').setup {
                global_resume = true,
                global_resume_query = true,
                winopts = {
                    height = 0.3,
                    width = 1,
                    row = 1,
                },
                files = {
                    previewer = false,
                },
                git = {
                    files = {
                        previewer = false,
                    },
                },
                buffers = {
                    previewer = false,
                },
                file_icon_padding = ' ',
            }
        end
	}

    use {
        'ms-jpq/coq_nvim',
        branch = 'coq',
        setup = function()
            vim.g.coq_settings = {
                auto_start = 'shut-up',
                display = {
                    icons = {
                        mode = 'short',
                    },
                    pum = {
                        kind_context = {' ', ''},
                    }
                },
                clients = {
                    ['buffers.enabled'] = false,
                    ['tmux.enabled'] = false,
                    ['snippets.warn'] = {},
                }
            }
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
