local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end
vim.api.nvim_command 'packadd packer.nvim'

return require('packer').startup(function()
    use 'wbthomason/packer.nvim'

    use 'nanotech/jellybeans.vim'
    use 'chriskempson/base16-vim'

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
        requires = { {'nvim-lua/plenary.nvim'} },
        config = function() 
            require('telescope').setup{
                defaults = { 
                    file_ignore_patterns = {"node_modules", "bin", "obj"} ,
                },
                pickers = {
                    find_files = {
                        hidden = true,
                    },
                }
            }
        end
    }

    use {
        'ahmedkhalf/project.nvim',
        requires = {'nvim-telescope/telescope.nvim'},
        config = function()
            require("project_nvim").setup{
                show_hidden = true
            }
            require('telescope').load_extension('projects')
        end
    }

    use {
        'ms-jpq/coq_nvim',
        branch = 'coq',
        setup = function()
            vim.g.coq_settings = {auto_start = 'shut-up'}
        end
    }

    use {
        'ms-jpq/coq.artifacts', 
        branch = 'artifacts'
    }

    use {
        'neovim/nvim-lspconfig',
        requires = {'ms-jpq/coq_nvim'},
        config = function()
            require'lspconfig'.tsserver.setup(coq.lsp_ensure_capabilities({}))
        end
    }

    use {
        'folke/todo-comments.nvim',
        requires = 'nvim-lua/plenary.nvim',
        config = function()
            require('todo-comments').setup{
                signs = false,
            }
        end
    }

    use {
        'maxmellon/vim-jsx-pretty',
        requires = {'leafgarland/typescript-vim'},
    }

    if packer_bootstrap then
        require('packer').sync()
    end
end)