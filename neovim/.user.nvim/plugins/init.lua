local use = require('packer').use

use {
    'B4mbus/oxocarbon-lua.nvim',
    config = function()
        vim.g.oxocarbon_lua_disable_italic = true
        vim.g.oxocarbon_lua_keep_terminal = true
        vim.cmd('colorscheme oxocarbon-lua')
    end
}

use {
    'charlie39/OneStop.nvim',
    branch = 'main',
    config = function()
        require('onestop').setup({
            lscmds = {
                rust_analyzer = {
                    'cargo build',
                    'cargo run',
                },
                omnisharp = {
                    'dotnet build',
                },
            },
        })
    end
}

use {
    'maxmellon/vim-jsx-pretty',
    requires = { 'leafgarland/typescript-vim' },
}

use 'jparise/vim-graphql'
