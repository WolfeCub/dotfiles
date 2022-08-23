function user_install_plugins()
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
        'maxmellon/vim-jsx-pretty',
        requires = { 'leafgarland/typescript-vim' },
    }

    use 'jparise/vim-graphql'
end

function user_on_lsp_attach()
    local map_opts = { buffer = true }

    require('legendary').bind_keymaps({
        { 'gd', '<cmd>Telescope lsp_definitions<cr>', opts = map_opts },
        { 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', opts = map_opts },
        { 'K', '<cmd>lua vim.lsp.buf.hover()<cr>', opts = map_opts },
        { 'gi', '<cmd>Telescope lsp_implementations<cr>', opts = map_opts },
        { '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts = map_opts },
        { 'gr', '<cmd>Telescope lsp_references<cr>', opts = map_opts },
        { '[d', '<cmd>lua vim.diagnostic.goto_prev()<cr>', opts = map_opts },
        { ']d', '<cmd>lua vim.diagnostic.goto_next()<cr>', opts = map_opts },
        { '<leader>F', '<cmd>lua vim.lsp.buf.formatting()<cr>', opts = map_opts },
        { '<leader>F', '<cmd>lua vim.lsp.buf.range_formatting()<cr>', opts = map_opts, mode = { 'v' } },
        { 'gx', '<cmd>lua vim.lsp.buf.code_action()<cr>', opts = map_opts },
        { 'gR', '<cmd>lua vim.lsp.buf.rename()<cr>', opts = map_opts },
    })

end

user_lsp_overrides = {
}

function user_config()
    require('mappings')
end
