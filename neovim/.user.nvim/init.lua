function user_install_plugins()
    require('plugins')
end

function user_on_lsp_attach()
    local map_opts = { buffer = true }
    local fzf = require('fzf-lua')

    require('legendary').bind_keymaps({
        { 'gd', fzf.lsp_definitions, opts = map_opts },
        { 'gD', vim.lsp.buf.declaration, opts = map_opts },
        { 'K', vim.lsp.buf.hover, opts = map_opts },
        { 'gi', fzf.lsp_implementations, opts = map_opts },
        { '<C-k>', vim.lsp.buf.signature_help, opts = map_opts },
        { 'gr', fzf.lsp_references, opts = map_opts },
        { '[d', vim.diagnostic.goto_prev, opts = map_opts },
        { ']d', vim.diagnostic.goto_next, opts = map_opts },
        { '<leader>F', {
               n = '<cmd>lua vim.lsp.buf.formatting()<cr>',
               v = ':lua vim.lsp.buf.range_formatting()<cr>',
           },
           opts = map_opts
        },
        { 'gx', vim.lsp.buf.code_action, opts = map_opts },
        { 'gR', vim.lsp.buf.rename, opts = map_opts },
    })

end

user_lsp_overrides = {
}

function user_config()
    require('mappings')
end
