function user_install_plugins()
    return require('plugins')
end

function user_on_lsp_attach()
    local map_opts = { buffer = true }
    local fzf = require('fzf-lua')

    local jump_to_singlify = function(f)
        return function() f({ jump_to_single_result = true }) end
    end

    require('legendary').keymaps({
        { 'gd',    jump_to_singlify(fzf.lsp_definitions), opts = map_opts },
        { 'gD',    vim.lsp.buf.declaration,               opts = map_opts },
        { 'K',     vim.lsp.buf.hover,                     opts = map_opts },
        { 'gi',    fzf.lsp_implementations,               opts = map_opts },
        { '<C-k>', vim.lsp.buf.signature_help,            opts = map_opts },
        { 'gr',    fzf.lsp_references,                    opts = map_opts },
        { '[d',    vim.diagnostic.goto_prev,              opts = map_opts },
        { ']d',    vim.diagnostic.goto_next,              opts = map_opts },
        { '<leader>F', vim.lsp.buf.format,                opts = map_opts },
        { '<leader>F', vim.lsp.buf.range_formatting, mode = { 'v' }, opts = map_opts },
        { 'gx',        vim.lsp.buf.code_action, mode = { 'n', 'v' }, opts = map_opts },
        { 'gR',        vim.lsp.buf.rename,      opts = map_opts },
        { '<leader>d', ':TroubleToggle<cr>',    opts = map_opts },
    })
end

user_lsp_overrides = {
    pyright = {
        settings = {
            python = {
                analysis = {
                    extraPaths = { '/Users/wolfe/src/server' },
                },
            },
        },
    },
}

function user_config()
    require('mappings')
end
