require('functions')

function user_install_plugins()
    return require('plugins')
end

function user_on_lsp_attach()
    local map_opts = { buffer = true }
    local fzf = require('fzf-lua')

    local jump_to_singlify = function(f)
        return function() f({ jump_to_single_result = true }) end
    end

    local maps = {
        { 'gd',        jump_to_singlify(fzf.lsp_definitions), },
        { 'gD',        vim.lsp.buf.declaration, },
        { 'gk',        vim.lsp.buf.type_definition, },
        { 'K',         vim.lsp.buf.hover, },
        { 'gi',        fzf.lsp_implementations, },
        { 'gr',        function() fzf.lsp_references({ ignore_current_line = true }) end, },
        { '[d',        vim.diagnostic.goto_prev, },
        { ']d',        vim.diagnostic.goto_next, },
        { '<leader>F', vim.lsp.buf.format, },
        { '<leader>F', vim.lsp.buf.range_formatting,          mode = { 'v' } },
        { 'gx',        vim.lsp.buf.code_action,               mode = { 'n', 'v' } },
        { '<C-k>',     vim.lsp.buf.signature_help,            mode = { 'i' } },
        { 'gR',        vim.lsp.buf.rename, },
        { '<leader>d', ':TroubleToggle document_diagnostics<cr>', },
        { '<leader>D', ':TroubleToggle workspace_diagnostics<cr>', },
    }

    -- Apply shared config to all maps
    for i, map in ipairs(maps) do
        maps[i] = vim.tbl_extend('keep', map, { opts = map_opts })
    end

    require('legendary').keymaps(maps)
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
