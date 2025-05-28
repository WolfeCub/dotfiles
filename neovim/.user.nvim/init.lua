require('functions')

function user_install_plugins()
    return require('plugins')
end

function user_on_lsp_attach()
    local map_opts = { buffer = true }
    local sp = require('snacks.picker')

    local maps = {
        { 'gd',        sp.lsp_definitions, },
        { 'gi',        sp.lsp_implementations, },
        { 'gr',        sp.lsp_references, },
        { 'gs',        sp.lsp_symbols, },
        { 'gS',        sp.lsp_workspace_symbols, },
        { 'gD',        vim.lsp.buf.declaration, },
        { 'gk',        vim.lsp.buf.type_definition, },
        { 'K',         vim.lsp.buf.hover, },
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

vim.lsp.config['rust_analyzer'] = {
    settings = {
        ['rust-analyzer'] = {
            cargo = {
                features = "all",
            },
        }
    }
}

vim.lsp.config['volar'] = {
    init_options = {
        vue = {
            hybridMode = false,
        },
    },
}

function user_config()
    require('mappings')
    vim.o.showtabline = 2
end
