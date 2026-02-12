require('functions')

require('shared.setup').setup({
    install_plugins = function()
        return require('plugins')
    end,

    on_lsp_attach = function()
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
            { 'gx',        vim.lsp.buf.code_action,    mode = { 'n', 'v' } },
            { '<C-k>',     vim.lsp.buf.signature_help, mode = { 'i' } },
            { 'gR',        vim.lsp.buf.rename, },
            { '<leader>F', vim.lsp.buf.format, },
            {
                '<leader>F',
                function()
                    vim.lsp.buf.format({
                        range = {
                            ['start'] = vim.api.nvim_buf_get_mark(0, "<"),
                            ['end'] = vim.api.nvim_buf_get_mark(0, ">"),
                        },
                    })
                    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<Esc>', true, false, true), 'n', false)
                end,
                mode = "v",
            },
            { '[d',        function() vim.diagnostic.jump({ count = -1, float = true }) end },
            { ']d',        function() vim.diagnostic.jump({ count = 1, float = true }) end },
        }

        -- Apply shared config to all maps
        for i, map in ipairs(maps) do
            maps[i] = vim.tbl_extend('keep', map, map_opts)
        end

        require('which-key').add(maps)
    end,

    config = function()
        require('mappings')
        vim.o.showtabline = 2
    end,
})

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
