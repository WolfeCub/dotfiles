local lspconfig = require('lspconfig')

require('mason').setup()
require('mason-lspconfig').setup()

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    -- Enable completion triggered by <c-x><c-o>
    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    local map_opts = { buffer = true }

    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
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

    vim.api.nvim_create_autocmd("CursorHold", {
        buffer = bufnr,
        callback = function()
            local opts = {
                focusable = false,
                close_events = { "BufLeave", "CursorMoved", "InsertEnter", "FocusLost" },
                source = 'always',
                prefix = ' ',
                scope = 'cursor',
                border = 'rounded',
            }
            vim.diagnostic.open_float(nil, opts)
        end
    });
end

local setup_lsp = function (server_name, overrides)
    local opts = {
        on_attach = on_attach,
        capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities()),
    }
    lspconfig[server_name].setup(
        vim.tbl_extend('force', opts, overrides)
    )
end

require("mason-lspconfig").setup_handlers({
    function (server_name)
        setup_lsp(server_name, {})
    end,
    sumneko_lua = function ()
        setup_lsp('sumneko_lua', require('lua-dev').setup())
    end,
})
