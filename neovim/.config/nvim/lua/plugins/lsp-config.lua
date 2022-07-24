local lspconfig = require('lspconfig')

require('mason').setup()
require('mason-lspconfig').setup()

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end

    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    -- Enable completion triggered by <c-x><c-o>
    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.

    local map_opts = { noremap=true, silent=true }

    -- See `:help vim.lsp.*` for documentation on any of the below functions
    buf_set_keymap('n', 'gd', '<cmd>Telescope lsp_definitions<cr>', map_opts)
    buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', map_opts)
    buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<cr>', map_opts)
    buf_set_keymap('n', 'gi', '<cmd>Telescope lsp_implementations<cr>', map_opts)
    buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<cr>', map_opts)
    buf_set_keymap('n', 'gr', '<cmd>Telescope lsp_references<cr>', map_opts)
    buf_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<cr>', map_opts)
    buf_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<cr>', map_opts)
    buf_set_keymap('n', '<leader>F', '<cmd>lua vim.lsp.buf.formatting()<cr>', map_opts)
    buf_set_keymap('v', '<leader>F', '<cmd>lua vim.lsp.buf.range_formatting()<cr>', map_opts)
    buf_set_keymap('n', 'gx', '<cmd>lua vim.lsp.buf.code_action()<cr>', map_opts)
    buf_set_keymap('n', 'gR', '<cmd>lua vim.lsp.buf.rename()<cr>', map_opts)

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
