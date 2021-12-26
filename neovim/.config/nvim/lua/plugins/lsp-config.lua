local lsp = require('lspconfig')

lsp.tsserver.setup(coq.lsp_ensure_capabilities({}))

lsp.omnisharp.setup{
    cmd = { "omnisharp", "--languageserver" , "--hostPID", tostring(vim.fn.getpid()) };
}

lsp.graphql.setup{}
