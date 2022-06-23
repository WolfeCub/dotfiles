-- Hover for error window
vim.cmd [[autocmd CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})]]

-- Don't show inline diagnostics
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
        virtual_text = false,
        severity_sort = true,
    }
)

-- Highlight based on lsp diagnostic
vim.highlight.create('DiagnosticUnderlineError', {ctermfg=1, cterm=underline, gui=underline, guisp=Red}, false)
vim.highlight.create('DiagnosticUnderlineWarn', {ctermfg=3, cterm=underline, gui=underline, guisp=Orange}, false)
vim.highlight.create('DiagnosticUnderlineInfo', {ctermfg=4, cterm=underline, gui=underline, guisp=LightBlue}, false)
vim.highlight.create('DiagnosticUnderlineHint', {ctermfg=7, cterm=underline, gui=underline, guisp=LightGrey}, false)

-- Highlight line number instead of having icons in sign column
vim.cmd [[
  highlight DiagnosticLineNrError guibg=#51202A guifg=#FF0000 gui=bold ctermfg=1
  highlight DiagnosticLineNrWarn guibg=#51412A guifg=#FFA500 gui=bold ctermfg=3
  highlight DiagnosticLineNrInfo guibg=#1E535D guifg=#00FFFF gui=bold ctermfg=4
  highlight DiagnosticLineNrHint guibg=#1E205D guifg=#0000FF gui=bold ctermfg=7

  sign define DiagnosticSignError text= texthl=DiagnosticSignError linehl= numhl=DiagnosticLineNrError
  sign define DiagnosticSignWarn text= texthl=DiagnosticSignWarn linehl= numhl=DiagnosticLineNrWarn
  sign define DiagnosticSignInfo text= texthl=DiagnosticSignInfo linehl= numhl=DiagnosticLineNrInfo
  sign define DiagnosticSignHint text= texthl=DiagnosticSignHint linehl= numhl=DiagnosticLineNrHint
]]
