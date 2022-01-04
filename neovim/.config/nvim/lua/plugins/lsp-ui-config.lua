-- Print diagnostic under cursor to modeline
function PrintDiagnostics(opts, bufnr, line_nr, client_id)
  bufnr = bufnr or 0
  line_nr = line_nr or (vim.api.nvim_win_get_cursor(0)[1] - 1)
  opts = opts or {['lnum'] = line_nr}

  local line_diagnostics = vim.diagnostic.get(bufnr, opts)
  if vim.tbl_isempty(line_diagnostics) then return end

  local diagnostic_message = ""
  for i, diagnostic in ipairs(line_diagnostics) do
    diagnostic_message = diagnostic_message .. string.format("%d: %s", i, diagnostic.message or "")
    print(diagnostic_message)
    if i ~= #line_diagnostics then
      diagnostic_message = diagnostic_message .. "\n"
    end
  end
  vim.api.nvim_echo({{diagnostic_message, "Normal"}}, false, {})
end

vim.cmd [[ autocmd CursorHold * lua PrintDiagnostics() ]]

-- Don't show inline diagnostics
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
        virtual_text = false
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
