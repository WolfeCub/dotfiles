function telescope_find_files_dwim()
    local opts = {};
    local ok = pcall(require"telescope.builtin".git_files, opts);
    if not ok then require"telescope.builtin".find_files(opts) end;
end

function open_toggle_term()
    vim.cmd(string.format("ToggleTerm ToggleTerm direction=vertical size=%d", vim.api.nvim_list_uis()[1].width * 0.4));
end

function alt_buf_with_fallback()
    if vim.fn.bufnr('#') == -1 then
        vim.cmd('bnext')
    else
        vim.cmd('b#')
    end
end
