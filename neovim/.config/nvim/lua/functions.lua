function telescope_find_files_dwim()
    local builtin = require('telescope.builtin')
    local ok = pcall(builtin.git_files, { cwd = get_buf_dir() })
    if not ok then
        buildin.find_files({})
    end
end

function open_toggle_term()
    vim.cmd(
        string.format("ToggleTerm ToggleTerm direction=vertical size=%d", vim.api.nvim_list_uis()[1].width * 0.4)
    )
end

function alt_buf_with_fallback()
    if vim.fn.bufnr('#') == -1 then
        vim.cmd('bnext')
    else
        vim.cmd('b#')
    end
end

function get_buf_dir()
    return vim.fn.expand("%:p:h")
end
