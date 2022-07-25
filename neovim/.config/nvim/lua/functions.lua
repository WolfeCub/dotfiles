function telescope_find_files_dwim()
    local opts = {};
    local ok = pcall(require"telescope.builtin".git_files, opts);
    if not ok then require"telescope.builtin".find_files(opts) end;
end

function open_toggle_term()
    vim.cmd(string.format("ToggleTerm ToggleTerm direction=vertical size=%d", vim.api.nvim_list_uis()[1].width * 0.4));
end

function vim_noremap(mode, lhs, rhs)
    local options = { noremap = true };
    vim.api.nvim_set_keymap(mode, lhs, rhs, options);
end

function nnoremap(lhs, rhs)
    vim_noremap('n', lhs, rhs);
end

function vnoremap(lhs, rhs)
    vim_noremap('v', lhs, rhs);
end
