function fzf_find_files_dwim()
    if require'fzf-lua.path'.is_git_repo(vim.loop.cwd(), true) then
        require'fzf-lua'.git_files()
    else
        require'fzf-lua'.files()
    end
end

function require_viml(vimlConfigPath)
    local path = vim.fn.stdpath('config')
    vim.cmd(string.format('source %s/viml/%s', path, vimlConfigPath))
end
