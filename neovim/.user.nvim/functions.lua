function fzf_find_files(cwd)
    require('fzf-lua').files({
        cwd = cwd,
        actions = {
            ["ctrl-w"] = {
                function()
                    local new_cwd = vim.loop.fs_realpath(cwd .. '/..')
                    fzf_find_files(new_cwd)
                end
            },
        },
    })
end
