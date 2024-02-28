function mp_files()
    local path_aliases = {
        ["go/src/dropbox/mp"] = "go",
        ["rust/mp"] = "rust",
        ["dropbox/mp"] = "python",
        ["configs/proto/dropbox/proto/mp"] = "proto",
        ["configs/services/mp"] = "service",
    }
    local root = '/Users/wolfe/src/server'
    fzf_path_aliases(path_aliases, root)
end
