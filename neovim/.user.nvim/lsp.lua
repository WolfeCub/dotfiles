local function rust_analyzer_cmd()
    if vim.fn.executable('lspmux') == 1 then
        vim.fn.system('lspmux status 2>/dev/null')
        if vim.v.shell_error == 0 then
            return { 'lspmux', 'client', '--server-path', 'rust-analyzer' }
        end
    end
    return { 'rust-analyzer' }
end

vim.lsp.config['rust_analyzer'] = {
    cmd = rust_analyzer_cmd(),
    ---@type lspconfig.settings.rust_analyzer
    settings = {
        ['rust-analyzer'] = {
            cargo = {
                features = "all",
                cfgs = { "motor", "motor_ia", "motor_ht", "motor_cc", "motor_cc_ia" },
                targetDir = true,
            },
        }
    }
}
vim.lsp.enable('rust_analyzer')

vim.lsp.config['volar'] = {
    init_options = {
        vue = {
            hybridMode = false,
        },
    },
}

vim.lsp.enable('hls')

local hostname = vim.loop.os_gethostname()
local username = vim.env.USER
vim.lsp.config['nixd'] = {
    cmd = { "nixd" },
    filetypes = { "nix" },
    root_markers = { "flake.nix", ".git" },
    ---@type lspconfig.settings.nixd
    settings = {
        nixd = {
            formatting = {
                command = { 'alejandra' },
            },
            nixpkgs = {
                expr = "import <nixpkgs> { }",
            },
            options = {
                nixos = {
                    expr = string.format('(builtins.getFlake (toString ./.)).nixosConfigurations.%s.options', hostname),
                },
                home_manager = {
                    expr = string.format('(builtins.getFlake (toString ./.)).homeConfigurations."%s@%s".options', username, hostname),
                },
            },
        },
    }
}

vim.lsp.enable('nixd')
vim.lsp.enable('yamlls')
