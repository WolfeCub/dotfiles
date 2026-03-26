return {

{
    'wtfox/jellybeans.nvim',
    priority = 1000,
    lazy = false,
    config = function()
        require('jellybeans').setup({ italics = false })
        vim.cmd.colorscheme('jellybeans')
    end,
},

{
    'seblyng/roslyn.nvim',
    ft = 'cs',
    dependencies = 'williamboman/mason-lspconfig.nvim',
    ---@module 'roslyn.config'
    ---@type RoslynNvimConfig
    opts = {
        ignore_target = function(target)
            return string.match(target, 'Unified.sln') ~= nil
        end,
        lock_target = true,
    },
},

}
