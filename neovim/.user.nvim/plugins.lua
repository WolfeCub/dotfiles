return {

{
    'wtfox/jellybeans.nvim',
    priority = 1000,
    lazy = false,
    opts = {
        italics = false
    },
},

{
    'scottmckendry/cyberdream.nvim',
    priority = 1000,
    lazy = false,
    opts = {
        transparent = true,
    },
},

{
    'afonsofrancof/OSC11.nvim',
    priority = 1000,
    lazy = false,
    opts = {
        on_dark = function()
            vim.cmd('colorscheme jellybeans')
        end,
        on_light = function()
            vim.cmd('colorscheme cyberdream-light')
        end,
    },
    config = function (_, opts)
        if vim.o.background == 'dark' then
            opts.on_dark()
        else
            opts.on_light()
        end
        require('osc11').setup(opts)
    end
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
