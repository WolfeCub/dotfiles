return {

{
    'nyoom-engineering/oxocarbon.nvim',
    lazy = false,
    priority = 1000,
    config = function()
        -- vim.g.oxocarbon_lua_disable_italic = true
        -- vim.g.oxocarbon_lua_keep_terminal = true
        vim.opt.background = "dark"
        vim.cmd('colorscheme oxocarbon')
        vim.api.nvim_set_hl(0, "FloatBorder", {fg = "#555555"})

    end
},

{
    'simrat39/symbols-outline.nvim',
    config = function()
        require('symbols-outline').setup({
            autofold_depth = 0,
        })
    end
},

{
    'tiagovla/scope.nvim',
    config = function()
        require('scope').setup({})
    end,
},

{
    'stevearc/oil.nvim',
    opts = {},
},

}
