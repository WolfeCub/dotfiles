return {

-- {
--     'nyoom-engineering/oxocarbon.nvim',
--     lazy = false,
--     priority = 1000,
--     config = function()
--         -- vim.g.oxocarbon_lua_disable_italic = true
--         -- vim.g.oxocarbon_lua_keep_terminal = true
--         vim.opt.background = "dark"
--         vim.cmd('colorscheme oxocarbon')
--         vim.api.nvim_set_hl(0, "FloatBorder", {fg = "#555555"})
--
--     end
-- },

{
    'wtfox/jellybeans.nvim',
    priority = 1000,
    config = function()
        require('jellybeans').setup({ italics = false })
        vim.cmd.colorscheme('jellybeans')
    end,
},

}
