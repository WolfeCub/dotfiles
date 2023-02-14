return {

{
    'B4mbus/oxocarbon-lua.nvim',
    lazy = false,
    priority = 1000,
    config = function()
        vim.g.oxocarbon_lua_disable_italic = true
        vim.g.oxocarbon_lua_keep_terminal = true
        vim.cmd('colorscheme oxocarbon-lua')
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

}
