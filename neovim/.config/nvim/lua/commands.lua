require('legendary').bind_commands({
    -- Use :W to sudo write file
    { ':W', ':SudaWrite' },

    -- Command to remove trailing whitespace
    { ':TrimWhitespace',
        function ()
            vim.cmd("%s/\\s\\+$//e")
            vim.cmd(
                vim.api.nvim_replace_termcodes("normal! <C-o>", true, false, true)
            )

        end,
        description = 'Trim trailing whitespace',
    },
})

