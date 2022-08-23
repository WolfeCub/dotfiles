require('legendary').bind_keymaps({
    { '<Space>', '<nop>' },

    -- Fast saving
    { '<leader>w', ':<C-u>silent update<cr>' },

    -- Make j and k behave like they should for wrapped lines
    { 'j', 'gj' },
    { 'k', 'gk' },

    -- Buffer navigation keybinds
    { '<leader>k', function() require('bufdelete').bufwipeout(0) end },
    { '<leader>b', alt_buf_with_fallback },
    { '<C-^>', alt_buf_with_fallback },

    -- Don't lose visual selection with < >
    { '<', '<gv', mode = { 'x' } },
    { '>', '>gv', mode = { 'x' } },

    -- Format
    { '<leader>F', ':Neoformat<cr>', mode = { 'n', 'v' } },

    -- Telescope
    { '<leader>f', telescope_find_files_dwim },
    { '<leader>m', '<cmd>Telescope buffers<cr>' },
    { '<leader>g', '<cmd>Telescope live_grep<cr>' },
    { '<leader>r', '<cmd>Telescope resume<cr>' },
    { '<leader>e',
        '<cmd>lua require("telescope").extensions.file_browser.file_browser({path = get_buf_dir()})<cr>' },
    { '<M-x>', '<cmd>Telescope commands<cr>' },

    -- Help
    { '<C-h>f', '<cmd>Telescope help_tags<cr>' },
    { '<C-h>v', '<cmd>Telescope vim_options<cr>' },
    { '<C-h>k', '<cmd>Legendary<cr>' },

    -- Terminal
    { '<Esc>', '<C-\\><C-n>', mode = { 't' } },
    { '<leader>t', open_toggle_term },
    { '<leader>T', '<cmd>ToggleTerm direction=float<cr>' },

    -- Neogit
    { '<leader>G', '<cmd>Neogit<cr>' },
})
