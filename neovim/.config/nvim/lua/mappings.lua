vim.g.mapleader = ' '
vim.g.maplocalleader = '\\'

require('legendary').bind_keymaps({
    { '<Space>', '<nop>' },

    -- Fast saving
    { '<leader>w', ':<C-u>silent update<cr>' },

    -- Make j and k behave like they should for wrapped lines
    { 'j', 'gj' },
    { 'k', 'gk' },

    -- Buffer navigation keybinds
    { '<leader>k', ':bwipeout<cr>' },
    { '<leader>b', alt_buf_with_fallback },
    { '<C-^>', alt_buf_with_fallback },

    -- Don't lose visual selection with < >
    { '<', '<gv', mode = { 'x' } },
    { '>', '>gv', mode = { 'x' } },

    -- Format
    { '<leader>F', '<cmd>Neoformat<cr>', mode = { 'n', 'v' } },

    -- Telescope
    { '<leader>f', '<cmd>lua telescope_find_files_dwim()<cr>' },
    { '<leader>m', '<cmd>Telescope buffers<cr>' },
    { '<leader>g', '<cmd>Telescope live_grep<cr>' },
    { '<leader>r', '<cmd>Telescope resume<cr>' },
    { '<leader>e',
        '<cmd>lua require("telescope").extensions.file_browser.file_browser({path = vim.fn.expand("%:p:h")})<cr>' },
    { '<M-x>', '<cmd>Telescope commands<cr>' },

    -- Help
    { '<C-h>f', '<cmd>Telescope help_tags<cr>' },
    { '<C-h>v', '<cmd>Telescope vim_options<cr>' },
    { '<C-h>k', '<cmd>Legendary<cr>' },

    -- Terminal
    { '<Esc>', '<C-\\><C-n>', mode = { 't' } },
    { '<leader>t', '<cmd>lua open_toggle_term()<cr>' },
    { '<leader>T', '<cmd>ToggleTerm direction=float<cr>' },

    -- Neogit
    { '<leader>G', '<cmd>Neogit<cr>' },
})
