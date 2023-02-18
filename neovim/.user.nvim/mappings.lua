local fzf = require('fzf-lua')

require('legendary').keymaps({
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


    { '<leader>n', narrow_to_function },

    -- Don't lose visual selection with < >
    { '<', '<gv', mode = { 'x' } },
    { '>', '>gv', mode = { 'x' } },

    -- Format
    { '<leader>F', ':Neoformat<cr>', mode = { 'n', 'v' } },

    -- Fzf
    { '<leader>f', fzf_files_browse },
    { '<leader>m', fzf.buffers },
    { '<leader>g', fzf.live_grep },
    { '<leader>r', fzf.resume },
    { '<leader>e', function() fzf.files({ cwd = get_buf_dir()}) end },
    { '<M-x>', fzf.commands },

    -- Help
    { '<C-h>f', fzf.help_tags },
    { '<C-h>k', '<cmd>Legendary<cr>' },

    -- Terminal
    { '<Esc>', '<C-\\><C-n>', mode = { 't' } },
    { '<leader>t', open_toggle_term },
    { '<leader>T', '<cmd>ToggleTerm direction=float<cr>' },

    { '<leader>Gs', fzf.git_status },
    { '<leader>Gb', fzf.git_branches },
})
