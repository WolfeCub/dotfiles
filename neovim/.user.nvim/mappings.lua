local sp = require('snacks.picker')
local grapple = require('grapple')
local f = require('shared.functions')

require('legendary').keymaps({
    { '<Space>', '<nop>' },

    -- Fast saving
    { '<leader>w', ':<C-u>silent update<cr>' },

    -- Make j and k behave like they should for wrapped lines
    { 'j', 'gj' },
    { 'k', 'gk' },

    -- Buffer navigation keybinds
    { '<leader>k', function() require('snacks.bufdelete').delete({ wipe = true }) end },
    { '<leader>b', f.alt_buf_with_fallback },
    { '<C-^>', f.alt_buf_with_fallback },

    { '<leader>n', f.narrow_to_function },

    -- Don't lose visual selection with < >
    { '<', '<gv', mode = { 'x' } },
    { '>', '>gv', mode = { 'x' } },

    -- Format
    { '<leader>F', ':Neoformat<cr>', mode = { 'n', 'v' } },

    -- Picker
    { '<leader>f', function() sp.git_files({ untracked = true }) end },
    { '<leader>e', f.snacks_find_file },
    { '<leader>E', function ()
        sp.explorer({
            tree = false,
            layout = { preset = 'default' },
        })
    end },
    { '<leader>m', sp.buffers },
    { '<leader>g', sp.grep },
    { '<leader>r', sp.resume },
    { '<M-x>', sp.commands },

    -- Help
    { '<C-h>f', sp.help },
    { '<C-h>k', '<cmd>Legendary<cr>' },

    -- Terminal
    { '<Esc>', '<C-\\><C-n>', mode = { 't' } },
    { '<leader>T', f.open_toggle_term },

    -- Git
    { '<leader>G', function() require('neogit').open({ cwd = f.get_buf_dir() }) end },
    { '<leader>l', require('snacks.git').blame_line },

    -- DAP
    { '<F5>', '<cmd>lua require("dap").continue()<cr>' },
    { '<F9>', '<cmd>lua require("persistent-breakpoints.api").toggle_breakpoint()<cr>' },
    { '<F10>', '<cmd>lua require("dap").step_over()<cr>' },
    { '<F11>', '<cmd>lua require("dap").step_into()<cr>' },
    { '<M-e>', '<cmd>lua require("dapui").eval()<cr>' },
    { '<M-e>', '<cmd>lua require("dapui").eval()<cr>' },

    -- Grapple
    { '\'', grapple.toggle },
    { '<leader>h', grapple.toggle_tags },
    { '<leader>H', function() grapple.toggle_tags({ scope = 'git' }) end },
    { '<M-;>', '<cmd>Grapple select index=1<cr>' },
    { '<M-,>', '<cmd>Grapple select index=2<cr>' },
    { '<M-.>', '<cmd>Grapple select index=3<cr>' },
    { '<M-p>', '<cmd>Grapple select index=4<cr>' },
    { '<M-y>', '<cmd>Grapple select index=5<cr>' },
    { '<M-i>', '<cmd>Grapple select index=5<cr>' },

})
