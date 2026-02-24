local sp = require('snacks.picker')
local sf = require('shared.functions')
local f = require('functions')
local ext = require('shared.extensions')
local wk = require('which-key')
local harpoon = require('harpoon')

wk.add({
    { '<Space>', '<nop>' },

    -- Fast saving
    { '<leader>w', ':<C-u>silent update<cr>' },

    -- Make j and k behave like they should for wrapped lines
    { 'j', 'gj' },
    { 'k', 'gk' },

    -- Buffer navigation keybinds
    { '<leader>k', function() require('snacks.bufdelete').delete({ wipe = true }) end },
    { '<leader>b', sf.alt_buf_with_fallback },
    { '<C-^>', sf.alt_buf_with_fallback },

    -- Quickfix
    { '<leader>q', function() require('quicker').toggle() end },
    { '<C-M-n>', function() f.qf_move(1) end },
    { '<C-M-p>', function() f.qf_move(-1) end },

    { '<leader>n', ext.narrow_to_function },

    -- Don't lose visual selection with < >
    { '<', '<gv', mode = { 'x' } },
    { '>', '>gv', mode = { 'x' } },

    -- Format
    { '<leader>F', ':Neoformat<cr>', mode = { 'n', 'v' } },

    -- Picker
    { '<leader>f', function() sp.git_files({ untracked = true }) end },
    { '<leader>l', sp.files },
    { '<leader>e', ext.snacks_find_file },
    { '<leader>E', function ()
        sp.explorer({
            tree = false,
            layout = { preset = 'default' },
        })
    end },
    { '<leader>m', function() sp.buffers({ formatters = { file = { filename_first = true } } }) end },
    { '<leader>g', sp.grep },
    { '<leader>r', sp.resume },
    { '<leader>R', sp.recent },
    { '<M-x>', sp.commands },

    -- Diagnostics
    { '<leader>d', function() vim.diagnostic.setqflist({ open = true }) end },
    { '<leader>D', function() wk.show({ keys = '<leader>D' }) end },
    { '<leader>Dd', function()
        local trouble = require('trouble')
        if trouble.is_open() then
            trouble.focus()
        else
            vim.cmd('Trouble diagnostics toggle filter.severity=vim.diagnostic.severity.ERROR')
        end
    end },
    { '<leader>Db', function() vim.cmd('Trouble diagnostics toggle filter.buf=0') end, desc = 'Buffer Diagnostics' },
    { '<leader>Dw', function() vim.cmd('Trouble diagnostics toggle') end, desc = 'Workspace Diagnostics' },
    { '<leader>Dy', sf.copy_diagnostic, desc = 'Copy Diagnostic' },

    -- Help
    { '<C-h>f', sp.help },
    { '<C-h>k', sp.keymaps },

    -- Terminal
    { '<Esc>', '<C-\\><C-n>', mode = { 't' } },
    { '<leader>T', sf.open_toggle_term },

    -- Git
    { '<leader>G', function() require('neogit').open({ cwd = sf.get_buf_dir() }) end },
    { '<leader>B', require('snacks.git').blame_line },

    -- DAP
    { '<F5>', function() require('dap').continue() end },
    { '<F6>', function() require('dapui').close() end },
    { '<F9>', function() require('persistent-breakpoints.api').toggle_breakpoint() end },
    { '<F10>', function() require('dap').step_over() end },
    { '<F11>', function() require('dap').step_into() end },
    { '<M-e>', function() require('dapui').eval() end },
    { '<M-e>', function() require('dapui').eval() end },

    -- Grapple
    { '\'', function() harpoon:list():add() end },
    { '<leader>h', function() harpoon.ui:toggle_quick_menu(harpoon:list()) end },
    { '<M-;>', function() harpoon:list():select(1) end },
    { '<M-,>', function() harpoon:list():select(2) end },
    { '<M-.>', function() harpoon:list():select(3) end },
    { '<M-p>', function() harpoon:list():select(4) end },
    { '<M-y>', function() harpoon:list():select(5) end },
    { '<M-i>', function() harpoon:list():select(6) end },

})
