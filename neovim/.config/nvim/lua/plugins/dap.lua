local dap = require('dap');
local dapui = require('dapui');

function get_dll_paths()
    local cwd = vim.fn.getcwd();
    local sln_file = vim.fn.globpath(cwd, '*.sln');

    local matcher = "";
    if sln_file == "" then
        local dir_name = vim.fn.fnamemodify(cwd, ':t');
        matcher = string.format('**/bin/Debug/*/%s.dll', dir_name);
    else
        local base_name = vim.fn.fnamemodify(sln_file, ':t:r');
        matcher = string.format('**/bin/Debug/*/%s*.dll', base_name);
    end

    local globs = vim.fn.globpath(cwd, matcher);
    local files = vim.split(globs, "\n");

    return require('dap.ui').pick_one_sync(files, 'Path to dll:', function(item) return item end);
end

dap.adapters.coreclr = {
    type = 'executable',
    command = 'netcoredbg',
    args = { '--interpreter=vscode' }
};
dap.configurations.cs = {
    {
        type = "coreclr",
        name = "launch - netcoredbg",
        request = "launch",
        program = get_dll_paths,
    },
};

dap.adapters.lldb = {
    type = 'executable',
    command = '/usr/sbin/lldb-vscode',
    name = 'lldb'
};
dap.configurations.rust = {
    {
        name = 'Launch',
        type = 'lldb',
        request = 'launch',
        program = function()
            return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
        end,
        cwd = '${workspaceFolder}',
        stopOnEntry = false,
        args = {},
    }
};

vim.fn.sign_define('DapBreakpoint', { text = '🛑', texthl = '', linehl = '', numhl = '' })

dapui.setup({
    icons = { expanded = "▾", collapsed = "▸" },
    mappings = {
        -- Use a table to apply multiple mappings
        expand = { "<CR>", "<2-LeftMouse>" },
        open = "o",
        remove = "d",
        edit = "e",
        repl = "r",
        toggle = "t",
    },
    -- Expand lines larger than the window
    -- Requires >= 0.7
    expand_lines = vim.fn.has("nvim-0.7"),
    -- Layouts define sections of the screen to place windows.
    -- The position can be "left", "right", "top" or "bottom".
    -- The size specifies the height/width depending on position. It can be an Int
    -- or a Float. Integer specifies height/width directly (i.e. 20 lines/columns) while
    -- Float value specifies percentage (i.e. 0.3 - 30% of available lines/columns)
    -- Elements are the elements shown in the layout (in order).
    -- Layouts are opened in order so that earlier layouts take priority in window sizing.
    layouts = {
        {
            elements = {
                -- Elements can be strings or table with id and size keys.
                { id = "scopes", size = 0.25 },
                "breakpoints",
                "stacks",
                "watches",
            },
            size = .2,
            position = "left",
        },
        {
            elements = {
                "repl",
            },
            size = 0.25, -- 25% of total lines
            position = "bottom",
        },
    },
    floating = {
        max_height = nil, -- These can be integers or a float between 0 and 1.
        max_width = nil, -- Floats will be treated as percentage of your screen.
        border = "single", -- Border style. Can be "single", "double" or "rounded"
        mappings = {
            close = { "q", "<Esc>" },
        },
    },
    windows = { indent = 1 },
    render = {
        max_type_length = nil, -- Can be integer or nil.
    }
});

-- Auto open dapui on debug
dap.listeners.after.event_initialized["dapui_config"] = function()
    dapui.open();
end
dap.listeners.before.event_terminated["dapui_config"] = function()
    dapui.close();
end
dap.listeners.before.event_exited["dapui_config"] = function()
    dapui.close();
end


require('persistent-breakpoints').setup({});
-- automatically load breakpoints when a file is loaded into the buffer.
vim.api.nvim_create_autocmd({"BufReadPost"},{ callback = require('persistent-breakpoints.api').load_breakpoints });

require('legendary').bind_keymaps({
    { '<F5>', '<cmd>lua require("dap").continue()<cr>' },
    { '<F9>', '<cmd>lua require("persistent-breakpoints.api").toggle_breakpoint()<cr>' },
    { '<F10>', '<cmd>lua require("dap").step_over()<cr>' },
    { '<F11>', '<cmd>lua require("dap").step_into()<cr>' },
    { '<M-e>', '<cmd>lua require("dapui").eval()<cr>' },
    { '<M-e>', '<cmd>lua require("dapui").eval()<cr>' },
})
