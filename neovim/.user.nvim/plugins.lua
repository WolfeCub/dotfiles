local f = require('functions')

return {

{
    'wtfox/jellybeans.nvim',
    priority = 1000,
    lazy = false,
    opts = {
        italics = false
    },
},

{
    'scottmckendry/cyberdream.nvim',
    priority = 1000,
    lazy = false,
    opts = {
        transparent = true,
    },
},

{
    'afonsofrancof/OSC11.nvim',
    priority = 1000,
    lazy = false,
    opts = {
        on_dark = function()
            vim.cmd('colorscheme jellybeans')
        end,
        on_light = function()
            vim.cmd('colorscheme cyberdream-light')
        end,
    },
    config = function (_, opts)
        if vim.o.background == 'dark' then
            opts.on_dark()
        else
            opts.on_light()
        end
        require('osc11').setup(opts)
    end
},

{
    'huggingface/llm.nvim',
    event = 'InsertEnter',
    config = function(_, opts)
        if f.tcp_port_open('host.orb.internal', 8080, 100) then
            require('llm').setup(opts)
        end
    end,
    opts = {
        backend = 'llamacpp',
        model = 'unsloth/Qwen3-Coder-30B-A3B-Instruct-GGUF:UD-Q4_K_XL',
        url = 'http://host.orb.internal:8080',
        -- must be non-empty or it gets JSON-encoded as [] and llm-ls rejects it;
        -- max_tokens keeps completions from decoding until they fill the context
        request_body = {
            temperature = 0.2,
            top_p = 0.95,
            max_tokens = 128,
        },
        -- Qwen's FIM tokens are pipe-wrapped, not llm.nvim's StarCoder-style defaults
        fim = {
            enabled = true,
            prefix = '<|fim_prefix|>',
            middle = '<|fim_middle|>',
            suffix = '<|fim_suffix|>',
        },
        lsp = {
            bin_path = vim.api.nvim_call_function("stdpath", { "data" }) .. "/mason/bin/llm-ls",
        },
    },
},

}
