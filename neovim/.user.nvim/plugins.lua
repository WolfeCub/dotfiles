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
    event = { 'BufReadPost', 'BufNewFile' },
    config = function(_, opts)
        if f.tcp_port_open('host.orb.internal', 8080, 100) then
            require('llm').setup(opts)
        end
    end,
    opts = {
        -- must be 'openai', not 'llamacpp': llama-swap routes on the "model" field,
        -- and only the openai backend sends one
        backend = 'openai',
        model = 'qwen3-coder-30b',
        url = 'http://host.orb.internal:8080',
        -- must be non-empty or it gets JSON-encoded as [] and llm-ls rejects it;
        request_body = {
            temperature = 0.2,
            top_p = 0.95,
            -- max_tokens keeps completions from decoding until they fill the context
            max_tokens = 128,
        },
        fim = {
            enabled = true,
            -- Qwen's style FIM tokens
            prefix = '<|fim_prefix|>',
            middle = '<|fim_middle|>',
            suffix = '<|fim_suffix|>',
        },
        lsp = {
            bin_path = vim.api.nvim_call_function("stdpath", { "data" }) .. "/mason/bin/llm-ls",
        },
    },
},

{
    'kndndrj/nvim-dbee',
    dependencies = {
        'MunifTanjim/nui.nvim',
    },
    build = function()
        require('dbee').install()
    end,
    config = function()
        require('dbee').setup()
    end,
},

}
