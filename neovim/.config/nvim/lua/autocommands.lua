require('legendary').bind_autocmds({
    -- Disable line numbers in terminal buffers
    { 'TermOpen', ':setlocal nonumber norelativenumber' },
})
