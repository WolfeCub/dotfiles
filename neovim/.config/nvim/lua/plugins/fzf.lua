local actions = require('fzf-lua.actions');

require('fzf-lua').setup({
    global_resume = true,
    global_resume_query = true,
    winopts = {
        height = 0.3,
        width = 1,
        row = 1,
    },
    files = {
        previewer = false,
    },
    git = {
        files = {
            cmd = 'git ls-files --exclude-standard --cached --others',
            previewer = false,
        },
    },
    buffers = {
        previewer = false,
    },
    file_icon_padding = ' ',
});
