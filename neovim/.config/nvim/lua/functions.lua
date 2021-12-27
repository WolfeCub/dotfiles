function get_current_project_root()
    local root, method = require("project_nvim.project").get_project_root();
    return root
end


function telescope_find_files_dwim()
    local opts = {}
    local ok = pcall(require"telescope.builtin".git_files, opts)
    if not ok then require"telescope.builtin".find_files(opts) end
end

function get_small_ivy(opts)
  opts = opts or {}

  local theme_opts = {
    theme = "ivy",

    sorting_strategy = "ascending",

    layout_strategy = "bottom_pane",
    layout_config = {
      height = 13,
    },

    border = true,
    borderchars = {
      prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
      results = { " " },
      preview = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
    },
  }
  if opts.layout_config and opts.layout_config.prompt_position == "bottom" then
    theme_opts.borderchars = {
      prompt = { " ", " ", "─", " ", " ", " ", "─", "─" },
      results = { "─", " ", " ", " ", "─", "─", " ", " " },
      preview = { "─", " ", "─", "│", "┬", "─", "─", "╰" },
    }
  end

  return vim.tbl_deep_extend("force", theme_opts, opts)
end

function require_viml(vimlConfigPath)
    local path = vim.fn.stdpath('config')
    vim.cmd(string.format('source %s/viml/%s', path, vimlConfigPath))
end