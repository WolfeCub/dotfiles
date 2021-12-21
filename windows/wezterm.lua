local wezterm = require 'wezterm';

return {
    font = wezterm.font("FiraCode NF"),
    color_scheme = "Builtin Dark",
    window_decorations = "RESIZE",
    window_padding = {
        left = 0,
        right = 0,
        top = 0,
        bottom = 0,
    },

    exit_behavior = "Close",
    default_prog = {"wsl", "-u", "wolfe"},
    default_cwd = "\\\\wsl$\\Arch\\home\\wolfe",

    leader = { key="h", mods="CTRL", timeout_milliseconds=1000 },
    keys = {
        -- New tab launcher
        {key="t", mods="CTRL|ALT", action="ShowLauncher"},

        -- Tmux like splitting
        {key="5", mods="LEADER|SHIFT", action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
        {key="\"", mods="LEADER|SHIFT", action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
        {key="h", mods="ALT", action=wezterm.action{ActivatePaneDirection="Left"}},
        {key="l", mods="ALT", action=wezterm.action{ActivatePaneDirection="Right"}},
        {key="k", mods="ALT", action=wezterm.action{ActivatePaneDirection="Up"}},
        {key="j", mods="ALT", action=wezterm.action{ActivatePaneDirection="Down"}},

        -- Clear
        {key="^", mods="CTRL", action="DisableDefaultAssignment"},
    },

    launch_menu = {
        { 
            label = "Git Bash",
            args = {"C:\\Program Files\\Git\\bin\\bash.exe"},
        },
    }
}
