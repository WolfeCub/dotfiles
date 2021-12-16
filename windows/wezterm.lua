local wezterm = require 'wezterm';

return {
  font = wezterm.font("Fira Code"),
  color_scheme = "Builtin Dark",
  exit_behavior = "Close",

  default_prog = {"wsl", "-u", "wolfe"},

  leader = { key="h", mods="CTRL", timeout_milliseconds=1000 },
  keys = {
      {key="5", mods="LEADER|SHIFT", action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
      {key="\"", mods="LEADER|SHIFT", action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
      {key="h", mods="ALT", action=wezterm.action{ActivatePaneDirection="Left"}},
      {key="l", mods="ALT", action=wezterm.action{ActivatePaneDirection="Right"}},
      {key="k", mods="ALT", action=wezterm.action{ActivatePaneDirection="Up"}},
      {key="j", mods="ALT", action=wezterm.action{ActivatePaneDirection="Down"}},
  }
}
