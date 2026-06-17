_: {
  flake.nixosModules.hyprland = {
    pkgs,
    inputs,
    ...
  }: {
    imports = [
      inputs.hyprland.nixosModules.default
    ];

    programs.hyprland = {
      enable = true;

      settings = let
        m = {
          main = "DP-3";
          left = "HDMI-A-1";
          vert = "DP-2";
        };
      in {
        general = {
          gaps_in = 0;
          gaps_out = 0;
          border_size = 2;
          allow_tearing = true; # Reduces input lag for gaming
        };

        input = {
          repeat_delay = 230;
          repeat_rate = 40;
        };

        misc = {
          force_default_wallpaper = 0;
          disable_hyprland_logo = true;
          disable_splash_rendering = true;
        };

        exec-once = [
          "noctalia"
        ];

        monitor = [
          # Middle Monitor (Primary) - vrr 2 supports variable refresh rate for fullscreen apps
          "${m.main}, preferred, 0x0, 1.25, vrr, 2"
          # Left Monitor
          "${m.left}, preferred, -3072x0, 1.25"
          # Right Monitor (Vertical)
          "${m.vert}, preferred, 3072x0, 1, transform, 1"
        ];

        workspace = [
          "1, monitor:${m.main}, default:true"
          "2, monitor:${m.main}"
          "3, monitor:${m.main}"
          "4, monitor:${m.main}"
          "5, monitor:${m.main}"

          "name:Side, monitor:${m.left}, default:true"
          "name:Chat, monitor:${m.vert}, default:true"
        ];

        "$mod" = "ALT";
        bind = [
          "$mod, RETURN, exec, noctalia msg panel-toggle launcher"
          "$mod, T, exec, ghostty"
          "$mod, B, exec, firefox-devedition"

          # Workspace bindings
          "$mod SHIFT, 7, workspace, 1"
          "$mod, bracketleft, workspace, 2"
          "$mod SHIFT, bracketleft, workspace, 3"
          "$mod SHIFT, bracketright, workspace, 4"
          "$mod SHIFT, 9, workspace, 5"
          "$mod CTRL SHIFT, 7, movetoworkspace, 1"
          "$mod CTRL, bracketleft, movetoworkspace, 2"
          "$mod CTRL SHIFT, bracketleft, movetoworkspace, 3"
          "$mod CTRL SHIFT, bracketright, movetoworkspace, 4"
          "$mod CTRL SHIFT, 9, movetoworkspace, 5"

          # Window bindings
          "$mod, Q, killactive"
          "$mod, SHIFT E, exit"
          "$mod, F, fullscreen"
          "$mod, SPACE, togglefloating"

          # Focus / Movement
          "$mod, h, movefocus, l"
          "$mod, l, movefocus, r"
          "$mod, j, movefocus, d"
          "$mod, k, movefocus, u"

          "$mod CTRL, h, movewindow, l"
          "$mod CTRL, l, movewindow, r"
          "$mod CTRL, j, movewindow, d"
          "$mod CTRL, k, movewindow, u"

          # Help
          "$mod, slash, exec, hypr-keybinds"
        ];

        bindm = [
          "$mod, mouse:272, movewindow"
          "$mod, mouse:273, resizewindow"
        ];

        windowrule = [
          # Auto fullscreen discord on the vert monitor
          "match:class ^([Dd]iscord|[Vv]esktop|com\\.discordapp\\.Discord)$, workspace name:Chat, fullscreen on"
        ];
      };
    };
  };
}
