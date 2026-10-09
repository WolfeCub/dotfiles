{inputs, ...}: {
  flake.nixosModules.umbriel = {...}: {
    imports = [
      inputs.umbriel.nixosModules.default
    ];

    programs.umbriel.enable = true;

    nix.settings = {
      extra-substituters = ["https://umbriel.cachix.org"];
      extra-trusted-public-keys = ["umbriel.cachix.org-1:JfNq/2yg2S6D6z4Z2dVSZrZlDPQTKtexB6GAVLD98nw="];
    };
  };

  flake.homeModules.umbrielConfig = {pkgs, ...}: {
    imports = [
      inputs.umbriel.homeModules.default
    ];

    home.packages = [
      pkgs.playerctl
    ];

    programs.umbriel = let
      monitors = {
        primary = "DP-3";
        secondary = "HDMI-A-1";
        vertical = "DP-2";
      };
    in {
      enable = true;

      settings = {
        # Colors generated from the wallpaper by noctalia's umbriel template
        include.optional.files = ["noctalia.toml"];

        general = {
          mod_key = "Alt";
          autostart = ["noctalia"];
        };

        input = {
          keyboard = {
            repeat_delay = 230;
            repeat_rate = 40;
          };
          focus.follows_mouse = true;
        };

        output = {
          # Left monitor
          ${monitors.secondary} = {
            scale = 1.25;
            position = [0 0];
            tearing = true;
            workspace_axis = "horizontal";
          };
          # Middle monitor (primary)
          ${monitors.primary} = {
            scale = 1.25;
            position = [3072 0];
            tearing = true;
            workspace_axis = "horizontal";
            focus_at_startup = true;
            workspaces = 5;
          };
          # Right monitor (vertical)
          ${monitors.vertical} = {
            scale = 1.0;
            position = [6144 0];
            transform = "90";
            tearing = true;
            workspace_axis = "horizontal";
          };
        };

        layout = {
          mode = "dwindle";
          gap = 0;
          scrolling.default_extent_fraction = 1.0;
        };

        appearance = {
          border_width = 2;
          corner_radius = 0;
          tab_bar = {
            height = 25;
            font = "monospace Bold 11";
            hide_when_single = true;
          };
        };

        window_rule = let
          discord = "^([Dd]iscord|[Vv]esktop|com[.]discordapp[.]Discord)$";
        in [
          # Reduces input lag for gaming. Only fullscreen windows can tear
          {tearing = true;}
          # Auto fullscreen Discord on the vertical monitor
          {
            match = {
              app_id = discord;
              title = "^([(][0-9]+[)] )?Discord( [|]|$)";
            };
            default_output = monitors.vertical;
            default_fullscreen = true;
          }
          # Popouts open titled "Discord Popout" before renaming themselves
          {
            match = {
              app_id = discord;
              title = "^Discord Popout$";
            };
            default_output = monitors.primary;
          }
        ];

        keybinds = {
          "Mod+Return" = "spawn:noctalia msg panel-toggle launcher";
          "Mod+T" = "spawn:ghostty";
          "Mod+B" = "spawn:firefox-devedition";
          "Mod+slash" = "spawn:vesktop-mute";

          # Workspace switching
          "Mod+Shift+7" = "workspace-switch:1";
          "Mod+bracketleft" = "workspace-switch:2";
          "Mod+Shift+bracketleft" = "workspace-switch:3";
          "Mod+Shift+bracketright" = "workspace-switch:4";
          "Mod+Shift+9" = "workspace-switch:5";

          # Move window to workspace
          "Mod+Ctrl+Shift+7" = "window-move-to-workspace:1";
          "Mod+Ctrl+bracketleft" = "window-move-to-workspace:2";
          "Mod+Ctrl+Shift+bracketleft" = "window-move-to-workspace:3";
          "Mod+Ctrl+Shift+bracketright" = "window-move-to-workspace:4";
          "Mod+Ctrl+Shift+9" = "window-move-to-workspace:5";

          # Window bindings
          "Mod+Q" = "window-close";
          "Mod+Shift+E" = "session-quit:skip-confirmation";
          "Mod+F" = "window-toggle-fullscreen";
          "Mod+Space" = "window-toggle-floating";
          "Mod+M" = "workspace-set-layout:scrolling";
          "Mod+D" = "workspace-set-layout:dwindle";
          "Mod+Tab" = "window-focus-next";
          "Mod+Shift+Tab" = "window-focus-previous";
          "Mod+O" = "overview-toggle";

          # Focus / Movement
          "Mod+H" = "window-focus-or-output-left";
          "Mod+L" = "window-focus-or-output-right";
          "Mod+J" = "window-focus-or-output-down";
          "Mod+K" = "window-focus-or-output-up";

          "Mod+Ctrl+H" = "window-swap-left";
          "Mod+Ctrl+L" = "window-swap-right";
          "Mod+Ctrl+J" = "window-swap-down";
          "Mod+Ctrl+K" = "window-swap-up";
          "Mod+Ctrl+Shift+H" = "window-move-to-output-left";
          "Mod+Ctrl+Shift+L" = "window-move-to-output-right";
          "Mod+Ctrl+Shift+J" = "window-move-to-output-down";
          "Mod+Ctrl+Shift+K" = "window-move-to-output-up";

          # Super binds
          "Mod+Ctrl+R" = "config-reload";
          "Super+Shift+S" = "spawn:noctalia msg screenshot-region";

          # Media keys
          "XF86AudioRaiseVolume" = "spawn:wpctl set-volume @DEFAULT_AUDIO_SINK@ 2%+ -l 1.0";
          "XF86AudioLowerVolume" = "spawn:wpctl set-volume @DEFAULT_AUDIO_SINK@ 2%-";
          "XF86AudioMute" = "spawn:wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
          "XF86AudioMicMute" = "spawn:wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle";
          "XF86AudioPlay" = "spawn:playerctl play-pause";
          "XF86AudioStop" = "spawn:playerctl stop";
          "XF86AudioPrev" = "spawn:playerctl previous";
          "XF86AudioNext" = "spawn:playerctl next";
        };
      };
    };
  };
}
