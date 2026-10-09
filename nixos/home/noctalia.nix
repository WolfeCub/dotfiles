{self, ...}: {
  flake.homeModules.noctalia = {
    pkgs,
    inputs,
    ...
  }: let
    noctalia-pkg = self.packages.${pkgs.stdenv.hostPlatform.system}.noctalia;
  in {
    imports = [
      inputs.noctalia.homeModules.default
    ];

    programs.noctalia = {
      enable = true;
      package = noctalia-pkg;

      settings = {
        theme = {
          mode = "dark";
          source = "wallpaper";
          wallpaper_scheme = "m3-content";
          templates.builtin_ids = ["umbriel"];
        };

        wallpaper = {
          directory = "~/Pictures/wallpapers/";
        };

        widget.gap = {
          type = "spacer";
          length = 40;
        };

        widget.cpu = {
          type = "sysmon";
          stat = "cpu_usage";
          visualization = "graph";
          show_value = true;
        };
        widget.ram = {
          type = "sysmon";
          stat = "ram_used";
          visualization = "graph";
          show_value = true;
        };
        widget.gpu = {
          type = "sysmon";
          stat = "gpu_vram";
          visualization = "graph";
          show_value = true;
        };

        widget.workspaces = {
          label_source = "name";
          hide_when_empty = true;
        };

        bar.default = {
          margin_edge = 0;
          margin_ends = 0;
          radius = 0;
          shadow = false;
          capsule = true;

          start = ["workspaces"];
          center = ["clock"];
          end = [
            "media"
            "tray"
            "gap"
            "cpu"
            "ram"
            "gpu"
            "gap"
            "screenshot"
            "clipboard"
            "network"
            "bluetooth"
            "volume"
            "brightness"
            "notifications"
          ];
        };

        # discord plays its own notification sound
        notification.filter.discord = {
          match = "vesktop";
          play_sound = false;
        };

        location.auto_locate = true;
        shell.telemetry_enabled = false;

        dock = {
          enabled = true;
          auto_hide = true;
          reserve_space = false;
        };

        system.monitor = {
          gpu_poll_seconds = 5.0;
        };
      };
    };

    # xdg.configFile."xdg-desktop-portal-wlr/config".text = ''
    #   [screencast]
    #   chooser_type=dmenu
    #   chooser_cmd=${noctalia-pkg}/bin/noctalia dmenu
    # '';
  };
}
