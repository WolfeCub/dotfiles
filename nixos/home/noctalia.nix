_: {
  flake.homeModules.noctalia = {
    pkgs,
    inputs,
    ...
  }: let
    noctalia-pkg = inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default;
  in {
    imports = [
      inputs.noctalia.homeModules.default
    ];

    # home.packages = [
    #   inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default
    # ];

    programs.noctalia = {
      enable = true;
      package = noctalia-pkg.override {cudaSupport = true;};

      settings = {
        theme = {
          builtin = "Ayu";
        };

        widget.gap = {
          type = "spacer";
          length = 40;
        };

        widget.cpu = {
          type = "sysmon";
          stat = "cpu_usage";
          display = "graph";
        };
        widget.ram = {
          type = "sysmon";
          stat = "ram_used";
          display = "graph";
        };
        widget.gpu = {
          type = "sysmon";
          stat = "gpu_vram";
          display = "graph";
        };

        bar.default = {
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
          margin_edge = 0;
          margin_ends = 0;
          radius = 0;
          shadow = false;
          start = ["workspaces"];
        };

        location.auto_locate = true;
        shell.telemetry_enabled = false;

        dock = {
          enabled = true;
          auto_hide = true;
        };

        system.monitor = {
          gpu_poll_seconds = 5.0;
        };
      };
    };
  };
}
