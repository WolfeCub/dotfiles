{
  pkgs,
  inputs,
  ...
}: {
  imports = [
    inputs.noctalia.homeModules.default
  ];

  home.packages = [
    inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default
  ];

  programs.noctalia = {
    enable = true;

    settings = {
      widget.gap = {
        type = "spacer";
      };

      bar.default = {
        center = ["clock" "gap" "taskbar"];
        end = [
          "media"
          "tray"
          "gap"
          "screenshot"
          "clipboard"
          "network"
          "bluetooth"
          "volume"
          "notifications"
          "brightness"
          "control-center"
          "session"
        ];
        margin_edge = 0;
        margin_ends = 0;
        radius = 0;
        shadow = false;
        start = ["workspaces"];
      };

      location.auto_locate = true;

      shell.telemetry_enabled = false;

      theme = {
        builtin = "Ayu";
      };
    };
  };
}
