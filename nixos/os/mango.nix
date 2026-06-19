_: {
  flake.nixosModules.mango = {inputs, ...}: {
    imports = [
      inputs.mangowm.nixosModules.mango
    ];

    programs.mango.enable = true;
  };

  flake.homeModules.mangoConfig = {
    pkgs,
    inputs,
    ...
  }: let
    monitors = {
      primary = "DP-3";
      secondary = "HDMI-A-1";
      vertical = "DP-2";
    };
    mod = "ALT";
  in {
    imports = [
      inputs.mangowm.hmModules.mango
    ];

    home.packages = with pkgs; [
      playerctl
    ];

    wayland.windowManager.mango = {
      enable = true;

      autostart_sh = ''
        noctalia &
      '';

      settings = {
        gappih = 0;
        gappiv = 0;
        gappoh = 0;
        gappov = 0;
        borderpx = 2;
        # Reduces input lag for gaming. Global but 2 = fullscreen-only
        allow_tearing = 2;

        repeat_delay = 230;
        repeat_rate = 40;
        focus_cross_monitor = 1; # let focusdir h/l cross monitor edges
        exchange_cross_monitor = 1; # let exchange_client move windows across monitors

        monitorrule = [
          # Left monitor
          "name:${monitors.secondary},scale:1.25,x:0,y:0"
          # Middle monitor (primary), vrr for variable refresh rate
          "name:${monitors.primary},scale:1.25,x:3072,y:0,vrr:1"
          # Right monitor (vertical), rr:1 = 90deg rotation
          "name:${monitors.vertical},scale:1,x:6144,y:0,rr:1"
        ];

        tagrule = [
          "id:1,monitor_name:${monitors.primary},layout_name:dwindle"
          "id:2,monitor_name:${monitors.primary},layout_name:dwindle"
          "id:3,monitor_name:${monitors.primary},layout_name:dwindle"
          "id:4,monitor_name:${monitors.primary},layout_name:dwindle"
          "id:5,monitor_name:${monitors.primary},layout_name:dwindle"
          "id:1,monitor_name:${monitors.secondary},layout_name:dwindle"
          "id:1,monitor_name:${monitors.vertical},layout_name:dwindle"
        ];

        mousebind = [
          "${mod},btn_left,moveresize,curmove"
          "${mod},btn_right,moveresize,curresize"
        ];

        # Auto fullscreen Discord on the vertical monitor
        windowrule = [
          "appid:^([Dd]iscord|[Vv]esktop|com\\.discordapp\\.Discord)$,monitor:${monitors.vertical},isfullscreen:1"
        ];

        bind = [
          "${mod},Return,spawn,noctalia msg panel-toggle launcher"
          "${mod},t,spawn,ghostty"
          "${mod},b,spawn,firefox-devedition"

          # Tag switching
          "${mod}+SHIFT,7,view,1,0"
          "${mod},bracketleft,view,2,0"
          "${mod}+SHIFT,bracketleft,view,3,0"
          "${mod}+SHIFT,bracketright,view,4,0"
          "${mod}+SHIFT,9,view,5,0"

          # Move window to tag
          "${mod}+CTRL+SHIFT,7,tag,1,0"
          "${mod}+CTRL,bracketleft,tag,2,0"
          "${mod}+CTRL+SHIFT,bracketleft,tag,3,0"
          "${mod}+CTRL+SHIFT,bracketright,tag,4,0"
          "${mod}+CTRL+SHIFT,9,tag,5,0"

          # Window bindings
          "${mod},q,killclient,"
          "${mod}+SHIFT,e,quit"
          "${mod},f,togglefullscreen,"
          "${mod},space,togglefloating,"
          "${mod},o,toggleoverview,"

          # Focus / Movement
          "${mod},h,focusdir,left"
          "${mod},l,focusdir,right"
          "${mod},j,focusdir,down"
          "${mod},k,focusdir,up"

          "${mod}+CTRL,h,exchange_client,left"
          "${mod}+CTRL,l,exchange_client,right"
          "${mod}+CTRL,j,exchange_client,down"
          "${mod}+CTRL,k,exchange_client,up"

          # Media keys
          "none,XF86AudioRaiseVolume,spawn,wpctl set-volume @DEFAULT_AUDIO_SINK@ 2%+ -l 1.0"
          "none,XF86AudioLowerVolume,spawn,wpctl set-volume @DEFAULT_AUDIO_SINK@ 2%-"
          "none,XF86AudioMute,spawn,wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
          "none,XF86AudioMicMute,spawn,wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
          "none,XF86AudioPlay,spawn,playerctl play-pause"
          "none,XF86AudioPause,spawn,playerctl play-pause"
          "none,XF86AudioStop,spawn,playerctl stop"
          "none,XF86AudioPrev,spawn,playerctl previous"
          "none,XF86AudioNext,spawn,playerctl next"
        ];
      };
    };
  };
}
