_: {
  flake.nixosModules.audio = {pkgs, ...}: {
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    hardware = {
      bluetooth.enable = true;
      alsa.enablePersistence = true;
    };

    environment.systemPackages = with pkgs; [
      alsa-utils
    ];

    # systemd.services.disable-yeti-loopback = {
    #   description = "Disable Blue Yeti mic loopback";
    #   wantedBy = ["multi-user.target"];
    #   after = ["pipewire.service"];
    #   serviceConfig = {
    #     Type = "oneshot";
    #     ExecStart = "${pkgs.alsa-utils}/bin/amixer -D hw:Microphones set 'Mic' playback mute";
    #   };
    # };
  };
}
