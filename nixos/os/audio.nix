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
  };
}
