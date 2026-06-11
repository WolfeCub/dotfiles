_: {
  flake.nixosModules.graphics = {...}: {
    hardware.graphics = {
      enable = true;
      enable32Bit = true; # Needed for Steam/WINE
    };

    services.xserver.videoDrivers = ["nvidia"];

    hardware.nvidia = {
      open = true;
      modesetting.enable = true;

      powerManagement.enable = false;
      powerManagement.finegrained = false;
    };

    boot.kernelParams = [
      "nvidia-drm.modeset=1"
      "nvidia-drm.fbdev=1" # needed on driver 545+
    ];
  };
}
