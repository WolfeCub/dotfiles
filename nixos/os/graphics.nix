_: {
  flake.nixosModules.graphics = {config, ...}: {
    hardware.graphics = {
      enable = true;
      enable32Bit = true; # Needed for Steam/WINE
    };

    services.xserver.videoDrivers = ["nvidia"];

    hardware.nvidia = {
      open = true;
      package = config.boot.kernelPackages.nvidiaPackages.new_feature;

      modesetting.enable = true;

      powerManagement.enable = false;
      powerManagement.finegrained = false;
    };

    boot.kernelParams = [
      "nvidia-drm.modeset=1"
      "nvidia-drm.fbdev=1" # needed on driver 545+
    ];

    environment.sessionVariables = {
      # for hyprland with nvidia gpu" = " ref https://wiki.hyprland.org/Nvidia/
      "LIBVA_DRIVER_NAME" = "nvidia";
      "__GLX_VENDOR_LIBRARY_NAME" = "nvidia";
      # VA-API hardware video acceleration
      "NVD_BACKEND" = "direct";

      "GBM_BACKEND" = "nvidia-drm";
    };
  };
}
