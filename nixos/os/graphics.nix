_: {
  flake.nixosModules.graphics = {
    pkgs,
    config,
    lib,
    ...
  }: {
    hardware.graphics = {
      enable = true;
      enable32Bit = true; # Needed for Steam/WINE
    };

    services.xserver.videoDrivers = ["nvidia"];

    hardware.nvidia = {
      open = true;
      # https://github.com/xddxdd/nix-cachyos-kernel/issues/101
      package = let
        base = config.boot.kernelPackages.nvidiaPackages.new_feature;
      in
        base
        // {
          open = base.open.overrideAttrs (old: {
            postPatch =
              (lib.optionalString (old.postPatch or null != null) old.postPatch)
              + ''
                substituteInPlace kernel-open/common/inc/nv-linux.h \
                  --replace-fail \
                    'struct gpio_chip *chip = gpio_device_get_chip(gdev);' \
                    'struct gpio_chip *chip = gpio_device_get_chip((struct gpio_device *)gdev);'
              '';
          });
        };

      modesetting.enable = true;

      powerManagement.enable = true;
      # powerManagement.finegrained = false;
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

    environment.systemPackages = [
      pkgs.nvtopPackages.nvidia
    ];
  };
}
