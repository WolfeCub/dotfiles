_: {
  flake.nixosModules.graphics = {...}: {
    hardware.graphics.enable = true;
    services.xserver.videoDrivers = ["nvidia"];
    hardware.nvidia.open = true;
  };
}
