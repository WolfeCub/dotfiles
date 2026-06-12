_: {
  flake.nixosModules.ly = {...}: {
    services.displayManager.ly = {
      enable = true;
    };
  };
}
