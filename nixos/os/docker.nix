_: {
  flake.nixosModules.docker = {...}: {
    virtualisation.docker = {
      enable = true;
    };
  };
}
