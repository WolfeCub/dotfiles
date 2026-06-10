_: {
  flake.nixosModules.games = {...}: {
    programs.gamemode.enable = true;
    programs.steam = {
      enable = true;
    };
  };
}
