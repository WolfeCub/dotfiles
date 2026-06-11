_: {
  flake.nixosModules.games = {pkgs, ...}: {
    programs.gamemode.enable = true;
    programs.steam = {
      enable = true;
      extraCompatPackages = [pkgs.proton-ge-bin];
    };
  };
}
