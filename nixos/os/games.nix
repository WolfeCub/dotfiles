_: {
  flake.nixosModules.games = {
    pkgs,
    inputs,
    ...
  }: {
    programs.gamemode.enable = true;
    users.users.wolfe.extraGroups = ["gamemode"];
    programs.steam = {
      enable = true;
      extraCompatPackages = [inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.proton-ge];
      gamescopeSession.enable = true;
    };

    environment.systemPackages = with pkgs; [
      heroic
      bottles
    ];
  };
}
