{inputs, ...}: {
  flake.homeModules.darktower = {pkgs, ...}: {
    imports =
      [inputs.nixcord.homeModules.nixcord]
      ++ (with inputs.self.homeModules; [
        shell
        neovim
        rio
        fonts
        niriConfig
        mangoConfig
        noctalia
        ghostty
      ]);

    home.packages = with pkgs; [
      rio
      firefox-devedition
    ];

    programs.nixcord = {
      enable = true;
      discord.vencord.enable = true;
      discord.krisp.enable = true;
    };
  };
}
