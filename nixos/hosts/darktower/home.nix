{inputs, ...}: {
  flake.homeModules.darktower = {pkgs, ...}: {
    imports =
      [inputs.nixcord.homeModules.nixcord]
      ++ (with inputs.self.homeModules; [
        shell
        firefox
        neovim
        rio
        fonts
        mangoConfig
        noctalia
        ghostty
        polkit
      ]);

    home.packages = let
      orca-slicer-wrapped = pkgs.symlinkJoin {
        name = "orca-slicer";
        paths = [pkgs.unstable.orca-slicer];
        buildInputs = [pkgs.makeWrapper];
        postBuild = ''
          wrapProgram $out/bin/orca-slicer \
            --suffix XDG_DATA_DIRS : "${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}"
        '';
      };
    in
      with pkgs; [
        rio
        orca-slicer-wrapped
      ];

    programs.nixcord = {
      enable = true;
      discord.vencord.enable = true;
      # discord.krisp.enable = true;

      vesktop.enable = true;
    };
  };
}
