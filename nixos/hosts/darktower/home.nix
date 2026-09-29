{inputs, ...}: {
  flake.homeModules.darktower = {
    pkgs,
    dfRoot,
    ...
  }: {
    imports =
      [inputs.nixcord.homeModules.nixcord]
      ++ (with inputs.self.homeModules; [
        shell
        ai
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
        (writeShellScriptBin "vesktop-mute" ''
          dir="''${XDG_RUNTIME_DIR:?XDG_RUNTIME_DIR is not set}/vesktop-global-mute"
          mkdir -p -m 700 "$dir"
          touch "$dir/mute"
        '')
      ];

    programs.nixcord = {
      enable = true;
      discord.enable = false;
      vesktop.enable = true;

      # Global mute toggle, triggered by vesktop-mute
      userPlugins.GlobalMute = dfRoot + /vesktop/globalMute;
      extraConfig.plugins.GlobalMute.enable = true;
    };
  };
}
