_: {
  flake.homeModules.rio = {
    pkgs,
    dfRoot,
    ...
  }: {
    home.packages = with pkgs; [
      rio
      nerd-fonts.fira-code
    ];

    xdg.configFile."rio".source = dfRoot + /rio/.config/rio;
  };
}
