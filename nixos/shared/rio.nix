{pkgs, ...}: {
  home.packages = with pkgs; [
    rio
    nerd-fonts.fira-code
  ];

  xdg.configFile."rio".source = ../../rio/.config/rio;
}
