{...}: {
  programs.nh = {
    enable = true;
    clean.enable = true;
    clean.extraArgs = "--keep-since 4d --keep 3";
  };

  environment.variables = {
      NH_FLAKE = "/home/wolfe/dotfiles/nixos";
  };
}
