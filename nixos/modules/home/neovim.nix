_: {
  flake.homeModules.neovim = {
    pkgs,
    inputs,
    ...
  }: {
    home.packages = [
      inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.neovim
    ];
  };
}
