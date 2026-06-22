{inputs, ...}: {
  flake.homeModules.vital-nix-orb = {
    imports = with inputs.self.homeModules; [
      shell
      neovim
    ];
  };
}
