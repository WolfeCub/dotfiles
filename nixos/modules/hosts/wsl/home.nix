{inputs, ...}: {
  flake.homeModules.nixos = {
    imports = with inputs.self.homeModules; [
      shell
      neovim
    ];
  };
}
