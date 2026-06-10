{inputs, ...}: {
  flake.homeModules.vital-nix-vm = {
    imports = with inputs.self.homeModules; [
      shell
      neovim
    ];
  };
}
