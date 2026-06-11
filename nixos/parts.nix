{inputs, ...}: let
  # The dotfiles repo root, one level above this flake. Threaded to both the
  # nixos/home configs (via hosts.nix) and perSystem packages as `dfRoot`.
  dfRoot = ../.;
in {
  imports = [
    inputs.home-manager.flakeModules.home-manager
  ];

  systems = ["x86_64-linux" "aarch64-linux"];

  _module.args = {inherit dfRoot;};

  perSystem = {pkgs, ...}: {
    _module.args = {inherit dfRoot;};
    formatter = pkgs.alejandra;
  };
}
