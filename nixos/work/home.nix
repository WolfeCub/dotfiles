{
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ../shared/shellEnv.nix
    ../shared/neovim.nix
  ];
}
