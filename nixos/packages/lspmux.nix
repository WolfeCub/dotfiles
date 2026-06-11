{inputs, ...}: {
  # Buildable / runnable as `.#lspmux` (e.g. nix run github:WolfeCub/dotfiles#lspmux).
  perSystem = {pkgs, ...}: {
    packages.lspmux = pkgs.callPackage (inputs.lspmux + "/package.nix") {};
  };
}
