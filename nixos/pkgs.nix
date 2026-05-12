{ pkgs, inputs }: {
  lspmux = pkgs.callPackage (inputs.lspmux + "/package.nix") {};
}
