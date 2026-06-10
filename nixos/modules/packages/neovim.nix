{inputs, ...}: {
  # Our wrapped neovim, buildable / runnable as `.#neovim`. The config lives in
  # the dotfiles repo (dfRoot), one level above this flake.
  perSystem = {
    pkgs,
    lib,
    dfRoot,
    ...
  }: {
    packages.neovim = let
      userNvim = lib.cleanSource (dfRoot + /neovim/.user.nvim);
    in
      inputs.nix-wrapper-modules.wrappers.neovim.wrap {
        inherit pkgs;

        # Base neovim. Wrap the unwrapped derivation so we don't double-wrap.
        package = pkgs.unstable.neovim-unwrapped;

        # Interpolate to coerce the cleanSource attrset to its store-path string;
        # passing it raw makes the wrapper emit a lua table, not a path.
        settings.config_directory = "${lib.cleanSource (dfRoot + /neovim/.nvim-shared)}";

        envDefault.MSS_NEOVIM_USER_DIR = "${userNvim}";
      };
  };
}
