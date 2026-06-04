{
  pkgs,
  lib,
  inputs,
  ...
}: let
  userNvim = lib.cleanSource ../neovim/.user.nvim;

  nvim-wrapped = inputs.nix-wrapper-modules.wrappers.neovim.wrap {
    inherit pkgs;

    # Base neovim. Wrap the unwrapped derivation so we don't double-wrap.
    package = pkgs.unstable.neovim-unwrapped;

    # Interpolate to coerce the cleanSource attrset to its store-path string;
    # passing it raw makes the wrapper emit a lua table, not a path.
    settings.config_directory = "${lib.cleanSource ../neovim/.nvim-shared}";

    envDefault.MSS_NEOVIM_USER_DIR = "${userNvim}";
  };
in {
  home.packages = [nvim-wrapped];
}
