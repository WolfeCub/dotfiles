{inputs, ...}: {
  perSystem = {pkgs, ...}: {
    packages.noctalia = inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.cuda;
  };
}
