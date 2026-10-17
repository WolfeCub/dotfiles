{inputs, ...}: {
  perSystem = {
    pkgs,
    lib,
    ...
  }: {
    packages.proton-ge = pkgs.proton-ge-bin.overrideAttrs (finalAttrs: _: {
      # The release's `version` file is "<timestamp> GE-ProtonX-Y"
      version = lib.last (lib.splitString " " (lib.trim (builtins.readFile "${inputs.proton-ge}/version")));
      src = inputs.proton-ge;

      # Keep the internal name fixed so per-game selections survive updates,
      # but show the version in Steam's dropdown.
      preFixup = ''
        substituteInPlace "$steamcompattool/compatibilitytool.vdf" \
          --replace-fail '"${finalAttrs.version}-x86_64" //' '"GE-Proton" //' \
          --replace-fail '"display_name" "${finalAttrs.version}-x86_64"' '"display_name" "GE Proton ${builtins.replaceStrings ["-"] ["."] (lib.removePrefix "GE-Proton" finalAttrs.version)}"'
      '';
    });
  };
}
