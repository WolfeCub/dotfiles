{...}: {
  flake.nixosModules.vital-nix-orb = {...}: {
    systemd.suppressedSystemUnits = [
      "sys-kernel-debug.mount"
    ];
  };
}
