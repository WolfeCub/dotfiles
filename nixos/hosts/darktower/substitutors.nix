_: {
  flake.nixosModules.darktower = {...}: {
    nix.settings = {
      extra-substituters = [
        "https://noctalia.cachix.org"
        "https://wolfe.cachix.org"
      ];
      extra-trusted-public-keys = [
        "noctalia.cachix.org-1:pCOR47nnMEo5thcxNDtzWpOxNFQsBRglJzxWPp3dkU4="
        "wolfe.cachix.org-1:gPSTXzXofRTEV1QdkyDIDEpA4sPTg98Mj5tKKcW0kpE="
      ];
    };
  };
}
