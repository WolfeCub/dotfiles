_: {
  flake.nixosModules.vital-nix-vm = {...}: {
    nix = {
      # Remote builder for swops ./push.sh
      distributedBuilds = true;
      settings.trusted-users = ["wolfe"]; # local username
    };

    environment.sessionVariables = {
      HYDRA_SSH_USER = "jwolfe";
      HYDRA_SSH_IDENTITY = "/home/wolfe/.ssh/id_ed25519";
      NIX_KEY = "/home/wolfe/nix-keys/wolfe-nix.private.pem";
    };

    # Root SSH config so nix-daemon can reach remote builders via the user's agent
    environment.etc."ssh/root-ssh-config".text = ''
      Host *
        User jwolfe
        IdentityAgent /run/user/1000/ssh-agent
    '';

    system.activationScripts.rootSshConfig = ''
      mkdir -p -m 0700 /root/.ssh
      ln -sf /etc/ssh/root-ssh-config /root/.ssh/config
    '';

    programs.ssh = {
      startAgent = true;
      extraConfig = ''
        Host nixbuild.vital.company
          Port 2222
          PubkeyAcceptedKeyTypes ssh-ed25519
          ServerAliveInterval 60
          IPQoS throughput
          IdentityFile /home/wolfe/.ssh/id_ed25519
      '';
      knownHosts = {
        nixbuild = {
          hostNames = ["nixbuild.vital.company"];
          publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ+jBIzENqxs/p7dFEAIjG8e5TT+A9Gvhi1cKNdIJ9vW";
        };
      };
    };
  };
}
