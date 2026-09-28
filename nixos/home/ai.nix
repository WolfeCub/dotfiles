_: {
  flake.homeModules.ai = {pkgs, ...}: {
    home.packages = with pkgs; [
      unstable.claude-code
      unstable.opencode
    ];
  };
}
