_: {
  flake.homeModules.ai = {
    lib,
    pkgs,
    dfRoot,
    ...
  }: {
    home.packages = with pkgs; [
      unstable.claude-code
      unstable.opencode
    ];

    home.file.".claude/themes".source = dfRoot + /claude/.claude/themes;

    # Nix keys win; machine-local keys (plugins, marketplaces) are left alone.
    home.activation.claudeSettings = lib.hm.dag.entryAfter ["writeBoundary"] ''
      f="$HOME/.claude/settings.json"
      mkdir -p "$HOME/.claude"
      [ -s "$f" ] || echo '{}' > "$f"
      ${lib.getExe pkgs.jq} -s '.[0] * .[1]' "$f" ${dfRoot + /claude/.claude/settings.json} > "$f.tmp"
      mv "$f.tmp" "$f"
    '';
  };
}
