#!/usr/bin/env bash
# desc: one-shot setup for things Nix can't do at eval time.
# Run once per machine, before the first `home-manager switch`.

set -euo pipefail

NVIM_SHARED="$HOME/.nvim-shared"

if [ ! -d "$NVIM_SHARED" ]; then
    echo "Cloning nvim-shared to $NVIM_SHARED"
    git clone git@github.com:Mulan-Szechuan-Sauce/nvim-config "$NVIM_SHARED"
else
    echo "nvim-shared already cloned at $NVIM_SHARED"
fi

echo
echo "Done. Run \`nh home switch\` next."
