# dotfiles

<p align="center"><img src="https://imgs.xkcd.com/comics/borrow_your_laptop.png"/></p>

Here are my personal dotfiles.

- [NixOS](https://nixos.org/) - my machines (and home directory) are built from the flake in this repo
- [zsh](http://www.zsh.org/) - shell
- [neovim](https://github.com/neovim/neovim) - my primary editor
  - Most of the config can be found here [Mulan-Szechuan-Sauce/nvim-config](https://github.com/Mulan-Szechuan-Sauce/nvim-config) which
    is a shared config that I maintain with a friend.
- [emacs](https://www.gnu.org/software/emacs/) - used to be my primary editor (my old `.emacs.d` makes up a good portion of this repo)


## NixOS

My machines are built from the Nix flake at the root of this repo. The flake is tiny — it uses
[`flake-parts`](https://github.com/hercules-ci/flake-parts) + [`import-tree`](https://github.com/vic/import-tree) so
every `.nix` file under `nixos/` is auto-imported as a module (the "dendritic" pattern). There's no central module list;
each file just registers what it adds via `flake.nixosModules.*`, `flake.homeModules.*`, or `perSystem.packages.*`.

```
nixos/
├── parts.nix        # flake-parts setup
├── overlays.nix     # the one place nixpkgs is built (allowUnfree + pkgs.unstable)
├── hosts.nix        # assembles nixosConfigurations + homeConfigurations
├── checks.nix       # exposes every host/home build as a flake check
├── os/              # nixos system modules: user, nh, audio, graphics, games
├── home/            # home-manager modules: shell, neovim, niri, noctalia, rio, fonts
├── packages/        # flake package outputs, runnable via `nix run`
└── hosts/           # per-machine config
```


## Neovim

My primary editor. Most of the config lives in the shared repo [Mulan-Szechuan-Sauce/nvim-config](https://github.com/Mulan-Szechuan-Sauce/nvim-config),
which I share with a friend. Per user overides can be found in this repo under `neovim/.user.nvm`.


### Plugin Highlights

- [`lazy.nvim`](https://github.com/folke/lazy.nvim) - plugin manager
- [`mason`](https://github.com/williamboman/mason.nvim) + [`mason-lspconfig`](https://github.com/williamboman/mason-lspconfig.nvim) - LSP server installation and management
- [`blink.cmp`](https://github.com/saghen/blink.cmp) - completion with LSP, snippets, path, and buffer sources
- [`nvim-treesitter`](https://github.com/nvim-treesitter/nvim-treesitter) - syntax highlighting and parsing, with textobjects and sticky context
- [`flash.nvim`](https://github.com/folke/flash.nvim) - fast jump navigation with `s` (jump) and `S` (treesitter)
- [`mini.nvim`](https://github.com/nvim-mini/mini.nvim) - text editing suite: surround (`ys`/`ds`/`cs`), AI text objects, alignment, operators, move, and splitjoin
- [`harpoon2`](https://github.com/ThePrimeagen/harpoon) + [`harpeek.nvim`](https://github.com/WolfeCub/harpeek.nvim) - quick file bookmarks with a custom tabline display (personal project)
- [`oil.nvim`](https://github.com/stevearc/oil.nvim) - file explorer as an editable buffer
- [`snacks.nvim`](https://github.com/folke/snacks.nvim) - file picker, buffer management, and git browse
- [`neogit`](https://github.com/NeogitOrg/neogit) - Magit-style git interface (still miss this from emacs)
- [`lualine.nvim`](https://github.com/nvim-lualine/lualine.nvim) - statusline with active LSP server, branch, and diagnostics
- [`nvim-dap`](https://github.com/mfussenegger/nvim-dap) + [`nvim-dap-ui`](https://github.com/rcarriga/nvim-dap-ui) + [`persistent-breakpoints`](https://github.com/Weissle/persistent-breakpoints.nvim) - debugging


## Zsh

<p align="center"><img src="https://i.imgur.com/yW2gOLl.png"/></p>

<p align="center"><img src="https://i.imgur.com/ypDccfn.jpg"/></p>

My preffered shell is `zsh`. My customization isn't anything insanely intricate
but it's functional and I find it comfy. I `ssh` a fair amount and I find that
having the user and host always visible is very handy. The exit code and time
aren't used as much but they're out of my way on the right side and they're handy
every now and then. Personally I love the syntax highlighting it helps me visually
parse the command much more easily.


### Other Highlights

- Better completion
- Colorized `man` pages
- `C-z` to foreground a backgrounded process
- Directory aliases
- [`fzf`](https://github.com/junegunn/fzf)


## Emacs

<p align="center"><img src="https://i.imgur.com/W2t0hAZ.jpg"/></p>

Emacs used to be my primary editor but I switched to `neovim` a while ago. I found I needed a large
config to get the features I wanted & tweaks to improve performance.

My emacs configuration used to be completely literate however the org file grew to unmanagable size.
It's since been split up into many self contained modules that can easily be plucked into other configs
or lazy loaded/disabled until needed.


### Highlights

-   GUI and terminal compatibility
-   Vim emulation (E.V.I.L.)
-   C#, Haskell, React, Vue and many other popular languages supported
-   General for keymaps and leader emulation
-   Hydra for the rest of my shortcut needs
-   Project management (using projectile)
-   Daemon support with isolated peprspectives for each open window
