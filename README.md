# dotfiles

<p align="center"><img src="https://imgs.xkcd.com/comics/borrow_your_laptop.png"/></p>

Here are my personal dotfiles.

- [zsh](http://www.zsh.org/) - shell
- [neovim](https://github.com/neovim/neovim) - my primary editor
  - Most of the config can be found here [Mulan-Szechuan-Sauce/nvim-config](https://github.com/Mulan-Szechuan-Sauce/nvim-config) which
    is a shared config that I maintain with a friend.
- [emacs](https://www.gnu.org/software/emacs/) - used to be my primary editor (my old `.emacs.d` makes up a good portion of this repo)


## Installing

My dotfiles are managed using [GNU Stow](https://www.gnu.org/software/stow/). This makes it easy to add and remove modules
(and their symbolic links).


### One Liner

This one line will clone my dotfiles and begin the installer.

```bash
curl https://raw.githubusercontent.com/WolfeCub/dotfiles/master/install.sh | bash -s -- -g <& 1
```


### Manual Installation

If you don't want to use the one liner or the install script.
You can install each module individually using `stow [name]` as shown below.
This allows you to only use the modules that you want.

```bash
    git clone --recursive https://github.com/WolfeCub/dotfiles.git
    cd dotfiles
    stow zsh
    stow emacs
    ...
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
