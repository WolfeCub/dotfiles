;;; core-keybinds.el -*- lexical-binding: t; -*-

(use-package general
  :demand t
  :config
  (general-auto-unbind-keys)

  (general-create-definer wolfe/bind-leader
    :keymaps 'global
    :states '(normal insert visual emacs)
    :prefix "SPC")

  (general-create-definer wolfe/bind-local-leader
     :keymaps 'global
     :states '(normal insert visual emacs)
     :prefix "SPC SPC"))

(provide 'core-keybinds)
