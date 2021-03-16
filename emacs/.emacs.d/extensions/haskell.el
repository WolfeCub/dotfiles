;;; haskell.el -*- lexical-binding: t; -*-

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package lsp-haskell
  :after (haskell-mode lsp-mode))

(provide 'haskell)
