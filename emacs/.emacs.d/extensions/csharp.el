;;; csharp.el -*- lexical-binding: t; -*-

(use-package csharp-mode
  :mode "\\.cs\\'")

(use-package dotnet
  :after csharp-mode
  :general
  (wolfe/bind-local-leader
    "c" '(dotnet-build         :wk "Compile")
    "r" '(dotnet-run           :wk "Run")
    "R" '(dotnet-run-with-args :wk "Run with args")
    "t" '(dotnet-test          :wk "Test")))

(provide 'csharp)
