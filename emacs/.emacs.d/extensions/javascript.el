;;; web.el -*- lexical-binding: t; -*-

(use-package purescript-mode
  :defer t
  :mode "\\.purs\\'")

(use-package psc-ide
  :after purescript-mode
  :config
  (add-hook 'purescript-mode-hook
            (lambda ()
              (setq psc-ide-use-npm-bin t)
              (psc-ide-mode)
              (company-mode)
              (flycheck-mode)
              (turn-on-purescript-indentation))))

(provide 'javascript)
