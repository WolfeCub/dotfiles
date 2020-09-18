;;; web.el -*- lexical-binding: t; -*-

(use-package typescript-mode
  :defer t
  :mode "\\.ts\\'")

;; Use 2 space indentation in =.vue= files.
(add-hook
 'web-mode-hook
 (lambda ()
   (when (and (stringp buffer-file-name)
              (string-match "\\.vue\\'" buffer-file-name))
     (setq web-mode-markup-indent-offset 2
           web-mode-css-indent-offset 2
           web-mode-code-indent-offset 2))))

(use-package purescript-mode
  :defer t
  :mode "\\.purs\\'")

(use-package psc-ide
  :after purescript-mode
  :config
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (company-mode)
              (flycheck-mode)
              (turn-on-purescript-indentation))))

(provide 'javascript)
