;;; org-babel.el -*- lexical-binding: t; -*-

(use-package ob-csharp
  :straight nil
  :after org
  :defer-incrementally ob)

(use-package my-ob-haskell
  :straight nil
  :after org
  :defer-incrementally ob)

(use-package ob-deno
  :after org
  :defer-incrementally ob
  :config
  (define-derived-mode web-ts-mode web-mode "WebTS"
   "Major mode for editing web mode ts."
   (web-mode)
   (setq web-mode-content-type "typescript"))

  (add-to-list 'org-src-lang-modes '("deno" . web-ts)))

(use-package ob-mermaid
  :after org
  :defer-incrementally ob)

(provide 'org-babel)
