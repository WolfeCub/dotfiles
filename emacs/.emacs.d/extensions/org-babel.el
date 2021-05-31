;;; org-babel.el -*- lexical-binding: t; -*-

(require 'ob-csharp)
(require 'my-ob-haskell)

(use-package ob-deno
  :config
  (define-derived-mode web-ts-mode web-mode "WebTS"
   "Major mode for editing web mode ts."
   (web-mode)
   (setq web-mode-content-type "typescript"))

  (add-to-list 'org-src-lang-modes '("deno" . web-ts)))

(provide 'org-babel)
