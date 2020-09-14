;;; csharp.el -*- lexical-binding: t; -*-

(use-package omnisharp
  :defer t
  :after company
  :config
  (add-hook 'csharp-mode-hook
            (lambda ()
              (evil-define-key 'normal omnisharp-mode-map (kbd "g d") 'omnisharp-go-to-definition)
              (unless (file-exists-p "Makefile")
                (set (make-local-variable 'compile-command) (concat "msbuild " (cdr (assoc :project-root omnisharp--server-info)))))))
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp))

(provide 'csharp)
