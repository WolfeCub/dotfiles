;;; web.el -*- lexical-binding: t; -*-

(use-package web-mode
  :defer t
  :mode ("\\.html\\'" "\\.php\\'"
         "\\.js\\'" "\\.ts\\'" "\\.jsx\\'" "\\.tsx\\'"
         "\\.vue\\'" "\\.eex\\'")
  :config
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-indentation t
        web-mode-enable-auto-quoting nil
        web-mode-script-padding 0
        web-mode-style-padding 0)
  ;; Use 2 space indentation in =.vue= files.
  (add-hook
     'web-mode-hook
     (lambda ()
         (when (and (stringp buffer-file-name)
                    (string-match "\\.vue\\'" buffer-file-name))
             (setq web-mode-markup-indent-offset 2
                   web-mode-css-indent-offset 2
                   web-mode-code-indent-offset 2)))))

(use-package graphql-mode
  :defer t
  :custom
  (graphql-indent-level 4))

(use-package fence-edit
  :defer t
  :straight (fence-edit :type git :host github :repo "aaronbieber/fence-edit.el")
  :config
  (add-to-list 'fence-edit-blocks '("gql[ \t\n]*(?`" "`" graphql)))

(use-package json-mode
  :defer t
  :mode ("\\.json\\'"))

(use-package company-restclient
  :after company
  :config
  (add-to-list 'company-backends 'company-restclient))

(provide 'web)
