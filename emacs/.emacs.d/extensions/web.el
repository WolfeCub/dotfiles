;;; web.el -*- lexical-binding: t; -*-

(use-package web-mode
  :defer t
  :mode ("\\.html\\'" "\\.php\\'" "\\.js\\'" "\\.tsx\\'" "\\.vue\\'")
  :config
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-indentation t
        web-mode-enable-auto-quoting nil
        web-mode-script-padding 0
        web-mode-style-padding 0))

(use-package json-mode
  :defer t
  :mode ("\\.json\\'"))

(use-package company-restclient
  :after company
  :config
  (add-to-list 'company-backends 'company-restclient))

(provide 'web)
