;;; web.el -*- lexical-binding: t; -*-

(use-package parinfer
  :defer t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (setq
   parinfer-extensions '(defaults pretty-parens evil smart-tab smart-yank)
   parinfer-lighters '(" Φi" . " Φp"))
  (add-hook 'racket-mode           #'parinfer-mode)
  (add-hook 'clojure-mode-hook     #'parinfer-mode)
  (add-hook 'emacs-lisp-mode-hook  #'parinfer-mode)
  (add-hook 'common-lisp-mode-hook #'parinfer-mode)
  (add-hook 'scheme-mode-hook      #'parinfer-mode)
  (add-hook 'lisp-mode-hook        #'parinfer-mode))

(provide 'lisp)
