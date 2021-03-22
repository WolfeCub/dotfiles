;;; web.el -*- lexical-binding: t; -*-

(use-package parinfer-rust-mode
  :hook emacs-lisp-mode
  :bind
  (("C-," . parinfer-rust-toggle-paren-mode))
  :init
  (setq parinfer-rust-auto-download t))

(provide 'lisp)
