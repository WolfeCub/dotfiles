;;; ui.el -*- lexical-binding: t; -*-

(setq custom-theme-directory "~/.emacs.d/themes")

(use-package base16-theme
  :config
  (load-theme 'base16-default-dark t)
  (defvar my/base16-colors base16-default-dark-colors)
  (setq evil-emacs-state-cursor   `(,(plist-get my/base16-colors :base0D) box)
        evil-insert-state-cursor  `(,(plist-get my/base16-colors :base0D) bar)
        evil-motion-state-cursor  `(,(plist-get my/base16-colors :base0E) box)
        evil-normal-state-cursor  `(,(plist-get my/base16-colors :base07) box)
        evil-replace-state-cursor `(,(plist-get my/base16-colors :base08) bar)
        evil-visual-state-cursor  `(,(plist-get my/base16-colors :base09) box)))

(eval-after-load 'ivy (lambda () (setq ivy-switch-buffer-faces-alist '((dired-mode . ivy-subdir)))))
(hook-if-daemonp (lambda () (set-face-attribute 'fringe nil :background nil)))

(custom-set-faces
 '(org-block-begin-line      ((t (:inherit (org-meta-line) :height 0.7))))
 '(org-block-end-line        ((t (:inherit (org-meta-line) :height 0.7))))
 '(region ((t (:background "gray19"))))
 '(org-block ((t (:foreground "#d8d8d8"))))
 '(org-done ((t (:box (:line-width 1) :weight bold))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-todo ((t (:box (:line-width 1) :weight bold)))))

(provide 'ui)
