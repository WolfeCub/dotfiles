;;; ui.el -*- lexical-binding: t; -*-

(setq custom-theme-directory "~/.emacs.d/themes")

(defun check-and-remove-command-line-arg (argument)
  "Checks `command-line-args' for argument and removes it if found returning t or nil"
  (if (member argument command-line-args)
      (progn
        (setq command-line-args (delete argument command-line-args))
        t)
    nil))

;; We should consume our cli arg to make sure it doesn't interfere with anything
(let ((result (check-and-remove-command-line-arg "-light")))
  (setq wolfe/using-light result)
  (setq wolfe/using-dark (not result)))

;; If we're starting in daemon mode then add func as hook for later
;; otherwise we can just run it right away
(defun hook-if-daemonp (func)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (funcall func))))
    (funcall func)))

(defun wolfe/dark-setup ()
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
   '(org-todo ((t (:box (:line-width 1) :weight bold))))))

(defun wolfe/light-setup ()
  (use-package leuven-theme
    :config
    (custom-set-faces
     '(ivy-subdir ((t (:background "gray88")))))
    (load-theme 'leuven t)))

;; Switch to light theme if the CLI arg was present
(if wolfe/using-light
    (wolfe/light-setup)
  (wolfe/dark-setup))

(provide 'ui)
