;;; utilities.el -*- lexical-binding: t; -*-

;; More vimy dired
(use-package ranger
  :config
  (setq ranger-cleanup-on-disable t)
  (ranger-override-dired-mode t))

;; Edit all matches
(use-package iedit
  :config
  (setq iedit-toggle-key-default nil))

;; Run your current buffer
(use-package quickrun
  :defer t)

;; Postman for emacs
(use-package restclient
  :defer t)

;; Shows hex colors inline.
(use-package rainbow-mode
  :defer t)

;; More helpful help pages
(use-package helpful
  :defer t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command))
  :config
  (evil-define-key 'normal helpful-mode-map (kbd "q") 'kill-buffer-and-window)
  (set-face-attribute 'helpful-heading nil :height 1.1))

;; Popup rules
(use-package shackle
  :config
  (setq shackle-default-alignment 'below
        shackle-default-size 0.4
        shackle-rules
        '(("*HTTP Response*" :popup t :align right)))
  (shackle-mode))

;; Auto window sizer
(use-package zoom
  :config
  (setq zoom-size '(0.618 . 0.618))
  (zoom-mode t))

;; Load env variables
(use-package exec-path-from-shell
  :when (not wolfe/windows?)
  :config
  (exec-path-from-shell-copy-env "GPG_TTY")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

;; Quickly access and close the shell
(use-package shell-pop
  :config
  (defun shell-pop--set-exit-action ()
    (if (string= shell-pop-internal-mode "eshell")
        (add-hook 'eshell-exit-hook 'shell-pop--kill-and-delete-window nil t)
      (let ((process (get-buffer-process (current-buffer))))
        (when process
          (set-process-sentinel
           process
           (lambda (_proc change)
             (when (string-match-p "\\(?:finished\\|exited\\)" change)
               (if (one-window-p)
                   (switch-to-buffer shell-pop-last-buffer)
                 (kill-buffer-and-window)))))))))

  (custom-set-variables
   '(shell-pop-shell-type (quote ("vterm" "*vterm*" (lambda nil (vterm)))))
   '(shell-pop-term-shell "/usr/bin/zsh")
   '(shell-pop-window-position "right")))

(provide 'utilities)
