;;; utilities.el -*- lexical-binding: t; -*-

;; Real terminal emulator inside of emacs
(use-package vterm
  :when wolfe/linux?)

;; More vimy dired
(use-package ranger
  :commands ranger
  :config
  (setq ranger-cleanup-on-disable t)
  (ranger-override-dired-mode t))

;; Edit all matches
(use-package iedit
  :config
  (setq iedit-toggle-key-default nil))

;; Run your current buffer
(use-package quickrun
  :commands quickrun)

;; Postman for emacs
(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)))

;; Shows hex colors inline.
(use-package rainbow-mode
  :commands rainbow-mode)

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
  (setq zoom-size '(0.618 . 0.618)
        zoom-ignored-buffer-names '(" *ctrl-p*" " *ctrl-p-results*"))
  (zoom-mode t))

;; Quickly access and close the shell
(use-package shell-pop
  :commands shell-pop
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

;; Startup profiler
(use-package esup
  :commands (esup))

(provide 'utilities)
