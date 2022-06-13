;;; utilities.el -*- lexical-binding: t; -*-

;; Real terminal emulator inside of emacs
(use-package vterm
  :when wolfe/linux?)

;; Edit all matches
(use-package iedit
  :general
  (wolfe/bind-leader
    "e" 'iedit-mode)
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
  :defer-incrementally
  elisp-refs help help-fns dash s f find-func
  nadvice info-look edebug trace imenu
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
  :after-call wolfe/first-buffer-hook
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
  :init
  (setq explicit-shell-file-name "C:/Program Files/git/bin/bash.exe")
  (setq explicit-bash.exe-args '("--login" "-i")) 
  :custom
  (shell-pop-shell-type
   (if wolfe/linux?
       (quote ("vterm" "*vterm*" (lambda nil (vterm))))
     (quote ("shell" "*shell*" (lambda nil (shell))))))
  (shell-pop-term-shell "/usr/bin/zsh")
  (shell-pop-window-position "right")
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
                 (kill-buffer-and-window))))))))))

;; Startup profiler
(use-package esup
  :commands (esup))

(provide 'utilities)
