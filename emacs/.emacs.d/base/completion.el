;;; completion.el -*- lexical-binding: t; -*-

(use-package ivy
  :demand
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         :map ivy-minibuffer-map
         ("TAB" . ivy-next-line)
         ("RET" . ivy-alt-done))
  :init
  (use-package smex)
  (use-package counsel)
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  (setq ivy-wrap t)
  (ivy-mode 1)

  ;; Extracted from doom config for finding TODOs
  (use-package doom-todo-ivy
    :straight nil
    :config
    (evil-define-command doom/ivy-tasks-ex (&optional bang)
      "An ex wrapper around `doom/ivy-tasks'."
      (interactive "<!>")
      (doom/ivy-tasks bang))
    (evil-ex-define-cmd "todo" 'doom/ivy-tasks-ex))

  (eval-after-load "hydra" (use-package ivy-hydra)))

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package lsp-mode
  :defer t
  :commands lsp
  :hook
  (typescript-mode . lsp)
  (web-mode        . lsp)
  (python-mode     . lsp)
  (csharp-mode     . lsp)
  :init
  (setq read-process-output-max (* 1024 1024)
        lsp-idle-delay 0.500)
  :config
  (setq lsp-completion-provider :capf))

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-completion-provider :capf
        lsp-enable-snippet nil
        company-lsp-enable-snippet nil))

(use-package company
  :bind (:map company-active-map
              ("C-n"   . company-select-next)
              ("C-p"   . company-select-previous)
              ("RET"   . company-complete-selection)
              ("<ret>" . company-complete-selection))
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0) ; Delay to complete
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t) ; Loops around suggestions

  (if (display-graphic-p)
      (define-key company-active-map [tab] 'company-select-next)
    (define-key company-active-map (kbd "C-i") 'company-select-next))

  ;; Automatically use theme colors to style completion box
  (hook-if-daemonp
   (lambda ()
     (require 'color)
     (let ((bg (face-attribute 'default :background))
           (ac (face-attribute 'match :foreground)))
       (custom-set-faces
        `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
        `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
        `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
        `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
        `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
        `(company-preview-common ((t (:foreground ,ac :background ,(color-lighten-name bg 10))))))))))

(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(provide 'completion)
