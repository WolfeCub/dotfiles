;;; completion.el -*- lexical-binding: t; -*-

(use-package lsp-mode
  :commands lsp
  :hook
  (typescript-mode . lsp)
  (web-mode        . lsp)
  (python-mode     . lsp)
  (csharp-mode     . lsp)
  (haskell-mode    . lsp)
  (rust-mode       . lsp)
  :general
  (wolfe/bind-leader
    "l"   '(nil                     :wk "LSP Mode")
    "l x" '(lsp-execute-code-action :wk "Code Action")
    "l r" '(lsp-rename              :wk "Rename")
    "l i" '(lsp-find-implementation :wk "Implementation")
    "l d" '(lsp-find-definition     :wk "Definition")
    "l u" '(lsp-find-references     :wk "Usages")
    "l D" '(dap-debug               :wk "Debug"))
  :init
  (setq read-process-output-max (* 1024 1024)
        lsp-idle-delay 0.500)
  :config
  (setq lsp-completion-provider :capf))

;; The lsp-ui overlays are very slow on windows
(use-package lsp-ui
  :after lsp-mode)

(use-package dap-mode
  :commands (dap-debug)
  :general
  (general-define-key
   :keymaps 'dap-mode-map
   :states '(normal insert)
   "<f5>"  'dap-continue
   "<f10>" 'dap-next
   "<f11>" 'dap-step-in
   "<f12>" 'dap-step-out)
  :custom
  (dap-auto-configure-mode t)
  :config
  (require 'netcoredbg-dap))

(defun wolfe/hot-load-company (orig-fun &rest args)
  (unless (bound-and-true-p company-mode)
    (company-mode 1)
    (company-complete)))

(advice-add 'evil-complete-next :around #'wolfe/hot-load-company)
(advice-add 'evil-complete-previous :around #'wolfe/hot-load-company)

(use-package company
  :hook
  ((lsp-mode        . company-mode)
   (restclient-mode . company-mode))
  :bind (:map company-active-map
              ("C-n"   . company-select-next)
              ("C-p"   . company-select-previous)
              ("RET"   . company-complete-selection)
              ("<ret>" . company-complete-selection))
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

(use-package company-prescient
  :after (prescient company)
  :config
  (company-prescient-mode))

(use-package flycheck
  :commands flycheck-list-errors flycheck-buffer
  :hook (wolfe/first-buffer . global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package yasnippet
  :defer-incrementally eldoc easymenu help-mode
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-activate-extra-mode
             yas-deactivate-extra-mode)  
  :init
  (--map (add-hook it #'yas-minor-mode-on)
         '(text-mode-hook
           prog-mode-hook
           conf-mode-hook
           snippet-mode-hook)))

(provide 'completion)
