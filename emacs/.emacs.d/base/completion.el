;;; completion.el -*- lexical-binding: t; -*-

(use-package lsp-mode
  :commands lsp
  :hook
  (typescript-mode . lsp)
  (web-mode        . lsp)
  (python-mode     . lsp)
  (csharp-mode     . lsp)
  (haskell-mode    . lsp)
  :general
  (wolfe/bind-leader
    "l"   '(nil                     :wk "LSP Mode")
    "l x" '(lsp-execute-code-action :wk "Code action")
    "l r" '(lsp-rename              :wk "Rename")
    "l i" '(lsp-goto-implementation :wk "Goto Implementation"))
  :init
  (setq read-process-output-max (* 1024 1024)
        lsp-idle-delay 0.500)
  :config
  (setq lsp-completion-provider :capf))

;; The lsp-ui overlays are very slow on windows
(when wolfe/linux?
  (use-package lsp-ui
    :after lsp-mode
    :config
    (setq lsp-completion-provider :capf
          lsp-enable-snippet nil
          company-lsp-enable-snippet nil)))

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
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(provide 'completion)
