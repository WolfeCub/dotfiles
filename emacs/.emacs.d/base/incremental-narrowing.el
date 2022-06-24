;;; incremental-narrowing.el -*- lexical-binding: t; -*-

(use-package vertico
  :hook (wolfe/first-input . vertico-mode)
  :custom-face
  (vertico-current
   ((t (:foreground ,(plist-get base16-default-dark-colors :base09)
                    :background ,(plist-get base16-default-dark-colors :base01)))))
  :config
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
 (defun crm-indicator (args)
   (cons (concat "[CRM] " (car args)) (cdr args)))
 (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
 (setq minibuffer-prompt-properties
       '(read-only t cursor-intangible t face minibuffer-prompt))
 (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
 (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
 (setq enable-recursive-minibuffers t)

 ;; Highlight directories
 (defun +completion-category-highlight-files (cand)
      (let ((len (length cand)))
        (when (and (> len 0)
                   (eq (aref cand (1- len)) ?/))
          (add-face-text-property 0 len 'dired-directory 'append cand)))
      cand)

 (defvar +completion-category-hl-func-overrides
   `((file . ,#'+completion-category-highlight-files))
   "Alist mapping category to highlight functions.")

 (advice-add #'vertico--arrange-candidates :around
             (defun vertico-format-candidates+ (func)
               (let ((hl-func (or (alist-get (vertico--metadata-get 'category)
                                             +completion-category-hl-func-overrides)
                                  #'identity)))
                 (cl-letf* (((symbol-function 'actual-vertico-format-candidate)
                             (symbol-function #'vertico--format-candidate))
                            ((symbol-function #'vertico--format-candidate)
                             (lambda (cand &rest args)
                               (apply #'actual-vertico-format-candidate
                                      (funcall hl-func cand) args))))
                   (funcall func)))))

 ;; Highlight enabled modes
 (defun +completion-category-highlight-commands (cand)
   (let ((len (length cand)))
     (when (and (> len 0)
                (with-current-buffer (nth 1 (buffer-list)) ; get buffer before minibuffer
                  (or (eq major-mode (intern cand)) ; check major mode
                      (ignore-errors (auto-minor-mode-enabled-p (intern cand)))))) ; check minor modes
       (add-face-text-property 0 len '(:foreground "red") 'append cand))) ; choose any color or face you like
   cand)

 (add-to-list '+completion-category-hl-func-overrides `(command . ,#'+completion-category-highlight-commands)))

(use-package vertico-directory
  :straight nil
  :load-path "straight/build/vertico/extensions"
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-repeat
  :straight nil
  :load-path "straight/build/vertico/extensions"
  :general
  (wolfe/bind-leader                     
    "r" '(vertico-repeat :wk "Vertico Repeat")))

(use-package consult
  :bind
  ("C-s" . consult-line)
  :general
  (wolfe/bind-leader                     
    "f" '(consult-ripgrep :wk "Consult Ripgrep"))
  :init
  (setq consult-preview-key (kbd "M-."))
  :config
  (setq consult-project-root-function 'projectile-project-root))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))


(provide 'incremental-narrowing)
