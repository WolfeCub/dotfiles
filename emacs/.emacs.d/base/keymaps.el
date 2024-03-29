;;; keymaps.el -*- lexical-binding: t; -*-

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

(use-package evil
  :demand t
  :bind
  (:map evil-motion-state-map
        ("C-u" . evil-scroll-up))
  :general
  (general-define-key
   :states 'motion
   "k" 'evil-previous-visual-line
   "j" 'evil-next-visual-line)

  (general-define-key
   :states 'operator
   "k" 'evil-previous-line
   "j" 'evil-next-line)

  (general-define-key
   :states 'visual
   "<" (lambda ()
         (interactive)
         (evil-shift-left (region-beginning) (region-end))
         (evil-normal-state)
         (evil-visual-restore))
   ">" (lambda ()
         (interactive)
         (evil-shift-right (region-beginning) (region-end))
         (evil-normal-state)
         (evil-visual-restore)))

  (general-define-key
   :states 'normal
   "C-z"  'wolfe/controlz
   "C-l"  'evil-ex-nohighlight)

  (wolfe/bind-leader
    "w"  'save-buffer
    "b"  'mode-line-other-buffer
    "k"  'kill-buffer
    "m"  'consult-buffer
    "s"  'eval-defun
    "S"  'eval-region
    "c"  'wolfe/compile-no-prompt
    "n"  'narrow-or-widen-dwim
    "G"  'magit-status
    "''" 'org-edit-src-exit
    "t"  'shell-pop
    ";"  (lambda() (interactive) (save-excursion (end-of-line) (insert-char ?\;)))
    "="  `(,(lambda() (interactive) (indent-region (point-min) (point-max))) :wk "Indent buffer")
    "o"  (lambda() (interactive) (wolfe/org-open "everything")))

  :init
  (setq evil-want-keybinding nil
        evil-undo-system 'undo-tree)
  :config
  (defun wolfe/define-ex-map (ex-text fun)
    (define-key
      evil-ex-map
      ex-text
      (lambda () (interactive)
        (wolfe/call-and-update-ex fun))))

  (evil-mode t)
  (setq evil-split-window-below t
        evil-vsplit-window-right t
        evil-lookup-func #'wolfe/man)
  (setq-default evil-symbol-word-search t)
  (custom-set-variables '(evil-search-module (quote evil-search)))
  (evil-ex-define-cmd "re[load]" 'wolfe/load-init) ; Custom reload command
  (evil-ex-define-cmd "Q" 'save-buffers-kill-terminal) ; For typos
  (wolfe/define-ex-map "e " 'find-file)
  (wolfe/define-ex-map "reg " 'consult-register)
  (global-unset-key (kbd "M-SPC")) ; Unbind secondary leader
  (add-to-list 'evil-emacs-state-modes 'vterm-mode))

;; Evil everywhere possible
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Tpope's surround
(use-package evil-surround
  :demand t
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config
  (global-evil-surround-mode 1))

;; Match smarter
(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

;; Start * or # from visual selection
(use-package evil-visualstar
  :demand t
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :config
  (global-evil-visualstar-mode))

;; Useful for macros
(use-package evil-numbers
  :defer t
  :bind
  (:map evil-normal-state-map
        ("C-a" . 'evil-numbers/inc-at-pt)
        ("C--" . 'evil-numbers/dec-at-pt)))

;; Align things the vim way
(use-package evil-lion
  :general
  (general-define-key
   :states 'normal
   "gl" #'evil-lion-left
   "gL" #'evil-lion-right
   "gl" #'evil-lion-left
   "gL" #'evil-lion-right)
  :config
  (evil-lion-mode))

;; Exchange places
(use-package evil-exchange
  :general
  (general-define-key
   :states '(normal visual)
   "gx" #'evil-exchange)
  :config
  (evil-exchange-install))

(use-package which-key
  :hook (wolfe/first-input . which-key-mode)
  :config
  (setq which-key-max-display-columns 3
        which-key-add-column-padding 1))

(provide 'keymaps)
