;;; keymaps.el -*- lexical-binding: t; -*-

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package evil
  :demand t
  :bind
  (:map evil-motion-state-map
        ("C-u" . evil-scroll-up))
  :init
  (setq evil-want-keybinding nil
        evil-undo-system 'undo-tree)
  :config
  (evil-mode t)
  (setq evil-split-window-below t
        evil-vsplit-window-right t
        evil-lookup-func #'wolfe/man)
  (setq-default evil-symbol-word-search t)
  (custom-set-variables '(evil-search-module (quote evil-search)))
  (evil-ex-define-cmd "re[load]" 'wolfe/load-init) ; Custom reload command
  (evil-ex-define-cmd "Q" 'save-buffers-kill-terminal) ; For typos
  (define-key evil-ex-map "e " (lambda () (interactive) (wolfe/call-and-update-ex 'counsel-find-file))) ; Trigger file completion :e
  (global-unset-key (kbd "M-SPC")) ; Unbind secondary leader
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)

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
    "S"  'wolfe/eval-and-replace
    "s"  'eval-defun
    "b"  'mode-line-other-buffer
    "k"  'kill-buffer
    "K"  'kill-buffer-and-window
    "e"  'iedit-mode
    "c"  'wolfe/compile-no-prompt
    "n"  'narrow-or-widen-dwim
    "a"  'org-agenda
    "g"  'magit-status
    "''" 'org-edit-src-exit
    "t"  'shell-pop
    "f"    (lambda() (interactive) (wolfe/if-else-projectile 'counsel-projectile-ag 'counsel-ag))
    ";"    (lambda() (interactive) (save-excursion (end-of-line) (insert-char ?\;)))
    "id"   (lambda() (interactive) (indent-region (point-min) (point-max)))
    "o"    (lambda() (interactive) (wolfe/org-open "everything"))))

;; Evil everywhere possible
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Tpope's surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Match smarter
(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

;; Start * or # from visual selection
(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

;; Useful for macros
(use-package evil-numbers
  :bind
  (:map evil-normal-state-map
        ("C-a" . 'evil-numbers/inc-at-pt)
        ("C--" . 'evil-numbers/dec-at-pt)))

;; Align things the vim way
(use-package evil-lion
  :config
  (evil-lion-mode))

;; Exchange places
(use-package evil-exchange
  :config
  (evil-exchange-install))

(use-package which-key
  :config
  (setq which-key-max-display-columns 3
        which-key-add-column-padding 1)
  (which-key-mode))

(provide 'keymaps)
