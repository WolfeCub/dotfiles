;;; keymaps.el -*- lexical-binding: t; -*-

;; This replaces evil-leader but is much more powerful
(use-package general)

(use-package evil
  :demand
  :bind
  (:map evil-motion-state-map
        ("C-u" . evil-scroll-up))
  :init
  (setq evil-want-keybinding nil)
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

  (general-create-definer wolfe/bind-leader
    :keymaps 'global
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

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
    "m"  'ivy-switch-buffer
    "e"  'iedit-mode
    "c"  'wolfe/compile-no-prompt
    "n"  'narrow-or-widen-dwim
    "a"  'org-agenda
    "g"  'magit-status
    "''" 'org-edit-src-exit
    "t"  'shell-pop
    "f"    (lambda() (interactive) (wolfe/if-else-projectile 'counsel-projectile-ag 'counsel-ag))
    "p"    (lambda() (interactive) (funcall wolfe/hydra-projectile))
    ";"    (lambda() (interactive) (save-excursion (end-of-line) (insert-char ?\;)))
    "id"   (lambda() (interactive) (indent-region (point-min) (point-max)))
    "o"    (lambda() (interactive) (wolfe/org-open "everything"))
    "init" (lambda() (interactive) (evil-buffer-new nil wolfe/init-file))
    "SPC"  'major-mode-hydra))

;; Evil everywhere possible
(use-package evil-collection
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

(provide 'keymaps)
