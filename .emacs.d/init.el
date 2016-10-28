
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message "wolfe")
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode t)
(setq initial-scratch-message "")
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(delete-selection-mode 1)
(when (member "Inconsolata" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Inconsolata-13" ))
  (set-face-attribute 'default t :font "Inconsolata-13"))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;(use-package zerodark-theme
;  :config
;  (zerodark-setup-modeline-format))
(setq custom-theme-directory "~/.emacs.d/themes")

(defun wolfe/theme-init ()
  (interactive)
  (use-package s)
  (use-package powerline)
  (use-package powerline-evil)
  (load-theme 'zerodark t)
  (zerodark-setup-modeline-format))

(defun wolfe/load-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(use-package general)

(use-package evil
  :demand
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode t)
  (wolfe/theme-init)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (evil-ex-define-cmd "re[load]" 'wolfe/load-init)
  (define-key evil-ex-map "e " 'ido-find-file)

  (global-unset-key (kbd "M-SPC"))

  (general-create-definer wolfe/bind-leader
                          :keymaps 'global
                          :states '(normal insert emacs)
                          :prefix "SPC"
                          :non-normal-prefix "M-SPC")

  :general
  (:states 'motion
           "k" 'evil-previous-visual-line
           "j" 'evil-next-visual-line)

  (:states 'normal
           "C-S-h" help-map
           "C-h"  'evil-window-left
           "C-j"  'evil-window-down
           "C-k"  'evil-window-up
           "C-l"  'evil-window-right)

  (wolfe/bind-leader
   "w" 'save-buffer
   "S" 'eval-buffer 
   "s" 'eval-defun
   "b" 'mode-line-other-buffer
   "k" 'kill-buffer
   "m" 'ido-switch-buffer
   "os" (lambda() (interactive) (wolfe/org-open "school"))
   "ol" (lambda() (interactive) (wolfe/org-open "life"))
   "init" (lambda() (interactive) (evil-buffer-new nil "~/.emacs.d/README.org"))))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-magit)

(setq org-pretty-entities t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-ellipsis "⤵") 

(defun wolfe/org-open (name)
 (interactive)
  (when (eq system-type 'gnu/linux)
    (evil-buffer-new nil (concat "~/Dropbox/org/" name ".org")))
  (when (eq system-type 'windows-nt)
    (evil-buffer-new nil (concat "C:\\Users\\Josh\\Dropbox\\org\\" name ".org"))))

(defun wolfe/org-dropbox-path ()
  (interactive)
  (cond
   ((eq system-type 'gnu/linux)
    "~/Dropbox/org/")
   ((eq system-type 'windows-nt)
    "C:\\Users\\Josh\\Dropbox\\org\\")
   (else "")))

(defun wolfe/dropbox-start ()
  (interactive)
  (if (eq nil (file-exists-p "/virtual/wolfejos/dropbox/.dropbox-dist"))
      (call-process-shell-command "(python ~/.emacs.d/dropbox.py start -i&)")
    (call-process-shell-command "(python ~/.emacs.d/dropbox.py start&)")))

(defun wolfe/dropbox-stop ()
  (interactive)
  (call-process-shell-command "python ~/.emacs.d/dropbox.py stop&"))

(add-hook 'org-mode-hook
          (lambda ()
            (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)))

(use-package ox-twbs)

;; Better looking org headers
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ido
    :init
    (defun wolfe/ido-set-keys ()
        "Add keybindings for ido"
        (define-key ido-completion-map [tab] 'ido-next-match))
    (add-hook 'ido-setup-hook #'wolfe/ido-set-keys)
    :config
    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t)
    (ido-mode 1))

(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-complete-space-or-hyphen)

;(use-package ido-vertical-mode
;  :config
;  (ido-vertical-mode 1))

(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package nlinum-relative
  :config
  (setq nlinum-relative-redisplay-delay 0)
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode))

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package magit
  :config
  (global-set-key "\C-x\g" 'magit-status))

(use-package ace-jump-mode
  :config
  (define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode))

(use-package neotree
  :config
  (global-set-key [f8] 'neotree-toggle)
  (add-hook 'neotree-mode-hook
      (lambda ()
        (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
        (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
        (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
        (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))

(use-package doom-neotree
  :ensure doom-themes
  :config
  (setq doom-neotree-enable-file-icons t))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 3))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config
  (setq git-gutter-fr:side 'right-fringe)
  (set-face-foreground 'git-gutter-fr:modified "#63747c")
  (set-face-foreground 'git-gutter-fr:added    "#63747c")
  (set-face-foreground 'git-gutter-fr:deleted  "#63747c")
  (global-git-gutter-mode +1))

(use-package latex-preview-pane)

(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-select-next)

  (require 'color)
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))

(use-package company-math
  :config
  (defun wolfe/latex-setup ()
    (setq-local company-backends
                (append '((company-math-symbols-latex company-latex-commands))
                        company-backends)))

  (add-hook 'tex-mode-hook 'wolfe/latex-setup))

(setq backup-by-copying t)
(setq backup-directory-alist '((".*" . "~/.bak.emacs/backup/")))
(if (eq nil (file-exists-p "~/.bak.emacs/"))
    (make-directory "~/.bak.emacs/"))
(if (eq nil (file-exists-p "~/.bak.emacs/auto"))
    (make-directory "~/.bak.emacs/auto"))
(setq auto-save-file-name-transforms '((".*" "~/.bak.emacs/auto/" t)))

(if (eq t (file-exists-p "~/.emacs.d/lisp/the-lab.el"))
    (load-file "~/.emacs.d/lisp/the-lab.el"))
