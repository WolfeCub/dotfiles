;; __        __    _  __      _       _       _ _         _
;; \ \      / /__ | |/ _| ___( )___  (_)_ __ (_) |_   ___| |
;;  \ \ /\ / / _ \| | |_ / _ \// __| | | '_ \| | __| / _ \ |
;;   \ V  V / (_) | |  _|  __/ \__ \ | | | | | | |_ |  __/ |
;;    \_/\_/ \___/|_|_|  \___| |___/ |_|_| |_|_|\__(_)___|_|

;; Temporarily set garbage collect threshold high to improve start time
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; Setup package control
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Have use-package auto download
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;
;; G E N E R A L   S E T T I N G S
;;
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message "wolfe")
(tool-bar-mode -1) ; No toolbar
(scroll-bar-mode -1) ; Hide scrollbars
(menu-bar-mode -1) ; No menubar
(show-paren-mode t) ; Highlights matching parens
(setq initial-scratch-message "") ; No scratch text
(fset 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no
(setq-default indent-tabs-mode nil)
(delete-selection-mode 1) ; Replace selection on insert
(setq vc-follow-symlinks t) ; Always follow symlinks
 (when (member "Inconsolata" (font-family-list)) ; Set default font
   (add-to-list 'default-frame-alist '(font . "Inconsolata-13" ))
   (set-face-attribute 'default t :font "Inconsolata-13"))
(setq custom-file "~/.emacs.d/custom.el") ; Set custom file
(load custom-file 'noerror) ; Load custom file
(global-hl-line-mode 1)
(custom-set-faces
 '(hl-line ((t (:weight bold)))))
(setq custom-theme-directory "~/.emacs.d/themes")
(load-theme 'cyberpunk t)


;; When in terminal
(unless (display-graphic-p) 
  (setq nlinum-format "%d ")
  (add-to-list 'default-frame-alist '(background-color . "color-16"))
  (custom-set-faces
   '(linum ((t (:background "color-16" :foreground "#9fc59f"))))))

;;
;; F U N C T I O N S
;;
(defun wolfe/find-tag ()
  "Jump to the tag at point without prompting"
  (interactive)
  (find-tag (find-tag-default)))

(defun wolfe/controlz ()
  (interactive)
  (when (eq (display-graphic-p) nil)
    (suspend-emacs)))

(defun wolfe/org-open (name)
  "Opens the file in the dropbox path"
  (interactive)
  (when (eq system-type 'gnu/linux)
    (evil-buffer-new nil (concat "~/Dropbox/org/" name ".org")))
  (when (eq system-type 'windows-nt)
    (evil-buffer-new nil (concat "C:\\Users\\Josh\\Dropbox\\org\\" name ".org"))))

(defun wolfe/org-dropbox-path ()
  "Returns the dropbox path"
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

;;
;; E V I L
;;
(use-package general)
(use-package evil
  :demand
  :init
  (setq evil-want-C-u-scroll t) ; Unbind <C-u> for evil mode's use
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq-default evil-symbol-word-search t) 
  (evil-ex-define-cmd "re[load]" 'wolfe/load-init) ; Custom reload command
  (define-key evil-ex-map "e " 'ido-find-file) ; Trigger ido with :e

  (global-unset-key (kbd "M-SPC")) ; Unbind secondary leader

  (general-create-definer wolfe/bind-leader
                          :keymaps 'global
                          :states '(normal insert emacs)
                          :prefix "SPC"
                          :non-normal-prefix "M-SPC")

  :general
  (:states 'motion
           "k" 'evil-previous-visual-line
           "j" 'evil-next-visual-line)
  (:states 'operator
           "k" 'evil-previous-line
           "j" 'evil-next-line)
  

  (:states 'normal
           "C-S-h" help-map
           "C-h"  'evil-window-left
           "C-j"  'evil-window-down
           "C-k"  'evil-window-up
           "C-l"  'evil-window-right
           "C-z"  'wolfe/controlz)

  (wolfe/bind-leader
   "w" 'save-buffer
   "S" 'eval-buffer
   "s" 'eval-defun
   "b" 'mode-line-other-buffer
   "k" 'kill-buffer
   "m" 'ido-switch-buffer
   "t" 'wolfe/find-tag
   "os" (lambda() (interactive) (wolfe/org-open "school"))
   "ol" (lambda() (interactive) (wolfe/org-open "life"))
   "init" (lambda() (interactive) (evil-buffer-new nil "~/.emacs.d/init.el"))))

;; Tpope's surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-magit)

;;
;; O R G - M O D E
;;
(setq org-pretty-entities t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-ellipsis "⤵")

;; Twitter bootstrap exporter
(use-package ox-twbs)

;; Better looking org headers
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;
;; G E N E R A L   P A C K A G E S
;;
(use-package ido
  :init
  (defun wolfe/ido-set-keys ()
    "Add keybindings for ido"
    (define-key ido-completion-map (kbd "C-i") 'ido-next-match))
  (add-hook 'ido-setup-hook #'wolfe/ido-set-keys)
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1))

(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-complete-space-or-hyphen)

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

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode))

(use-package magit
  :config
  (global-set-key "\C-x\g" 'magit-status))

(use-package ace-jump-mode
  :config
  (define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 10))

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

;;
;; C O M P A N Y
;;
(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0) ; Delay to complete
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t) ; Loops around suggestions

  (if (display-graphic-p)
      (define-key company-active-map [tab] 'company-select-next)
    (define-key company-active-map (kbd "C-i") 'company-select-next))

  ;; Inherits colors from theme to style autocomplete menu correctly
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

;;
;; M I S C
;;
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;;
;; B A C K U P S
;;
(setq backup-by-copying t) ; Stop shinanigans with links
(setq backup-directory-alist '((".*" . "~/.bak.emacs/backup/")))
(if (eq nil (file-exists-p "~/.bak.emacs/")) ; Creates directory if it doesn't already exist
    (make-directory "~/.bak.emacs/"))
(if (eq nil (file-exists-p "~/.bak.emacs/auto")); Creates auto directory if it doesn't already exist
    (make-directory "~/.bak.emacs/auto"))
(setq auto-save-file-name-transforms '((".*" "~/.bak.emacs/auto/" t)))
; backup in one place. flat, no tree structure

; Load misc private file
(if (eq t (file-exists-p "~/.emacs.d/lisp/the-lab.el"))
    (load-file "~/.emacs.d/lisp/the-lab.el"))
