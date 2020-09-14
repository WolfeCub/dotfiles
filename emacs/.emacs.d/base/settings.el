;;; settings.el -*- lexical-binding: t; -*-

;; Recursively add everything in dir to load path
(add-to-list 'load-path wolfe/lisp-dir-path)
(let ((default-directory wolfe/lisp-dir-path))
  (normal-top-level-add-subdirs-to-load-path))

(setq inhibit-splash-screen t ; No splash screen
      inhibit-startup-message t ; No startup message
      initial-scratch-message "") ; No scratch text
(defun display-startup-echo-area-message ()
  (message "Welcome to the church of GNU/Emacs"))

(hook-if-daemonp
 (lambda ()
   (tool-bar-mode -1) ; No toolbar
   (scroll-bar-mode -1) ; Hide scrollbars
   (menu-bar-mode -1))) ; No menubar

;; Format font string and set as default
(let* ((font "Fira Mono")
       (size 15)
       (font-size (format "%s-%s" font size)))
  (setq default-frame-alist `((font . ,font-size)))
  (set-face-attribute 'default t :font font-size))

;; Set UTF8 font
(hook-if-daemonp
 (lambda ()
   (when (display-graphic-p)
     (let ((utf8-font "Fira Code"))
       (set-fontset-font "fontset-startup" '(#x000000 . #x3FFFFF) utf8-font)
       (set-fontset-font "fontset-default" '(#x000000 . #x3FFFFF) utf8-font)
       (set-fontset-font "fontset-standard" '(#x000000 . #x3FFFFF) utf8-font)))))

;; Use UTF8 everywhere possible
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

;; Use pretty symbols
(setq prettify-symbols-unprettify-at-point 'right-edge)
(setq inhibit-compacting-font-caches t) ; No garbage collect speeds this up

;; Basic simple symbols
(defun wolfe/pretty-symbol-push-default ()
  (push '("!=" . ?≠) prettify-symbols-alist)
  (push '("<=" . ?≤) prettify-symbols-alist)
  (push '(">=" . ?≥) prettify-symbols-alist)
  (push '("=>" . ?⇒) prettify-symbols-alist))

;; Apply the basics to some modes
(mapc
 (lambda (hook)
   (add-hook 'hook (lambda () (wolfe/pretty-symbol-push-default))))
 '(c-mode))

;; Python symbols
(add-hook 'python-mode-hook
          (lambda ()
            (wolfe/pretty-symbol-push-default)
            (push '("def"    . ?ƒ) prettify-symbols-alist)
            (push '("sum"    . ?Σ) prettify-symbols-alist)
            (push '("**2"    . ?²) prettify-symbols-alist)
            (push '("**3"    . ?³) prettify-symbols-alist)
            (push '("None"   . ?∅) prettify-symbols-alist)
            (push '("in"     . ?∈) prettify-symbols-alist)
            (push '("not in" . ?∉) prettify-symbols-alist)
            (push '("return" . ?➡) prettify-symbols-alist)
            (prettify-symbols-mode t)))

;; Lisp symbols
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (wolfe/pretty-symbol-push-default)
            (push '("defun"    . ?ƒ) prettify-symbols-alist)
            (push '("defmacro" . ?μ) prettify-symbols-alist)
            (push '("defvar"   . ?ν) prettify-symbols-alist)
            (prettify-symbols-mode t)))

(use-package column-marker
  :straight nil
  :config
  (add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 81)))
  (custom-set-faces
   '(column-marker-1 ((t (:background "dim gray"))))))

(require 'highlight-escape-chars)

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\)" 1 font-lock-warning-face t)))))

(show-paren-mode t) ; Highlights matching parens
(fset 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no
(blink-cursor-mode -1) ; No need to flash the cursor
(column-number-mode t) ; Show column in mode-line
(delete-selection-mode 1) ; Replace selection on insert
(setq-default truncate-lines t ; Don't wrap
              indent-tabs-mode nil)
(setq vc-follow-symlinks t ; Always follow symlinks
      tags-revert-without-query t ; Don't ask to reload TAGS file
      echo-keystrokes 0.5
      custom-file "~/.emacs.d/custom.el" ; Set custom file and don't load it
      source-directory "~/Projects/emacs/")

;; Vim scrolloff behaviour
(setq scroll-margin 10
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Smartly set the shell
(setq explicit-shell-file-name
      (if (file-readable-p "/usr/bin/zsh") "/usr/bin/zsh" "/bin/bash"))
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "cmdproxy.exe"))

;; Look for spellcheck exe on windows
(when wolfe/windows?
  (when wolfe/windows?
    (setq ispell-program-name "c:/emacs/hunspell/bin/hunspell.exe"))
  (setq ispell-program-name "c:/emacs/hunspell/bin/hunspell.exe"))

;; Keep scrolling compilation output
(require 'compile)
(setq compilation-scroll-output t)

;; Q should quit help buffers not just bury it
(dolist (m (list help-mode-map compilation-mode-map))
  (bind-key (kbd "q") (lambda () (interactive) (quit-window t)) m))

(require 'modeline)

;; Newer versions of emacs support native relative line numbers but some old
;; versions don't in that case pull nlinum
(if (version< "26.0.50" emacs-version)
    (progn
      (add-hook 'prog-mode-hook (lambda ()
                                  (display-line-numbers-mode t)
                                  (setq display-line-numbers 'relative))))
  (progn
    (use-package nlinum-relative
      :config
      (nlinum-relative-setup-evil)
      (setq nlinum-relative-redisplay-delay 0.25)
      (setq nlinum-relative-current-symbol "")
      (add-hook 'prog-mode-hook 'nlinum-relative-mode))))

(provide 'settings)
