;;; init.el -*- lexical-binding: t; -*-
;; __        __    _  __      _       _       _ _         _
;; \ \      / /__ | |/ _| ___( )___  (_)_ __ (_) |_   ___| |
;;  \ \ /\ / / _ \| | |_ / _ \// __| | | '_ \| | __| / _ \ |
;;   \ V  V / (_) | |  _|  __/ \__ \ | | | | | | |_ |  __/ |
;;    \_/\_/ \___/|_|_|  \___| |___/ |_|_| |_|_|\__(_)___|_|


;; Disable file name handler for performance during startup
(defvar wolfe/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil
      ;; Set garbage collect high to speed up startup
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      ;; Ignore advice warnings
      ad-redefinition-action 'accept)

(defvar wolfe/gc-cons-threshold 16777216)

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold wolfe/gc-cons-threshold 
                      gc-cons-percentage 0.1
                      file-name-handler-alist wolfe/file-name-handler-alist
                      ad-redefinition-action 'warn)))

;; It may also be wise to raise gc-cons-threshold while the minibuffer is active,
;; so the GC doesn't slow down expensive commands (or completion frameworks, like
;; helm and ivy. The following is taken from doom-emacs
(defun defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold wolfe/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection-h)

;; Stop package.el from starting itself up
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always fetch packages
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)
(use-package bind-key :straight nil)
(use-package dash)
(use-package s)

(use-package general
  :demand t
  :config
  (general-auto-unbind-keys)
  (general-create-definer wolfe/bind-leader
    :keymaps 'global
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"))

(add-to-list 'load-path (concat user-emacs-directory "base"))
(require 'vars)
(require 'ui)
(require 'settings)
(require 'functions)
(require 'org-config)
;;(require 'popups)
(require 'keymaps)
(require 'project-settings)

(add-to-list 'load-path (concat user-emacs-directory "extensions"))
(require 'web)
(require 'javascript)
(require 'lisp)
(require 'latex)
(require 'c-cpp)
(require 'csharp)
(require 'fsharp)
(require 'py)
(require 'haskell)
(require 'elixir)

(require 'utilities)
(require 'completion)
(require 'backups)
