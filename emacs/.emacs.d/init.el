;;; init.el -*- lexical-binding: t; -*-
;; __        __    _  __      _       _       _ _         _
;; \ \      / /__ | |/ _| ___( )___  (_)_ __ (_) |_   ___| |
;;  \ \ /\ / / _ \| | |_ / _ \// __| | | '_ \| | __| / _ \ |
;;   \ V  V / (_) | |  _|  __/ \__ \ | | | | | | |_ |  __/ |
;;    \_/\_/ \___/|_|_|  \___| |___/ |_|_| |_|_|\__(_)___|_|


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

(defun recursive-add-to-load-path (base)
 (let* ((dir (concat user-emacs-directory base))
        (default-directory dir))
  (add-to-list 'load-path dir)
  (normal-top-level-add-subdirs-to-load-path)))


;; TODO: No clue where this should live
(custom-set-variables '(warning-suppress-types '((comp))))

;; Core packages
(recursive-add-to-load-path "base")
(require 'vars)
(require 'ui)
(require 'settings)
(require 'modeline)
(require 'functions)
(require 'org-config)
(require 'keymaps)
(require 'project-settings)

;; Optional packages
(recursive-add-to-load-path "extensions")
(require 'vars)
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
(require 'docker)
(require 'org-tree-slide)
(require 'yaml)

;; Load last packages
(require 'utilities)
(require 'completion)
(require 'backups)
