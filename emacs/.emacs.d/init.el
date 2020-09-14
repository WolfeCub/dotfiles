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

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 800000
                           gc-cons-percentage 0.1
                           file-name-handler-alist wolfe/file-name-handler-alist
                           ad-redefinition-action 'warn)))

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

(add-to-list 'load-path (concat user-emacs-directory "base"))
(require 'vars)
(require 'ui)
(require 'settings)
(require 'functions)
(require 'org-config)
(require 'popups)
(require 'keymaps)
(require 'project-settings)

(add-to-list 'load-path (concat user-emacs-directory "extensions"))
(require 'web)
(require 'javascript)
(require 'lisp)
(require 'latex)
(require 'c-cpp)
(require 'csharp)
(require 'python)

(require 'utilities)
(require 'completion)
(require 'backups)
