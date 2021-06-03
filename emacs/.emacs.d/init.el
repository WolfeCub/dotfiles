;;; init.el -*- lexical-binding: t; -*-
;; __        __    _  __      _       _       _ _         _
;; \ \      / /__ | |/ _| ___( )___  (_)_ __ (_) |_   ___| |
;;  \ \ /\ / / _ \| | |_ / _ \// __| | | '_ \| | __| / _ \ |
;;   \ V  V / (_) | |  _|  __/ \__ \ | | | | | | |_ |  __/ |
;;    \_/\_/ \___/|_|_|  \___| |___/ |_|_| |_|_|\__(_)___|_|


;; If for some reason early init hasn't loaded do it now
(unless (boundp 'wolfe/gc-cons-threshold)
  (load (concat user-emacs-directory "early-init") nil t))

(wolfe/initialize)

(use-package general
  :demand t
  :config
  (general-auto-unbind-keys)
  (general-create-definer wolfe/bind-leader
    :keymaps 'global
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"))

(wolfe! :base
        ui
        settings
        modeline
        functions
        org-config
        keymaps
        project-settings

        :extensions
        web
        javascript
        lisp
        latex
        c-cpp
        csharp
        fsharp
        py
        haskell
        elixir
        docker
        org-tree-slide
        yaml
        org-babel

        :base
        utilities
        incremental-narrowing
        completion
        backups)

