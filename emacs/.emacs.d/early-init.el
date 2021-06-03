;;; early-init.el --- -*- lexical-binding: t -*-

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

;; Stop package.el from starting itself up
(setq package-enable-at-startup nil
      package--init-file-ensured t)

(load (concat user-emacs-directory "core/core") nil 'nomessage)
