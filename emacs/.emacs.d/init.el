;; __        __    _  __      _       _       _ _         _
;; \ \      / /__ | |/ _| ___( )___  (_)_ __ (_) |_   ___| |
;;  \ \ /\ / / _ \| | |_ / _ \// __| | | '_ \| | __| / _ \ |
;;   \ V  V / (_) | |  _|  __/ \__ \ | | | | | | |_ |  __/ |
;;    \_/\_/ \___/|_|_|  \___| |___/ |_|_| |_|_|\__(_)___|_|

;; Set garbage collect high to speed up startup
(let ((gc-cons-threshold most-positive-fixnum)
      (ad-redefinition-action 'accept)) ;; Ignore advice warnings
  (require 'package)
  ;; Setup package sources
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")))
  (setq package-enable-at-startup nil)
  (setq package-pinned-packages '((use-package . "melpa")))

  ;; Bootstrap use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t)

  ;; Use latest org before calling babel
  (use-package org
    :pin org
    :ensure org-plus-contrib)

  (org-babel-load-file "~/.emacs.d/README.org"))
