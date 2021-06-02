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

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

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

;; If we're starting in daemon mode then add func as hook for later
;; otherwise we can just run it right away
(defun hook-if-daemonp (func)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (funcall func))))
    (funcall func)))

(hook-if-daemonp
 (lambda ()
   (tool-bar-mode -1) ; No toolbar
   (scroll-bar-mode -1) ; Hide scrollbars
   (horizontal-scroll-bar-mode -1)
   (menu-bar-mode -1))) ; No menubar
