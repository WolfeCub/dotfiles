;;; backups.el -*- lexical-binding: t; -*-

(setq backup-by-copying t) ; Stop shinanigans with links
(setq backup-directory-alist '((".*" . "~/.bak.emacs/backup/")))
;; Creates directory if it doesn't already exist
(make-directory "~/.bak.emacs/" t)
;; Creates auto directory if it doesn't already exist
(make-directory "~/.bak.emacs/auto" t)
;; backup in one place. flat, no tree structure
(setq auto-save-file-name-transforms '((".*" "~/.bak.emacs/auto/" t)))

(provide 'backups)
