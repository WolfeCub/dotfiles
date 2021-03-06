;;; core-vars.el -*- lexical-binding: t; -*-

(defvar wolfe/linux? (eq system-type 'gnu/linux)
  "Are we on linux?")

(defvar wolfe/windows? (eq system-type 'windows-nt)
  "Are we on windows?")

(defvar wolfe/org-ellipsis "⤵"
  "The indicates if an `org-mode' tree can be expanded")

(defvar wolfe/project-path
  (cond
   (wolfe/linux? "~/Projects/")
   (wolfe/windows? "C:/dev/")
   (:else nil))
  "Path to my projects directory")

(defvar wolfe/org-nextcloud-path
  (cond
   (wolfe/linux?
    "~/Nextcloud/Sync/org/")
   (wolfe/windows?
    (concat "C:\\Users\\" (getenv "USERNAME") "\\Nextcloud\\Sync\\org\\"))
   (:else nil))
  "Path to my org files inside nextcloud")

(defvar wolfe/using-dark t
  "Indicates whether we're using my dark theme")

(defvar wolfe/using-light nil
  "Indicates whether we're using my light theme")

(defvar wolfe/lisp-dir-path (concat user-emacs-directory "lisp/")
  "Path to my custom lisp files")

(defvar wolfe/init-file (concat user-emacs-directory "init.el")
  "Path to emacs init file")

(provide 'core-vars)
