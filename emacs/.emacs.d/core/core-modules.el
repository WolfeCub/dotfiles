;;; core-modules.el -*- lexical-binding: t; -*-

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
;; Install use-package and have it use straight
(straight-use-package 'use-package)

(use-package dash)
(use-package s)

;; Add :defer-incrementally to use-package
(dolist (keyword '(:defer-incrementally))
  (push keyword use-package-deferring-keywords)
  (setq use-package-keywords
        (use-package-list-insert keyword use-package-keywords :after)))

(defalias 'use-package-normalize/:defer-incrementally #'use-package-normalize-symlist)
(defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
  (use-package-concat
   `((wolfe/load-packages-incrementally
      ',(if (equal targets '(t))
            (list name)
          (append targets (list name)))))
   (use-package-process-keywords name rest state)))


(defmacro wolfe! (&rest modules)
  (let ((plist (copy-sequence modules))
        (key nil))
    (while plist
      (let ((curr (pop plist)))
        (if (keywordp curr)
            (setq key curr)
          (load-file (format "%s%s/%s.el" user-emacs-directory (string-trim-left (symbol-name key) ":") (symbol-name curr))))))))

;;
;;; Incremental lazy-loading from doom

(defvar wolfe/incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any large packages
here may cause noticeable pauses, so it's recommended you break them up into
sub-packages. For example, `org' is comprised of many packages, and can be
broken up into:
  (wolfe/load-packages-incrementally
   '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))
This is already done by the lang/org module, however.
If you want to disable incremental loading altogether, either remove
`wolfe/load-packages-incrementally-h' from `emacs-startup-hook' or set
`wolfe/incremental-first-idle-timer' to nil. Incremental loading does not occur
in daemon sessions (they are loaded immediately at startup).")

(defvar wolfe/incremental-first-idle-timer 2.0
  "How long (in idle seconds) until incremental loading starts.
Set this to nil to disable incremental loading.")

(defvar wolfe/incremental-idle-timer 0.75
  "How long (in idle seconds) in between incrementally loading packages.")

(defvar wolfe/incremental-load-immediately (daemonp)
  "If non-nil, load all incrementally deferred packages immediately at startup.")

;; The doom version of this doesn't remove 'req' from packages if it's already
;; loaded. I'm not sure if that's a bug. Here we just remove it if it's already
;; loaded so it can continue through the list.
(defun wolfe/load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.
If NOW is non-nil, load PACKAGES incrementally, in `wolfe/incremental-idle-timer'
intervals."
  (if (not now)
      (setq wolfe/incremental-packages (append wolfe/incremental-packages packages))
    (while packages
      (let* ((gc-cons-threshold most-positive-fixnum)
             (req (pop packages)))
        (unless (featurep req)
          (condition-case-unless-debug e
              (or (while-no-input
                    ;; If `default-directory' is a directory that doesn't exist
                    ;; or is unreadable, Emacs throws up file-missing errors, so
                    ;; we set it to a directory we know exists and is readable.
                    (let ((default-directory user-emacs-directory)
                          (inhibit-message t)
                          file-name-handler-alist)
                      (require req nil t))
                    t)
                  (push req packages))
            (error
             (message "Failed to load %S package incrementally, because: %s"
                      req e))))
        (if (not packages)
            (message "Finished incremental loading")
          (run-with-idle-timer wolfe/incremental-idle-timer
                               nil #'wolfe/load-packages-incrementally
                               packages t)
          (setq packages nil))))))

(defun wolfe/load-packages-incrementally-h ()
  "Begin incrementally loading packages in `wolfe/incremental-packages'.
If this is a daemon session, load them all immediately instead."
  (if wolfe/incremental-load-immediately
      (mapc #'require (cdr wolfe/incremental-packages))
    (when (numberp wolfe/incremental-first-idle-timer)
      (run-with-idle-timer wolfe/incremental-first-idle-timer
                           nil #'wolfe/load-packages-incrementally
                           (cdr wolfe/incremental-packages) t))))

(provide 'core-modules)
