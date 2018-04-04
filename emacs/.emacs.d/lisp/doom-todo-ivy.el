;;; -*- lexical-binding: t; -*-

(defvar doom/ivy-buffer-icons nil
  "If non-nil, show buffer mode icons in `ivy-switch-buffer' and the like.")

(defvar doom/ivy-task-tags
  '(("TODO"  . warning)
    ("XXX" . warning)
    ("BUG" . error)
    ("FIXME" . error))
  "An alist of tags for `doom/ivy-tasks' to include in its search, whose CDR is the
face to render it with.")

(defun doom/ivy--tasks-candidates (tasks)
  "Generate a list of task tags (specified by `doom/ivy-task-tags') for
`doom/ivy-tasks'."
  (let* ((max-type-width
          (cl-loop for task in doom/ivy-task-tags maximize (length (car task))))
         (max-desc-width
          (cl-loop for task in tasks maximize (length (cl-cdadr task))))
         (max-width (max (- (frame-width) (1+ max-type-width) max-desc-width)
                         25)))
    (cl-loop
     with fmt = (format "%%-%ds %%-%ds%%s%%s:%%s" max-type-width max-width)
     for alist in tasks
     collect
     (let-alist alist
       (format fmt
               (propertize .type 'face (cdr (assoc .type doom/ivy-task-tags)))
               (string-trim (substring .desc 0 (min max-desc-width (length .desc))))
               (propertize " | " 'face 'font-lock-comment-face)
               (propertize (abbreviate-file-name .file) 'face 'font-lock-keyword-face)
               (propertize .line 'face 'font-lock-constant-face))))))

(defun doom/ivy--tasks (target)
  (let* (case-fold-search
         (task-tags (mapcar #'car doom/ivy-task-tags))
         (cmd
          (format "%s -H -S --noheading -- %s %s"
                  (or (when-let* ((bin (executable-find "rg")))
                        (concat bin " --line-number"))
                      (when-let* ((bin (executable-find "ag")))
                        (concat bin " --numbers"))
                      (error "ripgrep & the_silver_searcher are unavailable"))
                  (shell-quote-argument
                   (concat "\\s("
                           (string-join task-tags "|")
                           ")([\\s:]|\\([^)]+\\):?)"))
                  target)))
    (save-match-data
      (cl-loop with out = (replace-regexp-in-string "c:" "" (replace-regexp-in-string "\\\\" "/" (shell-command-to-string cmd)))
               for x in (and out (split-string out "\n" t))
               when (condition-case-unless-debug ex
                      (string-match
                       (concat "^\\([^:]+\\):\\([0-9]+\\):.+\\("
                               (string-join task-tags "\\|")
                               "\\):?\\s-*\\(.+\\)")
                       x)
                      (error
                       (message! (red "Error matching task in file: (%s) %s"
                                      (error-message-string ex)
                                      (car (split-string x ":"))))
                       nil))
               collect `((type . ,(match-string 3 x))
                         (desc . ,(match-string 4 x))
                         (file . ,(match-string 1 x))
                         (line . ,(match-string 2 x)))))))

(defun doom/ivy--tasks-open-action (x)
  "Jump to the file and line of the current task."
  (let ((location (cadr (split-string x " | ")))
        (type (car (split-string x " "))))
    (cl-destructuring-bind (file line) (split-string location ":")
      (with-ivy-window
        (find-file (expand-file-name file (projectile-project-root)))
        (goto-char (point-min))
        (forward-line (1- (string-to-number line)))
        (search-forward type (line-end-position) t)
        (backward-char (length type))
        (recenter)))))

;;;###autoload
(defun doom/ivy-tasks (&optional arg)
  "Search through all TODO/FIXME tags in the current project. If ARG, only
search current file. See `doom/ivy-task-tags' to customize what this searches for."
  (interactive "P")
  (ivy-read (format "Tasks (%s): "
                    (if arg
                        (concat "in: " (file-relative-name buffer-file-name))
                      "project"))
            (doom/ivy--tasks-candidates
             (doom/ivy--tasks (if arg buffer-file-name (projectile-project-root))))
            :action #'doom/ivy--tasks-open-action
            :caller 'doom/ivy-tasks))

(provide 'doom-todo-ivy)
