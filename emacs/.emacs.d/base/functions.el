;;; functions.el -*- lexical-binding: t; -*-

(defun what-face (pos)
  "Returns the font lock face that's under the cursor."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun wolfe/compile-no-prompt ()
  "Compiles the project without a prompt."
  (interactive)
  (let ((compilation-read-command nil))
    (compile (eval compile-command))))

(defun wolfe/compile-dot-emacs ()
  "Compiles all el files in `wolfe/byte-compile-dirs'."
  (interactive)
  (byte-compile-file wolfe/init-file)
  (mapc (lambda (dir) (byte-recompile-directory dir 0)) wolfe/byte-compile-dirs))


(defun wolfe/clear-all-elc ()
  "Delete all elc files in .emacs user files"
  (interactive)
  (shell-command
   (format "find %s -name \"*.elc\" -type f -delete"
           (s-join " " wolfe/byte-compile-dirs))))

(defun wolfe/remove-elc-on-save ()
  "If you're saving an emacs-lisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil t))
(add-hook 'emacs-lisp-mode-hook 'wolfe/remove-elc-on-save)

(defun wolfe/find-tag ()
  "Jump to the tag at point without prompting"
  (interactive)
  (find-tag (find-tag-default)))

(defun wolfe/create-tags ()
  "Create the tags table"
  (interactive)
  (save-window-excursion (shell-command "etags -R -o ETAGS *")))

(defadvice xref-find-definitions (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
     If buffer is modified, ask about save before running etags."
  (condition-case err
      ad-do-it
    (error (and (buffer-modified-p) (not (ding))
                (save-buffer))
           (save-window-excursion (shell-command "etags -R *"))
           ad-do-it)))

(defun wolfe/controlz ()
  "Fixes C-z in terminal"
  (interactive)
  (when (eq (display-graphic-p) nil)
    (suspend-frame)))

(defun wolfe/org-open (name)
  "Opens the file in the nextcloud path"
  (interactive)
  (evil-buffer-new nil (concat wolfe/org-nextcloud-path name ".org")))

(defun wolfe/load-init ()
  "Reloads init file"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
  Dwim means: region, org-src-block, org-subtree, or
  defun, whichever applies first. Narrowing to
  org-src-block actually calls `org-edit-src-code'.

  With prefix P, don't widen, just narrow even if buffer
  is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(defun wolfe/man ()
  "Use the real man if available otherwise just use the emacs woman."
  (if (executable-find "man")
      (man (word-at-point))
    (woman)))

;; Is used in wolfe/hydra-org-expand=. For inserting org-templates.
(defun hot-expand (str &optional additional-text)
  "Expand org template."
  (insert str)
  (org-try-structure-completion)
  (when additional-text
    (insert additional-text)
    (forward-line)))

(defun wolfe/if-else-projectile (if-function else-function)
  "Calls the if-function if inside a project otherwise
  it calls the else-function"
  (interactive)
  (if (projectile-project-p)
      (call-interactively if-function)
    (call-interactively else-function)))

(defun wolfe/projectile-invalidate-list ()
  "Select project from list of projectile projects to invalidate."
  (interactive)
  (projectile-invalidate-cache t))

(defun wolfe/ag-kill-buffers-and-window ()
  "Kill all the open ag buffers and delete the window I'm in. Bound in ag-mode-map"
  (interactive)
  (ag-kill-buffers)
  (delete-window))

(defun wolfe/eval-and-replace (beginning end)
  "Replace the preceding sexp or region with its value."
  (interactive "r")
  (if (region-active-p)
      (delete-region beginning end)
    (backward-kill-sexp))
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun wolfe/call-and-update-ex (fun)
  "Calls the function and updates `evil-ex-history' with the result"
  (interactive)
  (setq evil-ex-history (cons (format "e %s" (funcall fun)) evil-ex-history)))

(provide 'functions)
