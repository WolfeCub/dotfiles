;;; incremental-narrowing.el -*- lexical-binding: t; -*-

(use-package prescient
  :config
  (setq consult-preview-key (kbd "M-."))
  (prescient-persist-mode))

(use-package consult
  :demand t
  :bind ("C-s" . consult-line)
  :config
  (setq consult-project-root-function 'projectile-project-root))

(defun selectrum-fido-backward-updir ()
  "Delete char before or go up directory, like `ido-mode'."
  (interactive)
  (if (and (eq (char-before) ?/)
           (eq (selectrum--get-meta 'category) 'file))
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (point-min) t)
          (delete-region (1+ (point)) (point-max))))
    (call-interactively 'backward-delete-char)))

(defun selectrum-fido-ret ()
  "Exit minibuffer or enter directory, like `ido-mode'."
  (interactive)
  (let* ((dir (and (eq (selectrum--get-meta 'category) 'file)
                   (file-name-directory (minibuffer-contents))))
         (current (selectrum-get-current-candidate))
         (probe (and dir current
                     (expand-file-name (directory-file-name current) dir))))
    (cond ((and probe (file-directory-p (substitute-in-file-name probe)) (not (string= current "./")))
           (selectrum-insert-current-candidate))
          (t
           (selectrum-select-current-candidate)))))

(use-package selectrum
  :after consult
  :demand t
  :bind
  (:map selectrum-minibuffer-map
        ("RET" . selectrum-fido-ret)
        ("DEL" . selectrum-fido-backward-updir))
  :general                               
  (wolfe/bind-leader                     
    "r"   '(selectrum-repeat :wk "Selectrum Repeat"))
  :config
  (selectrum-mode))

(use-package selectrum-prescient
  :after (prescient selectrum)
  :demand t
  :custom-face
  (selectrum-current-candidate
   ((t (:foreground ,(plist-get base16-default-dark-colors :base09)
        :background ,(plist-get base16-default-dark-colors :base01)))))
  (selectrum-prescient-primary-highlight
   ((t (:foreground ,(plist-get base16-default-dark-colors :base0D)))))
  :config
  (selectrum-prescient-mode)

  (defun selectrum-highlight-candidates-function+ (input cands)
    (let ((cands (if (eq 'file (completion-metadata-get
                                (completion-metadata
                                 input
                                 minibuffer-completion-table
                                 minibuffer-completion-predicate)
                                'category))
                     (cl-loop for cand in cands
                              for len = (length cand)
                              if (and (> len 0)
                                      (eq (aref cand (1- len)) ?/))
                              collect (progn (add-face-text-property
                                              0 (length cand)
                                              'dired-directory
                                              'append cand)
                                             cand)
                              else
                              collect cand)
                   cands)))
      (selectrum-prescient--highlight input cands)))

  (setq selectrum-highlight-candidates-function
        #'selectrum-highlight-candidates-function+))


(provide 'incremental-narrowing)
