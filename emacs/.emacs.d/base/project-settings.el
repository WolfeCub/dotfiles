;;; project-settings.el -*- lexical-binding: t; -*-

(use-package ag
  :bind (:map ag-mode-map
              ("Q" . wolfe/ag-kill-buffers-and-window)))

(use-package magit
  :defer 10
  :config
  (use-package evil-magit)
  (setq magit-bury-buffer-function
        (lambda (con)
          (kill-buffer-and-window))))

(use-package projectile
  :config
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-git-submodule-command nil)

  (setq projectile-globally-ignored-file-suffixes
        '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar" "*.class"))
  (setq projectile-globally-ignored-directories
        '(".git" "node_modules" "__pycache__" ".vs"))
  (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
  (projectile-mode))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

;; Automatically =grep= or =ag= through a project for a definition. This
;; is useful when semantic jump to definition or =TAGS= files aren't present
;; or don't exist for the language.
(use-package dumb-jump
  :bind
  (:map evil-normal-state-map
        ("g D" . dumb-jump-go))
  :config
  (setq dumb-jump-selector 'ivy))

(use-package persp-mode
  :hook (after-init . (lambda () (persp-mode 1)))
  :config
  (defvar wolfe/persp-default-workspace "main")
  (defvar wolfe/persp-shared-buffers '("*scratch*" "*Messages*"))
  (defvar wolfe/projectile-project-to-switch nil)

  (setq wg-morph-on nil ;; switch off animation
        persp-autokill-buffer-on-remove 'kill-weak
        persp-auto-save-opt 0
        persp-auto-resume-time -1
        persp-nil-hidden t
        persp-add-buffer-on-find-file t
        persp-add-buffer-on-after-change-major-mode t
        persp-hook-up-emacs-buffer-completion t)

  ;; Make ivy play nice
  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers
              #'(lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (not (persp-contain-buffer-p b persp))
                        nil)))))
    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch . nil)))))

  (defun wolfe/projectile-switch-project-by-name (counsel-projectile-switch-project-by-name &rest args)
    (setq wolfe/projectile-project-to-switch (car args))
    (apply counsel-projectile-switch-project-by-name args)
    (setq wolfe/projectile-project-to-switch nil))
  (advice-add #'counsel-projectile-switch-project-by-name :around #'wolfe/projectile-switch-project-by-name)

  (defun wolfe/persp-create-project-persp ()
    (let ((frame (selected-frame))
          (name (file-name-nondirectory
                 (directory-file-name
                  (file-name-directory
                   wolfe/projectile-project-to-switch)))))
      (with-selected-frame frame
        (persp-add-new name)
        (persp-frame-switch name)
        (persp-add-buffer wolfe/persp-shared-buffers (get-current-persp) nil))))

  (add-hook 'projectile-before-switch-project-hook 'wolfe/persp-create-project-persp)

  (defun wolfe/persp-concat-name (count)
    (if (eq count 0)
        wolfe/persp-default-workspace
      (format "%s-%s" wolfe/persp-default-workspace count)))

  (defun wolfe/persp-next-main-name (&optional count)
    (let ((count (or count 0)))
      (if (persp-with-name-exists-p (wolfe/persp-concat-name count))
          (wolfe/persp-next-main-name (+ count 1))
        (wolfe/persp-concat-name count))))

  (add-hook
   'after-make-frame-functions
   (lambda (frame)
     (let ((name (wolfe/persp-next-main-name)))
       (with-selected-frame frame
         (set-frame-parameter frame 'wolfe/persp-current-main name)
         (persp-add-new name)
         (persp-frame-switch name frame)
         (persp-add-buffer wolfe/persp-shared-buffers (get-current-persp) nil)))))

  (add-hook
   'delete-frame-functions
   (lambda (frame)
     (with-selected-frame frame
       (let* ((current-persp (get-current-persp))
              (current-persp-name (persp-name current-persp)))
         (when (not (string= 'wolfe/persp-current-main current-persp-name))
           (persp-kill current-persp-name)))
       (persp-kill (frame-parameter frame 'wolfe/persp-current-main))))))

(provide 'project-settings)
