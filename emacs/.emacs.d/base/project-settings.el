;;; project-settings.el -*- lexical-binding: t; -*-

(use-package ag
  :bind (:map ag-mode-map
              ("Q" . wolfe/ag-kill-buffers-and-window)))

(use-package magit
  :defer 30
  :defer-incrementally dash f s with-editor git-commit package eieio lv transient
  :config
  (setq magit-bury-buffer-function
        (lambda (con)
          (kill-buffer-and-window))))

(use-package projectile
  :general
  (wolfe/bind-leader
    "p"   '(nil                             :wk "Projectile")
    "p f" '(projectile-find-file            :wk "Find File")
    "p d" '(projectile-find-dir             :wk "Find Directory")
    "p S" '(projectile-kill-buffers         :wk "Switch & Clear")
    "p k" '(projectile-kill-buffers         :wk "Kill Buffers")
    "p g" '(consult-git-grep                :wk "Git Grep")
    "p o" '(projectile-multi-occur          :wk "Multi Occur")
    "p +" '(projectile-add-known-project    :wk "Add Project")
    "p i" '(projectile-invalidate-cache     :wk "Invalidate Cache"))

  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file)
  :config
  (setq projectile-enable-caching nil)
  (setq projectile-indexing-method 'alien)
  (setq projectile-git-submodule-command nil)

  (setq projectile-globally-ignored-file-suffixes
        '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar" "*.class"))
  (setq projectile-globally-ignored-directories
        '(".git" "node_modules" "__pycache__" ".vs"))
  (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
  (projectile-mode))

;; Automatically =grep= or =ag= through a project for a definition. This
;; is useful when semantic jump to definition or =TAGS= files aren't present
;; or don't exist for the language.
(use-package dumb-jump
  :bind
  (:map evil-normal-state-map
        ("g D" . dumb-jump-go)))

(use-package perspective
  :commands projectile-persp-switch-project
  :general
  (wolfe/bind-leader
    "m"   'persp-switch-to-buffer*
    "M"   `(,(lambda () (interactive)
               (setq current-prefix-arg '(4)) ; C-u
               (call-interactively 'persp-switch-to-buffer*))
            :wk "All Buffers")
    "k"   'persp-kill-buffer*
    "p s" '(projectile-persp-switch-project :wk "Switch Project")
    ;; Persp prefix bindings
    "TAB"   '(nil               :wk "Perspective")
    "TAB s" '(persp-switch      :wk "Switch")
    "TAB k" '(persp-kill        :wk "Kill")
    "TAB r" '(persp-rename      :wk "Rename")
    "TAB b" '(persp-switch-last :wk "Switch Last")
    "TAB n" '(persp-new         :wk "New"))
  :config
  (set-face-attribute 'persp-selected-face nil
                      :foreground (face-attribute 'match :foreground))
  (setq persp-sort 'access
        persp-modestring-short t)
  (persp-mode)
  (add-hook 'persp-created-hook
            (lambda ()
              (persp-add-buffer "*Messages*"))))

(use-package persp-projectile
  :after perspective projectile)

(use-package consult-lsp
  :after (lsp consult))

(provide 'project-settings)
