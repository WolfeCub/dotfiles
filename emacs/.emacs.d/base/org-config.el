;;; org-config.el -*- lexical-binding: t; -*-

(use-package org
  :straight nil
  :defer 60
  :mode ("\\.org\\'" . org-mode)
  :config
  ;; Basic quality of life settings
  (setq org-pretty-entities t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-highlight-latex-and-related '(latex)
        org-enforce-todo-dependencies t
        org-agenda-use-time-grid nil
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-ellipsis wolfe/org-ellipsis
        org-entities-user '(("bot" "\\bot" nil "" "" "" "⊥")
                            ("square" "$\\square$" nil "" "" "" "□")))


  (defun wolfe/org-tags-compute-width ()
    (- (floor (* 0.6 (frame-width)))))

  (add-hook 'org-mode-hook
            (lambda ()
              (setq org-tags-column (wolfe/org-tags-compute-width))
              (org-align-all-tags)))
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq org-agenda-tags-column (wolfe/org-tags-compute-width))
              (org-agenda-align-tags)))

  (defun wolfe/save-org-archive-buffers (orig-fun &rest args)
    (save-some-buffers 'no-confirm
                       (lambda ()
                         (string-match "_archive\\'" buffer-file-name))))

  (advice-add 'org-archive-subtree :after 'wolfe/save-org-archive-buffers)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (  dot . t)))

  (global-set-key "\C-cl" 'org-store-link)

  ;; ispell ignores SRC blocks
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_LATEX" . "#\\+END_LATEX"))

  ;; Refresh images after executing a src block
  (add-hook 'org-babel-after-execute-hook
            (lambda ()
              (when org-inline-image-overlays
                (org-redisplay-inline-images))))

  (defun wolfe/confirm-babel-evaluate (lang body)
    (not (string= lang "dot")))
  (setq org-confirm-babel-evaluate 'wolfe/confirm-babel-evaluate)

  ;; Open PDFs with zathura
  (add-hook 'org-mode-hook
            (lambda ()
              (setq org-file-apps
                    (append '(("\\.pdf\\'" . "zathura \"%s\"")) org-file-apps))))

  ;; Replaces the asterisks with nice UTF-8 bullets.
  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


  ;; Create TODOs anywhere
  (global-set-key "\C-cc" 'org-capture)

  (setq org-default-notes-file (concat wolfe/org-nextcloud-path "everything.org"))
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "" "Need Refiling")
           "* TODO %?\n  DEADLINE: %t\n")))

  ;; Setup htmlize for syntax highlighting on export and add the apppropriate
  ;; minted package for PDF export.
  (use-package htmlize)
  (use-package ox-latex :straight nil)

  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-export-allow-bind-keywords t
        org-latex-listings 'minted)
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

;; Pull org-argenda in from org mode
(use-package org-agenda
  :straight nil
  :after org
  :bind (("C-c a" . org-agenda)
         :map org-agenda-mode-map
         ("j" . org-agenda-next-item)
         ("k" . org-agenda-previous-item))
  :config
  ;; Formats the agenda into nice columns
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12t% s %-12(car (last (org-get-outline-path)))")
          (timeline . "  % s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))

  ;; Sets location of org files
  (setq org-agenda-files `(,(concat wolfe/org-nextcloud-path "everything.org")))
  (setq browse-url-browser-function 'browse-url-chromium))

(use-package org-babel
  :straight nil
  :after org)

(provide 'org-config)
