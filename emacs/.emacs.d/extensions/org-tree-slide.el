;;; org-tree-slide.el -*- lexical-binding: t; -*-

(use-package org-tree-slide
  :defer t
  :after org
  :commands org-tree-slide-mode
  :config
  (evil-define-key 'normal org-tree-slide-mode-map
    (kbd "C-n") 'org-tree-slide-move-next-tree
    (kbd "C-p") 'org-tree-slide-move-previous-tree)

  (defun wolfe/org-start-presentation ()
    (interactive)
    (org-tree-slide-simple-profile)
    (org-tree-slide-mode 1)))

  



(provide 'org-tree-slide)
