;;; ob-haskell.el --- Babel Functions for Haskell    -*- lexical-binding: t; -*-

(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("haskell" . "hs"))

(defvar org-babel-default-header-args:haskell
  '((:padlines . "no")))

(defun org-babel-haskell-execute (body params)
  "This function should only be called by `org-babel-execute:haskell'"
  (let* ((tmp-src-file (org-babel-temp-file "Haskell-src-" ".hs")))
    (with-temp-file tmp-src-file
      (insert body))
    (org-babel-eval (concat "stack runghc -- " tmp-src-file) "")))

(defun org-babel-execute:haskell (body params)
  "Execute a block of Haskell code."
  (org-babel-haskell-execute body params))

(provide 'my-ob-haskell)

;;; ob-haskell.el ends here
