;;; highlight-escape-chars.el -*- lexical-binding: t; -*-

(defface wolfe/backslash-escape-backslash-face
  '((t :inherit font-lock-regexp-grouping-backslash))
  "Face for the back-slash component of a back-slash escape."
  :group 'font-lock-faces)

(defface wolfe/backslash-escape-char-face
  '((t :inherit font-lock-regexp-grouping-construct))
  "Face for the charcter component of a back-slash escape."
  :group 'font-lock-faces)

(defface wolfe/format-code-format-face
  '((t :inherit font-lock-regexp-grouping-backslash))
  "Face for the % component of a printf format code."
  :group 'font-lock-faces)

(defface wolfe/format-code-directive-face
  '((t :inherit font-lock-regexp-grouping-construct))
  "Face for the directive component of a printf format code."
  :group 'font-lock-faces)

(mapc
 (lambda (mode)
   (font-lock-add-keywords
    mode
    '(("\\(\\\\\\)." 1 'wolfe/backslash-escape-backslash-face prepend)
      ("\\\\\\(.\\)" 1 'wolfe/backslash-escape-char-face      prepend)
      ("\\(%\\)."    1 'wolfe/format-code-format-face         prepend)
      ("%\\(.\\)"    1 'wolfe/format-code-directive-face      prepend))))
 wolfe/highlight-escapes-mode-list)

(provide 'highlight-escape-chars)
