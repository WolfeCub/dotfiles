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

(define-minor-mode highlight-escapes-mode
  "Highlight escape chars in this mode."
  :init-value nil
  :lighter "")

(add-hook 'highlight-escapes-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("\\(\\\\\\)." 1 'wolfe/backslash-escape-backslash-face prepend)
      ("\\\\\\(.\\)" 1 'wolfe/backslash-escape-char-face      prepend)
      ("\\(%\\)."    1 'wolfe/format-code-format-face         prepend)
      ("%\\(.\\)"    1 'wolfe/format-code-directive-face      prepend)))))

(provide 'highlight-escape-chars)
