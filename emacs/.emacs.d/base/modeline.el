;;; modeline.el -*- lexical-binding: t; -*-

(if (or (display-graphic-p) (daemonp))
    (use-package doom-modeline
      :hook (after-init . doom-modeline-mode)
      :config
      (setq doom-modeline-height 1)
      (set-face-attribute 'mode-line nil :height 125)
      (set-face-attribute 'mode-line-inactive nil :height 125))
  (setq-default
   mode-line-format
   (list
    " "
    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (propertize "RO"
                          'face 'font-lock-type-face
                          'help-echo "Buffer is read-only")))

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (propertize "M"
                          'face 'font-lock-warning-face
                          'help-echo "Buffer has been modified")))

    " "
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                        'help-echo (buffer-file-name)))


    ;; the current major mode for the buffer.
    "["

    '(:eval (propertize (format-mode-line mode-name) 'face '(:family "Arial")
                        'help-echo buffer-file-coding-system))
    '(:eval (propertize (format-mode-line minor-mode-alist)
                        'face '(:family "Arial")))
    "]             "

    ;; line and column
    "(" ;; '%02' to set to 2 chars at least; prevents flickering
    (propertize "%02l" 'face 'font-lock-type-face) ","
    (propertize "%02c" 'face 'font-lock-type-face)
    ") "

    ;; relative position, size of file
    "["
    (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
    "/"
    (propertize "%I" 'face 'font-lock-constant-face) ;; size
    "] "

    ;; add the time, with the date and the emacs uptime in the tooltip
    '(:eval (propertize (format-time-string "%H:%M")
                        'help-echo
                        (concat (format-time-string "%c; ")
                                (emacs-uptime "Uptime:%hh")))))))

(provide 'modeline)
