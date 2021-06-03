;;; core.el --- Much of this is inspired/taken from doom -*- lexical-binding: t; -*-

;; Ensure everything in core is visible for loading
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'core-vars)

;; It may also be wise to raise gc-cons-threshold while the minibuffer is active,
;; so the GC doesn't slow down expensive commands (or completion frameworks, like
;; helm and ivy. The following is taken from doom-emacs
(defun defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold wolfe/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection-h)

;;
;;; Optimizations from Doom Emacs

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it there anyway, just in case. This increases
;; memory usage, however!
(setq inhibit-compacting-font-caches t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when wolfe/windows?
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster IPC
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless wolfe/linux? (setq command-line-x-option-alist nil))

;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason; inexplicably doubling startup time for terminal Emacs. Keeping
;;      it disabled will have nasty side-effects, so we simply delay it instead,
;;      and invoke it later, at which point it runs quickly; how mysterious!
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
    (defun doom-init-tty-h ()
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t))))


;; If we're starting in daemon mode then add func as hook for later
;; otherwise we can just run it right away
(defun hook-if-daemonp (func)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (funcall func))))
    (funcall func)))

(hook-if-daemonp
 (lambda ()
   (tool-bar-mode -1) ; No toolbar
   (scroll-bar-mode -1) ; Hide scrollbars
   (horizontal-scroll-bar-mode -1)
   (menu-bar-mode -1))) ; No menubar

(unless after-init-time
  ;; prevent flash of unstyled modeline at startup
  (setq-default mode-line-format nil))

;; TODO: No clue where this should live
(custom-set-variables '(warning-suppress-types '((comp))))

(defun recursive-add-to-load-path (base)
 (let* ((dir (concat user-emacs-directory base))
        (default-directory dir))
  (add-to-list 'load-path dir)
  (normal-top-level-add-subdirs-to-load-path)))


;;
;;; Custom hooks from doom.
(defvar wolfe/first-input-hook nil
  "Transient hooks run before the first user input.")
(put 'wolfe/first-input-hook 'permanent-local t)

(defvar wolfe/first-file-hook nil
  "Transient hooks run before the first interactively opened file.")
(put 'wolfe/first-file-hook 'permanent-local t)

(defvar wolfe/first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")
(put 'wolfe/first-buffer-hook 'permanent-local t)

(defun wolfe/run-hook-on (hook-var trigger-hooks)
  "Configure HOOK-VAR to be invoked exactly once when any of the TRIGGER-HOOKS
are invoked *after* Emacs has initialized (to reduce false positives). Once
HOOK-VAR is triggered, it is reset to nil.
HOOK-VAR is a quoted hook.
TRIGGER-HOOK is a list of quoted hooks and/or sharp-quoted functions."
  (dolist (hook trigger-hooks)
    (let ((fn (intern (format "%s-init-on-%s-h" hook-var hook))))
      (fset
       fn (lambda (&rest _)
            ;; Only trigger this after Emacs has initialized.
            (when (and after-init-time
                       (or (daemonp)
                           ;; In some cases, hooks may be lexically unset to
                           ;; inhibit them during expensive batch operations on
                           ;; buffers (such as when processing buffers
                           ;; internally). In these cases we should assume this
                           ;; hook wasn't invoked interactively.
                           (and (boundp hook)
                                (symbol-value hook))))
              (run-hooks hook-var)
              (set hook-var nil))))
      (cond ((daemonp)
             ;; In a daemon session we don't need all these lazy loading
             ;; shenanigans. Just load everything immediately.
             (add-hook 'after-init-hook fn 'append))
            ((eq hook 'find-file-hook)
             ;; Advise `after-find-file' instead of using `find-file-hook'
             ;; because the latter is triggered too late (after the file has
             ;; opened and modes are all set up).
             (advice-add 'after-find-file :before fn '((depth . -101))))
            ((add-hook hook fn -101)))
      fn)))

(defun wolfe/initialize ()
  (require 'core-modules)
  (add-hook 'emacs-startup-hook #'wolfe/load-packages-incrementally-h)

  ;; TODO: Replicate switch buffer hook
  (wolfe/run-hook-on 'wolfe/first-buffer-hook '(find-file-hook doom-switch-buffer-hook))
  (wolfe/run-hook-on 'wolfe/first-file-hook   '(find-file-hook dired-initial-position-hook))
  (wolfe/run-hook-on 'wolfe/first-input-hook  '(pre-command-hook)))

(provide 'base)
