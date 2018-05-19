(use-package exwm)
(require 'exwm-config)
(require 'exwm-systemtray)
(exwm-systemtray-enable)

(defmacro wolfe/exwm-bind (key command)
  `(exwm-input-set-key ,(kbd key) ,command))

(defmacro wolfe/exwm-bind-lambda (key command)
  `(exwm-input-set-key ,(kbd key) (lambda () (interactive) ,command)))

(defmacro wolfe/exwm-bind-exec (key cmd)
  `(wolfe/exwm-bind-lambda ,key (start-process-shell-command ,cmd nil ,cmd)))

(fringe-mode 1)
(display-time-mode t) ;Show the time
(setq battery-mode-line-format "[%p]")
(display-battery-mode t)

;; Set the initial workspace number.
(setq exwm-workspace-number 4)
;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda () (exwm-workspace-rename-buffer exwm-class-name)))

;; 's-r': Reset
(wolfe/exwm-bind "s-r" #'exwm-reset)
;; 's-w': Switch workspace
(wolfe/exwm-bind "s-w" #'exwm-workspace-switch)
;; 's-c': Browser
(wolfe/exwm-bind-exec "s-c" "firefox-developer-edition")
;; 's-RET': Open terminal
(wolfe/exwm-bind-exec "s-<return>" "urxvt")

;; 's-N': Switch to certain workspace
(let ((workspace-list '("&" "[" "{" "}" "(" "=" "*" ")" "+" "]")))
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%s" (nth i workspace-list)))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i)))))

;; 's-&': Launch application
(exwm-input-set-key (kbd "s-SPC")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))

(exwm-input-set-simulation-keys '(([?\C-c ?\C-c] . ?\C-c)
                                  ([?\C-x ?\C-x] . ?\C-x)))

(wolfe/exwm-bind-exec "<XF86MonBrightnessUp>"   "xbacklight -inc 2")
(wolfe/exwm-bind-exec "<XF86MonBrightnessDown>" "xbacklight -dec 2")
(wolfe/exwm-bind-exec "<XF86KbdBrightnessUp>"   "asus-kbd-backlight up")
(wolfe/exwm-bind-exec "<XF86KbdBrightnessDown>" "asus-kbd-backlight down")
(wolfe/exwm-bind-exec "<XF86AudioRaiseVolume>"  "amixer -D pulse sset Master 2%+")
(wolfe/exwm-bind-exec "<XF86AudioLowerVolume>"  "amixer -D pulse sset Master 2%-")
(wolfe/exwm-bind-exec "<XF86AudioMute>"         "amixer set Master toggle")

;; Line-editing shortcuts
;;(setq exwm-input-simulation-keys
;;      '(([?\C-b] . [left])
;;        ([?\C-f] . [right])
;;        ([?\C-p] . [up])
;;        ([?\C-n] . [down])
;;        ([?\C-a] . [home])
;;        ([?\C-e] . [end])
;;        ([?\M-v] . [prior])
;;        ([?\C-v] . [next])
;;        ([?\C-d] . [delete])
;;        ([?\C-k] . [S-end delete]))

;; Enable EXWM
(exwm-enable)
