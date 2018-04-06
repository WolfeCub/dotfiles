(use-package exwm
  :config
  (require 'exwm-config))

;; Set the initial workspace number.
(setq exwm-workspace-number 4)
;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))
;; 's-r': Reset
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
;; 's-w': Switch workspace
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
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
;; Line-editing shortcuts
(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])))

;; Enable EXWM
(exwm-enable)
;; Other configurations
(fringe-mode 1)
