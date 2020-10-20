;;; popups.el -*- lexical-binding: t; -*-

(use-package hydra)
(use-package major-mode-hydra)

;; Major modes
(major-mode-hydra-define ("web-mode" "typescript-mode") nil
  ("Find"
   (("d" lsp-goto-implementation         "Goto implementation")
    ("r" lsp-find-references             "Find references")
    ("o" lsp-describe-thing-at-point     "Describe thing"))

   "Peek"
   (("D" lsp-ui-peek-find-implementation "Peek implementation")
    ("R" lsp-ui-peek-find-references     "Peek references"))

   "Fix/Refactor"
   (("n" lsp-rename                     "Rename symbol")
    ("f" lsp-format-region              "Format region")
    ("x" lsp-execute-code-action        "Execute code action"))))

;; Minor modes
(setq wolfe/hydra-projectile
      (pretty-hydra-define hydra-projectile (:exit t :hint nil)
        ("Files" (("f" counsel-projectile-find-file        "Find File")
                  ("d" counsel-projectile-find-dir         "Find Directory")
                  ("s" counsel-projectile-switch-project   "Switch Project")
                  ("S" projectile-kill-buffers             "Switch & Clear"))

         "Buffers" (("k" projectile-kill-buffers             "Kill Buffers"))

         "Search" (("a" projectile-ag                      "Ag")
                   ("A" counsel-projectile-ag              "Counsel Ag")
                   ("o" projectile-multi-occur             "Multi Occur"))

         "Cache" (("c" projectile-invalidate-cache         "Clear Cache")
                  ("C" wolfe/projectile-invalidate-list    "Clear A Cache")
                  ("P" projectile-clear-known-projects     "Clear Projects")))))

(provide 'popups)
