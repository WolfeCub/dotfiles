;;; popups.el -*- lexical-binding: t; -*-

(use-package hydra)
(use-package major-mode-hydra)

;; Major modes
(major-mode-hydra-define ("web-mode") nil
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

(major-mode-hydra-define csharp-mode nil
  ("Find"
   (("d" omnisharp-go-to-definition              "Goto definition")
    ("D" omnisharp-go-to-definition-other-window "Pop-open definition")
    ("u" omnisharp-find-usages                   "Find usages")
    ("p" omnisharp-find-implementations          "Find implementations"))

   "Fix/Refactor"
   (("r" omnisharp-rename                        "Rename symbol")
    ("f" omnisharp-run-code-action-refactoring   "Code action")
    ("i" omnisharp-code-format-region            "Indent region")
    ("I" omnisharp-code-format-entire-file       "Indent entire file"))

   "Solution"
   (("e" omnisharp-solution-errors               "Solution errors")
    ("a" omnisharp-add-to-solution-current-file  "Add current file to sln")
    ("s" omnisharp-reload-solution               "Reload solution"))))

(major-mode-hydra-bind python-mode "Python"
  ("i" elpy-importmagic-fixup "Importmagic fixup")
  ("d" elpy-goto-definition   "Goto definition")
  ("r" elpy-multiedit-python-symbol-at-point   "Rename symbol")
  ("f" elpy-format-code   "Format code")
  )

(major-mode-hydra-bind org-mode "Org Mode"
  ("t" (funcall wolfe/hydra-org-expand) "Expand template"))

(setq wolfe/hydra-org-expand
      (defhydra hydra-org-template (:color blue :hint nil)
        "
          _c_enter  _q_uote    _L_aTeX:
          _l_atex   _e_xample  _i_ndex:
          _a_scii   _v_erse    _I_NCLUDE:
          _s_rc     _t_angle   _H_TML:
          _h_tml    _d_ot src  _A_SCII:
          "
        ("s" (hot-expand "<s"))
        ("e" (hot-expand "<e"))
        ("q" (hot-expand "<q"))
        ("v" (hot-expand "<v"))
        ("t" (hot-expand "<s" "emacs-lisp :tangle yes"))
        ("d" (hot-expand "<s" "dot :file TMP.png :cmdline -Kdot -Tpng"))
        ("c" (hot-expand "<c"))
        ("l" (hot-expand "<l"))
        ("h" (hot-expand "<h"))
        ("a" (hot-expand "<a"))
        ("L" (hot-expand "<L"))
        ("i" (hot-expand "<i"))
        ("I" (hot-expand "<I"))
        ("H" (hot-expand "<H"))
        ("A" (hot-expand "<A"))))

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
