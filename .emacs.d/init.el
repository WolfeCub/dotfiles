(package-initialize)

(org-babel-load-file "~/.emacs.d/README.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("398f0209bfd642cf7a5e3e03bdc20db2822fd6746225a4bd99ccf9b26d3059d0" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "06dbcfac3705aaaa79e1a3264c6fd44ef0cf86ef5ed67930e4007e63a8c1e8ee" default)))
 '(package-selected-packages
   (quote
    (ox-twbs company neotree spaceline web-mode use-package tablist smex s powerline php+-mode org-bullets magit let-alist ido-ubiquitous ido-complete-space-or-hyphen haskell-mode expand-region doom-themes ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#383d48"))))
 '(company-scrollbar-fg ((t (:background "#2d313a"))))
 '(company-tooltip ((t (:inherit default :background "#262931"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))
