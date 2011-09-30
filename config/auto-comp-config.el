;; Auto-complete configuration
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete/ac-dict")

(setq ac-comphist-file (concat cache-dir "ac-comphist.dat"))
(ac-flyspell-workaround)
(global-auto-complete-mode t)
(ac-config-default)
(setq ac-auto-start 3
      ac-auto-show-menu 1
      ac-use-menu-map t)

(dolist (mode '(org-mode text-mode asm-mode))
  (add-to-list 'ac-modes mode))

;; Key Bindings
(ac-set-trigger-key "TAB")
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(define-key ac-completing-map [tab] 'ac-complete)

(provide 'auto-comp-config)
