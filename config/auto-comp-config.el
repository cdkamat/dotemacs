;; Auto-complete configuration
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete/ac-dict")

(ac-config-default)
(setq ac-comphist-file (concat cache-dir "ac-comphist.dat"))
(ac-flyspell-workaround)
(global-auto-complete-mode t)
(setq ac-auto-show-menu t
      ac-dwim t
      ac-use-menu-map t
      ac-quick-help-delay 1
      ac-quick-help-height 60)

(dolist (mode '(magit-log-edit-mode org-mode text-mode asm-mode))
  (add-to-list 'ac-modes mode))

;; Key Bindings
(ac-set-trigger-key "TAB")
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(define-key ac-completing-map [tab] 'ac-complete)

(provide 'auto-comp-config)
