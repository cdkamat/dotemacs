;; Auto-complete configuration

(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat el-get-dir "/auto-complete/dict/"))
(setq ac-comphist-file (concat cache-dir "ac-comphist.dat"))
(ac-config-default)
(ac-flyspell-workaround)
(setq ac-auto-start nil
      ac-auto-show-menu t
      ac-dwim t
      ac-use-menu-map t
      ac-quick-help-delay 5
      ac-quick-help-height 60)

;;----------------------------------------------------------------------------
;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
;;----------------------------------------------------------------------------
(setq tab-always-indent 'complete)  ;; use 'complete when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)

;; hook AC into completion-at-point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(dolist (mode '(magit-log-edit-mode org-mode text-mode asm-mode latex-mode))
  (add-to-list 'ac-modes mode))

;; Key Bindings
(ac-set-trigger-key "TAB")
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(add-to-list 'ac-sources 'ac-source-semantic)
(define-key ac-completing-map [tab] 'ac-complete)

(provide 'auto-comp-config)
